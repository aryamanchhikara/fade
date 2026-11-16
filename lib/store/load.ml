open! Core
open Fade_types

(* Build the analysis input from the cache. Only markets that were sampled for candles
   or trades are loaded; the rest of [markets] exists to define the population. *)

let col_int = function
  | Sqlite3.Data.INT i -> Some (Int64.to_int_exn i)
  | _ -> None
;;

let col_int_exn d = Option.value_exn (col_int d)

let col_float = function
  | Sqlite3.Data.FLOAT f -> Some f
  | Sqlite3.Data.INT i -> Some (Int64.to_float i)
  | _ -> None
;;

let col_text = function
  | Sqlite3.Data.TEXT s -> s
  | _ -> ""
;;

(* Kalshi's naming convention: an event ticker starts with its series ticker. *)
let series_prefix event_ticker =
  match String.lsplit2 event_ticker ~on:'-' with
  | Some (s, _) -> s
  | None -> event_ticker
;;

module Stats = struct
  type t =
    { markets_loaded : int
    ; markets_without_event_metadata : int
    ; event_driven_series : int
    ; scheduled_series : int
    }
  [@@deriving sexp_of]
end

let close_kinds store =
  (* Share of each series' in-scope markets that closed off the whole-minute grid.
     Uses every listed market in scope, not only sampled ones, and only timestamps. *)
  Db.rows
    store
    "SELECT substr(event_ticker, 1, instr(event_ticker || '-', '-') - 1), AVG(close_ts % \
     60 <> 0) FROM markets GROUP BY 1"
    []
  |> List.map ~f:(fun r ->
    let share = Option.value (col_float r.(1)) ~default:0. in
    ( col_text r.(0)
    , if Float.( >= ) share Close_kind.off_grid_threshold
      then Close_kind.Event_driven
      else Scheduled ))
  |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
;;

let fee_type_exn s =
  match Fee_type.of_kalshi_string s with
  | Some t -> t
  | None -> raise_s [%message "unknown fee type" (s : string)]
;;

let fees store ~series_used : Fee_schedule.t =
  let changes =
    Db.rows
      store
      "SELECT series_ticker, fee_type, fee_multiplier, ts FROM series_fee_changes ORDER \
       BY ts"
      []
    |> List.map ~f:(fun r ->
      ( col_text r.(0)
      , { Fee_schedule.Change.ts = col_int_exn r.(3)
        ; fee_type = Some (fee_type_exn (col_text r.(1)))
        ; multiplier = col_float r.(2)
        } ))
    |> String.Map.of_alist_multi
  in
  let series =
    Db.rows store "SELECT ticker, fee_type, fee_multiplier FROM series" []
    |> List.filter_map ~f:(fun r ->
      let ticker = col_text r.(0) in
      if Set.mem series_used ticker
      then
        Some
          { Fee_schedule.Series.series_ticker = ticker
          ; current_type = fee_type_exn (col_text r.(1))
          ; current_multiplier = Option.value (col_float r.(2)) ~default:1.
          ; changes = Map.find changes ticker |> Option.value ~default:[]
          }
      else None)
  in
  let event_overrides =
    Db.rows
      store
      "SELECT event_ticker, fee_type, fee_multiplier, ts FROM event_fee_changes ORDER BY \
       ts"
      []
    |> List.map ~f:(fun r ->
      ( col_text r.(0)
      , { Fee_schedule.Change.ts = col_int_exn r.(3)
        ; fee_type =
            (match r.(1) with
             | Sqlite3.Data.TEXT s -> Some (fee_type_exn s)
             | _ -> None)
        ; multiplier = col_float r.(2)
        } ))
    |> String.Map.of_alist_multi
    |> Map.to_alist
    |> List.map ~f:(fun (event_ticker, changes) ->
      { Fee_schedule.Event_override.event_ticker; changes })
  in
  { series; event_overrides }
;;

let dataset store ~(anchor : Anchor.t) : Dataset.t * Stats.t =
  let close_kinds = close_kinds store in
  let sampled =
    "(SELECT ticker FROM candles_done UNION SELECT ticker FROM trades_done)"
  in
  let event_sizes =
    Db.rows
      store
      (sprintf
         "SELECT event_ticker, COUNT(*) FROM listed WHERE event_ticker IN (SELECT \
          event_ticker FROM markets WHERE ticker IN %s) GROUP BY event_ticker"
         sampled)
      []
    |> List.map ~f:(fun r -> col_text r.(0), col_int_exn r.(1))
    |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
  in
  let missing_meta = ref 0 in
  let markets =
    Db.rows
      store
      (sprintf
         "SELECT m.ticker, m.event_ticker, e.series_ticker, s.category, m.open_ts, \
          m.close_ts, m.settle_ts, m.result, m.volume, m.can_close_early, \
          m.expected_expiration_ts, e.mutually_exclusive FROM markets m LEFT JOIN events \
          e USING (event_ticker) LEFT JOIN series s ON s.ticker = \
          COALESCE(e.series_ticker, substr(m.event_ticker, 1, instr(m.event_ticker || \
          '-', '-') - 1)) WHERE m.ticker IN %s"
         sampled)
      []
    |> List.filter_map ~f:(fun r ->
      let event_ticker = col_text r.(1) in
      let series_ticker =
        match r.(2) with
        | Sqlite3.Data.TEXT s -> s
        | _ ->
          incr missing_meta;
          series_prefix event_ticker
      in
      let category =
        match r.(3) with
        | Sqlite3.Data.TEXT c when not (String.is_empty c) -> c
        | _ -> "Unknown"
      in
      let mutually_exclusive =
        Option.value_map (col_int r.(11)) ~default:false ~f:(( = ) 1)
      in
      let n_markets = Map.find event_sizes event_ticker |> Option.value ~default:1 in
      Option.map
        (Side.of_kalshi_string (col_text r.(7)))
        ~f:(fun result ->
          { Market.ticker = col_text r.(0)
          ; event_ticker
          ; series_ticker
          ; category
          ; open_time = col_int_exn r.(4)
          ; close_time = col_int_exn r.(5)
          ; settle_time = col_int_exn r.(6)
          ; result
          ; volume = Option.value (col_float r.(8)) ~default:0.
          ; can_close_early = Option.value_map (col_int r.(9)) ~default:false ~f:(( = ) 1)
          ; expected_expiration = col_int r.(10)
          ; close_kind =
              Map.find close_kinds (series_prefix event_ticker)
              |> Option.value ~default:Close_kind.Scheduled
          ; event_kind = Event_kind.classify ~n_markets ~mutually_exclusive
          }))
  in
  let by_ticker =
    String.Map.of_alist_reduce
      (List.map markets ~f:(fun m -> m.ticker, m))
      ~f:(fun a _ -> a)
  in
  (* Candles -> snapshots, streaming row by row (there can be millions). *)
  let snapshots = ref [] in
  let flush ticker candles =
    match Map.find by_ticker ticker with
    | None -> ()
    | Some (m : Market.t) ->
      let anchor =
        Anchor.time
          anchor
          ~close_kind:m.close_kind
          ~close_time:m.close_time
          ~expected_expiration:m.expected_expiration
      in
      snapshots
      := List.rev_append
           (Fade_analysis.Snapshots.of_candles
              ~anchor
              ~ticker
              ~open_time:m.open_time
              ~close_time:m.close_time
              candles)
           !snapshots
  in
  let current = ref None in
  let acc = ref [] in
  Db.iter
    store
    "SELECT ticker, end_ts, yes_bid, yes_ask FROM candles ORDER BY ticker, end_ts"
    []
    ~f:(fun r ->
      let ticker = col_text r.(0) in
      (match !current with
       | Some t when String.equal t ticker -> ()
       | Some t ->
         flush t !acc;
         acc := []
       | None -> ());
      current := Some ticker;
      acc
      := { Candle.end_ts = col_int_exn r.(1)
         ; yes_bid = col_float r.(2)
         ; yes_ask = col_float r.(3)
         }
         :: !acc);
  Option.iter !current ~f:(fun t -> flush t !acc);
  let trades =
    Db.rows
      store
      "SELECT trade_id, ticker, ts, yes_price, count, taker_side, is_block FROM trades \
       WHERE ticker IN (SELECT ticker FROM trades_done)"
      []
    |> List.filter_map ~f:(fun r ->
      Option.map
        (Side.of_kalshi_string (String.lowercase (col_text r.(5))))
        ~f:(fun taker_side ->
          { Trade.trade_id = col_text r.(0)
          ; ticker = col_text r.(1)
          ; ts = col_int_exn r.(2)
          ; yes_price = Option.value_exn (col_float r.(3))
          ; count = Option.value_exn (col_float r.(4))
          ; taker_side
          ; is_block = Option.value_map (col_int r.(6)) ~default:false ~f:(( = ) 1)
          }))
  in
  let series_used = String.Set.of_list (List.map markets ~f:(fun m -> m.series_ticker)) in
  let event_driven = Map.count close_kinds ~f:(Close_kind.equal Event_driven) in
  ( { markets; snapshots = !snapshots; trades; fees = fees store ~series_used }
  , { markets_loaded = List.length markets
    ; markets_without_event_metadata = !missing_meta
    ; event_driven_series = event_driven
    ; scheduled_series = Map.length close_kinds - event_driven
    } )
;;
