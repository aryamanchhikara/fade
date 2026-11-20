open! Core
open Fade_types
module Pit = Fade_analysis.Pit

(* Point-in-time rows for the predictive model, streamed from the cache one market at a
   time (all trades of 50,000 markets would not fit in memory at once).

   A market is eligible if it is in the quote sample, has candles and has complete
   trades (not truncated). Its decision times count back from the expected expiration
   (see [Anchor.Expected]). The label and the settlement time travel next to the
   features, never inside them. *)

module Row = Pit.Row

module Summary = struct
  type t =
    { sampled_with_candles : int
    ; truncated_trades : int (** excluded: volume to date unknown *)
    ; trades_missing : int (** excluded: not fetched yet *)
    ; event_markets_missing : int (** excluded: event's market list not fetched yet *)
    ; no_expected_expiration : int (** excluded: no scheduled time known in advance *)
    ; markets_loaded : int
    ; rows : int
    ; leakage_checked : int (** markets re-scored with everything after t deleted *)
    ; leakage_failures : int
    }
  [@@deriving sexp_of]
end

let col_int = Load.col_int
let col_int_exn = Load.col_int_exn
let col_float = Load.col_float
let col_text = Load.col_text

(* [require_event_markets:false] keeps markets whose event list is not fetched yet
   (their event size feature is then 0); only for checks that do not use it. *)
let load ?(leakage_every = 50) ?(require_event_markets = true) store ~fraction
  : Row.t list * Summary.t
  =
  let threshold = Float.iround_down_exn (fraction *. Float.of_int (1 lsl 52)) in
  let candidates =
    Db.rows
      store
      "SELECT m.ticker, m.event_ticker, m.open_ts, m.close_ts, m.settle_ts, m.result, \
       m.expected_expiration_ts, COALESCE(s.category, 'Unknown'), \
       COALESCE(e.mutually_exclusive, 0), COALESCE(md.truncated, d.truncated), \
       (x.event_ticker IS NOT NULL), COALESCE(e.series_ticker, substr(m.event_ticker, 1, \
       instr(m.event_ticker || '-', '-') - 1)) FROM markets m JOIN candles_done c USING \
       (ticker) LEFT JOIN trades_done d USING (ticker) LEFT JOIN model_trades_done md \
       USING (ticker) LEFT JOIN event_markets_done x ON x.event_ticker = m.event_ticker \
       LEFT JOIN events e ON e.event_ticker = m.event_ticker LEFT JOIN series s ON \
       s.ticker = COALESCE(e.series_ticker, substr(m.event_ticker, 1, \
       instr(m.event_ticker || '-', '-') - 1)) WHERE m.sample_key < ? AND c.n_candles > \
       0 ORDER BY m.sample_key"
      [ Sqlite3.Data.INT (Int64.of_int threshold) ]
  in
  let truncated = ref 0
  and trades_missing = ref 0
  and events_missing = ref 0
  and no_expected = ref 0
  and loaded = ref 0
  and checked = ref 0
  and failures = ref 0 in
  let event_cache = String.Table.create () in
  let event_created event_ticker =
    Hashtbl.find_or_add event_cache event_ticker ~default:(fun () ->
      Db.rows
        store
        "SELECT created_ts FROM event_markets WHERE event_ticker = ? ORDER BY created_ts"
        [ Sqlite3.Data.TEXT event_ticker ]
      |> List.map ~f:(fun r -> col_int_exn r.(0))
      |> Array.of_list)
  in
  let rows =
    List.concat_map candidates ~f:(fun r ->
      let ticker = col_text r.(0) in
      let event_ticker = col_text r.(1) in
      match col_int r.(9), col_int r.(10), col_int r.(6) with
      | None, _, _ ->
        incr trades_missing;
        []
      | Some 1, _, _ ->
        incr truncated;
        []
      | _, Some 0, _ when require_event_markets ->
        incr events_missing;
        []
      | _, _, None ->
        incr no_expected;
        []
      | Some _, _, Some expected ->
        incr loaded;
        let view : Pit.View.t =
          { ticker
          ; event_ticker
          ; category = col_text r.(7)
          ; open_time = col_int_exn r.(2)
          ; scheduled_close = expected
          ; mutually_exclusive =
              Option.value_map (col_int r.(8)) ~default:false ~f:(( = ) 1)
          }
        in
        let candles =
          Db.rows
            store
            "SELECT end_ts, yes_bid, yes_ask FROM candles WHERE ticker = ? ORDER BY \
             end_ts"
            [ Sqlite3.Data.TEXT ticker ]
          |> List.map ~f:(fun c ->
            { Candle.end_ts = col_int_exn c.(0)
            ; yes_bid = col_float c.(1)
            ; yes_ask = col_float c.(2)
            })
          |> Array.of_list
        in
        let trades =
          Db.rows
            store
            (* The study's trades plus any the model stage fetched on top. *)
            "SELECT trade_id, ts, yes_price, count, taker_side, is_block FROM trades \
             WHERE ticker = ? UNION SELECT trade_id, ts, yes_price, count, taker_side, \
             is_block FROM model_trades_extra WHERE ticker = ? ORDER BY 2, 1"
            [ Sqlite3.Data.TEXT ticker; Sqlite3.Data.TEXT ticker ]
          |> List.filter_map ~f:(fun t ->
            Option.map
              (Side.of_kalshi_string (String.lowercase (col_text t.(4))))
              ~f:(fun taker_side ->
                { Trade.trade_id = col_text t.(0)
                ; ticker
                ; ts = col_int_exn t.(1)
                ; yes_price = Option.value_exn (col_float t.(2))
                ; count = Option.value_exn (col_float t.(3))
                ; taker_side
                ; is_block = Option.value_map (col_int t.(5)) ~default:false ~f:(( = ) 1)
                }))
          |> Array.of_list
        in
        let pit : Pit.t =
          { view
          ; closed_at = Some (col_int_exn r.(3))
          ; candles
          ; trades
          ; event_created = event_created event_ticker
          }
        in
        (* The leakage check, on a fixed slice of markets. *)
        if String.hash ticker % leakage_every = 0
        then (
          incr checked;
          if not (List.is_empty (Pit.leaks pit)) then incr failures);
        let no_won = String.equal (col_text r.(5)) "no" in
        let settle_time = col_int_exn r.(4) in
        List.filter_map Horizon.all ~f:(fun horizon ->
          Pit.features pit ~horizon
          |> Option.map ~f:(fun features ->
            { Pit.Row.ticker
            ; event_ticker
            ; series_ticker = col_text r.(11)
            ; settle_time
            ; no_won
            ; features
            ; min_no_after = Pit.min_no_after pit ~after:features.asof
            })))
  in
  ( rows
  , { sampled_with_candles = List.length candidates
    ; truncated_trades = !truncated
    ; trades_missing = !trades_missing
    ; event_markets_missing = !events_missing
    ; no_expected_expiration = !no_expected
    ; markets_loaded = !loaded
    ; rows = List.length rows
    ; leakage_checked = !checked
    ; leakage_failures = !failures
    } )
;;
