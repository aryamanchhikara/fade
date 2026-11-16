open! Core
open! Async
open Fade_types
module E = Kalshi.Endpoints
module Store = Fade_store

module Stage = struct
  type t =
    | Reference
    | Markets
    | Events
    | Candles
    | Trades
  [@@deriving sexp, compare, equal, enumerate]

  let of_string s = t_of_sexp (Sexp.Atom (String.capitalize s))
end

module Config = struct
  type t =
    { db_path : string
    ; since : Unix_time.t
    ; min_volume : float
    ; candle_fraction : float
      (** fetch candles for markets whose [sample_key] falls in this fraction of the
              key space: a Bernoulli sample, independent of listing order *)
    ; trade_sample : int
    ; max_trade_pages : int
    ; requests_per_second : float
    ; stages : Stage.t list
    }
end

(* Shorthands for SQLite parameters. *)
let text s = Sqlite3.Data.TEXT s
let int i = Sqlite3.Data.INT (Int64.of_int i)
let float f = Sqlite3.Data.FLOAT f
let bool b = int (if b then 1 else 0)

let opt f = function
  | None -> Sqlite3.Data.NULL
  | Some x -> f x
;;

let col_int = function
  | Sqlite3.Data.INT i -> Int64.to_int_exn i
  | d -> raise_s [%message "expected INT" (Sqlite3.Data.to_string_debug d : string)]
;;

let col_text = function
  | Sqlite3.Data.TEXT s -> s
  | d -> raise_s [%message "expected TEXT" (Sqlite3.Data.to_string_debug d : string)]
;;

(* A stable pseudo-random key per ticker, uniform on [0, 2^52). The candle sample is
   every market with key below [fraction * 2^52] (so it can be drawn while listing is
   still running), the trade sample is the first N markets by key, and work is done in
   key order so any prefix of it is itself a uniform random sample. *)
let sample_key ticker =
  let hex = Md5.to_hex (Md5.digest_string ticker) in
  Int.of_string ("0x" ^ String.prefix hex 13)
;;

let key_space = 1 lsl 52
let key_threshold fraction = Float.to_int (Float.of_int key_space *. fraction)
let log fmt = ksprintf (fun s -> eprintf "%s\n%!" s) fmt

(* --- reference data ---------------------------------------------------------- *)

let reference store client =
  let%bind series = E.series client >>| ok_exn in
  Store.transaction store ~f:(fun () ->
    List.iter series.series ~f:(fun s ->
      Store.exec
        store
        "INSERT INTO series(ticker, category, fee_type, fee_multiplier) VALUES (?,?,?,?) \
         ON CONFLICT(ticker) DO UPDATE SET category = excluded.category, fee_type = \
         excluded.fee_type, fee_multiplier = excluded.fee_multiplier"
        [ text s.ticker; text s.category; text s.fee_type; float s.fee_multiplier ]));
  let%bind changes = E.series_fee_changes client >>| ok_exn in
  Store.transaction store ~f:(fun () ->
    List.iter changes.series_fee_change_arr ~f:(fun c ->
      Store.exec
        store
        "INSERT OR REPLACE INTO series_fee_changes(id, series_ticker, fee_type, \
         fee_multiplier, ts) VALUES (?,?,?,?,?)"
        [ text c.id
        ; text c.series_ticker
        ; text c.fee_type
        ; float c.fee_multiplier
        ; int (E.ts_exn c.scheduled_ts)
        ]));
  let rec event_changes cursor n =
    let%bind page = E.event_fee_changes client ~cursor >>| ok_exn in
    Store.transaction store ~f:(fun () ->
      List.iter page.event_fee_changes ~f:(fun c ->
        Store.exec
          store
          "INSERT OR REPLACE INTO event_fee_changes(id, event_ticker, fee_type, \
           fee_multiplier, ts) VALUES (?,?,?,?,?)"
          [ text c.id
          ; text c.event_ticker
          ; opt text c.fee_type_override
          ; opt float c.fee_multiplier_override
          ; int (E.ts_exn c.scheduled_ts)
          ]));
    let n = n + List.length page.event_fee_changes in
    if String.is_empty page.cursor then return n else event_changes page.cursor n
  in
  let%map n_event_changes = event_changes "" 0 in
  log
    "reference: %d series, %d series fee changes, %d event fee changes"
    (List.length series.series)
    (List.length changes.series_fee_change_arr)
    n_event_changes
;;

(* --- markets ------------------------------------------------------------------ *)

let settle_ts (m : E.Market_raw.t) =
  E.ts_exn (Option.value m.settlement_ts ~default:m.close_time)
;;

let qualifies ~since ~until ~min_volume (m : E.Market_raw.t) =
  String.equal m.market_type "binary"
  && List.mem [ "yes"; "no" ] m.result ~equal:String.equal
  && Float.( >= ) (Float.of_string m.volume_fp) min_volume
  &&
  let s = settle_ts m in
  s >= since && s < until
;;

let save_page store ~source ~since ~until ~min_volume (markets : E.Market_raw.t list) =
  Store.transaction store ~f:(fun () ->
    List.fold markets ~init:0 ~f:(fun kept (m : E.Market_raw.t) ->
      Store.exec
        store
        "INSERT OR IGNORE INTO listed(ticker, event_ticker) VALUES (?,?)"
        [ text m.ticker; text m.event_ticker ];
      if qualifies ~since ~until ~min_volume m
      then (
        let ts s = opt int (Option.bind s ~f:Unix_time.of_rfc3339) in
        Store.exec
          store
          "INSERT INTO markets(ticker, event_ticker, open_ts, close_ts, settle_ts, \
           result, volume, can_close_early, expected_expiration_ts, \
           latest_expiration_ts, occurrence_ts, source, sample_key) VALUES \
           (?,?,?,?,?,?,?,?,?,?,?,?,?) ON CONFLICT(ticker) DO UPDATE SET volume = \
           excluded.volume, settle_ts = excluded.settle_ts, result = excluded.result"
          [ text m.ticker
          ; text m.event_ticker
          ; int (E.ts_exn m.open_time)
          ; int (E.ts_exn m.close_time)
          ; int (settle_ts m)
          ; text m.result
          ; float (Float.of_string m.volume_fp)
          ; bool m.can_close_early
          ; ts m.expected_expiration_time
          ; ts m.latest_expiration_time
          ; ts m.occurrence_datetime
          ; text source
          ; int (sample_key m.ticker)
          ];
        kept + 1)
      else kept))
;;

let historical_markets store client ~since ~cutoff ~min_volume =
  if Option.is_some (Store.get_meta store "historical_markets_done")
  then return (log "historical markets: already listed")
  else (
    let progress = Progress.create "historical mkts" in
    (* Listing is newest-created first. Markets settling in the window can have been
       created before it, so keep going until creation dates are a year before [since].
       Markets created more than a year before they settle are rare and are missed. *)
    let stop_before = since - (365 * 86_400) in
    let rec loop cursor kept_total =
      let%bind page = E.historical_markets client ~cursor >>| ok_exn in
      let kept =
        save_page store ~source:"historical" ~since ~until:cutoff ~min_volume page.markets
      in
      let oldest =
        List.map page.markets ~f:(fun m -> E.ts_exn m.created_time)
        |> List.min_elt ~compare:Int.compare
      in
      Store.set_meta store "historical_markets_cursor" page.cursor;
      let kept_total = kept_total + kept in
      Progress.advance
        progress
        ~note:
          (sprintf
             "pages; %d in scope; created back to %s"
             kept_total
             (Option.value_map oldest ~default:"?" ~f:Unix_time.to_date_string));
      let finished =
        String.is_empty page.cursor
        || Option.value_map oldest ~default:true ~f:(fun o -> o < stop_before)
      in
      if finished
      then (
        Store.set_meta store "historical_markets_done" "1";
        Progress.finish progress;
        return ())
      else loop page.cursor kept_total
    in
    loop (Option.value (Store.get_meta store "historical_markets_cursor") ~default:"") 0)
;;

let live_markets store client ~since ~cutoff ~min_volume =
  let progress = Progress.create "live mkts" in
  let min_settled_ts = Int.max since cutoff in
  let rec loop cursor kept_total =
    let%bind page = E.settled_markets client ~min_settled_ts ~cursor >>| ok_exn in
    let kept =
      save_page store ~source:"live" ~since ~until:Int.max_value ~min_volume page.markets
    in
    let kept_total = kept_total + kept in
    Progress.advance progress ~note:(sprintf "pages; %d in scope" kept_total);
    if String.is_empty page.cursor
    then (
      Progress.finish progress;
      return ())
    else loop page.cursor kept_total
  in
  loop "" 0
;;

(* --- events ------------------------------------------------------------------- *)

let events store client ~fraction ~trade_sample =
  (* Only events of sampled markets (the candle and trade samples) are needed. *)
  let missing =
    Store.rows
      store
      "SELECT DISTINCT m.event_ticker FROM markets m LEFT JOIN events e USING \
       (event_ticker) WHERE e.event_ticker IS NULL AND (m.sample_key < ? OR m.ticker IN \
       (SELECT ticker FROM markets ORDER BY sample_key LIMIT ?))"
      [ int (key_threshold fraction); int trade_sample ]
    |> List.map ~f:(fun r -> col_text r.(0))
  in
  let progress = Progress.create ~total:(List.length missing) "events" in
  let%map () =
    Deferred.List.iter
      ~how:(`Max_concurrent_jobs 2)
      (List.chunks_of missing ~length:100)
      ~f:(fun tickers ->
        match%map E.events client ~tickers with
        | Error e -> log "events batch failed: %s" (Error.to_string_hum e)
        | Ok page ->
          let returned =
            String.Set.of_list (List.map page.events ~f:(fun e -> e.event_ticker))
          in
          Store.transaction store ~f:(fun () ->
            List.iter page.events ~f:(fun e ->
              Store.exec
                store
                "INSERT OR REPLACE INTO events(event_ticker, series_ticker, \
                 mutually_exclusive, title) VALUES (?,?,?,?)"
                [ text e.event_ticker
                ; text e.series_ticker
                ; bool e.mutually_exclusive
                ; text e.title
                ]);
            (* Not returned: fall back to the ticker prefix, which is the series by
               Kalshi's naming convention, and record that we guessed. *)
            List.iter tickers ~f:(fun t ->
              if not (Set.mem returned t)
              then
                Store.exec
                  store
                  "INSERT OR IGNORE INTO events(event_ticker, series_ticker, \
                   mutually_exclusive, title) VALUES (?,?,0,NULL)"
                  [ text t; text (List.hd_exn (String.split t ~on:'-')) ]));
          Progress.advance progress ~by:(List.length tickers))
  in
  Progress.finish progress
;;

(* --- candles ------------------------------------------------------------------ *)

type pending =
  { ticker : string
  ; open_ts : int
  ; close_ts : int
  ; settle_ts : int
  ; expected_ts : int option
  }

let save_candles store ~ticker (candles : Fade_types.Candle.t list) =
  Store.transaction store ~f:(fun () ->
    List.iter candles ~f:(fun (c : Fade_types.Candle.t) ->
      Store.exec
        store
        "INSERT OR REPLACE INTO candles(ticker, end_ts, yes_bid, yes_ask) VALUES \
         (?,?,?,?)"
        [ text ticker; int c.end_ts; opt float c.yes_bid; opt float c.yes_ask ]);
    Store.exec
      store
      "INSERT OR REPLACE INTO candles_done(ticker, n_candles) VALUES (?,?)"
      [ text ticker; int (List.length candles) ])
;;

let one_historical store client progress errors (p : pending) =
  let start_ts, end_ts =
    Fade_analysis.Snapshots.window
      ?expected_expiration:p.expected_ts
      ~open_time:p.open_ts
      ~close_time:p.close_ts
      ()
  in
  match%map E.historical_candles client ~ticker:p.ticker ~start_ts ~end_ts with
  | Error e ->
    incr errors;
    if !errors <= 5 then log "\ncandles %s: %s" p.ticker (Error.to_string_hum e);
    Progress.advance progress
  | Ok candles ->
    save_candles store ~ticker:p.ticker candles;
    Progress.advance progress
;;

(* Group live markets into batch requests: at most 100 tickers, and at most 10,000
   hourly periods in total across the shared time window. *)
let live_batches (ps : pending list) =
  let ps = List.sort ps ~compare:(fun a b -> Int.compare a.close_ts b.close_ts) in
  let periods ~start ~stop n = (((stop - start) / 3600) + 2) * n in
  List.fold ps ~init:[] ~f:(fun acc p ->
    let s, e =
      Fade_analysis.Snapshots.window
        ?expected_expiration:p.expected_ts
        ~open_time:p.open_ts
        ~close_time:p.close_ts
        ()
    in
    match acc with
    | (start, stop, batch) :: rest
      when List.length batch < 100
           && periods
                ~start:(Int.min start s)
                ~stop:(Int.max stop e)
                (List.length batch + 1)
              <= 10_000 -> (Int.min start s, Int.max stop e, p :: batch) :: rest
    | _ -> (s, e, [ p ]) :: acc)
  |> List.rev
;;

let candles store client ~fraction ~market_cutoff =
  let pending =
    Store.rows
      store
      "SELECT s.ticker, s.open_ts, s.close_ts, s.settle_ts, s.expected_expiration_ts \
       FROM markets s LEFT JOIN candles_done c USING (ticker) WHERE c.ticker IS NULL AND \
       s.sample_key < ? ORDER BY s.sample_key"
      [ int (key_threshold fraction) ]
    |> List.map ~f:(fun r ->
      { ticker = col_text r.(0)
      ; open_ts = col_int r.(1)
      ; close_ts = col_int r.(2)
      ; settle_ts = col_int r.(3)
      ; expected_ts =
          (match r.(4) with
           | Sqlite3.Data.INT i -> Some (Int64.to_int_exn i)
           | _ -> None)
      })
  in
  (* A market that opened less than an hour before close has no horizon to read. *)
  let too_short, pending =
    List.partition_tf pending ~f:(fun p -> p.close_ts - p.open_ts < 3600)
  in
  List.iter too_short ~f:(fun p -> save_candles store ~ticker:p.ticker []);
  (* Settled after the archive cutoff: the live batch endpoint has the candles. *)
  let live, historical =
    List.partition_tf pending ~f:(fun p -> p.settle_ts >= market_cutoff)
  in
  let errors = ref 0 in
  let progress = Progress.create ~total:(List.length pending) (sprintf "candles") in
  (* Live markets first, before the archive cutoff moves past them. *)
  let%bind () =
    Deferred.List.iter
      ~how:(`Max_concurrent_jobs 2)
      (live_batches live)
      ~f:(fun (start_ts, end_ts, batch) ->
        let tickers = List.map batch ~f:(fun p -> p.ticker) in
        let%bind result = E.batch_candles client ~tickers ~start_ts ~end_ts in
        let by_ticker =
          match result with
          | Ok rows -> String.Map.of_alist_reduce rows ~f:( @ )
          | Error e ->
            log "\nbatch candles failed: %s" (Error.to_string_hum e);
            String.Map.empty
        in
        Deferred.List.iter ~how:`Sequential batch ~f:(fun p ->
          match Map.find by_ticker p.ticker with
          | Some (_ :: _ as candles) ->
            save_candles store ~ticker:p.ticker candles;
            Progress.advance progress;
            return ()
          | _ ->
            (* Archived since we listed it: ask the historical endpoint instead. *)
            one_historical store client progress errors p))
  in
  let%map () =
    Deferred.List.iter
      ~how:(`Max_concurrent_jobs 4)
      historical
      ~f:(one_historical store client progress errors)
  in
  Progress.finish progress;
  if !errors > 0 then log "candles: %d markets failed; rerun to retry them" !errors
;;

(* --- trades ------------------------------------------------------------------- *)

let trades store client ~sample ~max_pages ~trades_cutoff =
  let pending =
    Store.rows
      store
      "SELECT s.ticker, s.open_ts, s.close_ts FROM (SELECT ticker, open_ts, close_ts, \
       sample_key FROM markets ORDER BY sample_key LIMIT ?) s LEFT JOIN trades_done d \
       USING (ticker) WHERE d.ticker IS NULL ORDER BY s.sample_key"
      [ int sample ]
    |> List.map ~f:(fun r -> col_text r.(0), col_int r.(1), col_int r.(2))
  in
  let progress = Progress.create ~total:(List.length pending) "trades" in
  let errors = ref 0 in
  let fetch_all ~historical ticker =
    let rec loop cursor pages acc =
      match%bind E.trades client ~historical ~ticker ~cursor with
      | Error e -> return (Error e)
      | Ok page ->
        let acc = List.rev_append page.trades acc in
        if String.is_empty page.cursor
        then return (Ok (acc, false))
        else if pages + 1 >= max_pages
        then return (Ok (acc, true))
        else loop page.cursor (pages + 1) acc
    in
    loop "" 0 []
  in
  let%map () =
    Deferred.List.iter
      ~how:(`Max_concurrent_jobs 4)
      pending
      ~f:(fun (ticker, open_ts, close_ts) ->
        let%bind hist =
          if open_ts < trades_cutoff
          then fetch_all ~historical:true ticker
          else return (Ok ([], false))
        in
        let%map live =
          if close_ts >= trades_cutoff
          then fetch_all ~historical:false ticker
          else return (Ok ([], false))
        in
        match hist, live with
        | Ok (h, ht), Ok (l, lt) ->
          let all = List.filter_map (h @ l) ~f:E.trade_of_raw in
          Store.transaction store ~f:(fun () ->
            List.iter all ~f:(fun (t : Trade.t) ->
              Store.exec
                store
                "INSERT OR IGNORE INTO trades(trade_id, ticker, ts, yes_price, count, \
                 taker_side, is_block) VALUES (?,?,?,?,?,?,?)"
                [ text t.trade_id
                ; text t.ticker
                ; int t.ts
                ; float t.yes_price
                ; float t.count
                ; text (Side.to_string t.taker_side)
                ; bool t.is_block
                ]);
            Store.exec
              store
              "INSERT OR REPLACE INTO trades_done(ticker, n_trades, truncated) VALUES \
               (?,?,?)"
              [ text ticker; int (List.length all); bool (ht || lt) ]);
          Progress.advance progress
        | Error e, _ | _, Error e ->
          incr errors;
          if !errors <= 5 then log "\ntrades %s: %s" ticker (Error.to_string_hum e);
          Progress.advance progress)
  in
  Progress.finish progress;
  if !errors > 0 then log "trades: %d markets failed; rerun to retry them" !errors
;;

(* --- driver ------------------------------------------------------------------- *)

let run (config : Config.t) =
  let store = Store.open_ config.db_path in
  let client = Kalshi.Client.create ~requests_per_second:config.requests_per_second () in
  let since_s = Unix_time.to_date_string config.since in
  (match Store.get_meta store "since" with
   | Some s when not (String.equal s since_s) ->
     log
       "warning: this cache was started with --since %s; keeping that window (delete %s \
        to change it)"
       s
       config.db_path
   | Some _ -> ()
   | None -> Store.set_meta store "since" since_s);
  let since =
    Unix_time.of_date_string_exn (Option.value_exn (Store.get_meta store "since"))
  in
  let%bind current = E.cutoff client >>| ok_exn in
  (* The market cutoff that partitions historical vs live listing is pinned on the
     first run, so a resumed listing uses the same boundary. *)
  let market_cutoff =
    match Store.get_meta store "market_cutoff" with
    | Some c -> E.ts_exn c
    | None ->
      Store.set_meta store "market_cutoff" current.market_settled_ts;
      E.ts_exn current.market_settled_ts
  in
  let trades_cutoff = E.ts_exn current.trades_created_ts in
  log
    "window: settled since %s; archive cutoff %s; db %s"
    since_s
    (Unix_time.to_rfc3339 market_cutoff)
    config.db_path;
  let wants s = List.mem config.stages s ~equal:Stage.equal in
  let%bind () = if wants Reference then reference store client else return () in
  let%bind () =
    if wants Markets
    then (
      let%bind () =
        historical_markets
          store
          client
          ~since
          ~cutoff:market_cutoff
          ~min_volume:config.min_volume
      in
      let%map () =
        live_markets
          store
          client
          ~since
          ~cutoff:market_cutoff
          ~min_volume:config.min_volume
      in
      Store.set_meta
        store
        "last_listed"
        (Unix_time.to_rfc3339 (Float.to_int (Unix.time ()))))
    else return ()
  in
  let%bind () =
    if wants Events
    then
      events
        store
        client
        ~fraction:config.candle_fraction
        ~trade_sample:config.trade_sample
    else return ()
  in
  let%bind () =
    if wants Candles
    then candles store client ~fraction:config.candle_fraction ~market_cutoff
    else return ()
  in
  let%map () =
    if wants Trades
    then
      trades
        store
        client
        ~sample:config.trade_sample
        ~max_pages:config.max_trade_pages
        ~trades_cutoff
    else return ()
  in
  log
    "done: %d markets in scope, %d with candles fetched, %d candles, %d trades (%d \
     requests, %d retries)"
    (Store.count store "SELECT COUNT(*) FROM markets" [])
    (Store.count store "SELECT COUNT(*) FROM candles_done" [])
    (Store.count store "SELECT COUNT(*) FROM candles" [])
    (Store.count store "SELECT COUNT(*) FROM trades" [])
    (Kalshi.Client.requests_sent client)
    (Kalshi.Client.retries client);
  Store.close store
;;

let%expect_test "live candle batches respect 100 tickers and 10,000 periods" =
  let mk i ~close =
    { ticker = sprintf "T%d" i
    ; open_ts = 0
    ; close_ts = close
    ; settle_ts = close
    ; expected_ts = None
    }
  in
  let hour = 3600 in
  (* 250 markets closing together, then 3 spread a week apart. *)
  let together = List.init 250 ~f:(fun i -> mk i ~close:(1000 * hour)) in
  let spread =
    List.init 3 ~f:(fun i -> mk (1000 + i) ~close:((1000 + (168 * (i + 1))) * hour))
  in
  live_batches (together @ spread)
  |> List.iter ~f:(fun (start, stop, batch) ->
    (* [Core.printf], not Async's buffered [printf], so ppx_expect sees it now. *)
    Core.printf
      "%d tickers, %d hours, %d periods\n"
      (List.length batch)
      ((stop - start) / hour)
      ((((stop - start) / hour) + 2) * List.length batch));
  [%expect
    {|
    100 tickers, 25 hours, 2700 periods
    100 tickers, 25 hours, 2700 periods
    51 tickers, 193 hours, 9945 periods
    2 tickers, 193 hours, 390 periods
    |}];
  (* With [open Async], expect tests return a [Deferred.t]. *)
  return ()
;;
