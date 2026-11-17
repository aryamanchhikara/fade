open! Core
open! Async
open Fade_types
module E = Kalshi.Endpoints
module Store = Fade_store

(* `fade forward`'s fetch: every in-scope market that settled in the candidate's forward
   window (the whole population, not a sample), its hourly candles, and the trades of
   markets where a maker rule would have rested a bid. Uses its own cache, so the
   pinned study cache is never touched. Safe to run repeatedly: each run lists from two
   days before the previous listing and skips what is already fetched. *)

let log = Fetch.log

let run ~db_path ~(candidate : Candidate.t) ~requests_per_second ~now =
  let config = candidate.config in
  let start, stop = Fade_analysis.Forward_builder.window config in
  let store = Store.open_ db_path in
  (match Store.get_meta store "candidate_hash" with
   | Some h when not (String.equal h candidate.hash) ->
     raise_s
       [%message
         "this forward cache belongs to a different candidate file; use another --db"
           (db_path : string)
           ~cache:(h : string)
           ~candidate:(candidate.hash : string)]
   | Some _ -> ()
   | None -> Store.set_meta store "candidate_hash" candidate.hash);
  let client = Kalshi.Client.create ~requests_per_second () in
  let%bind current = E.cutoff client >>| ok_exn in
  let market_cutoff = E.ts_exn current.market_settled_ts in
  let trades_cutoff = E.ts_exn current.trades_created_ts in
  if market_cutoff > start
  then
    log
      "warning: Kalshi has archived markets settled before %s; markets in the forward \
       window settled before then are only found if an earlier run listed them"
      (Unix_time.to_rfc3339 market_cutoff);
  let%bind () = Fetch.reference store client in
  let since =
    match
      Option.bind (Store.get_meta store "forward_listed_through") ~f:Int.of_string_opt
    with
    | Some t -> Int.max start (t - (2 * 86_400))
    | None -> start
  in
  log
    "forward window %s to %s; listing markets settled since %s"
    config.forward_start
    (Unix_time.to_date_string (stop - 86_400))
    (Unix_time.to_rfc3339 since);
  let%bind () =
    if since >= stop
    then return ()
    else
      Fetch.live_markets
        store
        client
        ~since
        ~until:stop
        ~cutoff:since
        ~min_volume:config.min_volume
  in
  Store.set_meta store "forward_listed_through" (Int.to_string now);
  let%bind () = Fetch.events store client ~fraction:1. ~trade_sample:0 in
  let%bind () = Fetch.candles store client ~fraction:1. ~market_cutoff in
  (* Trades only where a maker rule placed an order: that is all fills need. *)
  let d, _ = Store.Load.dataset ~complete_prefix:false store ~anchor:config.anchor in
  let fees = Fade_analysis.Fees.Index.create d.fees in
  let done_ =
    Store.rows store "SELECT ticker FROM trades_done" []
    |> List.map ~f:(fun r -> Fetch.col_text r.(0))
    |> String.Hash_set.of_list
  in
  let pending =
    List.concat_map candidate.rules ~f:(fun rule ->
      match rule.execution with
      | Taker_at_ask -> []
      | Maker_bid _ ->
        Fade_analysis.Candidate_score.orders
          rule
          ~anchor:config.anchor
          ~fees
          ~d
          ~include_market:(Fade_analysis.Forward_builder.in_window config)
          ~traded:(fun _ -> true))
    |> List.filter_map ~f:(fun (o : Fade_analysis.Candidate_score.order) ->
      if Hash_set.mem done_ o.market.ticker
      then None
      else Some (o.market.ticker, o.market.open_time, o.market.close_time))
    |> List.dedup_and_sort ~compare:[%compare: string * int * int]
  in
  let%map () =
    Fetch.trades_of_pending store client ~pending ~max_pages:20 ~trades_cutoff
  in
  log
    "forward cache: %d markets, %d with candles, %d with trades (%d requests, %d retries)"
    (Store.count store "SELECT COUNT(*) FROM markets" [])
    (Store.count store "SELECT COUNT(*) FROM candles_done" [])
    (Store.count store "SELECT COUNT(*) FROM trades_done" [])
    (Kalshi.Client.requests_sent client)
    (Kalshi.Client.retries client);
  Store.close store
;;
