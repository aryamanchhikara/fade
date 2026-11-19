open! Core
open Fade_types
open Fade_analysis

(* `fade recheck`: the study's out-of-time "buy NO at the ask, 1h before close" table
   again, three ways, to see whether the point-in-time fixes change it:
   1. as frozen (the study's anchor);
   2. every horizon counted back from the expected expiration (Anchor.Expected);
   3. as 2, and only markets that had traded at least 500 contracts by the decision
      (volume to date instead of lifetime volume). *)

let pct x = sprintf "%+.1f%%" (100. *. x)

let command =
  Command.basic
    ~summary:"Re-run the Phase 3 out-of-time 1h NO table with point-in-time fixes"
    (let%map_open.Command db =
       flag "--db" (optional_with_default "data/fade.db" string) ~doc:"PATH study cache"
     and frozen_path =
       flag
         "--frozen"
         (optional_with_default "rules/frozen.json" string)
         ~doc:"PATH study frozen rules"
     in
     fun () ->
       let study = Frozen.of_string (In_channel.read_all frozen_path) |> ok_exn in
       let store = Fade_store.open_ db in
       let threshold = Float.iround_down_exn (0.02 *. Float.of_int (1 lsl 52)) in
       let count sql =
         Fade_store.count store sql [ Sqlite3.Data.INT (Int64.of_int threshold) ]
       in
       printf
         "Quote sample (2%%): %d markets\n"
         (count "SELECT COUNT(*) FROM markets WHERE sample_key < ?");
       printf
         "  can close early: %d\n"
         (count
            "SELECT COUNT(*) FROM markets WHERE sample_key < ? AND can_close_early = 1");
       printf
         "  closed more than 1h before their expected expiration: %d\n"
         (count
            "SELECT COUNT(*) FROM markets WHERE sample_key < ? AND close_ts < \
             expected_expiration_ts - 3600");
       printf
         "  closed off the whole minute (an event-driven close): %d\n\n"
         (count
            "SELECT COUNT(*) FROM markets WHERE sample_key < ? AND close_ts % 60 <> 0");
       let table ~anchor ~keep =
         let d, _ =
           Fade_store.Load.dataset
             store
             ~anchor
             ~with_trades:false
             ~max_trade_sample:Data_source.study_trade_sample
         in
         let config = { study.config with anchor } in
         let d = Report_builder.restrict ~config d in
         let split = Split.create ~config d.markets in
         let fees = Fees.Index.create d.fees in
         let drops = Observation.Drops.create () in
         let obs =
           Observation.of_quotes d ~config ~fees ~drops
           |> List.filter ~f:(fun (o : Observation.t) ->
             Side.equal o.side No && Horizon.equal o.horizon H1 && keep o)
         in
         let tables = Tables.build ~config ~split ~observations:[ Quotes, obs ] () in
         let key : Table.Key.t =
           { sample = Out_of_time; view = Quotes; side = No; horizon = H1; segment = All }
         in
         Option.value_map (Tables.find tables key) ~default:[] ~f:(fun t -> t.buckets)
       in
       let frozen = table ~anchor:study.config.anchor ~keep:(fun _ -> true) in
       let expected = table ~anchor:Expected ~keep:(fun _ -> true) in
       let rows, summary =
         Fade_store.Pit_load.load store ~fraction:0.02 ~require_event_markets:false
       in
       let traded =
         List.filter_map rows ~f:(fun r ->
           Option.some_if (Horizon.equal r.features.horizon H1) r.ticker)
         |> String.Set.of_list
       in
       let pit = table ~anchor:Expected ~keep:(fun o -> Set.mem traded o.market.ticker) in
       Fade_store.close store;
       printf
         "Volume-to-date filter: %d markets with complete trades (%d truncated, %d not \
          fetched yet are left out of column 3)\n\n"
         summary.markets_loaded
         summary.truncated_trades
         summary.trades_missing;
       let cell (b : Bucket_result.t option) =
         match b with
         | None -> sprintf "%26s" "-"
         | Some b ->
           sprintf
             "%5d %7s %s"
             b.n
             (pct b.mean_return)
             (match b.return_ci with
              | Some c -> sprintf "%-17s" (sprintf "%s..%s" (pct c.lo) (pct c.hi))
              | None -> sprintf "%-17s" "(n < 30)")
       in
       printf
         "Out-of-time, buy NO at the ask 1h before close, all markets (n, return/$, 95%% \
          CI)\n";
       printf
         "%-9s %-31s %-31s %-31s\n"
         "bucket"
         "1. frozen (study anchor)"
         "2. expected expiration"
         "3. + volume to date >= 500";
       let find bs lo =
         List.find bs ~f:(fun (b : Bucket_result.t) -> Float.equal b.lo lo)
       in
       List.iter (List.range 0 20) ~f:(fun i ->
         let lo = Float.of_int (i * 5) in
         printf
           "%-9s %s %s %s\n"
           (sprintf "%.0f-%.0f¢" lo (lo +. 5.))
           (cell (find frozen lo))
           (cell (find expected lo))
           (cell (find pit lo)));
       let total bs = List.sum (module Int) bs ~f:(fun (b : Bucket_result.t) -> b.n) in
       let neg bs =
         List.count bs ~f:(fun (b : Bucket_result.t) -> Bucket_result.proven_negative b)
       in
       let pos bs =
         List.count bs ~f:(fun (b : Bucket_result.t) -> Bucket_result.proven_positive b)
       in
       printf
         "%-9s %5d obs, %2d CI<0, %d CI>0 %5d obs, %2d CI<0, %d CI>0 %5d obs, %2d CI<0, \
          %d CI>0\n"
         "total"
         (total frozen)
         (neg frozen)
         (pos frozen)
         (total expected)
         (neg expected)
         (pos expected)
         (total pit)
         (neg pit)
         (pos pit))
;;
