open! Core
open Fade_types
open Fade_analysis

let pct x = sprintf "%+.1f%%" (100. *. x)

let ci_string (b : Bucket_result.t) =
  match b.return_ci with
  | Some ci -> sprintf "%s to %s" (pct ci.lo) (pct ci.hi)
  | None -> "n < min"
;;

let print_table ~title (t : Table.t option) =
  printf "\n%s\n" title;
  match t with
  | None -> printf "  (no observations)\n"
  | Some t ->
    printf
      "  %-9s %7s %7s %7s %7s %7s %8s  %s\n"
      "bucket"
      "n"
      "events"
      "win"
      "b/even"
      "fee"
      "return"
      "95% CI (event-clustered)";
    List.iter t.buckets ~f:(fun b ->
      printf
        "  %-9s %7d %7d %6.1f%% %6.1f%% %6.2f¢ %8s  %s%s\n"
        (Bucket_result.label b)
        b.n
        b.n_events
        (100. *. b.win_rate)
        (100. *. b.breakeven)
        b.avg_fee
        (pct b.mean_return)
        (ci_string b)
        (if Bucket_result.proven_positive b
         then "  <- CI above zero"
         else if b.enough
         then ""
         else "  (below min n)"))
;;

let print_report (r : Report.t) ~frozen_path =
  let w = r.window in
  printf
    "fade report: %s\n"
    (if r.synthetic then "SYNTHETIC DATA (not Kalshi)" else "Kalshi data");
  printf
    "window: markets settled %s to %s; population %d markets in scope\n"
    w.first_settle
    w.last_settle
    w.population;
  printf
    "sample: %d markets with quotes (uniform random), %d with trades; %d markets in %d \
     events loaded; %d trades\n"
    w.quote_sample
    w.trade_sample
    w.n_markets
    w.n_events
    w.n_trades;
  printf
    "split: cutoff %s; %d in-sample events, %d out-of-time, %d embargoed (straddle the \
     cutoff)\n"
    r.config.cutoff
    w.n_in_sample_events
    w.n_out_of_time_events
    w.n_embargoed_events;
  printf
    "config: %s sha256 %s; horizons from %s; fees for %.0f-contract orders; min n %d\n"
    frozen_path
    r.config_hash
    (Anchor.to_string r.config.anchor)
    r.config.contracts_per_order
    r.config.min_n;
  printf
    "in-sample search: %d cells examined, %d rules frozen\n"
    r.examined_in_sample
    (List.length r.rules);
  List.iter w.dropped ~f:(fun (why, n) -> printf "dropped: %s: %d\n" why n);
  printf "\nFrozen rules, scored out-of-time\n";
  if List.is_empty r.rules then printf "  (none: nothing cleared zero in-sample)\n";
  List.iter r.rules ~f:(fun rr ->
    printf
      "  %s\n    in-sample   %s (%s), n=%d\n    out-of-time %s\n"
      (Rule.describe rr.rule)
      (pct rr.in_sample.mean_return)
      (ci_string rr.in_sample)
      rr.in_sample.n
      (match rr.out_of_time with
       | None -> "no observations"
       | Some b -> sprintf "%s (%s), n=%d" (pct b.mean_return) (ci_string b) b.n));
  let table sample view side horizon =
    Tables.find r.tables { Table.Key.sample; view; side; horizon; segment = All }
  in
  List.iter Horizon.all ~f:(fun h ->
    print_table
      ~title:
        (sprintf
           "Out-of-time, buy NO as taker at the ask, %s before close, all markets"
           (Horizon.to_string h))
      (table Out_of_time Quotes No h));
  print_table
    ~title:"Out-of-time, buy YES as taker at the ask, 1h before close (for comparison)"
    (table Out_of_time Quotes Yes H1);
  printf "\nOut-of-time trade view, all markets (return per dollar, contract-weighted)\n";
  List.iter [ Role.Taker; Maker ] ~f:(fun role ->
    List.iter Side.all ~f:(fun side ->
      List.iter Horizon.all ~f:(fun h ->
        match table Out_of_time (Trades role) side h with
        | None -> ()
        | Some t ->
          let cost =
            List.sum
              (module Float)
              t.buckets
              ~f:(fun b -> b.contracts *. (b.avg_price +. b.avg_fee))
          in
          let profit =
            List.sum
              (module Float)
              t.buckets
              ~f:(fun b -> b.mean_return *. b.contracts *. (b.avg_price +. b.avg_fee))
          in
          printf
            "  %-6s buying %-3s %-4s %8d trades  %s\n"
            (Role.to_string role)
            (Side.to_string side)
            (Horizon.to_string h)
            (List.sum (module Int) t.buckets ~f:(fun b -> b.n))
            (if Float.( > ) cost 0. then pct (profit /. cost) else "-"))));
  printf
    "\nVERDICT (%s): %s\n%s\n"
    (match r.verdict.tone with
     | Green -> "green"
     | Red -> "red"
     | Neutral -> "neutral")
    r.verdict.headline
    r.verdict.detail
;;

let build_report source ~frozen_path =
  let frozen =
    match Sys_unix.file_exists_exn frozen_path, source with
    | true, _ -> Frozen.of_string (In_channel.read_all frozen_path) |> ok_exn
    | false, Data_source.Demo _ ->
      (* The demo freezes in memory, from the fixtures' own in-sample half. *)
      let d = Data_source.load source ~anchor:Schedule in
      Report_builder.freeze
        ~frozen_at:(Data_source.now_string ())
        ~config:(Report_builder.default_config d)
        d
    | false, Db _ ->
      raise_s
        [%message
          "no frozen rules; run `fade freeze` first (it only looks at in-sample data)"
            (frozen_path : string)]
  in
  let d, sampling = Data_source.load_with_sampling source ~anchor:frozen.config.anchor in
  Report_builder.build
    ?sampling
    ~generated_at:(Data_source.now_string ())
    ~synthetic:(Data_source.is_demo source)
    ~frozen
    d
;;

let default_frozen source =
  if Data_source.is_demo source then "rules/frozen.demo.json" else "rules/frozen.json"
;;

let default_out source =
  if Data_source.is_demo source
  then "web/data/demo-report.json"
  else "web/data/report.json"
;;

let command =
  Command.basic
    ~summary:"Score the frozen rules out-of-time, print tables, write report.json"
    (let%map_open.Command source = Data_source.param
     and frozen = flag "--frozen" (optional string) ~doc:"PATH frozen rules"
     and out = flag "--out" (optional string) ~doc:"PATH report.json destination"
     and quiet = flag "--quiet" no_arg ~doc:" only write the JSON" in
     fun () ->
       let frozen_path = Option.value frozen ~default:(default_frozen source) in
       let out = Option.value out ~default:(default_out source) in
       let r = build_report source ~frozen_path in
       Core_unix.mkdir_p (Filename.dirname out);
       Out_channel.write_all out ~data:(Yojson.Safe.to_string (Report.yojson_of_t r));
       if not quiet then print_report r ~frozen_path;
       eprintf "wrote %s\n" out)
;;
