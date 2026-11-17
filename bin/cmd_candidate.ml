open! Core
open Fade_types

(* `fade candidate`: score the pre-registered candidate rules on the study's in-sample
   data and lock them, with those numbers, in rules/candidate.json. *)

let pct x = sprintf "%+.1f%%" (100. *. x)

let print_scores ~title (rules : Candidate.Rule.t list) (scores : Candidate.Score.t list) =
  printf "\n%s\n" title;
  printf
    "  %-8s %7s %7s %7s %7s %7s %8s %8s %8s  %s\n"
    "rule"
    "orders"
    "fills"
    "events"
    "win"
    "price"
    "b/even"
    "fee"
    "return"
    "95% CI (event-clustered)";
  List.iter2_exn rules scores ~f:(fun rule (s : Candidate.Score.t) ->
    let b = s.result in
    printf
      "  %-8s %7d %7d %7d %6.1f%% %6.1f¢ %7.1f%% %7.2f¢ %8s  %s\n"
      rule.id
      s.orders
      b.n
      b.n_events
      (100. *. b.win_rate)
      b.avg_price
      (100. *. b.breakeven)
      b.avg_fee
      (if b.n = 0 then "-" else pct b.mean_return)
      (match b.return_ci with
       | Some ci -> sprintf "%s to %s" (pct ci.lo) (pct ci.hi)
       | None -> "(too few fills for a CI)"));
  List.iter rules ~f:(fun r -> printf "  %-8s %s\n" r.id (Candidate.Rule.describe r))
;;

let command =
  Command.basic
    ~summary:"Score the forward-test candidate in-sample and lock it in a hashed file"
    ~readme:(fun () ->
      "Rules A (taker) and B (maker, strict and loose fills) are defined in \
       Fade_analysis.Candidate_file. In-sample means the study's in-sample events \
       (settled before its cutoff); maker fills need complete trades, so B uses the \
       trade sample. --dry-run prints the numbers without writing. Refuses to overwrite \
       an existing file without --force.")
    (let%map_open.Command source = Data_source.param
     and frozen_path =
       flag
         "--frozen"
         (optional string)
         ~doc:"PATH the study's frozen rules (default rules/frozen.json)"
     and out =
       flag
         "--out"
         (optional_with_default "rules/candidate.json" string)
         ~doc:"PATH (default rules/candidate.json)"
     and forward_start =
       flag
         "--forward-start"
         (optional string)
         ~doc:
           "YYYY-MM-DD first settlement date of the forward test (default: the day after \
            the study window)"
     and forward_days =
       flag "--days" (optional_with_default 60 int) ~doc:"N forward test length (60)"
     and dry_run = flag "--dry-run" no_arg ~doc:" print in-sample numbers, write nothing"
     and force = flag "--force" no_arg ~doc:" overwrite an existing candidate file" in
     fun () ->
       if (not dry_run) && Sys_unix.file_exists_exn out && not force
       then (
         eprintf
           "%s already exists. Relocking after forward data has been fetched defeats the \
            test; pass --force only if no forward data has been looked at.\n"
           out;
         exit 1);
       let frozen_path =
         Option.value frozen_path ~default:(Cmd_report.default_frozen source)
       in
       let study = Cmd_report.load_frozen source ~frozen_path in
       let d, stats = Data_source.load_with_stats source ~anchor:study.config.anchor in
       let d = Fade_analysis.Report_builder.restrict ~config:study.config d in
       let split = Fade_analysis.Split.create ~config:study.config d.markets in
       let in_sample (m : Market.t) =
         match Fade_analysis.Split.sample split m with
         | Some In_sample -> true
         | _ -> false
       in
       (* Already looked at in the study, so not a holdout: printed for context only. *)
       let seen (m : Market.t) =
         match Fade_analysis.Split.sample split m with
         | Some Out_of_time -> true
         | _ -> false
       in
       let traded =
         match stats with
         | Some s -> Set.mem s.trade_tickers
         | None -> fun _ -> true
       in
       let forward_start =
         Option.value_or_thunk forward_start ~default:(fun () ->
           Unix_time.to_date_string
             (Unix_time.of_date_string_exn study.config.data_until + 86_400))
       in
       let config =
         Fade_analysis.Candidate_file.config ~study ~forward_start ~forward_days
       in
       let rules = Fade_analysis.Candidate_file.rules in
       let score include_market ~stream =
         Fade_analysis.Candidate_score.score_all
           ~config
           ~rules
           ~d
           ~include_market
           ~traded
           ~stream
       in
       let in_sample_scores = score in_sample ~stream:0 in
       print_scores
         ~title:
           (sprintf
              "In-sample: events settled %s to before %s (study %s)"
              config.in_sample_since
              config.in_sample_cutoff
              (String.prefix study.hash 12))
         rules
         in_sample_scores;
       if dry_run
       then
         print_scores
           ~title:
             (sprintf
                "Already seen in the study (%s to %s): NOT a holdout, context only"
                study.config.cutoff
                study.config.data_until)
           rules
           (score seen ~stream:2)
       else (
         let candidate =
           Fade_analysis.Candidate_file.create
             ~frozen_at:(Data_source.now_string ())
             ~config
             ~rules
             ~in_sample:in_sample_scores
             ~notes:
               [ sprintf
                   "In-sample: %d events settled before %s. Rule A uses the quote \
                    sample; B and B-loose use markets with complete trades (the trade \
                    sample)."
                   (Fade_analysis.Split.count split In_sample)
                   config.in_sample_cutoff
               ; sprintf
                   "Forward window: markets settled from %s for %d days, fetched only \
                    after this file was committed."
                   config.forward_start
                   config.forward_days
               ]
         in
         Core_unix.mkdir_p (Filename.dirname out);
         Out_channel.write_all
           out
           ~data:(Fade_analysis.Candidate_file.to_string candidate);
         printf
           "\nlocked %d rules to %s\nsha256 %s\n"
           (List.length rules)
           out
           candidate.hash))
;;
