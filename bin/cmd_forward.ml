open! Core
open! Async
open Fade_types
module Fb = Fade_analysis.Forward_builder

(* `fade forward`: paper-trade the locked candidate (rules/candidate.json) on markets
   that settle after the study window. Nothing is ever ordered. *)

let pct x = sprintf "%+.1f%%" (100. *. x)
let now () = Float.to_int (Core_unix.time ())

(* --demo: the same rules on the synthetic fixtures, with the fixtures' out-of-time
   half standing in for the forward window, seen from its 30th day. *)
let demo_report ~fixtures =
  let source = Data_source.Demo fixtures in
  let study = Cmd_report.load_frozen source ~frozen_path:"rules/frozen.demo.json" in
  let d = Data_source.load source ~anchor:study.config.anchor in
  let in_study = Fade_analysis.Report_builder.restrict ~config:study.config d in
  let split = Fade_analysis.Split.create ~config:study.config in_study.markets in
  let config =
    Fade_analysis.Candidate_file.config
      ~study
      ~forward_start:study.config.cutoff
      ~forward_days:60
  in
  let rules = Fade_analysis.Candidate_file.rules in
  let in_sample =
    Fade_analysis.Candidate_score.score_all
      ~config
      ~rules
      ~d:in_study
      ~include_market:(fun m ->
        match Fade_analysis.Split.sample split m with
        | Some In_sample -> true
        | _ -> false)
      ~traded:(fun _ -> true)
      ~stream:0
  in
  let candidate =
    Fade_analysis.Candidate_file.create
      ~frozen_at:(Data_source.now_string ())
      ~config
      ~rules
      ~in_sample
      ~notes:[ "synthetic" ]
  in
  let start, _ = Fb.window config in
  Fb.build
    ~generated_at:(Data_source.now_string ())
    ~now:(start + (29 * 86_400) + 3600)
    ~synthetic:true
    ~candidate
    ~d
    ~traded:(fun _ -> true)
    ~concluded:(fun _ -> None)
;;

(* Conclusions are kept in the forward cache: once a rule's CI has excluded zero, its
   test is over, whatever later runs would say. *)
let concluded_key id = "concluded:" ^ id

let read_conclusion store id =
  Option.bind
    (Fade_store.get_meta store (concluded_key id))
    ~f:(fun v ->
      match String.lsplit2 v ~on:' ' with
      | Some (date, status) ->
        Some (date, Forward.Status.t_of_sexp (Sexp.of_string status))
      | None -> None)
;;

let real_report ~candidate ~db =
  if not (Sys_unix.file_exists_exn db)
  then raise_s [%message "no forward cache yet; run `fade forward` (it fetches)" db];
  let store = Fade_store.open_ db in
  let (d : Dataset.t), stats =
    Fade_store.Load.dataset
      ~complete_prefix:false
      store
      ~anchor:(candidate : Candidate.t).config.anchor
  in
  let report =
    Fb.build
      ~generated_at:(Data_source.now_string ())
      ~now:(now ())
      ~synthetic:false
      ~candidate
      ~d
      ~traded:(Set.mem stats.trade_tickers)
      ~concluded:(read_conclusion store)
  in
  List.iter report.rules ~f:(fun r ->
    match r.concluded_on, read_conclusion store r.rule.id with
    | Some date, None ->
      Fade_store.set_meta
        store
        (concluded_key r.rule.id)
        (sprintf "%s %s" date (Sexp.to_string (Forward.Status.sexp_of_t r.status)))
    | _ -> ());
  Fade_store.close store;
  report
;;

let print_report (r : Forward.t) =
  printf
    "fade forward%s: candidate %s, markets settled %s to %s, day %d of %d\n"
    (if r.synthetic then " (SYNTHETIC)" else "")
    (String.prefix r.candidate_hash 12)
    r.config.forward_start
    r.forward_end
    r.day
    r.config.forward_days;
  printf
    "markets: %d in scope, %d with a 1h quote, %d with trades (latest settle %s)\n\n"
    r.listed_markets
    r.quoted_markets
    r.traded_markets
    (Option.value r.last_settle ~default:"none yet");
  printf
    "  %-8s %-15s %7s %6s %7s %8s  %-20s %s\n"
    "rule"
    "status"
    "orders"
    "fills"
    "events"
    "return"
    "95% CI"
    "to a conclusion";
  List.iter r.rules ~f:(fun x ->
    let b = x.forward.result in
    printf
      "  %-8s %-15s %7d %6d %7d %8s  %-20s %s\n"
      x.rule.id
      (Forward.Status.to_string x.status)
      x.forward.orders
      b.n
      b.n_events
      (if b.n = 0 then "-" else pct b.mean_return)
      (match b.return_ci with
       | Some ci -> sprintf "%s to %s" (pct ci.lo) (pct ci.hi)
       | None -> sprintf "(needs %d fills)" r.config.min_n)
      (match x.more_events with
       | Some 0 -> "concluded " ^ Option.value x.concluded_on ~default:""
       | Some n -> sprintf "~%d more events (%s)" n x.projection
       | None -> x.projection))
;;

let command =
  Command.async
    ~summary:"Forward-test the locked candidate on newly settled markets (paper only)"
    ~readme:(fun () ->
      "Fetches every in-scope market settled in the candidate's forward window (from its \
       forward_start for forward_days), with candles, plus trades where a maker rule \
       rested a bid, into its own cache (default data/forward.db). Then scores rules A, \
       B and B-loose and writes web/data/forward.json. Safe to run daily.")
    (let%map_open.Command demo =
       flag "--demo" no_arg ~doc:" synthetic fixtures, no network"
     and fixtures =
       flag
         "--fixtures"
         (optional_with_default "test/fixtures/synthetic.json" string)
         ~doc:"PATH synthetic dataset for --demo"
     and candidate_path =
       flag
         "--candidate"
         (optional_with_default "rules/candidate.json" string)
         ~doc:"PATH the locked candidate (default rules/candidate.json)"
     and db =
       flag
         "--db"
         (optional_with_default "data/forward.db" string)
         ~doc:"PATH forward cache (default data/forward.db)"
     and out = flag "--out" (optional string) ~doc:"PATH (default web/data/forward.json)"
     and rate =
       flag "--rate" (optional_with_default 4. float) ~doc:"R requests per second (4)"
     and no_fetch = flag "--no-fetch" no_arg ~doc:" score the cache without fetching"
     and quiet = flag "--quiet" no_arg ~doc:" do not print the summary" in
     fun () ->
       let%map report =
         if demo
         then return (demo_report ~fixtures)
         else (
           let candidate =
             Fade_analysis.Candidate_file.of_string (In_channel.read_all candidate_path)
             |> ok_exn
           in
           let%map () =
             if no_fetch
             then return ()
             else
               Fade_fetch.Forward_fetch.run
                 ~db_path:db
                 ~candidate
                 ~requests_per_second:rate
                 ~now:(now ())
           in
           real_report ~candidate ~db)
       in
       let out =
         Option.value
           out
           ~default:
             (if demo then "web/data/demo-forward.json" else "web/data/forward.json")
       in
       Core_unix.mkdir_p (Filename.dirname out);
       Out_channel.write_all
         out
         ~data:(Yojson.Safe.to_string (Forward.yojson_of_t report) ^ "\n");
       if not quiet then print_report report;
       printf "wrote %s\n" out)
;;
