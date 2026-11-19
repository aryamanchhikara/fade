open! Core
open Fade_types
module Eb = Fade_analysis.Explain_builder

(* `fade explain`: which point-in-time factors predict NO outcomes beyond the
   recalibrated price. Writes web/data/explain.json for the Explain tab. *)

let pct x = sprintf "%+.1f" (100. *. x)

let print (e : Explain.t) =
  printf
    "fade explain%s: %d rows (%d markets, %d events), %d out-of-sample test rows\n"
    (if e.synthetic then " (SYNTHETIC)" else "")
    e.rows
    e.markets
    e.events
    e.test_rows;
  printf
    "leakage check: %d markets re-scored with everything after t deleted, %d changed\n"
    e.leakage_checked
    e.leakage_failures;
  List.iter e.exclusions ~f:(fun (what, n) -> printf "excluded: %s: %d\n" what n);
  printf "\nWalk-forward test months (log loss: raw ask as P vs recalibrated baseline)\n";
  printf
    "  %-8s %7s %6s %6s %-9s %8s %8s\n"
    "month"
    "train"
    "test"
    "events"
    "baseline"
    "raw"
    "baseline";
  List.iter e.folds ~f:(fun f ->
    printf
      "  %-8s %7d %6d %6d %-9s %8.4f %8.4f\n"
      f.month
      f.n_train
      f.n_test
      f.events_test
      f.baseline
      f.test_raw
      f.test_baseline);
  printf
    "\n\
     Residual = 1[NO won] - recalibrated P(NO), in points (x100), out of sample. 95%% CI \
     resamples events.\n";
  List.iter e.features ~f:(fun feat ->
    printf "\n%s\n" feat.name;
    List.filter e.levels ~f:(fun l -> String.equal l.feature feat.id)
    |> List.sort ~compare:(fun (a : Explain.Level.t) b -> Int.compare a.order b.order)
    |> List.iter ~f:(fun l ->
      printf
        "  %-22s n=%6d  P(NO) %5.1f%%  residual %6s  CI %-16s  agree %2d/%-2d%s\n"
        l.level
        l.n
        (100. *. l.mean_price_p)
        (pct l.mean_residual)
        (match l.ci with
         | Some c -> sprintf "%s..%s" (pct c.lo) (pct c.hi)
         | None -> "-")
        l.folds_agree
        (List.length l.folds)
        (if l.single_fold then "  ONE FOLD" else "")));
  printf
    "\nRanked factors (pooled CI excludes zero; most consistent across months first)\n";
  List.iteri e.ranking ~f:(fun i key ->
    match List.find e.levels ~f:(fun l -> String.equal (Explain.Level.key l) key) with
    | None -> ()
    | Some l ->
      printf
        "  %2d. %-34s %6s  agree %2d/%-2d%s\n"
        (i + 1)
        key
        (pct l.mean_residual)
        l.folds_agree
        (List.length l.folds)
        (if l.single_fold then "  FLAG: depends on one fold" else ""))
;;

let demo_report () =
  let markets = Fade_synth.Synth_pit.generate () in
  let leaks =
    List.count markets ~f:(fun m -> not (List.is_empty (Fade_analysis.Pit.leaks m.pit)))
  in
  Eb.build
    ~generated_at:(Data_source.now_string ())
    ~synthetic:true
    ~data_since:"2025-06-01"
    ~data_until:"2026-11-30"
    ~exclusions:[]
    ~leakage_checked:(List.length markets)
    ~leakage_failures:leaks
    (Fade_synth.Synth_pit.rows markets)
;;

let command =
  Command.basic
    ~summary:"Explain NO outcomes beyond the recalibrated price (residual analysis)"
    ~readme:(fun () ->
      "Builds point-in-time rows (decisions 24h/6h/1h before the expected expiration; \
       volume to date >= 500), fits the baseline (Platt or isotonic on the NO ask, \
       chosen on validation months) in walk-forward folds, and tabulates out-of-sample \
       residuals by feature with event-clustered CIs and per-month consistency.")
    (let%map_open.Command demo = flag "--demo" no_arg ~doc:" synthetic markets"
     and db =
       flag "--db" (optional_with_default "data/fade.db" string) ~doc:"PATH study cache"
     and frozen_path =
       flag
         "--frozen"
         (optional_with_default "rules/frozen.json" string)
         ~doc:"PATH study frozen rules (for the data window)"
     and out = flag "--out" (optional string) ~doc:"PATH (default web/data/explain.json)"
     and quiet = flag "--quiet" no_arg ~doc:" do not print the tables" in
     fun () ->
       let e =
         if demo
         then demo_report ()
         else (
           let study =
             Fade_analysis.Frozen.of_string (In_channel.read_all frozen_path) |> ok_exn
           in
           let store = Fade_store.open_ db in
           let rows, s = Fade_store.Pit_load.load store ~fraction:0.02 in
           Fade_store.close store;
           if s.leakage_failures > 0
           then
             raise_s
               [%message
                 "leakage check failed: features changed when data after t was deleted"
                   ~markets:(s.leakage_failures : int)];
           Eb.build
             ~generated_at:(Data_source.now_string ())
             ~synthetic:false
             ~data_since:study.config.data_since
             ~data_until:study.config.data_until
             ~exclusions:
               [ "trades truncated (volume to date unknown)", s.truncated_trades
               ; "trades not fetched yet", s.trades_missing
               ; "event market list not fetched yet", s.event_markets_missing
               ; "no expected expiration", s.no_expected_expiration
               ]
             ~leakage_checked:s.leakage_checked
             ~leakage_failures:s.leakage_failures
             rows)
       in
       let out =
         Option.value
           out
           ~default:
             (if demo then "web/data/demo-explain.json" else "web/data/explain.json")
       in
       Core_unix.mkdir_p (Filename.dirname out);
       Out_channel.write_all
         out
         ~data:(Yojson.Safe.to_string (Explain.yojson_of_t e) ^ "\n");
       if not quiet then print e;
       printf "\nwrote %s\n" out)
;;
