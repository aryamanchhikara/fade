open! Core
open Fade_types
module Synth_pit = Fade_synth.Synth_pit
module Eb = Fade_analysis.Explain_builder

(* The explain pipeline end to end on synthetic markets with one planted effect: a
   one-sided book overprices NO. It should be found, hold in most test months, and the
   price buckets themselves should show no residual after recalibration. *)

let%expect_test "explain finds the planted one-sided effect and nothing in price" =
  let markets = Synth_pit.generate () in
  let rows = Synth_pit.rows markets in
  let leaks =
    List.count markets ~f:(fun m -> not (List.is_empty (Fade_analysis.Pit.leaks m.pit)))
  in
  let e =
    Eb.build
      ~config:{ Eb.default_config with resamples = 300 }
      ~generated_at:"test"
      ~synthetic:true
      ~data_since:"2025-06-01"
      ~data_until:"2026-11-30"
      ~exclusions:[]
      ~leakage_checked:(List.length markets)
      ~leakage_failures:leaks
      rows
  in
  printf
    "rows %d (test %d), folds %d, leaking markets %d\n"
    e.rows
    e.test_rows
    (List.length e.folds)
    leaks;
  let show (l : Explain.Level.t) =
    printf
      "%-22s n=%5d mean %+.3f CI %s agree %d/%d%s\n"
      (Explain.Level.key l)
      l.n
      l.mean_residual
      (match l.ci with
       | Some c -> sprintf "%+.3f..%+.3f" c.lo c.hi
       | None -> "-")
      l.folds_agree
      (List.length l.folds)
      (if l.single_fold then " SINGLE-FOLD" else "")
  in
  List.iter e.levels ~f:(fun l -> if String.equal l.feature "book" then show l);
  let price_cleared =
    List.count e.levels ~f:(fun l ->
      String.equal l.feature "price"
      && Option.value_map l.ci ~default:false ~f:(fun c ->
        Ci.above_zero c || Ci.below_zero c))
  in
  printf
    "price buckets with a CI excluding zero: %d of %d\n"
    price_cleared
    (List.count e.levels ~f:(fun l -> String.equal l.feature "price"));
  printf "top of the ranking: %s\n" (List.hd e.ranking |> Option.value ~default:"(none)");
  [%expect
    {|
    rows 23895 (test 16644), folds 13, leaking markets 0
    book: two-sided        n=11219 mean +0.014 CI +0.002..+0.025 agree 9/13 SINGLE-FOLD
    book: one-sided        n= 5425 mean -0.047 CI -0.060..-0.034 agree 13/13
    price buckets with a CI excluding zero: 1 of 10
    top of the ranking: book: one-sided
    |}]
;;
