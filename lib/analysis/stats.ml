open! Core
open Fade_types

let z95 = 1.959963984540054

let wilson ~successes ~n : Ci.t =
  if Float.( <= ) n 0.
  then { lo = 0.; hi = 1. }
  else (
    let p = successes /. n in
    let z2 = z95 *. z95 in
    let denom = 1. +. (z2 /. n) in
    let center = (p +. (z2 /. (2. *. n))) /. denom in
    let half =
      z95 *. Float.sqrt ((p *. (1. -. p) /. n) +. (z2 /. (4. *. n *. n))) /. denom
    in
    { lo = Float.max 0. (center -. half); hi = Float.min 1. (center +. half) })
;;

let percentile sorted p =
  let n = Array.length sorted in
  let pos = p *. Float.of_int (n - 1) in
  let i = Float.to_int (Float.round_down pos) in
  let j = Int.min (i + 1) (n - 1) in
  let frac = pos -. Float.of_int i in
  sorted.(i) +. (frac *. (sorted.(j) -. sorted.(i)))
;;

type cluster =
  { num : float
  ; den : float
  }

let cluster_bootstrap ~rng ~resamples clusters =
  let k = Array.length clusters in
  if k = 0
  then None
  else (
    (* Arrays are mutable and fixed-size; this inner loop is the hot spot of the
       whole report, so it avoids allocating per resample. *)
    let stats = Array.create ~len:resamples 0. in
    for r = 0 to resamples - 1 do
      let num = ref 0. in
      let den = ref 0. in
      for _ = 1 to k do
        let c = clusters.(Random.State.int rng k) in
        num := !num +. c.num;
        den := !den +. c.den
      done;
      stats.(r) <- (if Float.( > ) !den 0. then !num /. !den else 0.)
    done;
    Array.sort stats ~compare:Float.compare;
    Some { Ci.lo = percentile stats 0.025; hi = percentile stats 0.975 })
;;

(* [let%expect_test] (ppx_expect) runs the body and compares what it prints with the
   [%expect] block. A mismatch fails [dune runtest] and dune shows a diff; [dune promote]
   accepts the new output. Inline tests live next to the code they test. *)
let%expect_test "wilson matches a textbook value" =
  let ci = wilson ~successes:81. ~n:263. in
  printf "%.4f %.4f\n" ci.lo ci.hi;
  [%expect {| 0.2553 0.3662 |}];
  let ci = wilson ~successes:0. ~n:10. in
  printf "%.4f %.4f\n" ci.lo ci.hi;
  [%expect {| 0.0000 0.2775 |}]
;;

let%expect_test "percentile interpolates" =
  let a = [| 1.; 2.; 3.; 4.; 5. |] in
  List.iter [ 0.; 0.25; 0.5; 0.975; 1. ] ~f:(fun p -> printf "%.3f " (percentile a p));
  [%expect {| 1.000 2.000 3.000 4.900 5.000 |}]
;;

let%expect_test "cluster bootstrap: correlated clusters widen the interval" =
  let rng () = Random.State.make [| 42 |] in
  (* 200 markets with returns +1 or -1. Independent: 200 clusters of one market.
     Clustered: the same 200 markets in 20 events of 10 that all move together. *)
  let indep =
    Array.init 200 ~f:(fun i -> { num = (if i % 2 = 0 then 1. else -1.); den = 1. })
  in
  let clustered =
    Array.init 20 ~f:(fun i -> { num = (if i % 2 = 0 then 10. else -10.); den = 10. })
  in
  let show (ci : Ci.t option) =
    let ci = Option.value_exn ci in
    printf "[%.3f, %.3f] width %.3f\n" ci.lo ci.hi (ci.hi -. ci.lo)
  in
  show (cluster_bootstrap ~rng:(rng ()) ~resamples:2000 indep);
  show (cluster_bootstrap ~rng:(rng ()) ~resamples:2000 clustered);
  [%expect
    {|
    [-0.130, 0.140] width 0.270
    [-0.400, 0.402] width 0.802
    |}]
;;
