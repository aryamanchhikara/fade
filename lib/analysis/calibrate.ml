open! Core
open Fade_types

(* The baseline: Kalshi's own price, recalibrated on training data. A raw 92c NO ask is
   not a 92% probability (the favorite-longshot bias, wide spreads); recalibration maps
   ask -> P(NO) the way the training data says it should. Fitted per horizon, since
   books are thinner further from close.

   - Platt: P = sigma(a + b * logit(ask)), a logistic regression on one feature.
   - Isotonic: the best non-decreasing step function of the ask (pool adjacent
     violators), interpolated between block centres. More flexible, noisier.

   Which one is the baseline is decided on validation data. *)

type kind =
  | Platt
  | Isotonic
[@@deriving sexp, compare, equal]

let kind_to_string = function
  | Platt -> "Platt"
  | Isotonic -> "isotonic"
;;

type curve =
  | Platt_fit of Logistic.t
  | Isotonic_fit of
      { x : float array (** block centres, ascending ask in cents *)
      ; p : float array
      }
[@@deriving sexp]

type t =
  { kind : kind
  ; by_horizon : (Horizon.t * curve) list
  ; pooled : curve (** for a horizon with too few rows *)
  }
[@@deriving sexp]

let clip p = Float.min 0.999 (Float.max 0.001 p)

let logit_ask ask =
  let q = clip (ask /. 100.) in
  Float.log (q /. (1. -. q))
;;

let fit_curve kind (rows : (float * bool) array) =
  match kind with
  | Platt ->
    let x = Array.map rows ~f:(fun (ask, _) -> [| logit_ask ask |]) in
    let y = Array.map rows ~f:snd in
    Platt_fit (fst (Logistic.fit_irls ~x ~y ()))
  | Isotonic ->
    (* Pool adjacent violators: sweep in ask order, merging blocks while a block's
       win rate is above the next one's. Each block keeps (sum of asks, wins, n). *)
    let sorted = Array.copy rows in
    Array.sort sorted ~compare:(fun (a, _) (b, _) -> Float.compare a b);
    (* Rows with the same ask are one point: group them first. *)
    let groups =
      Array.fold sorted ~init:[] ~f:(fun acc (ask, won) ->
        let w = if won then 1. else 0. in
        match acc with
        | (s, wins, n) :: rest when Float.equal (s /. n) ask ->
          (s +. ask, wins +. w, n +. 1.) :: rest
        | _ -> (ask, w, 1.) :: acc)
      |> List.rev
    in
    let blocks = Stack.create () in
    List.iter groups ~f:(fun group ->
      let block = ref group in
      let rate (_, w, n) = w /. n in
      let continue = ref true in
      while !continue do
        match Stack.top blocks with
        | Some prev when Float.( >= ) (rate prev) (rate !block) ->
          ignore (Stack.pop_exn blocks : float * float * float);
          let sa, wa, na = prev
          and sb, wb, nb = !block in
          block := sa +. sb, wa +. wb, na +. nb
        | _ -> continue := false
      done;
      Stack.push blocks !block);
    let blocks = Stack.to_list blocks |> List.rev |> Array.of_list in
    (* Shrink each block's rate toward 1/2 by half an observation, so a block with no
       losses is not predicted as certain. Shrinking small blocks more than big ones
       can reorder neighbours, so a running maximum restores the order. *)
    let p = Array.map blocks ~f:(fun (_, w, n) -> clip ((w +. 0.5) /. (n +. 1.))) in
    for i = 1 to Array.length p - 1 do
      p.(i) <- Float.max p.(i) p.(i - 1)
    done;
    Isotonic_fit { x = Array.map blocks ~f:(fun (s, _, n) -> s /. n); p }
;;

let predict_curve curve ask =
  match curve with
  | Platt_fit m -> clip (Logistic.predict m [| logit_ask ask |])
  | Isotonic_fit { x; p } ->
    let n = Array.length x in
    if n = 0
    then 0.5
    else if Float.( <= ) ask x.(0)
    then p.(0)
    else if Float.( >= ) ask x.(n - 1)
    then p.(n - 1)
    else (
      (* First block centre above [ask] (x is ascending; x.(0) < ask < x.(n-1)). *)
      let k = ref 1 in
      while Float.( <= ) x.(!k) ask do
        incr k
      done;
      let k = !k in
      let x0 = x.(k - 1)
      and x1 = x.(k) in
      let w = (ask -. x0) /. (x1 -. x0) in
      clip (p.(k - 1) +. (w *. (p.(k) -. p.(k - 1)))))
;;

let min_rows_per_horizon = 200

(* [rows]: (horizon, NO ask in cents, did NO win). *)
let fit kind (rows : (Horizon.t * float * bool) list) =
  let pooled =
    fit_curve kind (Array.of_list (List.map rows ~f:(fun (_, a, w) -> a, w)))
  in
  let by_horizon =
    List.filter_map Horizon.all ~f:(fun h ->
      let rs =
        List.filter_map rows ~f:(fun (h', a, w) ->
          Option.some_if (Horizon.equal h h') (a, w))
      in
      if List.length rs >= min_rows_per_horizon
      then Some (h, fit_curve kind (Array.of_list rs))
      else None)
  in
  { kind; by_horizon; pooled }
;;

let predict t ~horizon ~ask =
  let curve =
    List.Assoc.find t.by_horizon horizon ~equal:Horizon.equal
    |> Option.value ~default:t.pooled
  in
  predict_curve curve ask
;;

let log_loss t rows =
  let p =
    Array.of_list (List.map rows ~f:(fun (h, a, _) -> predict t ~horizon:h ~ask:a))
  in
  let y = Array.of_list (List.map rows ~f:(fun (_, _, w) -> w)) in
  Logistic.log_loss ~p ~y
;;

(* Fit both on [fit_rows], keep the one with the lower log loss on [validation], and
   refit it on [final_rows]. *)
let choose ~fit_rows ~validation ~final_rows =
  let score kind = log_loss (fit kind fit_rows) validation in
  let platt = score Platt
  and iso = score Isotonic in
  let kind = if Float.( <= ) platt iso then Platt else Isotonic in
  fit kind final_rows, (platt, iso)
;;

let%expect_test "recalibration learns the longshot bias; isotonic is monotone" =
  (* True P(NO) = ask - 4c for asks under 50, ask + 2c above: cheap NO overpriced, dear
     NO underpriced. *)
  let rng = Random.State.make [| 3 |] in
  let rows =
    List.init 6000 ~f:(fun _ ->
      let ask = 5. +. Random.State.float rng 90. in
      let truth = (if Float.( < ) ask 50. then ask -. 4. else ask +. 2.) /. 100. in
      Horizon.H1, ask, Float.( < ) (Random.State.float rng 1.) truth)
  in
  List.iter [ Platt; Isotonic ] ~f:(fun kind ->
    let t = fit kind rows in
    printf "%-9s" (kind_to_string kind);
    List.iter [ 10.; 30.; 50.; 70.; 92. ] ~f:(fun ask ->
      printf " %2.0fc->%4.1f%%" ask (100. *. predict t ~horizon:H1 ~ask));
    print_endline "");
  let iso = fit Isotonic rows in
  let ps =
    List.map (List.range 1 100) ~f:(fun a ->
      predict iso ~horizon:H1 ~ask:(Float.of_int a))
  in
  printf "isotonic non-decreasing: %b\n" (List.is_sorted ps ~compare:Float.compare);
  [%expect
    {|
    Platt     10c-> 5.4% 30c->24.0% 50c->48.0% 70c->72.9% 92c->95.3%
    isotonic  10c-> 4.8% 30c->23.3% 50c->51.5% 70c->73.4% 92c->96.7%
    isotonic non-decreasing: true
    |}]
;;
