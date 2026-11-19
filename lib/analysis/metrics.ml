open! Core

(* Scoring probabilistic predictions of "NO won".
   - log loss: -mean(y log p + (1-y) log(1-p)); punishes confident mistakes hard.
   - Brier: mean (p - y)^2; the squared error of the probability.
   - ECE (expected calibration error): bin predictions into 10 equal-width bins and
     average |mean predicted - observed rate|, weighted by bin size.
   Comparisons with the baseline use the per-row difference in loss, summed per event,
   so the CI resamples whole events. *)

let clip p = Float.min (1. -. 1e-12) (Float.max 1e-12 p)
let row_log_loss ~p ~y = if y then -.Float.log (clip p) else -.Float.log (1. -. clip p)

let row_brier ~p ~y =
  let d = p -. if y then 1. else 0. in
  d *. d
;;

let mean xs =
  List.sum (module Float) xs ~f:Fn.id /. Float.of_int (Int.max 1 (List.length xs))
;;

module Bin = struct
  type t =
    { lo : float
    ; hi : float
    ; n : int
    ; mean_p : float
    ; observed : float
    }
  [@@deriving sexp]
end

let calibration ?(bins = 10) (pairs : (float * bool) list) =
  List.filter_map (List.range 0 bins) ~f:(fun b ->
    let lo = Float.of_int b /. Float.of_int bins in
    let hi = Float.of_int (b + 1) /. Float.of_int bins in
    let inside =
      List.filter pairs ~f:(fun (p, _) ->
        Float.( >= ) p lo && (Float.( < ) p hi || (b = bins - 1 && Float.( <= ) p hi)))
    in
    if List.is_empty inside
    then None
    else
      Some
        { Bin.lo
        ; hi
        ; n = List.length inside
        ; mean_p = mean (List.map inside ~f:fst)
        ; observed = mean (List.map inside ~f:(fun (_, y) -> if y then 1. else 0.))
        })
;;

let ece pairs =
  let n = Float.of_int (List.length pairs) in
  List.sum
    (module Float)
    (calibration pairs)
    ~f:(fun b -> Float.of_int b.n /. n *. Float.abs (b.mean_p -. b.observed))
;;

module Summary = struct
  type t =
    { log_loss : float
    ; brier : float
    ; ece : float
    }
  [@@deriving sexp]
end

let summary pairs : Summary.t =
  { log_loss = mean (List.map pairs ~f:(fun (p, y) -> row_log_loss ~p ~y))
  ; brier = mean (List.map pairs ~f:(fun (p, y) -> row_brier ~p ~y))
  ; ece = ece pairs
  }
;;

(* Mean of [model loss - baseline loss] over rows, with a 95% CI resampling events.
   Negative means the model is better. [rows] are (event, model p, baseline p, y). *)
let difference ~rng ~resamples ~(loss : p:float -> y:bool -> float) rows =
  let by_event = String.Table.create () in
  List.iter rows ~f:(fun (event, pm, pb, y) ->
    Hashtbl.update by_event event ~f:(fun prev ->
      let num, den = Option.value prev ~default:(0., 0.) in
      num +. (loss ~p:pm ~y -. loss ~p:pb ~y), den +. 1.));
  let clusters =
    Hashtbl.data by_event
    |> List.map ~f:(fun (num, den) -> { Stats.num; den })
    |> Array.of_list
  in
  let total =
    Array.fold clusters ~init:(0., 0.) ~f:(fun (a, b) c -> a +. c.num, b +. c.den)
  in
  let point = if Float.( > ) (snd total) 0. then fst total /. snd total else 0. in
  point, Stats.cluster_bootstrap ~rng ~resamples clusters
;;

let%expect_test "metrics on a tiny example" =
  let pairs = [ 0.9, true; 0.8, true; 0.3, false; 0.6, false; 0.1, false ] in
  let s = summary pairs in
  printf "log loss %.4f, Brier %.4f, ECE %.4f\n" s.log_loss s.brier s.ece;
  List.iter (calibration pairs) ~f:(fun b ->
    printf
      "  [%.1f, %.1f) n=%d mean p %.2f observed %.2f\n"
      b.lo
      b.hi
      b.n
      b.mean_p
      b.observed);
  [%expect
    {|
    log loss 0.3414, Brier 0.1020, ECE 0.2600
      [0.1, 0.2) n=1 mean p 0.10 observed 0.00
      [0.3, 0.4) n=1 mean p 0.30 observed 0.00
      [0.6, 0.7) n=1 mean p 0.60 observed 0.00
      [0.8, 0.9) n=1 mean p 0.80 observed 1.00
      [0.9, 1.0) n=1 mean p 0.90 observed 1.00
    |}]
;;
