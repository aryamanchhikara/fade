open! Core
open Fade_types
module F = Pit.Features

(* The design matrix: point-in-time features as numbers, the same for both models.
   Fitted on training rows only (which categories get their own column). Missing
   values (no trade before a move window) are 0 plus an indicator column, so a model
   can tell "flat" from "unknown". *)

type t =
  { categories : string list (** categories with their own column, from training *)
  ; names : string array
  }
[@@deriving sexp]

(* The price enters as logit(ask) plus "hinge" terms max(0, logit(ask) - k): a linear
   spline, so the logistic model can bend its price curve at the extremes the way the
   isotonic baseline does. Knots at 5%, 20%, 50%, 80%, 95%. *)
let knots = [| -2.944; -1.386; 0.; 1.386; 2.944 |]

let base_names =
  [| "logit_no_ask"
   ; "logit_hinge_5"
   ; "logit_hinge_20"
   ; "logit_hinge_50"
   ; "logit_hinge_80"
   ; "logit_hinge_95"
   ; "spread_cents"
   ; "one_sided"
   ; "log10_volume"
   ; "log10_trades"
   ; "move_1h"
   ; "move_1h_missing"
   ; "move_6h"
   ; "move_6h_missing"
   ; "move_24h"
   ; "move_24h_missing"
   ; "horizon_6h"
   ; "horizon_1h"
   ; "bracket"
   ; "single_market"
   ; "log10_event_markets"
  |]
;;

(* Columns that are non-decreasing functions of the NO price: the gradient-boosted model
   is constrained to be increasing in each, so it is increasing in the price. *)
let price_columns = [ 0; 1; 2; 3; 4; 5 ]
let min_category_rows = 200

let fit (rows : Pit.Row.t list) =
  let categories =
    List.map rows ~f:(fun r -> r.features.category)
    |> List.sort_and_group ~compare:String.compare
    |> List.filter_map ~f:(fun g ->
      Option.some_if (List.length g >= min_category_rows) (List.hd_exn g))
  in
  { categories
  ; names =
      Array.append
        base_names
        (Array.of_list (List.map categories ~f:(fun c -> "cat_" ^ c)))
  }
;;

let logit_ask ask =
  let q = Float.min 0.999 (Float.max 0.001 (ask /. 100.)) in
  Float.log (q /. (1. -. q))
;;

let encode t (f : F.t) =
  let opt = function
    | Some v -> v, 0.
    | None -> 0., 1.
  in
  let flag b = if b then 1. else 0. in
  let m1, m1_missing = opt f.move_1h in
  let m6, m6_missing = opt f.move_6h in
  let m24, m24_missing = opt f.move_24h in
  let z = logit_ask f.no_ask in
  let base =
    [| z
     ; Float.max 0. (z -. knots.(0))
     ; Float.max 0. (z -. knots.(1))
     ; Float.max 0. (z -. knots.(2))
     ; Float.max 0. (z -. knots.(3))
     ; Float.max 0. (z -. knots.(4))
     ; Option.value f.spread ~default:0.
     ; flag (F.one_sided f)
     ; Float.log10 (Float.max 1. f.volume_to_date)
     ; Float.log10 (Float.of_int (Int.max 1 f.trades_to_date))
     ; m1
     ; m1_missing
     ; m6
     ; m6_missing
     ; m24
     ; m24_missing
     ; flag (Horizon.equal f.horizon H6)
     ; flag (Horizon.equal f.horizon H1)
     ; flag (f.mutually_exclusive && f.event_markets > 1)
     ; flag (f.event_markets <= 1)
     ; Float.log10 (Float.of_int (Int.max 1 f.event_markets))
    |]
  in
  Array.append
    base
    (Array.of_list (List.map t.categories ~f:(fun c -> flag (String.equal c f.category))))
;;
