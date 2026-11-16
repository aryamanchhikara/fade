open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Kalshi's per-series fee structures (GET /series, field [fee_type]).
   - [Quadratic]: taker pays round_up(M x 0.07 x C x P x (1 - P)); makers pay nothing.
   - [Quadratic_with_maker_fees]: same taker fee; makers pay a quarter of it (0.0175).
   - [Quadratic_with_combo_maker_fees]: same taker fee; makers pay half (0.035).
   - [Flat]: the "Specific Trading Fees Table" of the fee schedule PDF. No series uses
     it today and its table is not public in machine-readable form, so analysis drops
     markets whose fee resolves to [Flat] and counts them. *)
type t =
  | Quadratic
  | Quadratic_with_maker_fees
  | Quadratic_with_combo_maker_fees
  | Flat
[@@deriving sexp, yojson, compare, equal, hash, enumerate]

let of_kalshi_string = function
  | "quadratic" -> Some Quadratic
  | "quadratic_with_maker_fees" -> Some Quadratic_with_maker_fees
  | "quadratic_with_combo_maker_fees" -> Some Quadratic_with_combo_maker_fees
  | "flat" -> Some Flat
  | _ -> None
;;

let to_kalshi_string = function
  | Quadratic -> "quadratic"
  | Quadratic_with_maker_fees -> "quadratic_with_maker_fees"
  | Quadratic_with_combo_maker_fees -> "quadratic_with_combo_maker_fees"
  | Flat -> "flat"
;;
