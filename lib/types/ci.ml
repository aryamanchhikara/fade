open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* A 95% confidence interval. *)
type t =
  { lo : float
  ; hi : float
  }
[@@deriving sexp, yojson, compare, equal]

let above_zero t = Float.( > ) t.lo 0.
let below_zero t = Float.( < ) t.hi 0.
