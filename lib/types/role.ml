open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Taker: crossed the spread and hit a resting order. Maker: the resting order. *)
type t =
  | Taker
  | Maker
[@@deriving sexp, yojson, compare, equal, hash, enumerate]

let to_string = function
  | Taker -> "taker"
  | Maker -> "maker"
;;
