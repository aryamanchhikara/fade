open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* In-sample: events that fully settled before the cutoff; rules are chosen here.
   Out-of-time: events whose every decision time is after the cutoff; the frozen rules
   are only scored here. Events straddling the cutoff are embargoed (dropped). *)
type t =
  | In_sample
  | Out_of_time
[@@deriving sexp, yojson, compare, equal, hash, enumerate]

let to_string = function
  | In_sample -> "in-sample"
  | Out_of_time -> "out-of-time"
;;
