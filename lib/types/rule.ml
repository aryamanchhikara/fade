open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* A trading rule chosen in-sample: "buy [side] as a taker at the quoted ask,
   [horizon] before close, when that ask is in [lo, hi) cents, within [segment]". *)
type t =
  { side : Side.t
  ; horizon : Horizon.t
  ; segment : Segment.t
  ; lo : float
  ; hi : float
  }
[@@deriving sexp, yojson, compare, equal, hash]

let describe t =
  let where =
    match t.segment with
    | All -> ""
    | s -> sprintf " (%s)" (Segment.to_string s)
  in
  sprintf
    "Buying %s at %.0f-%.0f¢, %s before close%s"
    (Side.to_string t.side)
    t.lo
    t.hi
    (Horizon.to_string t.horizon)
    where
;;
