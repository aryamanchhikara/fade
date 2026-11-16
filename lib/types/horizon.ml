open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* How long before a market's close we read its quote. *)
type t =
  | H24
  | H6
  | H1
[@@deriving sexp, yojson, compare, equal, hash, enumerate]

let hours = function
  | H24 -> 24
  | H6 -> 6
  | H1 -> 1
;;

let seconds t = hours t * 3600
let to_string t = sprintf "%dh" (hours t)

(* The trade view has no single snapshot time, so each horizon maps to the band of
   trades executed between this horizon and the next shorter one:
   H1 = last hour, H6 = 1 to 6 hours out, H24 = 6 to 24 hours out. *)
let band_seconds = function
  | H1 -> 0, 3600
  | H6 -> 3600, 6 * 3600
  | H24 -> 6 * 3600, 24 * 3600
;;

let of_seconds_before_close s =
  List.find all ~f:(fun h ->
    let lo, hi = band_seconds h in
    s > lo && s <= hi)
;;
