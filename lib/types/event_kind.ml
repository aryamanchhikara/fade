open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* How a market relates to the other markets in its event. Bracket markets are
   strongly correlated (if one resolves YES the rest resolve NO), which is why the
   bootstrap resamples whole events rather than individual markets. *)
type t =
  | Single (** the event has exactly one market *)
  | Bracket (** several mutually exclusive markets: at most one resolves YES *)
  | Multi (** several markets that are not mutually exclusive, e.g. spread ladders *)
[@@deriving sexp, yojson, compare, equal, hash, enumerate]

let to_string = function
  | Single -> "single market"
  | Bracket -> "bracket event"
  | Multi -> "multi-market event"
;;

let classify ~n_markets ~mutually_exclusive =
  if n_markets <= 1 then Single else if mutually_exclusive then Bracket else Multi
;;
