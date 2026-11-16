open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Where returns come from.
   - [Quotes]: "buy at the quoted ask as a taker, [horizon] before close", from hourly
     candlesticks; one observation per market.
   - [Trades role]: realized returns of actual fills for that role, with trades placed
     in horizon bands by time to close; weighted by contracts. *)
type t =
  | Quotes
  | Trades of Role.t
[@@deriving sexp, yojson, compare, equal, hash]

let all = [ Quotes; Trades Taker; Trades Maker ]

let to_string = function
  | Quotes -> "Taker at quoted ask"
  | Trades Taker -> "Takers (actual trades)"
  | Trades Maker -> "Makers (actual trades)"
;;
