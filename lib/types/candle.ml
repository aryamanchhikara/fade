open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* One hourly candlestick, reduced to what we use: the YES bid and ask (cents) at the
   end of the hour. [None]: nobody was quoting that side. *)
type t =
  { end_ts : Unix_time.t
  ; yes_bid : float option
  ; yes_ask : float option
  }
[@@deriving sexp, yojson, compare, equal]
