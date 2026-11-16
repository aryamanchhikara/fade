open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* The top of book at [ts], read from the hourly candlestick that ends at or just
   before [close_time - horizon]. Prices are in cents (0 to 100). [option] means the
   value may be missing: [None] when nobody was quoting that side. *)
type t =
  { ticker : string
  ; horizon : Horizon.t
  ; ts : Unix_time.t
  ; yes_bid : float option
  ; yes_ask : float option
  }
[@@deriving sexp, yojson, compare, equal]

(* Kalshi only quotes YES. Buying NO means selling YES to the best YES bid, so the NO
   ask is 100 minus the YES bid, and the NO bid is 100 minus the YES ask. *)
let ask t (side : Side.t) =
  match side with
  | Yes -> t.yes_ask
  | No -> Option.map t.yes_bid ~f:(fun b -> 100. -. b)
;;

let bid t (side : Side.t) =
  match side with
  | Yes -> t.yes_bid
  | No -> Option.map t.yes_ask ~f:(fun a -> 100. -. a)
;;
