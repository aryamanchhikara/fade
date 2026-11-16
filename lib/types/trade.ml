open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* One public fill. [yes_price] is in cents. The taker bought [taker_side] at that
   side's price; the maker took the other side at the complementary price. *)
type t =
  { trade_id : string
  ; ticker : string
  ; ts : Unix_time.t
  ; yes_price : float
  ; count : float
  ; taker_side : Side.t
  ; is_block : bool (** negotiated off-book block trade *)
  }
[@@deriving sexp, yojson, compare, equal]

let price t (side : Side.t) =
  match side with
  | Yes -> t.yes_price
  | No -> 100. -. t.yes_price
;;

(* Which side, and at what price, a participant in [role] ended up holding. *)
let position t (role : Role.t) =
  let side =
    match role with
    | Taker -> t.taker_side
    | Maker -> Side.opposite t.taker_side
  in
  side, price t side
;;
