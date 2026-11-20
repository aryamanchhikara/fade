open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* web/data/backtest.json, written by `fade backtest`: the EV rule traded on
   out-of-sample predictions, walk-forward. Aggregates only. *)

module Summary = struct
  type t =
    { trades : int
    ; events : int
    ; hit_rate : float (** share of trades where NO won *)
    ; avg_price : float (** cents paid per contract, before fees *)
    ; avg_fee : float (** cents per contract *)
    ; avg_ev : float (** predicted EV per contract, dollars *)
    ; return_per_dollar : float
    ; ci : Ci.t option (** 95%, resampling events *)
    ; profit : float (** dollars, for the order size *)
    ; max_drawdown : float (** dollars, cumulative P&L in settlement order *)
    }
  [@@deriving sexp, yojson, compare, equal]
end

module Month = struct
  type t =
    { month : string
    ; threshold : float (** EV threshold in dollars, chosen on the two months before *)
    ; summary : Summary.t
    }
  [@@deriving sexp, yojson, compare, equal]
end

module Variant = struct
  type t =
    { model : string
    ; size : int
    ; slippage : float (** cents added to the ask, for size > 1 *)
    ; months : Month.t list
    ; total : Summary.t
    ; by_category : (string * Summary.t) list
    }
  [@@deriving sexp, yojson, compare, equal]
end

type t =
  { generated_at : string
  ; synthetic : bool
  ; max_spread : float
  ; thresholds : float list (** the grid, dollars *)
  ; disagreement : (string * float) list
    (** per model: share of test rows where |P_model - ask| exceeds the fee *)
  ; variants : Variant.t list
  ; notes : string list
  }
[@@deriving sexp, yojson, compare, equal]
