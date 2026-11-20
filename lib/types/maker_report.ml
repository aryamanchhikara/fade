open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* web/data/maker.json, written by `fade maker`: the model-gated maker rule, walk-forward.
   Aggregates only. *)

(* Adverse selection: do the orders that fill win less often than the ones that do
   not, and less often than the model said? *)
module Adverse = struct
  type t =
    { orders : int
    ; filled : int
    ; win_rate_filled : float
    ; win_rate_unfilled : float
    ; model_p_filled : float (** mean P_model(NO) of filled orders *)
    ; model_p_unfilled : float
    }
  [@@deriving sexp, yojson, compare, equal]
end

module Month = struct
  type t =
    { month : string
    ; threshold : float (** dollars, chosen on the two months before *)
    ; horizon : string (** "24h", "6h", "1h" or "first" *)
    ; orders : int
    ; summary : Backtest_report.Summary.t (** over filled orders *)
    }
  [@@deriving sexp, yojson, compare, equal]
end

module Variant = struct
  type t =
    { fill_model : string (** "strict" or "loose" *)
    ; months : Month.t list
    ; orders : int
    ; total : Backtest_report.Summary.t
    ; adverse : Adverse.t
    ; calibration_on_fills : Train_report.Calibration_bin.t list
    }
  [@@deriving sexp, yojson, compare, equal]
end

module Tally = struct
  type t =
    { phase : string
    ; what : string
    ; count : int
    }
  [@@deriving sexp, yojson, compare, equal]
end

type t =
  { generated_at : string
  ; synthetic : bool
  ; model : string
  ; rows : int
  ; no_room : int (** two-sided rows with a 1c spread: no bid strictly inside *)
  ; variants : Variant.t list
  ; tally : Tally.t list
  ; tally_total : int
  ; verdict : string
  ; notes : string list
  }
[@@deriving sexp, yojson, compare, equal]
