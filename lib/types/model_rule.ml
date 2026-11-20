open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* rules/model_rule.json: the EV rule chosen in the Phase 9 backtest, frozen with the
   exact model files it uses before any forward evaluation. `fade opportunities` and
   `fade paper` refuse a file whose hash no longer matches, or model files whose
   contents changed. *)

module Model_file = struct
  type t =
    { name : string (** "logistic", "lightgbm" or "baseline" *)
    ; path : string
    ; sha256 : string (** of the file's bytes *)
    }
  [@@deriving sexp, yojson, compare, equal]
end

module Config = struct
  type t =
    { model : Model_file.t (** P(NO) for the EV *)
    ; baseline : Model_file.t (** the recalibrated price, shown for comparison *)
    ; data_since : string (** the models' training window *)
    ; data_until : string
    ; threshold : float (** trade when EV per contract exceeds this, dollars *)
    ; max_spread : float (** cents *)
    ; sizes : int list
    ; slippage : float (** cents above the ask for sizes above 1 *)
    ; min_volume : float (** volume to date at the quote *)
    ; horizons : Horizon.t list
    ; one_trade_per_market : bool
    ; selection : string
    }
  [@@deriving sexp, yojson, compare, equal]
end

type t =
  { frozen_at : string
  ; config : Config.t
  ; backtest : (string * Backtest_report.Summary.t) list
    (** the chosen model's walk-forward backtest, per size, for the record *)
  ; hash : string
  ; notes : string list (** not hashed *)
  }
[@@deriving sexp, yojson, compare, equal]
