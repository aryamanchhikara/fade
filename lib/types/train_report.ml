open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* web/data/train.json, written by `fade train`: each model against the recalibrated
   price, out of sample, per walk-forward month and pooled. Aggregates only. *)

module Metrics = struct
  type t =
    { log_loss : float
    ; brier : float
    ; ece : float
    }
  [@@deriving sexp, yojson, compare, equal]
end

(* Mean per-row (model - baseline) loss: negative is better than the baseline. *)
module Diff = struct
  type t =
    { mean : float
    ; ci : Ci.t option (** 95%, resampling events *)
    }
  [@@deriving sexp, yojson, compare, equal]

  let better t = Option.value_map t.ci ~default:false ~f:Ci.below_zero
end

module Model_score = struct
  type t =
    { model : string
    ; metrics : Metrics.t
    ; log_loss_vs_baseline : Diff.t option (** [None] for the baseline itself *)
    ; brier_vs_baseline : Diff.t option
    ; chosen : string (** hyperparameters picked on validation months *)
    }
  [@@deriving sexp, yojson, compare, equal]
end

module Fold = struct
  type t =
    { month : string
    ; n_train : int
    ; n_test : int
    ; events_test : int
    ; models : Model_score.t list
    }
  [@@deriving sexp, yojson, compare, equal]
end

module Calibration_bin = struct
  type t =
    { lo : float
    ; hi : float
    ; n : int
    ; mean_p : float
    ; observed : float
    }
  [@@deriving sexp, yojson, compare, equal]
end

module Pooled = struct
  type t =
    { model : string
    ; metrics : Metrics.t
    ; log_loss_vs_baseline : Diff.t option
    ; brier_vs_baseline : Diff.t option
    ; calibration : Calibration_bin.t list
    }
  [@@deriving sexp, yojson, compare, equal]
end

module Saved = struct
  type t =
    { model : string
    ; path : string
    ; hash : string (** sha256 of data window + feature list + parameters *)
    }
  [@@deriving sexp, yojson, compare, equal]
end

type t =
  { generated_at : string
  ; synthetic : bool
  ; data_since : string
  ; data_until : string
  ; features : string list
  ; rows : int
  ; test_rows : int
  ; folds : Fold.t list
  ; pooled : Pooled.t list
  ; beats_baseline : string list (** models whose pooled log-loss CI is below zero *)
  ; verdict : string
  ; saved : Saved.t list
  ; notes : string list
  }
[@@deriving sexp, yojson, compare, equal]
