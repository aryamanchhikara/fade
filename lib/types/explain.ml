open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* web/data/explain.json, written by `fade explain`: what predicts NO outcomes beyond
   the recalibrated price. Aggregates only. *)

module Fold_value = struct
  type t =
    { month : string
    ; n : int
    ; mean : float
    }
  [@@deriving sexp, yojson, compare, equal]
end

(* One level of one feature, e.g. spread "> 10c": the mean out-of-sample residual
   (1 if NO won, minus the recalibrated price's P(NO)) of the rows at that level. *)
module Level = struct
  type t =
    { feature : string
    ; level : string
    ; order : int (** display order within the feature *)
    ; n : int
    ; n_events : int
    ; mean_price_p : float (** average recalibrated P(NO) *)
    ; mean_residual : float
    ; ci : Ci.t option (** event-clustered 95% CI *)
    ; folds : Fold_value.t list (** test months with at least [min_fold_rows] rows *)
    ; folds_agree : int (** of those, how many have the pooled sign *)
    ; single_fold : bool
      (** the pooled CI excludes zero, but leaving out one fold makes it include zero,
          or only one fold has data *)
    }
  [@@deriving sexp, yojson, compare, equal]

  let key t = t.feature ^ ": " ^ t.level
end

(* Within one price bucket, the feature's mean among NO winners vs NO losers. *)
module Win_loss = struct
  type t =
    { feature : string
    ; lo : float
    ; hi : float
    ; n_win : int
    ; n_loss : int
    ; mean_win : float
    ; mean_loss : float
    ; diff : float (** win minus loss *)
    ; ci : Ci.t option (** event-clustered 95% CI on the difference *)
    }
  [@@deriving sexp, yojson, compare, equal]
end

module Fold = struct
  type t =
    { month : string
    ; n_train : int
    ; n_test : int
    ; events_test : int
    ; baseline : string (** "Platt" or "isotonic", chosen on validation *)
    ; validation_platt : float (** validation log loss *)
    ; validation_isotonic : float
    ; test_raw : float (** test log loss of the raw ask as a probability *)
    ; test_baseline : float (** test log loss of the recalibrated price *)
    }
  [@@deriving sexp, yojson, compare, equal]
end

module Feature = struct
  type t =
    { id : string
    ; name : string
    ; description : string
    ; numeric : bool (** has a win-vs-loss comparison *)
    }
  [@@deriving sexp, yojson, compare, equal]
end

type t =
  { generated_at : string
  ; synthetic : bool
  ; data_since : string
  ; data_until : string
  ; anchor : string
  ; min_volume : float
  ; embargo_days : int
  ; validation_months : int
  ; min_fold_rows : int
  ; rows : int
  ; markets : int
  ; events : int
  ; test_rows : int
  ; leakage_checked : int
  ; leakage_failures : int
  ; exclusions : (string * int) list
  ; folds : Fold.t list
  ; features : Feature.t list
  ; levels : Level.t list
  ; ranking : string list (** [Level.key]s, strongest consistent effect first *)
  ; win_loss : Win_loss.t list
  ; notes : string list
  }
[@@deriving sexp, yojson, compare, equal]
