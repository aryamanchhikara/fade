open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Every knob that changes results. The whole record is hashed when rules are frozen,
   so changing any of these after freezing is detected. *)
type t =
  { data_since : string (** first settlement date included, YYYY-MM-DD *)
  ; cutoff : string (** in-sample / out-of-time boundary, YYYY-MM-DD *)
  ; min_n : int (** buckets with fewer observations are greyed out and never selected *)
  ; bucket_width : float (** cents *)
  ; contracts_per_order : float
    (** fees round up per order, so the per-contract fee depends on order size *)
  ; bootstrap_resamples : int
  ; seed : int
  ; volume_tercile_bounds : float list
    (** two volume thresholds, computed on in-sample markets only *)
  ; drop_early_closers : bool
    (** drop markets that closed before their scheduled time, whose "1h before
          close" was not knowable in advance *)
  ; selection : string (** human-readable statement of the selection criterion *)
  }
[@@deriving sexp, yojson, compare, equal]
