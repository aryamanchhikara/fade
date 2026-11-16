open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Rule_result = struct
  type t =
    { rule : Rule.t
    ; in_sample : Bucket_result.t
    ; out_of_time : Bucket_result.t option (** [None]: no out-of-time observations *)
    }
  [@@deriving sexp, yojson, compare, equal]
end

module Window = struct
  type t =
    { first_settle : string
    ; last_settle : string
    ; n_markets : int
    ; n_events : int
    ; n_trades : int
    ; n_in_sample_events : int
    ; n_out_of_time_events : int
    ; n_embargoed_events : int
    ; dropped : (string * int) list (** reason, count *)
    }
  [@@deriving sexp, yojson, compare, equal]
end

(* Everything the UI needs, written to web/data/report.json. *)
type t =
  { generated_at : string
  ; synthetic : bool (** true when built from the fixture data, not Kalshi *)
  ; window : Window.t
  ; config : Config.t
  ; config_hash : string
  ; examined_in_sample : int
    (** bucket x segment x horizon cells looked at to pick rules *)
  ; rules : Rule_result.t list
  ; verdict : Verdict.t
  ; segments : Segment.t list
  ; tables : Table.t list
  }
[@@deriving sexp, yojson, compare, equal]
