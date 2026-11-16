open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* A record: a fixed set of named fields. Fields are immutable unless declared
   [mutable]; you "update" a record by building a copy, [{ m with volume = 0. }]. *)
type t =
  { ticker : string
  ; event_ticker : string
  ; series_ticker : string
  ; category : string
  ; open_time : Unix_time.t
  ; close_time : Unix_time.t
    (** When trading stopped. For markets that can close early this is the actual
          close, which may be earlier than scheduled. *)
  ; settle_time : Unix_time.t
  ; result : Side.t
  ; volume : float (** contracts traded over the market's life *)
  ; can_close_early : bool
  ; expected_expiration : Unix_time.t option
    (** Kalshi's [expected_expiration_time], fixed in advance *)
  ; close_kind : Close_kind.t
  ; event_kind : Event_kind.t
  }
[@@deriving sexp, yojson, compare, equal]
