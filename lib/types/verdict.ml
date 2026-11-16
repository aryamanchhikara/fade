open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Tone = struct
  type t =
    | Green (** some frozen rule's out-of-time return CI is entirely above zero *)
    | Red (** every scored frozen rule's out-of-time return CI is entirely below zero *)
    | Neutral (** anything else: no proven edge *)
  [@@deriving sexp, yojson, compare, equal]
end

type t =
  { tone : Tone.t
  ; headline : string
  ; detail : string
  }
[@@deriving sexp, yojson, compare, equal]
