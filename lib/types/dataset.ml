open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* The full input to the analysis: what [Fade_store] loads from SQLite, or what the
   synthetic fixtures contain. Analysis code never does IO; it only sees this. *)
type t =
  { markets : Market.t list
  ; snapshots : Snapshot.t list
  ; trades : Trade.t list
  ; fees : Fee_schedule.t
  }
[@@deriving sexp, yojson, compare, equal]
