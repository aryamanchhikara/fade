open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* One slice of the data. Splits are one dimension at a time (category OR volume
   tercile OR event kind), never crossed, to keep the number of cells examined small. *)
type t =
  | All
  | Category of string
  | Volume_tercile of int (** 1 = lowest-volume third, 3 = highest *)
  | Event_kind of Event_kind.t
[@@deriving sexp, yojson, compare, equal, hash]

let to_string = function
  | All -> "All markets"
  | Category c -> c
  | Volume_tercile 1 -> "Low volume (bottom third)"
  | Volume_tercile 2 -> "Mid volume (middle third)"
  | Volume_tercile 3 -> "High volume (top third)"
  | Volume_tercile n -> sprintf "Volume tercile %d" n
  | Event_kind k -> String.capitalize (Event_kind.to_string k) ^ "s"
;;

(* A stable string id, used as a <select> value in the UI. *)
let to_id = function
  | All -> "all"
  | Category c -> "cat:" ^ c
  | Volume_tercile n -> sprintf "vol:%d" n
  | Event_kind k -> "kind:" ^ String.lowercase (Sexp.to_string (Event_kind.sexp_of_t k))
;;

let group = function
  | All -> "All"
  | Category _ -> "Category"
  | Volume_tercile _ -> "Volume"
  | Event_kind _ -> "Event type"
;;
