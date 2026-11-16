open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Everything needed to price a fee at any moment in the data window. *)

module Change = struct
  (* A scheduled change. For event overrides, [None] in both fields clears the
     override and the event falls back to its series' structure. *)
  type t =
    { ts : Unix_time.t
    ; fee_type : Fee_type.t option
    ; multiplier : float option
    }
  [@@deriving sexp, yojson, compare, equal]
end

module Series = struct
  type t =
    { series_ticker : string
    ; current_type : Fee_type.t
    ; current_multiplier : float
    ; changes : Change.t list (** from GET /series/fee_changes?show_historical=true *)
    }
  [@@deriving sexp, yojson, compare, equal]
end

module Event_override = struct
  type t =
    { event_ticker : string
    ; changes : Change.t list (** from GET /events/fee_changes *)
    }
  [@@deriving sexp, yojson, compare, equal]
end

type t =
  { series : Series.t list
  ; event_overrides : Event_override.t list
  }
[@@deriving sexp, yojson, compare, equal]

let empty = { series = []; event_overrides = [] }
