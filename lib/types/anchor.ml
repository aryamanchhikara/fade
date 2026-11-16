open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* What "h hours before close" is measured from.
   - [Close]: the market's actual close time. Literal, but for event-driven markets it
     uses information from the future (when the game ended, when the word was said).
   - [Schedule]: actual close for scheduled-close series; Kalshi's
     expected_expiration_time (fixed in advance) for event-driven series, keeping a
     horizon only if the market was still open at that moment. Implementable in real
     time, so this is the default. *)
type t =
  | Close
  | Schedule
[@@deriving sexp, yojson, compare, equal, hash, enumerate]

let to_string = function
  | Close -> "actual close"
  | Schedule -> "scheduled time (expected expiration for event-driven markets)"
;;

let time t ~(close_kind : Close_kind.t) ~close_time ~expected_expiration =
  match t, close_kind with
  | Close, _ | Schedule, Scheduled -> close_time
  | Schedule, Event_driven -> Option.value expected_expiration ~default:close_time
;;
