open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* What "h hours before close" is measured from.
   - [Close]: the market's actual close time. Literal, but for event-driven markets it
     uses information from the future (when the game ended, when the word was said).
   - [Schedule]: actual close for scheduled-close series; Kalshi's
     expected_expiration_time (fixed in advance) for event-driven series, keeping a
     horizon only if the market was still open at that moment. Implementable in real
     time, so this is the study's default.
   - [Expected]: expected_expiration_time for every market. The point-in-time rule
     used from Phase 7 on: almost every Kalshi market can close early, and an early
     close overwrites close_time, so the actual close of a scheduled-series market
     can still leak the outcome. The expected expiration is set in advance. *)
type t =
  | Close
  | Schedule
  | Expected
[@@deriving sexp, yojson, compare, equal, hash, enumerate]

let to_string = function
  | Close -> "actual close"
  | Schedule -> "scheduled time (expected expiration for event-driven markets)"
  | Expected -> "expected expiration (every market)"
;;

let time t ~(close_kind : Close_kind.t) ~close_time ~expected_expiration =
  match t, close_kind with
  | Close, _ | Schedule, Scheduled -> close_time
  | Schedule, Event_driven | Expected, _ ->
    Option.value expected_expiration ~default:close_time
;;
