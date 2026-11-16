open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* How a series' markets stop trading.
   - [Scheduled]: at a time fixed when the market was created (crypto, indices,
     weather). These close exactly on a whole minute.
   - [Event_driven]: when something happens (a game ends, a word is said). These close
     at arbitrary seconds, and "1h before close" is not knowable in advance. Worse, for
     "will X happen by ..." markets an early close usually means YES.
   Classified per series from close timestamps alone, never from outcomes or prices. *)
type t =
  | Scheduled
  | Event_driven
[@@deriving sexp, yojson, compare, equal, hash, enumerate]

let to_string = function
  | Scheduled -> "scheduled close"
  | Event_driven -> "event-driven close"
;;

(* A series is event-driven if at least this share of its markets closed off the
   whole-minute grid. Scheduled series are at 0% in practice; event-driven ones at
   95% or more. *)
let off_grid_threshold = 0.10
let off_grid (close_time : Unix_time.t) = close_time mod 60 <> 0
