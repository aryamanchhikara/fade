(** A rate-limited, retrying HTTP GET client for Kalshi's public market-data API. *)

open! Core
open! Async

type t

(** [requests_per_second] spaces out request *starts*; [max_in_flight] caps
    concurrency. Unauthenticated reads are limited per IP; about 5 per second is
    sustainable. *)
val create
  :  ?base_url:string
  -> ?requests_per_second:float
  -> ?max_in_flight:int
  -> unit
  -> t

val default_base_url : string

(** GET [path] (relative to the API root, e.g. ["/historical/cutoff"]). Retries with
    exponential backoff on 429, 5xx and network errors. Returns [Error] for other 4xx
    responses and after the retry budget is spent. *)
val get_json
  :  t
  -> ?query:(string * string) list
  -> string
  -> Yojson.Safe.t Deferred.Or_error.t

(** True for the [Error] that [get_json] returns on a 404 response. *)
val is_not_found : Error.t -> bool

(** Counters for the progress bar. *)
val requests_sent : t -> int

val retries : t -> int
