open! Core
open! Async

let default_base_url = "https://external-api.kalshi.com/trade-api/v2"

(* [mutable] fields can be updated in place with [<-]. Everything else in a record is
   immutable. *)
type t =
  { base : Uri.t
  ; interval : Time_ns.Span.t
  ; throttle : unit Throttle.t
  ; mutable next_slot : Time_ns.t
  ; mutable requests_sent : int
  ; mutable retries : int
  }

let create
      ?(base_url = default_base_url)
      ?(requests_per_second = 5.)
      ?(max_in_flight = 4)
      ()
  =
  { base = Uri.of_string base_url
  ; interval = Time_ns.Span.of_sec (1. /. requests_per_second)
  ; throttle = Throttle.create ~continue_on_error:true ~max_concurrent_jobs:max_in_flight
  ; next_slot = Time_ns.now ()
  ; requests_sent = 0
  ; retries = 0
  }
;;

let requests_sent t = t.requests_sent
let retries t = t.retries

(* Reserve the next start slot, so request starts are at least [interval] apart even
   with several requests in flight. *)
let wait_for_slot t =
  let now = Time_ns.now () in
  let slot = Time_ns.max now t.next_slot in
  t.next_slot <- Time_ns.add slot t.interval;
  Clock_ns.at slot
;;

(* After a 429 everyone backs off, not just the request that saw it. *)
let push_back t span =
  t.next_slot <- Time_ns.max t.next_slot (Time_ns.add (Time_ns.now ()) span)
;;

let uri t path query =
  let base_path = Uri.path t.base in
  Uri.with_query' (Uri.with_path t.base (base_path ^ path)) query
;;

let max_attempts = 9

let backoff attempt =
  (* 0.5s, 1s, 2s, ... capped at 60s, with +-25% jitter so retries don't line up. *)
  let base = Float.min 60. (0.5 *. (2. ** Float.of_int attempt)) in
  Time_ns.Span.of_sec (base *. (0.75 +. Random.float 0.5))
;;

let get_once t uri =
  (* [Deferred.t] is Async's promise: a value that becomes determined later.
     [let%bind] waits for one and continues with its result; [let%map] does the
     same when the last step is a pure function. Both come from ppx_let. *)
  let%bind () = wait_for_slot t in
  t.requests_sent <- t.requests_sent + 1;
  (* [Monitor.try_with] turns exceptions (DNS failure, TLS error, reset) into
     [Error] values instead of crashing the program. *)
  match%bind
    Monitor.try_with (fun () ->
      let%bind resp, body =
        Cohttp_async.Client.get
          ~headers:
            (Cohttp.Header.of_list
               [ "accept", "application/json"; "accept-encoding", "gzip" ])
          uri
      in
      let%map body = Cohttp_async.Body.to_string body in
      (* A page of 1000 markets is about 2.5MB of JSON but ~110KB gzipped: asking for
         gzip makes listing several times faster. *)
      let body =
        match Cohttp.Header.get (Cohttp.Response.headers resp) "content-encoding" with
        | Some "gzip" ->
          (match Ezgzip.decompress body with
           | Ok s -> s
           | Error (`Gzip e) ->
             raise_s
               [%message "gzip decode failed" (Format.asprintf "%a" Ezgzip.pp_error e)])
        | _ -> body
      in
      Cohttp.Response.status resp |> Cohttp.Code.code_of_status, body)
  with
  | Ok (status, body) -> return (`Status (status, body))
  | Error exn -> return (`Network exn)
;;

let get_json t ?(query = []) path =
  let uri = uri t path query in
  let rec attempt n =
    let retry why =
      if n + 1 >= max_attempts
      then
        return
          (Or_error.error_s
             [%message "giving up" (Uri.to_string uri) (why : string) (n : int)])
      else (
        t.retries <- t.retries + 1;
        let span = backoff n in
        if String.equal why "429" then push_back t span;
        let%bind () = Clock_ns.after span in
        attempt (n + 1))
    in
    match%bind Throttle.enqueue t.throttle (fun () -> get_once t uri) with
    | `Network exn -> retry (Exn.to_string exn)
    | `Status (200, body) ->
      return
        (Or_error.try_with (fun () -> Yojson.Safe.from_string body)
         |> Or_error.tag ~tag:(Uri.to_string uri))
    | `Status (429, _) -> retry "429"
    | `Status (code, _) when code >= 500 -> retry (Int.to_string code)
    | `Status (code, body) ->
      return
        (Or_error.error_s
           [%message
             "Kalshi API error"
               (Uri.to_string uri)
               (code : int)
               ~body:(String.prefix body 300 : string)])
  in
  attempt 0
;;
