open! Core
open! Async
open Fade_types

(* Today minus 18 calendar months, e.g. 2026-11-16 -> 2025-05-16. *)
let eighteen_months_ago () =
  let today = Unix_time.to_date_string (Float.to_int (Unix.time ())) in
  match String.split today ~on:'-' |> List.map ~f:Int.of_string with
  | [ y; m; d ] ->
    let months = (y * 12) + (m - 1) - 18 in
    sprintf "%04d-%02d-%02d" (months / 12) ((months mod 12) + 1) (Int.min d 28)
  | _ -> assert false
;;

let command =
  Command.async
    ~summary:"Fetch Kalshi markets, quotes and trades into the SQLite cache"
    ~readme:(fun () ->
      "Resumable and idempotent: rerun after an interruption and it continues where it \
       stopped. Unauthenticated public endpoints only.")
    (let%map_open.Command since =
       flag
         "--since"
         (optional string)
         ~doc:"YYYY-MM-DD first settlement date (default: 18 months ago)"
     and until =
       flag
         "--until"
         (optional string)
         ~doc:"YYYY-MM-DD last settlement date, inclusive (default: none, list to today)"
     and db =
       flag "--db" (optional_with_default "data/fade.db" string) ~doc:"PATH SQLite cache"
     and min_volume =
       flag
         "--min-volume"
         (optional_with_default 500. float)
         ~doc:"N minimum lifetime volume in contracts (default 500)"
     and candle_fraction =
       flag
         "--candle-fraction"
         (optional_with_default 1. float)
         ~doc:
           "P fetch candles for a uniform random fraction P of markets in scope (default \
            1 = all)"
     and trade_sample =
       flag
         "--trade-sample"
         (optional_with_default 20_000 int)
         ~doc:"N markets whose trades to pull (default 20000)"
     and max_trade_pages =
       flag
         "--max-trade-pages"
         (optional_with_default 20 int)
         ~doc:"N per market per endpoint, 1000 trades each (default 20)"
     and rate =
       flag
         "--rate"
         (optional_with_default 5. float)
         ~doc:"R requests per second (default 5)"
     and stages =
       flag
         "--stages"
         (optional_with_default
            (List.map Fade_fetch.Fetch.Stage.all ~f:(fun s ->
               String.lowercase (Sexp.to_string (Fade_fetch.Fetch.Stage.sexp_of_t s))))
            (Arg_type.comma_separated string))
         ~doc:"LIST reference,markets,events,candles,trades (default all)"
     in
     fun () ->
       let since = Option.value_or_thunk since ~default:eighteen_months_ago in
       Fade_fetch.Fetch.run
         { db_path = db
         ; since = Unix_time.of_date_string_exn since
         ; until = Option.map until ~f:Unix_time.of_date_string_exn
         ; min_volume
         ; candle_fraction
         ; trade_sample
         ; max_trade_pages
         ; requests_per_second = rate
         ; stages = List.map stages ~f:Fade_fetch.Fetch.Stage.of_string
         })
;;
