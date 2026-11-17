open! Core
open Fade_types

(* Where `freeze`, `report` and `serve` get their dataset: the SQLite cache, or the
   synthetic fixtures with --demo. *)

type t =
  | Db of string
  | Demo of string

let param =
  let%map_open.Command demo = flag "--demo" no_arg ~doc:" use the synthetic fixtures"
  and db =
    flag "--db" (optional_with_default "data/fade.db" string) ~doc:"PATH SQLite cache"
  and fixtures =
    flag
      "--fixtures"
      (optional_with_default "test/fixtures/synthetic.json" string)
      ~doc:"PATH synthetic dataset for --demo"
  in
  if demo then Demo fixtures else Db db
;;

let is_demo = function
  | Demo _ -> true
  | Db _ -> false
;;

(* The dataset, plus the cache's sampling statistics for real data. *)
let load_with_stats t ~(anchor : Anchor.t) : Dataset.t * Fade_store.Load.Stats.t option =
  match t with
  | Demo path -> Fade_synth.of_json_string (In_channel.read_all path), None
  | Db path ->
    if not (Sys_unix.file_exists_exn path)
    then raise_s [%message "no cache found; run `fade fetch` first" (path : string)];
    let store = Fade_store.open_ path in
    let (d : Dataset.t), stats = Fade_store.Load.dataset store ~anchor in
    Fade_store.close store;
    eprintf
      "loaded %d markets (%d without event metadata), %d snapshots, %d trades; %d \
       event-driven and %d scheduled-close series\n\
       %!"
      stats.markets_loaded
      stats.markets_without_event_metadata
      (List.length d.snapshots)
      (List.length d.trades)
      stats.event_driven_series
      stats.scheduled_series;
    eprintf
      "population %d markets in scope; quote sample %d (%.1f%%); trade sample %d\n%!"
      stats.population
      stats.quote_sample
      (100.
       *. Float.of_int stats.quote_sample
       /. Float.of_int (Int.max 1 stats.population))
      stats.trade_sample;
    d, Some stats
;;

(* The dataset, plus (population, quote sample, trade sample) for real data. *)
let load_with_sampling t ~anchor =
  let d, stats = load_with_stats t ~anchor in
  ( d
  , Option.map stats ~f:(fun (s : Fade_store.Load.Stats.t) ->
      s.population, s.quote_sample, s.trade_sample) )
;;

let load t ~anchor = fst (load_with_sampling t ~anchor)
let now_string () = Unix_time.to_rfc3339 (Float.to_int (Core_unix.time ()))
