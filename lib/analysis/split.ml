open! Core
open Fade_types

(* In-sample / out-of-time partition with an embargo, at the level of events (markets
   in one event are correlated, so an event is never split across the boundary).

   - in-sample: every market of the event settled before the cutoff;
   - out-of-time: every decision the event could produce (up to 24h before each
     market's anchor) happens at or after the cutoff;
   - otherwise the event straddles the cutoff and is embargoed (dropped). *)

type t =
  { sample_of_event : Sample.t String.Table.t
  ; embargoed : int
  }

let anchor_time (config : Config.t) (m : Market.t) =
  Anchor.time
    config.anchor
    ~close_kind:m.close_kind
    ~close_time:m.close_time
    ~expected_expiration:m.expected_expiration
;;

let create ~(config : Config.t) (markets : Market.t list) =
  let cutoff = Unix_time.of_date_string_exn config.cutoff in
  let by_event =
    List.map markets ~f:(fun m -> m.event_ticker, m) |> String.Map.of_alist_multi
  in
  let sample_of_event = String.Table.create () in
  let embargoed = ref 0 in
  Map.iteri by_event ~f:(fun ~key ~data:ms ->
    let last_settle =
      List.map ms ~f:(fun m -> m.settle_time) |> List.max_elt ~compare:Int.compare
    in
    let first_decision =
      List.map ms ~f:(fun m -> anchor_time config m - Horizon.seconds H24)
      |> List.min_elt ~compare:Int.compare
    in
    match last_settle, first_decision with
    | Some s, _ when s < cutoff -> Hashtbl.set sample_of_event ~key ~data:Sample.In_sample
    | _, Some d when d >= cutoff ->
      Hashtbl.set sample_of_event ~key ~data:Sample.Out_of_time
    | _ -> incr embargoed);
  { sample_of_event; embargoed = !embargoed }
;;

let sample t (m : Market.t) = Hashtbl.find t.sample_of_event m.event_ticker
let count t s = Hashtbl.count t.sample_of_event ~f:(Sample.equal s)

(* Default cutoff: six calendar months before the last settlement in the data. *)
let default_cutoff (markets : Market.t list) =
  let last =
    List.map markets ~f:(fun m -> m.settle_time)
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  match
    String.split (Unix_time.to_date_string last) ~on:'-' |> List.map ~f:Int.of_string
  with
  | [ y; m; d ] ->
    let months = (y * 12) + (m - 1) - 6 in
    sprintf "%04d-%02d-%02d" (months / 12) ((months mod 12) + 1) (Int.min d 28)
  | _ -> assert false
;;

let%expect_test "partition with embargo" =
  let day d = Unix_time.of_date_string_exn d in
  let market ~event ~close ~settle : Market.t =
    { ticker = event ^ "-" ^ close
    ; event_ticker = event
    ; series_ticker = "S"
    ; category = "C"
    ; open_time = day close - (10 * 86_400)
    ; close_time = day close
    ; settle_time = day settle
    ; result = No
    ; volume = 1000.
    ; can_close_early = false
    ; expected_expiration = None
    ; close_kind = Scheduled
    ; event_kind = Single
    }
  in
  let config : Config.t =
    { data_since = "2025-05-16"
    ; cutoff = "2026-03-01"
    ; min_n = 30
    ; bucket_width = 5.
    ; contracts_per_order = 100.
    ; bootstrap_resamples = 100
    ; seed = 1
    ; volume_tercile_bounds = []
    ; anchor = Schedule
    ; selection = ""
    }
  in
  let t =
    create
      ~config
      [ market ~event:"BEFORE" ~close:"2026-02-10" ~settle:"2026-02-11"
      ; (* one market settles after the cutoff: the whole event is embargoed *)
        market ~event:"STRADDLE" ~close:"2026-02-27" ~settle:"2026-02-28"
      ; market ~event:"STRADDLE" ~close:"2026-03-05" ~settle:"2026-03-05"
      ; (* closes the day after the cutoff: its 24h-before decision is before it *)
        market ~event:"EDGE" ~close:"2026-03-01" ~settle:"2026-03-02"
      ; market ~event:"AFTER" ~close:"2026-03-03" ~settle:"2026-03-03"
      ]
  in
  List.iter [ "BEFORE"; "STRADDLE"; "EDGE"; "AFTER" ] ~f:(fun e ->
    let sample = Hashtbl.find t.sample_of_event e in
    print_s [%message e (sample : Sample.t option)]);
  print_s [%message (t.embargoed : int)];
  [%expect
    {|
    (BEFORE (sample (In_sample)))
    (STRADDLE (sample ()))
    (EDGE (sample ()))
    (AFTER (sample (Out_of_time)))
    (t.embargoed 2)
    |}]
;;
