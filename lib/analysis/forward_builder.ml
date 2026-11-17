open! Core
open Fade_types

(* The forward test of the locked candidate: which markets count, each rule's status,
   and how far it is from a conclusion. Pure; `fade forward` does the fetching and
   remembers conclusions between runs. *)

(* Markets settled in [start, stop): [forward_days] calendar days from the start. *)
let window (config : Candidate.Config.t) =
  let start = Unix_time.of_date_string_exn config.forward_start in
  start, start + (config.forward_days * 86_400)
;;

let in_window config (m : Market.t) =
  let start, stop = window config in
  m.settle_time >= start && m.settle_time < stop
;;

(* Day of the window that [now] falls on: 1 on the start date, [forward_days] on the
   last, 0 before the start. *)
let day (config : Candidate.Config.t) ~now =
  let start, _ = window config in
  if now < start then 0 else Int.min config.forward_days (((now - start) / 86_400) + 1)
;;

let excludes_zero (b : Bucket_result.t) =
  Bucket_result.proven_positive b || Bucket_result.proven_negative b
;;

(* A return CI narrows roughly like 1 / sqrt(events). With mean [m] and half-width [h]
   at [n] events, it clears zero once h * sqrt(n / N) < |m|, i.e. at
   N = n * (h / m)^2 events. Before the forward test has enough fills for a CI, the
   in-sample effect and spread stand in. *)
let projection ~min_n ~(forward : Bucket_result.t) ~(in_sample : Bucket_result.t) =
  let total (b : Bucket_result.t) =
    match b.return_ci with
    | Some ci when Float.( > ) (Float.abs b.mean_return) 1e-6 ->
      let h = (ci.hi -. ci.lo) /. 2. in
      Some (Float.iround_up_exn (Float.of_int b.n_events *. ((h /. b.mean_return) ** 2.)))
    | _ -> None
  in
  if excludes_zero forward
  then Some 0, "concluded"
  else if forward.enough
  then (
    match total forward with
    | Some n -> Some (Int.max 1 (n - forward.n_events)), "forward data"
    | None -> None, "no estimate: the forward return is about zero")
  else (
    match total in_sample with
    | Some n ->
      ( Some (Int.max (n - forward.n_events) (min_n - forward.n))
      , sprintf
          "in-sample effect (forward has %d of the %d fills a CI needs)"
          forward.n
          min_n )
    | None -> None, "no estimate yet")
;;

let status ~(config : Candidate.Config.t) ~now (b : Bucket_result.t) : Forward.Status.t =
  let _, stop = window config in
  if Bucket_result.proven_positive b
  then Supported
  else if Bucket_result.proven_negative b
  then Rejected
  else if now >= stop
  then Inconclusive
  else Running
;;

(* [concluded rule_id] is a conclusion recorded by an earlier run (the date and the
   status): the test of that rule stopped then, whatever later data says. *)
let build
      ~generated_at
      ~now
      ~synthetic
      ~(candidate : Candidate.t)
      ~(d : Dataset.t)
      ~traded
      ~concluded
  : Forward.t
  =
  let config = candidate.config in
  let markets = List.filter d.markets ~f:(in_window config) in
  let d = { d with markets } in
  let scores =
    Candidate_score.score_all
      ~config
      ~rules:candidate.rules
      ~d
      ~include_market:(fun _ -> true)
      ~traded
      ~stream:1
  in
  let rules =
    List.map2_exn candidate.rules scores ~f:(fun rule (forward : Candidate.Score.t) ->
      let in_sample =
        List.find_exn candidate.in_sample ~f:(fun s -> String.equal s.rule_id rule.id)
      in
      let status, concluded_on =
        match concluded rule.id with
        | Some (date, status) -> status, Some date
        | None ->
          let s = status ~config ~now forward.result in
          ( s
          , (match s with
             | Supported | Rejected -> Some (Unix_time.to_date_string now)
             | Running | Inconclusive -> None) )
      in
      let more_events, projection =
        projection ~min_n:config.min_n ~forward:forward.result ~in_sample:in_sample.result
      in
      { Forward.Rule_report.rule
      ; in_sample
      ; forward
      ; status
      ; concluded_on
      ; more_events
      ; projection
      })
  in
  let horizons = List.map candidate.rules ~f:(fun r -> r.horizon) in
  let in_d = String.Hash_set.of_list (List.map markets ~f:(fun m -> m.ticker)) in
  let quoted =
    List.filter d.snapshots ~f:(fun s ->
      Hash_set.mem in_d s.ticker && List.mem horizons s.horizon ~equal:Horizon.equal)
    |> List.map ~f:(fun s -> s.ticker)
    |> String.Set.of_list
  in
  let settles = List.map markets ~f:(fun m -> m.settle_time) in
  let _, stop = window config in
  { generated_at
  ; synthetic
  ; candidate_hash = candidate.hash
  ; config
  ; forward_end = Unix_time.to_date_string (stop - 86_400)
  ; day = day config ~now
  ; listed_markets = List.length markets
  ; quoted_markets = Set.length quoted
  ; traded_markets = List.count markets ~f:(fun m -> traded m.ticker)
  ; first_settle =
      List.min_elt settles ~compare:Int.compare |> Option.map ~f:Unix_time.to_date_string
  ; last_settle =
      List.max_elt settles ~compare:Int.compare |> Option.map ~f:Unix_time.to_date_string
  ; rules
  ; notes = []
  }
;;

let%expect_test "window, day and projection" =
  let config : Candidate.Config.t =
    { study_hash = ""
    ; in_sample_since = "2025-05-16"
    ; in_sample_cutoff = "2026-05-16"
    ; forward_start = "2026-11-17"
    ; forward_days = 60
    ; min_volume = 500.
    ; anchor = Schedule
    ; min_n = 30
    ; bootstrap_resamples = 1000
    ; seed = 1
    ; stop_rule = ""
    }
  in
  let start, stop = window config in
  printf
    "%s to %s (exclusive); days: %d %d %d %d\n"
    (Unix_time.to_date_string start)
    (Unix_time.to_date_string stop)
    (day config ~now:(start - 1))
    (day config ~now:start)
    (day config ~now:(start + (59 * 86_400) + 1))
    (day config ~now:(stop + (30 * 86_400)));
  let b ~n ~events ~mean ~lo ~hi : Bucket_result.t =
    { lo = 90.
    ; hi = 95.
    ; n
    ; n_events = events
    ; contracts = Float.of_int n
    ; wins = 0.
    ; win_rate = 0.
    ; win_ci = { lo = 0.; hi = 0. }
    ; avg_price = 92.
    ; avg_fee = 0.5
    ; breakeven = 0.925
    ; mean_return = mean
    ; return_ci = (if n >= 30 then Some { Ci.lo; hi } else None)
    ; enough = n >= 30
    }
  in
  let show ~forward ~in_sample =
    let more, basis = projection ~min_n:30 ~forward ~in_sample in
    printf
      "%s | %s | %s\n"
      (Option.value_map more ~default:"-" ~f:Int.to_string)
      basis
      (Forward.Status.to_string (status ~config ~now:start forward))
  in
  let in_sample = b ~n:186 ~events:185 ~mean:(-0.027) ~lo:(-0.082) ~hi:0.013 in
  (* Nothing yet: projected from in-sample: 185 * (0.0475 / 0.027)^2 = 573 events. *)
  show ~forward:(b ~n:0 ~events:0 ~mean:0. ~lo:0. ~hi:0.) ~in_sample;
  (* 100 events at +3% +- 6%: 100 * (0.06 / 0.03)^2 = 400, so 300 more. *)
  show ~forward:(b ~n:100 ~events:100 ~mean:0.03 ~lo:(-0.03) ~hi:0.09) ~in_sample;
  show ~forward:(b ~n:400 ~events:400 ~mean:0.03 ~lo:0.001 ~hi:0.06) ~in_sample;
  show ~forward:(b ~n:400 ~events:400 ~mean:(-0.05) ~lo:(-0.08) ~hi:(-0.02)) ~in_sample;
  [%expect
    {|
    2026-11-17 to 2027-01-16 (exclusive); days: 0 1 60 60
    573 | in-sample effect (forward has 0 of the 30 fills a CI needs) | Running
    300 | forward data | Running
    0 | concluded | Edge supported
    0 | concluded | Rejected
    |}]
;;
