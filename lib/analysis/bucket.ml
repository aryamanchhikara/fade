open! Core
open Fade_types

(* Summary statistics for one price bucket of observations. *)

let lo_of ~width price = Float.round_down (price /. width) *. width

(* [compute] takes the few parameters it needs directly, so the candidate scorer can use
   it without a study [Config.t]; [result] is the study's wrapper. *)
let compute
      ?(rng : Random.State.t option)
      ~min_n
      ~resamples
      ~lo
      ~hi
      (obs : Observation.t list)
  : Bucket_result.t
  =
  let n = List.length obs in
  let sum f = List.sum (module Float) obs ~f in
  let contracts = sum (fun o -> o.weight) in
  let wins = sum (fun o -> if o.won then o.weight else 0.) in
  let total_cost = sum (fun o -> o.weight *. Observation.cost o) in
  let total_profit = sum (fun o -> o.weight *. Observation.profit o) in
  let win_rate = if Float.( > ) contracts 0. then wins /. contracts else 0. in
  let enough = n >= min_n in
  (* Return per dollar staked: total profit over total cost. In the quote view every
     observation is one contract, so this is "buy one contract in each market". *)
  let mean_return =
    if Float.( > ) total_cost 0. then total_profit /. total_cost else 0.
  in
  let n_events, clusters =
    let by_event = String.Table.create () in
    List.iter obs ~f:(fun o ->
      Hashtbl.update by_event o.market.event_ticker ~f:(fun prev ->
        let num, den = Option.value prev ~default:(0., 0.) in
        num +. (o.weight *. Observation.profit o), den +. (o.weight *. Observation.cost o)));
    ( Hashtbl.length by_event
    , Hashtbl.data by_event
      |> List.map ~f:(fun (num, den) -> { Stats.num; den })
      |> Array.of_list )
  in
  let avg_cost = if Float.( > ) contracts 0. then total_cost /. contracts else 0. in
  let return_ci =
    match rng with
    | Some rng when enough && Float.( > ) avg_cost 0. ->
      (* Two intervals, report the wider envelope:
         1. percentile bootstrap resampling whole events (handles correlation, but
            collapses to a point when a bucket happens to contain no losses, which is
            common for 95c contracts);
         2. Wilson interval on the win rate using the number of *events* as the sample
            size (conservative for clustering, and well behaved at 0 or n losses),
            mapped to a return: profit per dollar is linear in the win rate. *)
      let boot = Stats.cluster_bootstrap ~rng ~resamples clusters in
      let n_eff = Float.of_int n_events in
      let w = Stats.wilson ~successes:(win_rate *. n_eff) ~n:n_eff in
      let to_return rate = ((100. *. rate) -. avg_cost) /. avg_cost in
      let wilson : Ci.t = { lo = to_return w.lo; hi = to_return w.hi } in
      Some
        (match boot with
         | None -> wilson
         | Some b -> { Ci.lo = Float.min b.lo wilson.lo; hi = Float.max b.hi wilson.hi })
    | _ -> None
  in
  { lo
  ; hi
  ; n
  ; n_events
  ; contracts
  ; wins
  ; win_rate
  ; win_ci = Stats.wilson ~successes:(win_rate *. Float.of_int n) ~n:(Float.of_int n)
  ; avg_price =
      (if Float.( > ) contracts 0.
       then sum (fun o -> o.weight *. o.price) /. contracts
       else 0.)
  ; avg_fee =
      (if Float.( > ) contracts 0.
       then sum (fun o -> o.weight *. o.fee) /. contracts
       else 0.)
  ; breakeven = avg_cost /. 100.
  ; mean_return
  ; return_ci
  ; enough
  }
;;

let result ?rng ~(config : Config.t) ~lo obs =
  compute
    ?rng
    ~min_n:config.min_n
    ~resamples:config.bootstrap_resamples
    ~lo
    ~hi:(lo +. config.bucket_width)
    obs
;;
