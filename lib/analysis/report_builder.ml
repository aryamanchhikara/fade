open! Core
open Fade_types

(* From a dataset and frozen rules to the full [Report.t]. *)

let default_config
      ?cutoff
      ?(min_n = 30)
      ?(anchor = Anchor.Schedule)
      ?(contracts_per_order = 100.)
      ?(bootstrap_resamples = 1000)
      (d : Dataset.t)
  : Config.t
  =
  let first_settle =
    List.map d.markets ~f:(fun m -> m.settle_time)
    |> List.min_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  let base : Config.t =
    { data_since = Unix_time.to_date_string first_settle
    ; cutoff =
        Option.value_or_thunk cutoff ~default:(fun () -> Split.default_cutoff d.markets)
    ; min_n
    ; bucket_width = 5.
    ; contracts_per_order
    ; bootstrap_resamples
    ; seed = 20260516
    ; volume_tercile_bounds = []
    ; anchor
    ; selection = Frozen.selection_text
    }
  in
  (* Tercile bounds come from in-sample markets only. *)
  let split = Split.create ~config:base d.markets in
  let in_sample_volumes =
    List.filter_map d.markets ~f:(fun m ->
      match Split.sample split m with
      | Some In_sample -> Some m.volume
      | _ -> None)
  in
  { base with
    volume_tercile_bounds =
      Tables.tercile_bounds in_sample_volumes
      |> List.map ~f:(Float.round_decimal ~decimal_digits:2)
  }
;;

let observations ~(config : Config.t) (d : Dataset.t) ~drops =
  let fees = Fees.Index.create d.fees in
  [ View.Quotes, Observation.of_quotes d ~config ~fees ~drops
  ; Trades Taker, Observation.of_trades d ~config ~fees ~drops ~role:Taker
  ; Trades Maker, Observation.of_trades d ~config ~fees ~drops ~role:Maker
  ]
;;

(* Freezing only ever builds in-sample tables: out-of-time observations are filtered out
   before any statistic is computed. *)
let freeze ~frozen_at ~(config : Config.t) (d : Dataset.t) =
  let split = Split.create ~config d.markets in
  let drops = Observation.Drops.create () in
  let in_sample_only =
    observations ~config d ~drops
    |> List.filter_map ~f:(fun (view, obs) ->
      match view with
      | View.Quotes ->
        Some
          ( view
          , List.filter obs ~f:(fun (o : Observation.t) ->
              match Split.sample split o.market with
              | Some In_sample -> true
              | _ -> false) )
      | Trades _ -> None)
  in
  let tables = Tables.build ~config ~split ~observations:in_sample_only () in
  let rules, examined = Frozen.select ~in_sample_tables:tables in
  Frozen.create
    ~frozen_at
    ~config
    ~rules
    ~examined
    ~notes:
      [ sprintf
          "%d in-sample events, %d embargoed; %d rules selected from %d cells examined"
          (Split.count split In_sample)
          split.embargoed
          (List.length rules)
          examined
      ]
;;

let pct x = sprintf "%+.1f%%" (100. *. x)

let describe_result (r : Report.Rule_result.t) (b : Bucket_result.t) =
  let ci =
    match b.return_ci with
    | Some ci -> sprintf " (95%% CI %s to %s)" (pct ci.lo) (pct ci.hi)
    | None -> ""
  in
  sprintf
    "%s %s %s after fees out-of-time%s, n = %d markets in %d events"
    (Rule.describe r.rule)
    (if Float.( >= ) b.mean_return 0. then "returned" else "lost")
    (if Float.( >= ) b.mean_return 0.
     then pct b.mean_return
     else sprintf "%.1f%%" (-100. *. b.mean_return))
    ci
    b.n
    b.n_events
;;

let verdict ~(frozen : Frozen.t) (results : Report.Rule_result.t list) : Verdict.t =
  let scored =
    List.filter_map results ~f:(fun r ->
      match r.out_of_time with
      | Some b when b.enough && Option.is_some b.return_ci -> Some (r, b)
      | _ -> None)
  in
  let ci_lo (_, (b : Bucket_result.t)) = (Option.value_exn b.return_ci).lo in
  let best xs = List.max_elt xs ~compare:(fun a b -> Float.compare (ci_lo a) (ci_lo b)) in
  let k = List.length frozen.rules in
  let context =
    sprintf
      "%d rule%s frozen in-sample from %d cells examined; cutoff %s."
      k
      (if k = 1 then " was" else "s were")
      frozen.examined
      frozen.config.cutoff
  in
  match List.filter scored ~f:(fun (_, b) -> Bucket_result.proven_positive b) with
  | _ :: _ as winners ->
    let r, b = Option.value_exn (best winners) in
    { tone = Green
    ; headline = describe_result r b ^ "."
    ; detail =
        sprintf
          "%d of %d scored rules cleared zero out-of-time; this is the one with the \
           highest lower bound. %s"
          (List.length winners)
          (List.length scored)
          context
    }
  | [] ->
    (match scored with
     | _ :: _ when List.for_all scored ~f:(fun (_, b) -> Bucket_result.proven_negative b)
       ->
       let r, b = Option.value_exn (best scored) in
       { tone = Red
       ; headline = "Every frozen rule lost money out-of-time."
       ; detail = sprintf "Best: %s. %s" (describe_result r b) context
       }
     | _ :: _ ->
       let r, b = Option.value_exn (best scored) in
       { tone = Neutral
       ; headline = "No proven edge"
       ; detail =
           sprintf
             "0 of %d scored rules has an out-of-time CI above zero. Best: %s. %s"
             (List.length scored)
             (describe_result r b)
             context
       }
     | [] when k = 0 ->
       { tone = Neutral
       ; headline = "No proven edge"
       ; detail =
           sprintf
             "No in-sample cell had a return CI above zero after fees (%d cells with n \
              >= %d examined), so no rule was frozen to test out-of-time."
             frozen.examined
             frozen.config.min_n
       }
     | [] ->
       { tone = Neutral
       ; headline = "No proven edge"
       ; detail =
           sprintf
             "%d rule%s frozen, but none has n >= %d out-of-time yet. %s"
             k
             (if k = 1 then " was" else "s were")
             frozen.config.min_n
             context
       })
;;

(* Keep report.json small: round to what the UI shows (and a little more). *)
let round_bucket (b : Bucket_result.t) : Bucket_result.t =
  let r d x = Float.round_decimal ~decimal_digits:d x in
  let ci d (c : Ci.t) : Ci.t = { lo = r d c.lo; hi = r d c.hi } in
  { b with
    contracts = r 2 b.contracts
  ; wins = r 2 b.wins
  ; win_rate = r 5 b.win_rate
  ; win_ci = ci 5 b.win_ci
  ; avg_price = r 3 b.avg_price
  ; avg_fee = r 4 b.avg_fee
  ; breakeven = r 5 b.breakeven
  ; mean_return = r 5 b.mean_return
  ; return_ci = Option.map b.return_ci ~f:(ci 5)
  }
;;

let segment_order (s : Segment.t) =
  match s with
  | All -> 0, ""
  | Category c -> 1, c
  | Volume_tercile k -> 2, Int.to_string k
  | Event_kind k -> 3, Event_kind.to_string k
  | Close_kind k -> 4, Close_kind.to_string k
;;

let build ?sampling ~generated_at ~synthetic ~(frozen : Frozen.t) (d : Dataset.t)
  : Report.t
  =
  (* [sampling]: (population, quote sample, trade sample). Synthetic data is not sampled. *)
  let population, quote_sample, trade_sample =
    match sampling with
    | Some s -> s
    | None ->
      let n = List.length d.markets in
      ( n
      , List.map d.snapshots ~f:(fun s -> s.ticker)
        |> List.dedup_and_sort ~compare:String.compare
        |> List.length
      , List.map d.trades ~f:(fun t -> t.ticker)
        |> List.dedup_and_sort ~compare:String.compare
        |> List.length )
  in
  let config = frozen.config in
  let split = Split.create ~config d.markets in
  let drops = Observation.Drops.create () in
  let observations = observations ~config d ~drops in
  let tables =
    Tables.build ~config ~split ~observations ()
    |> List.map ~f:(fun (t : Table.t) ->
      { t with buckets = List.map t.buckets ~f:round_bucket })
  in
  let rule_results =
    List.filter_map frozen.rules ~f:(fun rule ->
      let key sample : Table.Key.t =
        { sample
        ; view = Quotes
        ; side = rule.side
        ; horizon = rule.horizon
        ; segment = rule.segment
        }
      in
      Option.map
        (Tables.find_bucket tables (key In_sample) ~lo:rule.lo)
        ~f:(fun in_sample ->
          { Report.Rule_result.rule
          ; in_sample
          ; out_of_time = Tables.find_bucket tables (key Out_of_time) ~lo:rule.lo
          }))
  in
  let settles = List.map d.markets ~f:(fun m -> m.settle_time) in
  let date f =
    Option.value_map
      (f settles ~compare:Int.compare)
      ~default:""
      ~f:Unix_time.to_date_string
  in
  let segments =
    List.map tables ~f:(fun t -> t.key.segment)
    |> List.dedup_and_sort ~compare:Segment.compare
    |> List.sort ~compare:(fun a b ->
      [%compare: int * string] (segment_order a) (segment_order b))
  in
  { generated_at
  ; synthetic
  ; window =
      { first_settle = date List.min_elt
      ; last_settle = date List.max_elt
      ; population
      ; quote_sample
      ; trade_sample
      ; n_markets = List.length d.markets
      ; n_events =
          List.map d.markets ~f:(fun m -> m.event_ticker)
          |> List.dedup_and_sort ~compare:String.compare
          |> List.length
      ; n_trades = List.count d.trades ~f:(fun t -> not t.is_block)
      ; n_in_sample_events = Split.count split In_sample
      ; n_out_of_time_events = Split.count split Out_of_time
      ; n_embargoed_events = split.embargoed
      ; dropped = Observation.Drops.to_list drops
      }
  ; config
  ; config_hash = frozen.hash
  ; examined_in_sample = frozen.examined
  ; rules = rule_results
  ; verdict = verdict ~frozen rule_results
  ; segments
  ; tables
  }
;;
