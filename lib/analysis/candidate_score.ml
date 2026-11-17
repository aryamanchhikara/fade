open! Core
open Fade_types

(* Scoring the pre-registered candidate rules (see [Candidate]). Pure: the caller says
   which markets count ([include_market]: in-sample, or the forward window) and which
   have complete trade data ([traded]: a maker fill can only be judged there). *)

module Rule = Candidate.Rule

let decision_time ~anchor (m : Market.t) (horizon : Horizon.t) =
  Anchor.time
    anchor
    ~close_kind:m.close_kind
    ~close_time:m.close_time
    ~expected_expiration:m.expected_expiration
  - Horizon.seconds horizon
;;

let in_range (r : Rule.t) p = Float.( >= ) p r.lo && Float.( < ) p r.hi

let spread (s : Snapshot.t) =
  match s.yes_bid, s.yes_ask with
  | Some b, Some a -> Some (a -. b)
  | _ -> None
;;

(* An order the rule would have placed: [price] cents for [rule.side], at [decision],
   paying [fee] cents per contract if it fills. *)
type order =
  { market : Market.t
  ; decision : Unix_time.t
  ; price : float
  ; fee : float
  }

let orders (rule : Rule.t) ~anchor ~fees ~(d : Dataset.t) ~include_market ~traded =
  let markets = Observation.market_index d in
  let role : Role.t =
    match rule.execution with
    | Taker_at_ask -> Taker
    | Maker_bid _ -> Maker
  in
  List.filter_map d.snapshots ~f:(fun (s : Snapshot.t) ->
    match Hashtbl.find markets s.ticker with
    | Some m when Horizon.equal s.horizon rule.horizon && include_market m ->
      (* A taker pays the ask; a maker joins the best bid, and can only be judged
         where every trade of the market is known. *)
      let price =
        match rule.execution with
        | Taker_at_ask -> Snapshot.ask s rule.side
        | Maker_bid _ -> if traded m.ticker then Snapshot.bid s rule.side else None
      in
      let spread_ok =
        match rule.max_spread, spread s with
        | None, _ -> true
        | Some max, Some sp -> Float.( <= ) sp max
        | Some _, None -> false
      in
      let decision = decision_time ~anchor m rule.horizon in
      (match price with
       | Some price when spread_ok && in_range rule price ->
         (* Markets whose fee is not modelled (flat-fee series) place no order. *)
         Fees.Index.lookup
           fees
           ~series_ticker:m.series_ticker
           ~event_ticker:m.event_ticker
           ~ts:decision
         |> Option.bind ~f:(fun spec ->
           Fees.fee_per_contract_cents
             spec
             ~role
             ~contracts:rule.contracts
             ~price_cents:price)
         |> Option.map ~f:(fun fee -> { market = m; decision; price; fee })
       | _ -> None)
    | _ -> None)
;;

let observation (rule : Rule.t) (o : order) ~ts : Observation.t =
  { market = o.market
  ; side = rule.side
  ; horizon = rule.horizon
  ; ts
  ; price = o.price
  ; fee = o.fee
  ; weight = 1.
  ; won = Side.equal o.market.result rule.side
  }
;;

(* A taker order fills at once. A resting bid fills at the first later trade, before
   close, that prints through it (strict) or at or through it (loose). Block trades are
   negotiated off the book and never touch a resting order. *)
let fills (rule : Rule.t) ~(d : Dataset.t) (orders : order list) =
  match rule.execution with
  | Taker_at_ask -> List.map orders ~f:(fun o -> observation rule o ~ts:o.decision)
  | Maker_bid fill ->
    let wanted =
      String.Hash_set.of_list (List.map orders ~f:(fun o -> o.market.ticker))
    in
    let trades = String.Table.create () in
    List.iter d.trades ~f:(fun (t : Trade.t) ->
      if Hash_set.mem wanted t.ticker && not t.is_block
      then Hashtbl.add_multi trades ~key:t.ticker ~data:t);
    List.filter_map orders ~f:(fun o ->
      let fills_us (t : Trade.t) =
        let p = Trade.price t rule.side in
        t.ts > o.decision
        && t.ts <= o.market.close_time
        &&
        match fill with
        | Through -> Float.( < ) p o.price
        | At_or_through -> Float.( <= ) p o.price
      in
      Hashtbl.find_multi trades o.market.ticker
      |> List.filter ~f:fills_us
      |> List.min_elt ~compare:(fun (a : Trade.t) b -> Int.compare a.ts b.ts)
      |> Option.map ~f:(fun (t : Trade.t) -> observation rule o ~ts:t.ts))
;;

let rng ~(config : Candidate.Config.t) ~rule_index ~stream =
  Random.State.make [| config.seed; rule_index; stream |]
;;

let score
      ?rng
      ~(config : Candidate.Config.t)
      (rule : Rule.t)
      ~fees
      ~d
      ~include_market
      ~traded
  : Candidate.Score.t
  =
  let os = orders rule ~anchor:config.anchor ~fees ~d ~include_market ~traded in
  { rule_id = rule.id
  ; orders = List.length os
  ; result =
      Bucket.compute
        ?rng
        ~min_n:config.min_n
        ~resamples:config.bootstrap_resamples
        ~lo:rule.lo
        ~hi:rule.hi
        (fills rule ~d os)
  }
;;

(* Every rule, each with its own seeded RNG. [stream] separates samples (0 in-sample,
   1 forward) so their bootstraps never share random numbers. *)
let score_all ~(config : Candidate.Config.t) ~rules ~d ~include_market ~traded ~stream =
  let fees = Fees.Index.create (d : Dataset.t).fees in
  List.mapi rules ~f:(fun rule_index rule ->
    score
      ~rng:(rng ~config ~rule_index ~stream)
      ~config
      rule
      ~fees
      ~d
      ~include_market
      ~traded)
;;

(* ---- tests ---------------------------------------------------------------------- *)

let test_rules : Rule.t list =
  let base : Rule.t =
    { id = "A"
    ; execution = Taker_at_ask
    ; side = No
    ; horizon = H1
    ; lo = 90.
    ; hi = 95.
    ; max_spread = Some 5.
    ; contracts = 100.
    }
  in
  [ base
  ; { base with
      id = "B"
    ; execution = Maker_bid Through
    ; max_spread = None
    ; contracts = 1.
    }
  ; { base with
      id = "B-loose"
    ; execution = Maker_bid At_or_through
    ; max_spread = None
    ; contracts = 1.
    }
  ]
;;

let%expect_test "taker spread filter; maker strict vs loose fills" =
  let hour = 3600 in
  let close = 100 * hour in
  let market ticker (result : Side.t) : Market.t =
    { ticker
    ; event_ticker = "EV-" ^ ticker
    ; series_ticker = "S"
    ; category = "Test"
    ; open_time = 0
    ; close_time = close
    ; settle_time = close
    ; result
    ; volume = 1000.
    ; can_close_early = false
    ; expected_expiration = None
    ; close_kind = Scheduled
    ; event_kind = Single
    }
  in
  let snap ticker ~bid ~ask : Snapshot.t =
    { ticker; horizon = H1; ts = close - hour; yes_bid = Some bid; yes_ask = Some ask }
  in
  let trade ticker i ~mins_before_close ~yes_price : Trade.t =
    { trade_id = sprintf "%s-%d" ticker i
    ; ticker
    ; ts = close - (mins_before_close * 60)
    ; yes_price
    ; count = 5.
    ; taker_side = Yes
    ; is_block = false
    }
  in
  let d : Dataset.t =
    { markets = [ market "M1" No; market "M2" No; market "M3" Yes ]
    ; snapshots =
        [ (* NO ask 92, NO bid 90, spread 2: A buys at 92, B bids 90. *)
          snap "M1" ~bid:8. ~ask:10.
        ; (* NO ask 92 but spread 12: A skips; NO bid 80 is out of range for B. *)
          snap "M2" ~bid:8. ~ask:20.
        ; (* NO ask 97 is out of range for A; NO bid 94: B bids 94. *)
          snap "M3" ~bid:3. ~ask:6.
        ]
    ; trades =
        [ (* Before the decision: never fills. *)
          trade "M1" 0 ~mins_before_close:90 ~yes_price:15.
        ; (* At our NO bid of 90 (YES 10): loose only. *)
          trade "M1" 1 ~mins_before_close:40 ~yes_price:10.
        ; (* Through it (NO 88): strict fills here. *)
          trade "M1" 2 ~mins_before_close:20 ~yes_price:12.
        ; (* At our NO bid of 94: loose only. *)
          trade "M3" 0 ~mins_before_close:30 ~yes_price:6.
        ]
    ; fees =
        { series =
            [ { series_ticker = "S"
              ; current_type = Quadratic
              ; current_multiplier = 1.
              ; changes = []
              }
            ]
        ; event_overrides = []
        }
    }
  in
  let fees = Fees.Index.create d.fees in
  List.iter test_rules ~f:(fun rule ->
    let os =
      orders
        rule
        ~anchor:Schedule
        ~fees
        ~d
        ~include_market:(fun _ -> true)
        ~traded:(fun _ -> true)
    in
    let fs = fills rule ~d os in
    printf "%-8s orders:" rule.id;
    List.iter os ~f:(fun o -> printf " %s@%.0f" o.market.ticker o.price);
    printf "  fills:";
    List.iter fs ~f:(fun f ->
      printf
        " %s %dmin-before-close fee %.2f %s"
        f.market.ticker
        ((close - f.ts) / 60)
        f.fee
        (if f.won then "won" else "lost"));
    print_endline "");
  [%expect
    {|
    A        orders: M1@92  fills: M1 60min-before-close fee 0.52 won
    B        orders: M1@90 M3@94  fills: M1 20min-before-close fee 0.00 won
    B-loose  orders: M1@90 M3@94  fills: M1 40min-before-close fee 0.00 won M3 30min-before-close fee 0.00 lost
    |}]
;;
