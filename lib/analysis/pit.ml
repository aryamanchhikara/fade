open! Core
open Fade_types

(* Point-in-time data for one market, and the features computed from it.

   The feature code only ever sees a [t]: fields fixed before the market opened (the
   [View]) plus time-stamped data (candles, trades, when each market of the event was
   created, and when the market stopped trading). The result, the settlement time, the
   actual close of a market still trading and the lifetime volume are simply not in
   the type, so no feature can use them. [truncate ~at] deletes everything after a
   moment; the leakage test checks that features at t are unchanged by it. *)

module View = struct
  type t =
    { ticker : string
    ; event_ticker : string
    ; category : string
    ; open_time : Unix_time.t
    ; scheduled_close : Unix_time.t
      (** Kalshi's expected expiration, set in advance (see [Anchor.Expected]) *)
    ; mutually_exclusive : bool
    }
  [@@deriving sexp, compare, equal]
end

type t =
  { view : View.t
  ; closed_at : Unix_time.t option
    (** when trading stopped, if it already has; [truncate] hides a later close *)
  ; candles : Candle.t array (** ascending end_ts *)
  ; trades : Trade.t array (** ascending ts *)
  ; event_created : Unix_time.t array (** ascending creation times, whole event *)
  }

let truncate t ~at =
  { t with
    closed_at = Option.filter t.closed_at ~f:(fun c -> c <= at)
  ; candles = Array.filter t.candles ~f:(fun c -> c.end_ts <= at)
  ; trades = Array.filter t.trades ~f:(fun tr -> tr.ts <= at)
  ; event_created = Array.filter t.event_created ~f:(fun c -> c <= at)
  }
;;

module Features = struct
  type t =
    { horizon : Horizon.t
    ; decision : Unix_time.t (** scheduled close minus the horizon *)
    ; asof : Unix_time.t
      (** when the quote was observed (the candle's end, up to an hour before the
          decision); every other feature is computed as of this moment too *)
    ; no_ask : float (** cents *)
    ; spread : float option (** YES ask minus YES bid, cents; [None]: one-sided *)
    ; volume_to_date : float (** contracts traded up to [asof] *)
    ; trades_to_date : int
    ; move_1h : float option (** change in the last NO trade price over the hour *)
    ; move_6h : float option
    ; move_24h : float option
    ; hours_to_close : float (** to the scheduled close *)
    ; category : string
    ; mutually_exclusive : bool
    ; event_markets : int (** markets the event had at [asof] *)
    }
  [@@deriving sexp, compare, equal]

  let one_sided t = Option.is_none t.spread
end

(* The universe: rows only where buying NO was possible and the market had traded at
   least [min_volume] contracts by then (volume to date, not lifetime volume). *)
let min_volume = 500.

(* Number of elements of a sorted array with key <= [at] (binary search). *)
let count_upto (a : 'a array) ~key ~at =
  let lo = ref 0
  and hi = ref (Array.length a) in
  while !lo < !hi do
    let mid = (!lo + !hi) / 2 in
    if key a.(mid) <= at then lo := mid + 1 else hi := mid
  done;
  !lo
;;

let features (p : t) ~(horizon : Horizon.t) : Features.t option =
  let decision = p.view.scheduled_close - Horizon.seconds horizon in
  let still_open =
    decision > p.view.open_time
    &&
    match p.closed_at with
    | Some c -> c >= decision
    | None -> true
  in
  (* The quote: the close of the last hourly candle ending in the hour up to the
     decision (at most an hour stale). *)
  let quote =
    let k = count_upto p.candles ~key:(fun (c : Candle.t) -> c.end_ts) ~at:decision in
    if k = 0
    then None
    else (
      let c = p.candles.(k - 1) in
      Option.some_if (c.end_ts > decision - 3600 && c.end_ts > p.view.open_time) c)
  in
  match still_open, quote with
  | false, _ | _, None -> None
  | true, Some q ->
    (match q.yes_bid, q.yes_ask with
     | None, _ -> None (* no YES bid: nobody to sell NO to us *)
     | Some bid, Some ask when Float.( < ) ask bid -> None (* crossed book *)
     | Some bid, ask ->
       (* Everything is measured at the moment the quote was observed, not at the
          decision. The quote can be up to an hour old; trades after it would describe
          a price that the stale ask does not reflect, and a backtest buying at that
          ask would be trading on news it could not have traded at. *)
       let asof = q.end_ts in
       let n_trades =
         count_upto p.trades ~key:(fun (tr : Trade.t) -> tr.ts) ~at:asof
       in
       let volume = ref 0. in
       for i = 0 to n_trades - 1 do
         volume := !volume +. p.trades.(i).count
       done;
       let no_ask = 100. -. bid in
       if
         Float.( < ) !volume min_volume
         || Float.( <= ) no_ask 0.
         || Float.( >= ) no_ask 100.
       then None
       else (
         let last_no at =
           let k = count_upto p.trades ~key:(fun (tr : Trade.t) -> tr.ts) ~at in
           if k = 0 then None else Some (100. -. p.trades.(k - 1).yes_price)
         in
         let move seconds =
           match last_no asof, last_no (asof - seconds) with
           | Some now, Some before -> Some (now -. before)
           | _ -> None
         in
         Some
           { horizon
           ; decision
           ; asof
           ; no_ask
           ; spread = Option.map ask ~f:(fun a -> a -. bid)
           ; volume_to_date = !volume
           ; trades_to_date = n_trades
           ; move_1h = move 3600
           ; move_6h = move (6 * 3600)
           ; move_24h = move (24 * 3600)
           ; hours_to_close = Float.of_int (p.view.scheduled_close - asof) /. 3600.
           ; category = p.view.category
           ; mutually_exclusive = p.view.mutually_exclusive
           ; event_markets = count_upto p.event_created ~key:Fn.id ~at:asof
           }))
;;

(* One model row: the features at a decision, and next to them (never inside them) the
   label and the settlement time, which only the walk-forward splits use. *)
module Row = struct
  type t =
    { ticker : string
    ; event_ticker : string
    ; settle_time : Unix_time.t
    ; no_won : bool
    ; features : Features.t
    }
end

(* The leakage check: features at the decision must not change when everything after
   it is deleted. Returns the horizons where they do. *)
let leaks (p : t) =
  List.filter_map Horizon.all ~f:(fun horizon ->
    let full = features p ~horizon in
    let cut =
      Option.bind full ~f:(fun (f : Features.t) ->
        features (truncate p ~at:f.decision) ~horizon)
    in
    if Option.equal Features.equal full cut then None else Some horizon)
;;

let%expect_test "features use nothing after the decision" =
  let hour = 3600 in
  let close = 100 * hour in
  let candle h ~bid ~ask : Candle.t =
    { end_ts = close - (h * hour); yes_bid = Some bid; yes_ask = Some ask }
  in
  let trade i ~h ~yes_price ~count : Trade.t =
    { trade_id = Int.to_string i
    ; ticker = "M"
    ; ts = close - (h * hour) - 60
    ; yes_price
    ; count
    ; taker_side = Yes
    ; is_block = false
    }
  in
  let p =
    { view =
        { ticker = "M"
        ; event_ticker = "E"
        ; category = "Test"
        ; open_time = 0
        ; scheduled_close = close
        ; mutually_exclusive = true
        }
    ; (* It actually closed 30 minutes early: the event happened. *)
      closed_at = Some (close - 1800)
    ; candles =
        Array.of_list
          (List.map
             (List.range 0 30 |> List.rev)
             ~f:(fun h ->
               candle h ~bid:(Float.of_int (10 + h)) ~ask:(Float.of_int (12 + h))))
    ; trades =
        Array.of_list
          (List.mapi
             (List.range 0 60 |> List.rev)
             ~f:(fun i h ->
               trade i ~h ~yes_price:(Float.of_int (20 + (h mod 7))) ~count:40.))
    ; event_created = [| 0; 50 * hour; 99 * hour |]
    }
  in
  List.iter Horizon.all ~f:(fun horizon ->
    match features p ~horizon with
    | None -> printf "%s: no row\n" (Horizon.to_string horizon)
    | Some f ->
      printf
        "%s: ask %.0f spread %s vol %.0f trades %d move1h %s event markets %d\n"
        (Horizon.to_string horizon)
        f.no_ask
        (Option.value_map f.spread ~default:"-" ~f:(sprintf "%.0f"))
        f.volume_to_date
        f.trades_to_date
        (Option.value_map f.move_1h ~default:"-" ~f:(sprintf "%+.0f"))
        f.event_markets);
  printf "leaking horizons: %d\n" (List.length (leaks p));
  (* A deliberately leaky feature would be caught: lifetime trade count. *)
  let leaky (p : t) ~horizon =
    Option.map (features p ~horizon) ~f:(fun f ->
      { f with trades_to_date = Array.length p.trades })
  in
  let caught =
    List.count Horizon.all ~f:(fun horizon ->
      match leaky p ~horizon with
      | None -> false
      | Some f ->
        not
          (Option.equal
             Features.equal
             (Some f)
             (leaky (truncate p ~at:f.decision) ~horizon)))
  in
  printf "a leaky feature is caught at %d horizons\n" caught;
  [%expect
    {|
    24h: ask 66 spread 2 vol 1440 trades 36 move1h +1 event markets 2
    6h: ask 84 spread 2 vol 2160 trades 54 move1h -6 event markets 2
    1h: ask 89 spread 2 vol 2360 trades 59 move1h +1 event markets 3
    leaking horizons: 0
    a leaky feature is caught at 3 horizons
    |}]
;;

let%expect_test "with a stale quote, features stop at the quote, not at the decision" =
  let hour = 3600 in
  let close = 100 * hour in
  let decision = close - hour in
  let quote_end = decision - (40 * 60) in
  let trade i ~ts ~yes_price : Trade.t =
    { trade_id = Int.to_string i; ticker = "M"; ts; yes_price; count = 300.; taker_side = No; is_block = false }
  in
  let p =
    { view =
        { ticker = "M"
        ; event_ticker = "E"
        ; category = "Test"
        ; open_time = 0
        ; scheduled_close = close
        ; mutually_exclusive = false
        }
    ; closed_at = Some close
    ; candles = [| { Candle.end_ts = quote_end; yes_bid = Some 40.; yes_ask = Some 42. } |]
    ; (* Two trades before the quote, then NO jumps 15c after it. *)
      trades =
        [| trade 0 ~ts:(quote_end - (2 * hour)) ~yes_price:58.
         ; trade 1 ~ts:(quote_end - 600) ~yes_price:58.
         ; trade 2 ~ts:(quote_end + 600) ~yes_price:43.
        |]
    ; event_created = [| 0 |]
    }
  in
  (match features p ~horizon:H1 with
   | None -> print_endline "no row"
   | Some f ->
     printf
       "quote %d min before the decision; volume %.0f, trades %d, 1h move %s\n"
       ((f.decision - f.asof) / 60)
       f.volume_to_date
       f.trades_to_date
       (Option.value_map f.move_1h ~default:"-" ~f:(sprintf "%+.0f")));
  [%expect {| quote 40 min before the decision; volume 600, trades 2, 1h move +0 |}]
;;
