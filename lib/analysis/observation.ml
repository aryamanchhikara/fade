open! Core
open Fade_types

(* One hypothetical or actual purchase: [weight] contracts of [side] bought at [price]
   cents, paying [fee] cents per contract, at [ts]. Quote-view observations are one
   contract per market and horizon; trade-view observations are real fills. *)
type t =
  { market : Market.t
  ; side : Side.t
  ; horizon : Horizon.t
  ; ts : Unix_time.t
  ; price : float
  ; fee : float
  ; weight : float
  ; won : bool
  }

let cost t = t.price +. t.fee
let payout t = if t.won then 100. else 0.
let profit t = payout t -. cost t

(* Why observations were dropped, for the report's data-quality line. *)
module Drops = struct
  type t = int String.Table.t

  let create () : t = String.Table.create ()
  let add (t : t) reason = Hashtbl.incr t reason
  let to_list (t : t) = Hashtbl.to_alist t |> List.sort ~compare:[%compare: string * int]
end

let market_index (d : Dataset.t) =
  let t = String.Table.create () in
  List.iter d.markets ~f:(fun m ->
    ignore (Hashtbl.add t ~key:m.ticker ~data:m : [ `Ok | `Duplicate ]));
  t
;;

let valid_price p = Float.( > ) p 0. && Float.( < ) p 100.

(* "Buy [side] as a taker at the quoted ask, [horizon] before close": one contract per
   market per horizon per side, sized as part of a [contracts_per_order] order for fee
   rounding. *)
let of_quotes (d : Dataset.t) ~(config : Config.t) ~fees ~drops =
  let markets = market_index d in
  List.concat_map d.snapshots ~f:(fun (s : Snapshot.t) ->
    match Hashtbl.find markets s.ticker with
    | None -> []
    | Some m ->
      List.filter_map Side.all ~f:(fun side ->
        match Snapshot.ask s side with
        | None ->
          Drops.add drops "quote: no ask on that side";
          None
        | Some price when not (valid_price price) -> None
        | Some price ->
          (match
             Fees.Index.lookup
               fees
               ~series_ticker:m.series_ticker
               ~event_ticker:m.event_ticker
               ~ts:s.ts
           with
           | None ->
             Drops.add drops "quote: series fee structure unknown";
             None
           | Some spec ->
             (match
                Fees.fee_per_contract_cents
                  spec
                  ~role:Taker
                  ~contracts:config.contracts_per_order
                  ~price_cents:price
              with
              | None ->
                Drops.add drops "quote: flat fee schedule (not modelled)";
                None
              | Some fee ->
                Some
                  { market = m
                  ; side
                  ; horizon = s.horizon
                  ; ts = s.ts
                  ; price
                  ; fee
                  ; weight = 1.
                  ; won = Side.equal m.result side
                  }))))
;;

(* Realized returns of real fills for one [role]. Each trade lands in the horizon band of
   its time before the (anchored) close; trades more than 24h out are not used. Block
   trades are negotiated off-book and excluded. *)
let of_trades (d : Dataset.t) ~(config : Config.t) ~fees ~drops ~(role : Role.t) =
  let markets = market_index d in
  List.filter_map d.trades ~f:(fun (t : Trade.t) ->
    match Hashtbl.find markets t.ticker with
    | None -> None
    | Some _ when t.is_block -> None
    | Some m ->
      let anchor =
        Anchor.time
          config.anchor
          ~close_kind:m.close_kind
          ~close_time:m.close_time
          ~expected_expiration:m.expected_expiration
      in
      (match Horizon.of_seconds_before_close (anchor - t.ts) with
       | None -> None
       | Some horizon ->
         let side, price = Trade.position t role in
         if not (valid_price price)
         then None
         else (
           match
             Fees.Index.lookup
               fees
               ~series_ticker:m.series_ticker
               ~event_ticker:m.event_ticker
               ~ts:t.ts
           with
           | None ->
             Drops.add drops "trade: series fee structure unknown";
             None
           | Some spec ->
             Fees.fee_per_contract_cents spec ~role ~contracts:t.count ~price_cents:price
             |> Option.map ~f:(fun fee ->
               { market = m
               ; side
               ; horizon
               ; ts = t.ts
               ; price
               ; fee
               ; weight = t.count
               ; won = Side.equal m.result side
               }))))
;;
