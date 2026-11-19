open! Core
open Fade_types
module Pit = Fade_analysis.Pit

(* Synthetic point-in-time markets for tests and `fade explain --demo`: raw candles,
   trades and event structure, not ready-made features, so the demo goes through the
   same feature and leakage code as real data. Every ticker starts with SYN.

   One effect is planted: when the book is one-sided at the decision, the NO ask is
   inflated by [one_sided_premium] cents relative to the truth, so NO wins less often
   than even the recalibrated price says. Nothing else predicts the outcome beyond the
   price. *)

let one_sided_premium = 6.

type market =
  { pit : Pit.t
  ; no_won : bool
  ; settle_time : Unix_time.t
  }

let expit x = 1. /. (1. +. Float.exp (-.x))
let logit p = Float.log (p /. (1. -. p))

let gaussian st =
  let u1 = Float.max 1e-12 (Random.State.float st 1.) in
  let u2 = Random.State.float st 1. in
  Float.sqrt (-2. *. Float.log u1) *. Float.cos (2. *. Float.pi *. u2)
;;

let categories = [| "Sports"; "Crypto"; "Economics"; "Climate and Weather" |]

let generate ?(seed = 20260516) ?(events_per_month = 220) () : market list =
  let st = Random.State.make [| seed; 7 |] in
  let hour = 3600 in
  let start = Unix_time.of_date_string_exn "2025-06-01" in
  let months = 18 in
  List.concat_map
    (List.range 0 (months * events_per_month))
    ~f:(fun e ->
      let month = e / events_per_month in
      let settle_base =
        start + (month * 30 * 86_400) + Random.State.int st (29 * 86_400)
      in
      let n_markets = 1 + Random.State.int st 3 in
      let category = categories.(Random.State.int st (Array.length categories)) in
      let event_ticker = sprintf "SYNE-%05d" e in
      let created = Array.init n_markets ~f:(fun _ -> settle_base - (72 * hour)) in
      List.init n_markets ~f:(fun k ->
        let ticker = sprintf "%s-M%d" event_ticker k in
        let scheduled_close = settle_base in
        let open_time = scheduled_close - (60 * hour) in
        (* Latent log-odds that NO wins, a random walk ending at the outcome draw. *)
        let x0 = gaussian st *. 1.6 in
        let path = Array.create ~len:61 x0 in
        for h = 1 to 60 do
          path.(h) <- path.(h - 1) +. (gaussian st *. 0.12)
        done;
        let truth_at_close = expit path.(60) in
        let no_won = Float.( < ) (Random.State.float st 1.) truth_at_close in
        (* Hourly candles for the last 30 hours; about a third one-sided. *)
        let candles =
          Array.init 30 ~f:(fun i ->
            let h = 30 + i in
            let p = 100. *. expit path.(h) in
            let one_sided = Float.( < ) (Random.State.float st 1.) 0.33 in
            let half = 1. +. Float.of_int (Random.State.int st 3) in
            let bid_no = Float.max 1. (Float.round (p -. half)) in
            let ask_no =
              Float.min
                99.
                (Float.round (p +. half +. if one_sided then one_sided_premium else 0.))
            in
            { Candle.end_ts = open_time + ((h + 1) * hour)
            ; yes_bid = Some (100. -. ask_no)
            ; yes_ask = (if one_sided then None else Some (100. -. bid_no))
            })
        in
        let trades =
          Array.init 60 ~f:(fun h ->
            { Trade.trade_id = sprintf "%s-T%d" ticker h
            ; ticker
            ; ts = open_time + (h * hour) + 600
            ; yes_price = Float.round (100. -. (100. *. expit path.(h)))
            ; count = Float.of_int (10 + Random.State.int st 40)
            ; taker_side = (if Random.State.bool st then Yes else No)
            ; is_block = false
            })
        in
        { pit =
            { view =
                { ticker
                ; event_ticker
                ; category
                ; open_time
                ; scheduled_close
                ; mutually_exclusive = n_markets > 1
                }
            ; closed_at = Some scheduled_close
            ; candles
            ; trades
            ; event_created = created
            }
        ; no_won
        ; settle_time = scheduled_close + 1800
        }))
;;

let rows markets : Pit.Row.t list =
  List.concat_map markets ~f:(fun m ->
    List.filter_map Horizon.all ~f:(fun horizon ->
      Pit.features m.pit ~horizon
      |> Option.map ~f:(fun features ->
        { Pit.Row.ticker = m.pit.view.ticker
        ; event_ticker = m.pit.view.event_ticker
        ; settle_time = m.settle_time
        ; no_won = m.no_won
        ; features
        })))
;;
