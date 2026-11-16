open! Core
open Endpoints

(* Hand-written payloads in the documented shapes (docs.kalshi.com OpenAPI), not real
   market data. Tests never touch the network. *)

let%expect_test "historical market page" =
  let json =
    {|{"cursor":"abc","markets":[{"ticker":"KXT-1-A","event_ticker":"KXT-1",
      "market_type":"binary","created_time":"2026-09-14T14:00:00Z",
      "open_time":"2026-09-14T14:00:00Z","close_time":"2026-09-16T05:00:00Z",
      "settlement_ts":"2026-09-16T05:41:28.444339Z","result":"no","volume_fp":"2774.83",
      "can_close_early":true,"occurrence_datetime":null,"rules_primary":"ignored",
      "yes_bid_dollars":"0.0000"}]}|}
  in
  let page = Markets_page.t_of_yojson (Yojson.Safe.from_string json) in
  print_s [%sexp (page.markets : Market_raw.t list)];
  [%expect
    {|
    (((ticker KXT-1-A) (event_ticker KXT-1) (market_type binary)
      (created_time 2026-09-14T14:00:00Z) (open_time 2026-09-14T14:00:00Z)
      (close_time 2026-09-16T05:00:00Z)
      (settlement_ts (2026-09-16T05:41:28.444339Z)) (result no)
      (volume_fp 2774.83) (can_close_early true) (expected_expiration_time ())
      (latest_expiration_time ()) (occurrence_datetime ())))
    |}]
;;

let%expect_test "historical vs live candle field names" =
  let hist =
    {|{"ticker":"T","candlesticks":[{"end_period_ts":1789444800,"volume":"1.00",
      "open_interest":"2.00","price":{"close":null},"yes_bid":{"open":"0.5","high":"0.5",
      "low":"0.5","close":"0.0000"},"yes_ask":{"open":"0.5","high":"0.5","low":"0.5",
      "close":"0.0200"}}]}|}
  in
  let live =
    {|{"markets":[{"market_ticker":"T","candlesticks":[{"end_period_ts":1789444800,
      "volume_fp":"1.00","open_interest_fp":"2.00","price":{},
      "yes_bid":{"close_dollars":"0.5500"},"yes_ask":{"close_dollars":"1.0000"}}]}]}|}
  in
  let h = Candles_hist.t_of_yojson (Yojson.Safe.from_string hist) in
  let l = Batch_candles.t_of_yojson (Yojson.Safe.from_string live) in
  print_s [%sexp (List.map h.candlesticks ~f:candle_of_hist : Fade_types.Candle.t list)];
  print_s
    [%sexp
      (List.concat_map l.markets ~f:(fun m -> List.map m.candlesticks ~f:candle_of_live)
       : Fade_types.Candle.t list)];
  [%expect
    {|
    (((end_ts 1789444800) (yes_bid ()) (yes_ask (2))))
    (((end_ts 1789444800) (yes_bid (55)) (yes_ask ())))
    |}]
;;

let%expect_test "trade" =
  let json =
    {|{"trade_id":"x","ticker":"T","count_fp":"2.50","yes_price_dollars":"0.0100",
      "no_price_dollars":"0.9900","taker_outcome_side":"no","taker_book_side":"ask",
      "taker_side":"no","created_time":"2025-06-15T17:16:00.075948Z","is_block_trade":false}|}
  in
  let t = trade_of_raw (Trade_raw.t_of_yojson (Yojson.Safe.from_string json)) in
  print_s [%sexp (t : Fade_types.Trade.t option)];
  [%expect
    {|
    (((trade_id x) (ticker T) (ts 1750007760) (yes_price 1) (count 2.5)
      (taker_side No) (is_block false)))
    |}]
;;

let%expect_test "series and fee changes, integer multipliers" =
  let s =
    Series_list.t_of_yojson
      (Yojson.Safe.from_string
         {|{"series":[{"ticker":"KXA","category":"Sports","fee_type":"quadratic",
           "fee_multiplier":1,"title":"x"}]}|})
  in
  print_s [%sexp (s.series : Series_raw.t list)];
  [%expect
    {| (((ticker KXA) (category Sports) (fee_type quadratic) (fee_multiplier 1))) |}]
;;
