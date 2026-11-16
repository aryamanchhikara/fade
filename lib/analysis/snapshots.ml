open! Core
open Fade_types

(* Turning hourly candles into the quotes the analysis uses. Pure functions. *)

type candle = Candle.t =
  { end_ts : Unix_time.t
  ; yes_bid : float option
  ; yes_ask : float option
  }

(* The request window for one market: from 25 hours before close (so the candle
   covering "24h before close" is included) or the market's open, whichever is later.
   The final [()]: OCaml needs a positional argument after optional ones, so it can
   tell when a call is complete. *)
let window ?expected_expiration ~open_time ~close_time () =
  (* If the market was expected to expire before it closed, a horizon anchored on that
     expectation needs earlier candles too. *)
  let earliest_anchor =
    Option.value_map expected_expiration ~default:close_time ~f:(Int.min close_time)
  in
  let start = Int.max open_time (earliest_anchor - (25 * 3600)) in
  start, close_time
;;

(* The quote for horizon [h] is the close of the latest candle that ends at or before
   the decision time [anchor - h], provided that candle ends within the hour before it
   (so the quote is at most an hour stale) and after the market opened. The market must
   still be open at the decision time. A horizon with no quote on either side, or a
   crossed book, is skipped. [anchor] defaults to [close_time]. *)
let of_candles ?anchor ~ticker ~open_time ~close_time (candles : candle list) =
  let anchor = Option.value anchor ~default:close_time in
  List.filter_map Horizon.all ~f:(fun horizon ->
    let target = anchor - Horizon.seconds horizon in
    let eligible =
      if target > close_time
      then []
      else
        List.filter candles ~f:(fun c ->
          c.end_ts <= target && c.end_ts > target - 3600 && c.end_ts > open_time)
    in
    match List.max_elt eligible ~compare:(fun a b -> Int.compare a.end_ts b.end_ts) with
    | None -> None
    | Some c ->
      let crossed =
        match c.yes_bid, c.yes_ask with
        | Some b, Some a -> Float.( >= ) b a
        | _ -> false
      in
      if crossed || (Option.is_none c.yes_bid && Option.is_none c.yes_ask)
      then None
      else
        Some
          { Snapshot.ticker
          ; horizon
          ; ts = c.end_ts
          ; yes_bid = c.yes_bid
          ; yes_ask = c.yes_ask
          })
;;

let%expect_test "snapshot extraction" =
  let hour = 3600 in
  let close_time =
    1_789_534_800
    (* 2026-09-16T05:00:00Z, on the hour *)
  in
  let open_time = close_time - (30 * hour) in
  let candle k ~bid ~ask =
    { end_ts = close_time - (k * hour); yes_bid = bid; yes_ask = ask }
  in
  let candles =
    [ candle 25 ~bid:(Some 10.) ~ask:(Some 12.)
    ; candle 24 ~bid:(Some 11.) ~ask:(Some 13.) (* exactly 24h before: used *)
    ; candle 6 ~bid:None ~ask:None (* nobody quoting: 6h skipped *)
    ; candle 2 ~bid:(Some 40.) ~ask:(Some 45.)
    ; candle 1 ~bid:(Some 50.) ~ask:None (* only a bid: NO ask exists, NO bid doesn't *)
    ]
  in
  of_candles ~ticker:"T" ~open_time ~close_time candles
  |> List.iter ~f:(fun s ->
    print_s
      [%message
        ""
          ~h:(Horizon.to_string s.horizon : string)
          ~hours_before:((close_time - s.ts) / hour : int)
          ~no_ask:(Snapshot.ask s No : float option)
          ~no_bid:(Snapshot.bid s No : float option)]);
  [%expect
    {|
    ((h 24h) (hours_before 24) (no_ask (89)) (no_bid (87)))
    ((h 1h) (hours_before 1) (no_ask (50)) (no_bid ()))
    |}]
;;

let%expect_test "a close that is not on the hour uses the candle ending just before" =
  let close_time =
    1_789_534_800 + 1800
    (* half past *)
  in
  let candles =
    [ { end_ts = 1_789_534_800 - 3600; yes_bid = Some 30.; yes_ask = Some 31. }
    ; { end_ts = 1_789_534_800; yes_bid = Some 70.; yes_ask = Some 72. }
    ]
  in
  of_candles ~ticker:"T" ~open_time:0 ~close_time candles
  |> List.iter ~f:(fun s ->
    printf
      "%s quote taken %d minutes before close\n"
      (Horizon.to_string s.horizon)
      ((close_time - s.ts) / 60));
  [%expect {| 1h quote taken 90 minutes before close |}]
;;

let%expect_test "anchored after close: horizons past the close are skipped" =
  let hour = 3600 in
  let close_time = 1_789_534_800 in
  (* An event-driven market that closed 2h before its expected expiration. *)
  let anchor = close_time + (2 * hour) in
  let candles =
    List.init 26 ~f:(fun k ->
      { end_ts = close_time - (k * hour)
      ; yes_bid = Some (Float.of_int k)
      ; yes_ask = Some 99.
      })
  in
  of_candles ~anchor ~ticker:"T" ~open_time:0 ~close_time candles
  |> List.iter ~f:(fun s ->
    printf
      "%s: quote from %dh before the actual close\n"
      (Horizon.to_string s.horizon)
      ((close_time - s.ts) / hour));
  [%expect
    {|
    24h: quote from 22h before the actual close
    6h: quote from 4h before the actual close
    |}]
;;

let%expect_test "markets opened after the target time have no snapshot" =
  let close_time = 1_789_534_800 in
  let candles =
    [ { end_ts = close_time - 3600; yes_bid = Some 5.; yes_ask = Some 6. } ]
  in
  print_s
    [%sexp
      (List.length
         (of_candles ~ticker:"T" ~open_time:(close_time - 1800) ~close_time candles)
       : int)];
  [%expect {| 0 |}]
;;
