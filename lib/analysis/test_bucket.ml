open! Core
open Fade_types

(* Hand-checkable numbers for the quote view and the bucket math. *)

let market ?(result = Side.No) ticker event : Market.t =
  { ticker
  ; event_ticker = event
  ; series_ticker = "S"
  ; category = "C"
  ; open_time = 0
  ; close_time = 100_000
  ; settle_time = 100_100
  ; result
  ; volume = 1000.
  ; can_close_early = false
  ; expected_expiration = None
  ; close_kind = Scheduled
  ; event_kind = Single
  }
;;

let config : Config.t =
  { data_since = "2025-05-16"
  ; data_until = "2026-11-15"
  ; cutoff = "2026-03-01"
  ; min_n = 3
  ; bucket_width = 5.
  ; contracts_per_order = 100.
  ; bootstrap_resamples = 200
  ; seed = 1
  ; volume_tercile_bounds = []
  ; anchor = Schedule
  ; selection = ""
  }
;;

let%expect_test "quote observation: NO ask from the YES bid, taker fee at 100 contracts" =
  let d : Dataset.t =
    { markets = [ market "A" "E1" ]
    ; snapshots =
        [ { ticker = "A"
          ; horizon = H1
          ; ts = 96_400
          ; yes_bid = Some 8.
          ; yes_ask = Some 10.
          }
        ]
    ; trades = []
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
  let drops = Observation.Drops.create () in
  let obs = Observation.of_quotes d ~config ~fees:(Fees.Index.create d.fees) ~drops in
  List.iter obs ~f:(fun o ->
    printf
      "buy %s at %.0f¢ + %.2f¢ fee -> %s, profit %+.2f¢ per contract\n"
      (Side.to_string o.side)
      o.price
      o.fee
      (if o.won then "won" else "lost")
      (Observation.profit o));
  [%expect
    {|
    buy YES at 10¢ + 0.63¢ fee -> lost, profit -10.63¢ per contract
    buy NO at 92¢ + 0.52¢ fee -> won, profit +7.48¢ per contract
    |}]
;;

let%expect_test "bucket: win rate, break-even and return per dollar" =
  (* Four NO contracts bought at 90c with a 0.5c fee: three win, one loses.
     Cost 4 x 90.5 = 362; payout 300; profit -62; return -62/362 = -17.1%. *)
  let obs =
    List.mapi [ true; true; true; false ] ~f:(fun i won ->
      { Observation.market = market (sprintf "M%d" i) (sprintf "E%d" (i / 2))
      ; side = No
      ; horizon = H1
      ; ts = 0
      ; price = 90.
      ; fee = 0.5
      ; weight = 1.
      ; won
      })
  in
  let b = Bucket.result ~rng:(Random.State.make [| 1 |]) ~config ~lo:90. obs in
  printf
    "n=%d events=%d win=%.2f breakeven=%.4f return=%.4f enough=%b\n"
    b.n
    b.n_events
    b.win_rate
    b.breakeven
    b.mean_return
    b.enough;
  let ci = Option.value_exn b.return_ci in
  printf
    "CI covers the estimate: %b\n"
    Float.(ci.lo <= b.mean_return && b.mean_return <= ci.hi);
  [%expect
    {|
    n=4 events=2 win=0.75 breakeven=0.9050 return=-0.1713 enough=true
    CI covers the estimate: true
    |}]
;;
