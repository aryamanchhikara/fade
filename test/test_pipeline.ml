open! Core
open Fade_types
open Fade_analysis

let dataset = lazy (Test_fixtures.load ())

let frozen =
  lazy
    (let d = force dataset in
     let config = Report_builder.default_config d in
     Report_builder.freeze ~frozen_at:"2026-11-16" ~config d)
;;

let report =
  lazy
    (Report_builder.build
       ~generated_at:"2026-11-16T00:00:00Z"
       ~synthetic:true
       ~frozen:(force frozen)
       (force dataset))
;;

let%expect_test "freeze on synthetic data" =
  let f = force frozen in
  print_s
    [%message
      ""
        ~cutoff:(f.config.cutoff : string)
        ~terciles:(f.config.volume_tercile_bounds : float list)
        ~examined:(f.examined : int)
        ~rules:(List.map f.rules ~f:Rule.describe : string list)
        ~notes:(f.notes : string list)];
  [%expect
    {|
    ((cutoff 2026-03-28) (terciles (885.27 1634.44)) (examined 215)
     (rules
      ("Buying NO at 85-90\194\162, 24h before close"
       "Buying NO at 55-60\194\162, 6h before close"
       "Buying NO at 90-95\194\162, 1h before close"
       "Buying NO at 90-95\194\162, 1h before close (Bracket events)"
       "Buying NO at 90-95\194\162, 1h before close (Markets with scheduled close)"))
     (notes
      ("568 in-sample events, 2 embargoed; 5 rules selected from 215 cells examined")))
    |}]
;;

let%expect_test "frozen file round-trips through its hash check" =
  let f = force frozen in
  print_s [%sexp (Or_error.is_ok (Frozen.of_string (Frozen.to_string f)) : bool)];
  [%expect {| true |}]
;;

let%expect_test "report: verdict, window, rule results" =
  let r = force report in
  print_s [%sexp (r.window : Report.Window.t)];
  print_s [%sexp (r.verdict : Verdict.t)];
  List.iter r.rules ~f:(fun rr ->
    printf
      "%s: in-sample %+.1f%% (n=%d), out-of-time %s\n"
      (Rule.describe rr.rule)
      (100. *. rr.in_sample.mean_return)
      rr.in_sample.n
      (match rr.out_of_time with
       | None -> "none"
       | Some b -> sprintf "%+.1f%% (n=%d)" (100. *. b.mean_return) b.n));
  [%expect
    {|
    ((first_settle 2025-05-17) (last_settle 2026-09-28) (n_markets 2590)
     (n_events 900) (n_trades 5223) (n_in_sample_events 568)
     (n_out_of_time_events 330) (n_embargoed_events 2)
     (dropped (("quote: no ask on that side" 95))))
    ((tone Neutral) (headline "No proven edge")
     (detail
      "0 of 5 scored rules has an out-of-time CI above zero. Best: Buying NO at 85-90\194\162, 24h before close returned +6.1% after fees out-of-time (95% CI -3.6% to +11.9%), n = 77 markets in 67 events. 5 rules were frozen in-sample from 215 cells examined; cutoff 2026-03-28."))
    Buying NO at 85-90¢, 24h before close: in-sample +6.6% (n=157), out-of-time +6.1% (n=77)
    Buying NO at 55-60¢, 6h before close: in-sample +20.8% (n=79), out-of-time +0.2% (n=58)
    Buying NO at 90-95¢, 1h before close: in-sample +6.1% (n=151), out-of-time +2.0% (n=73)
    Buying NO at 90-95¢, 1h before close (Bracket events): in-sample +6.6% (n=132), out-of-time +1.2% (n=64)
    Buying NO at 90-95¢, 1h before close (Markets with scheduled close): in-sample +6.1% (n=151), out-of-time +2.0% (n=73)
    |}]
;;

let%expect_test "out-of-time NO table, 1h, all markets" =
  let r = force report in
  let key : Table.Key.t =
    { sample = Out_of_time; view = Quotes; side = No; horizon = H1; segment = All }
  in
  let t = Option.value_exn (Tables.find r.tables key) in
  printf
    "%-8s %5s %5s %7s %7s %8s %s\n"
    "bucket"
    "n"
    "evts"
    "win"
    "b/even"
    "return"
    "95% CI";
  List.iter t.buckets ~f:(fun b ->
    printf
      "%-8s %5d %5d %6.1f%% %6.1f%% %+7.1f%% %s\n"
      (Bucket_result.label b)
      b.n
      b.n_events
      (100. *. b.win_rate)
      (100. *. b.breakeven)
      (100. *. b.mean_return)
      (match b.return_ci with
       | None -> "(n < min)"
       | Some ci -> sprintf "%+.1f%% to %+.1f%%" (100. *. ci.lo) (100. *. ci.hi)));
  [%expect
    {|
    bucket       n  evts     win  b/even   return 95% CI
    0-5¢        3     3    0.0%    3.6%  -100.0% (n < min)
    5-10¢      14    12    0.0%    7.9%  -100.0% (n < min)
    10-15¢     13    13    0.0%   13.2%  -100.0% (n < min)
    15-20¢     20    20    5.0%   17.9%   -72.1% (n < min)
    20-25¢     21    20    4.8%   23.1%   -79.3% (n < min)
    25-30¢     27    26   22.2%   28.5%   -21.9% (n < min)
    30-35¢     21    20   19.0%   33.3%   -42.8% (n < min)
    35-40¢     27    26   33.3%   39.0%   -14.6% (n < min)
    40-45¢     32    31   25.0%   44.1%   -43.3% -72.8% to -3.8%
    45-50¢     30    30   46.7%   49.1%    -4.9% -39.5% to +30.1%
    50-55¢     29    28   51.7%   54.1%    -4.4% (n < min)
    55-60¢     28    26   64.3%   58.5%    +9.9% (n < min)
    60-65¢     38    36   65.8%   63.9%    +2.9% -22.6% to +23.9%
    65-70¢     57    51   66.7%   68.1%    -2.1% -22.2% to +14.6%
    70-75¢     48    47   72.9%   73.7%    -1.1% -20.1% to +15.7%
    75-80¢     46    41   82.6%   78.0%    +6.0% -12.3% to +19.9%
    80-85¢     74    65   91.9%   83.4%   +10.2% -0.9% to +16.8%
    85-90¢     62    53   90.3%   87.9%    +2.8% -9.6% to +10.2%
    90-95¢     73    62   94.5%   92.7%    +2.0% -7.4% to +6.6%
    95-100¢    89    68   98.9%   97.3%    +1.7% -4.7% to +3.0%
    |}]
;;

let%expect_test "report.json size and round trip" =
  let r = force report in
  let s = Yojson.Safe.to_string (Report.yojson_of_t r) in
  let back = Report.t_of_yojson (Yojson.Safe.from_string s) in
  print_s
    [%message
      ""
        ~tables:(List.length r.tables : int)
        ~kb:(String.length s / 1024 : int)
        ~round_trip:(Report.equal r back : bool)];
  [%expect {| ((tables 516) (kb 2054) (round_trip true)) |}]
;;

let%expect_test "trade view: takers vs makers, out-of-time, all markets, last hour" =
  let r = force report in
  List.iter [ Role.Taker; Maker ] ~f:(fun role ->
    List.iter Side.all ~f:(fun side ->
      let key : Table.Key.t =
        { sample = Out_of_time; view = Trades role; side; horizon = H1; segment = All }
      in
      match Tables.find r.tables key with
      | None -> printf "%s %s: no trades\n" (Role.to_string role) (Side.to_string side)
      | Some t ->
        let contracts = List.sum (module Float) t.buckets ~f:(fun b -> b.contracts) in
        let profit =
          List.sum
            (module Float)
            t.buckets
            ~f:(fun b -> b.mean_return *. b.contracts *. (b.avg_price +. b.avg_fee))
        in
        let cost =
          List.sum
            (module Float)
            t.buckets
            ~f:(fun b -> b.contracts *. (b.avg_price +. b.avg_fee))
        in
        printf
          "%s buying %s: %d trades, %.0f contracts, return %+.1f%% per dollar\n"
          (Role.to_string role)
          (Side.to_string side)
          (List.sum (module Int) t.buckets ~f:(fun b -> b.n))
          contracts
          (100. *. profit /. cost)));
  [%expect
    {|
    taker buying YES: 38 trades, 5030 contracts, return -46.9% per dollar
    taker buying NO: 36 trades, 4541 contracts, return -3.2% per dollar
    maker buying YES: 36 trades, 4541 contracts, return +3.5% per dollar
    maker buying NO: 38 trades, 5030 contracts, return +22.3% per dollar
    |}]
;;
