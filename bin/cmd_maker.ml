open! Core
open Fade_types
open Fade_analysis
module Trainer = Fade_model.Trainer
module Mb = Maker_backtest

(* `fade maker`: post a NO bid one cent inside the spread when the model says it is
   worth it, instead of paying the ask. Walk-forward, adverse selection measured. *)

let grid = [ 0.; 0.005; 0.01; 0.02; 0.03; 0.05; 0.08 ]
let window = 2
let pct x = sprintf "%+.1f%%" (100. *. x)

let ci_str (c : Ci.t option) =
  match c with
  | Some c -> sprintf "%s to %s" (pct c.lo) (pct c.hi)
  | None -> "n/a"
;;

(* Everything examined so far, phase by phase, so the result can be read against the
   number of looks it took. *)
let tally ~synthetic : Maker_report.Tally.t list =
  let json path f =
    if Sys_unix.file_exists_exn path then f (Yojson.Safe.from_file path) else 0
  in
  let report_cells =
    json "web/data/report.json" (fun j ->
      let r = Report.t_of_yojson j in
      List.sum
        (module Int)
        r.tables
        ~f:(fun t -> List.count t.buckets ~f:(fun b -> b.enough)))
  in
  let frozen_cells =
    json "rules/frozen.json" (fun j -> Yojson.Safe.Util.(member "examined" j |> to_int))
  in
  let explain_levels =
    json "web/data/explain.json" (fun j -> List.length (Explain.t_of_yojson j).levels)
  in
  if synthetic
  then []
  else
    [ { phase = "3"
      ; what = "in-sample NO quote cells searched for the frozen rules"
      ; count = frozen_cells
      }
    ; { phase = "3-5"
      ; what =
          "report cells (both samples, sides, views, horizons, segments; n >= 30) looked \
           at"
      ; count = report_cells
      }
    ; { phase = "6"; what = "candidate rules (A, B, B-loose), forward-tested"; count = 3 }
    ; { phase = "7"
      ; what = "factor levels in the residual analysis"
      ; count = explain_levels
      }
    ; { phase = "8"
      ; what = "model settings (2 baselines, 5 L2 strengths, 5 LightGBM grids)"
      ; count = 12
      }
    ; { phase = "9"
      ; what = "taker EV rules (3 models x 4 executions x 7 thresholds)"
      ; count = 84
      }
    ; { phase = "9b"
      ; what = "maker rules (2 fill models x 7 thresholds x 4 horizons)"
      ; count = 56
      }
    ]
;;

let calibration fill (os : Mb.order list) =
  Metrics.calibration
    (List.filter_map os ~f:(fun o ->
       Option.some_if (Mb.filled fill o) (o.pred.p, o.pred.row.no_won)))
  |> List.map ~f:(fun (b : Metrics.Bin.t) ->
    { Train_report.Calibration_bin.lo = b.lo
    ; hi = b.hi
    ; n = b.n
    ; mean_p = b.mean_p
    ; observed = b.observed
    })
;;

let variant ~months ~seed fill os : Maker_report.Variant.t =
  let wf = Mb.walk_forward ~grid ~window ~fill ~months os in
  let rng k =
    Random.State.make [| seed; String.hash (Mb.fill_model_to_string fill); k |]
  in
  let summary ~k placed =
    Backtest.summary
      ~rng:(rng k)
      ~resamples:1000
      ~size:1
      (List.filter_map placed ~f:(fun o ->
         Option.some_if (Mb.filled fill o) (Mb.to_candidate o)))
  in
  let all = List.concat_map wf ~f:(fun (_, _, _, p) -> p) in
  { fill_model = Mb.fill_model_to_string fill
  ; months =
      List.mapi wf ~f:(fun i (month, threshold, horizon, placed) ->
        { Maker_report.Month.month
        ; threshold
        ; horizon = Mb.horizon_choice_to_string horizon
        ; orders = List.length placed
        ; summary = summary ~k:(i + 1) placed
        })
  ; orders = List.length all
  ; total = summary ~k:0 all
  ; adverse = Mb.adverse fill all
  ; calibration_on_fills = calibration fill all
  }
;;

let print (r : Maker_report.t) =
  printf
    "fade maker%s: rest a 1-contract NO bid 1c above the best NO bid when P_%s(NO) - bid \
     - maker fee > threshold; threshold and horizon chosen on the %d months before each \
     month\n"
    (if r.synthetic then " (SYNTHETIC)" else "")
    r.model
    window;
  printf
    "%d rows; %d two-sided rows had a 1c spread (no room inside)\n\n"
    r.rows
    r.no_room;
  printf
    "  %-7s %7s %6s %6s %6s %6s %6s %9s  %-18s %8s %8s\n"
    "fills"
    "orders"
    "filled"
    "rate"
    "hit"
    "bid"
    "fee"
    "return"
    "95% CI"
    "profit $"
    "max DD $";
  List.iter r.variants ~f:(fun v ->
    let t = v.total in
    printf
      "  %-7s %7d %6d %5.1f%% %5.1f%% %5.1fc %5.2fc %9s  %-18s %8.2f %8.2f\n"
      v.fill_model
      v.orders
      t.trades
      (100. *. Float.of_int t.trades /. Float.of_int (Int.max 1 v.orders))
      (100. *. t.hit_rate)
      t.avg_price
      t.avg_fee
      (pct t.return_per_dollar)
      (ci_str t.ci)
      t.profit
      t.max_drawdown);
  printf "\nAdverse selection (walk-forward orders)\n";
  printf
    "  %-7s %8s %8s %16s %18s %16s %18s\n"
    "fills"
    "orders"
    "filled"
    "win rate, filled"
    "win rate, unfilled"
    "model P, filled"
    "model P, unfilled";
  List.iter r.variants ~f:(fun v ->
    let a = v.adverse in
    printf
      "  %-7s %8d %8d %15.1f%% %17.1f%% %15.1f%% %17.1f%%\n"
      v.fill_model
      a.orders
      a.filled
      (100. *. a.win_rate_filled)
      (100. *. a.win_rate_unfilled)
      (100. *. a.model_p_filled)
      (100. *. a.model_p_unfilled));
  List.iter r.variants ~f:(fun v ->
    printf
      "\n%s fills by month (threshold and horizon chosen on the two months before)\n"
      v.fill_model;
    List.iter v.months ~f:(fun m ->
      printf
        "  %-8s threshold %4.1fc  horizon %-5s  orders %4d  fills %4d  hit %5.1f%%  \
         return %8s  %s\n"
        m.month
        (100. *. m.threshold)
        m.horizon
        m.orders
        m.summary.trades
        (100. *. m.summary.hit_rate)
        (pct m.summary.return_per_dollar)
        (ci_str m.summary.ci));
    printf "%s fills: model P(NO) vs outcome\n" v.fill_model;
    List.iter v.calibration_on_fills ~f:(fun b ->
      printf
        "    %.1f-%.1f  n=%5d  predicted %5.1f%%  won %5.1f%%\n"
        b.lo
        b.hi
        b.n
        (100. *. b.mean_p)
        (100. *. b.observed)));
  if not (List.is_empty r.tally)
  then (
    printf "\nLooks taken so far (Phases 3 to 9b)\n";
    List.iter r.tally ~f:(fun t -> printf "  %-5s %6d  %s\n" t.phase t.count t.what);
    printf "  total %6d\n" r.tally_total);
  printf "\nVERDICT: %s\n" r.verdict
;;

let command =
  Command.basic
    ~summary:"Backtest the model-gated maker rule (post inside the spread), walk-forward"
    (let%map_open.Command demo = flag "--demo" no_arg ~doc:" synthetic markets"
     and db =
       flag "--db" (optional_with_default "data/fade.db" string) ~doc:"PATH study cache"
     and out = flag "--out" (optional string) ~doc:"PATH (default web/data/maker.json)" in
     fun () ->
       let rows, fees, synthetic =
         if demo
         then
           ( Fade_synth.Synth_pit.rows (Fade_synth.Synth_pit.generate ())
           , Fees.Index.create
               { series =
                   [ { series_ticker = "SYN"
                     ; current_type = Quadratic_with_maker_fees
                     ; current_multiplier = 1.
                     ; changes = []
                     }
                   ]
               ; event_overrides = []
               }
           , true )
         else (
           let store = Fade_store.open_ db in
           let rows, _ = Fade_store.Pit_load.load store ~fraction:0.02 in
           let series_used =
             String.Set.of_list
               (List.map rows ~f:(fun (r : Pit.Row.t) -> r.series_ticker))
           in
           let fees = Fees.Index.create (Fade_store.Load.fees store ~series_used) in
           Fade_store.close store;
           rows, fees, false)
       in
       let config =
         Trainer.default_config
           ~workdir:(if demo then "data/models/demo-work" else "data/models/work")
       in
       (* Only the logistic model's predictions are used; skip LightGBM to save time. *)
       let config = { config with use_gbm = false } in
       let results = Trainer.run config rows in
       let months = List.map results ~f:(fun (_, c) -> c.month) in
       let preds =
         List.concat_map results ~f:fst
         |> List.map ~f:(fun (p : Trainer.prediction) ->
           { Backtest.row = p.row; month = p.month; p = p.logistic })
       in
       let os, no_room = Mb.orders ~fees preds in
       let variants =
         List.map [ Mb.Strict; Loose ] ~f:(fun fill ->
           variant ~months ~seed:config.explain.seed fill os)
       in
       let tally = tally ~synthetic in
       let strict = List.hd_exn variants in
       let clears = Option.value_map strict.total.ci ~default:false ~f:Ci.above_zero in
       let report : Maker_report.t =
         { generated_at = Data_source.now_string ()
         ; synthetic
         ; model = "logistic"
         ; rows = List.length rows
         ; no_room
         ; variants
         ; tally
         ; tally_total = List.sum (module Int) tally ~f:(fun t -> t.count)
         ; verdict =
             (if clears
              then
                "The strict-fill CI clears zero: freeze the rule and add it to the \
                 forward test."
              else
                sprintf
                  "The strict-fill return CI does not clear zero (%s, %s): the maker \
                   rule does not convert the model's edge into profit either."
                  (pct strict.total.return_per_dollar)
                  (ci_str strict.total.ci))
         ; notes =
             [ "Months 2025-11 and 2025-12 only choose the first threshold and horizon." ]
         }
       in
       print report;
       let out =
         Option.value
           out
           ~default:(if demo then "web/data/demo-maker.json" else "web/data/maker.json")
       in
       Core_unix.mkdir_p (Filename.dirname out);
       Out_channel.write_all
         out
         ~data:(Yojson.Safe.to_string (Maker_report.yojson_of_t report) ^ "\n");
       printf "wrote %s\n" out)
;;
