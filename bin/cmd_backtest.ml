open! Core
open Fade_types
open Fade_analysis
module Trainer = Fade_model.Trainer

(* `fade backtest`: the EV rule on out-of-sample predictions, walk-forward. *)

let grid = [ 0.; 0.005; 0.01; 0.02; 0.03; 0.05; 0.08 ]
let window = 2
let max_spread = 5.
let executions = [ 1, 0.; 10, 0.; 10, 1.; 10, 2. ]
let pct x = sprintf "%+.1f%%" (100. *. x)

let ci_str (c : Ci.t option) =
  match c with
  | Some c -> sprintf "%s to %s" (pct c.lo) (pct c.hi)
  | None -> "n/a"
;;

let preds_of (ps : Trainer.prediction list) =
  [ ( "logistic"
    , List.map ps ~f:(fun p -> { Backtest.row = p.row; month = p.month; p = p.logistic })
    )
  ; ( "baseline"
    , List.map ps ~f:(fun p -> { Backtest.row = p.row; month = p.month; p = p.baseline })
    )
  ]
  @
  if List.for_all ps ~f:(fun p -> Option.is_some p.gbm)
  then
    [ ( "lightgbm"
      , List.map ps ~f:(fun p ->
          { Backtest.row = p.row; month = p.month; p = Option.value_exn p.gbm }) )
    ]
  else []
;;

let variant ~fees ~months ~seed (model, preds) (size, slippage) =
  let cs = Backtest.candidates ~fees ~params:{ max_spread; size; slippage } preds in
  let wf = Backtest.walk_forward ~grid ~window ~months cs in
  let rng k =
    Random.State.make
      [| seed; String.hash model; size; Float.to_int (slippage *. 10.); k |]
  in
  let summary ~k ts = Backtest.summary ~rng:(rng k) ~resamples:1000 ~size ts in
  let all = List.concat_map wf ~f:(fun (_, _, ts) -> ts) in
  { Backtest_report.Variant.model
  ; size
  ; slippage
  ; months =
      List.mapi wf ~f:(fun i (month, threshold, ts) ->
        { Backtest_report.Month.month; threshold; summary = summary ~k:(i + 1) ts })
  ; total = summary ~k:0 all
  ; by_category =
      List.map all ~f:(fun c -> c.pred.row.features.category, c)
      |> String.Map.of_alist_multi
      |> Map.to_alist
      |> List.map ~f:(fun (cat, ts) -> cat, summary ~k:(String.hash cat) ts)
  }
;;

let print (r : Backtest_report.t) =
  printf
    "fade backtest%s: taker at the NO ask, spread <= %.0fc, one trade per market, \
     threshold chosen on the %d months before each test month\n\n"
    (if r.synthetic then " (SYNTHETIC)" else "")
    r.max_spread
    window;
  printf
    "  %-9s %4s %5s %7s %6s %7s %7s %9s  %-18s %9s %9s\n"
    "model"
    "size"
    "slip"
    "trades"
    "hit"
    "price"
    "fee"
    "return"
    "95% CI"
    "profit $"
    "max DD $";
  List.iter r.variants ~f:(fun v ->
    let t = v.total in
    printf
      "  %-9s %4d %4.0fc %7d %5.1f%% %6.1fc %6.2fc %9s  %-18s %9.2f %9.2f\n"
      v.model
      v.size
      v.slippage
      t.trades
      (100. *. t.hit_rate)
      t.avg_price
      t.avg_fee
      (pct t.return_per_dollar)
      (ci_str t.ci)
      t.profit
      t.max_drawdown);
  printf "\nModel disagrees with the ask by more than the fee (share of test rows):\n";
  List.iter r.disagreement ~f:(fun (m, s) -> printf "  %-9s %.1f%%\n" m (100. *. s))
;;

let print_detail (v : Backtest_report.Variant.t) =
  printf "\n%s, size %d, slippage %.0fc: by month\n" v.model v.size v.slippage;
  List.iter v.months ~f:(fun m ->
    printf
      "  %-8s threshold %4.1fc  trades %5d  hit %5.1f%%  return %8s  %s\n"
      m.month
      (100. *. m.threshold)
      m.summary.trades
      (100. *. m.summary.hit_rate)
      (pct m.summary.return_per_dollar)
      (ci_str m.summary.ci));
  printf "%s, size %d: by category\n" v.model v.size;
  List.iter v.by_category ~f:(fun (cat, s) ->
    printf
      "  %-22s trades %5d  hit %5.1f%%  return %8s  %s\n"
      cat
      s.trades
      (100. *. s.hit_rate)
      (pct s.return_per_dollar)
      (ci_str s.ci))
;;

let command =
  Command.basic
    ~summary:"Backtest the EV rule on out-of-sample predictions (walk-forward)"
    (let%map_open.Command demo = flag "--demo" no_arg ~doc:" synthetic markets"
     and db =
       flag "--db" (optional_with_default "data/fade.db" string) ~doc:"PATH study cache"
     and frozen_path =
       flag
         "--frozen"
         (optional_with_default "rules/frozen.json" string)
         ~doc:"PATH study rules"
     and out = flag "--out" (optional string) ~doc:"PATH (default web/data/backtest.json)"
     and freeze =
       flag
         "--freeze"
         (optional string)
         ~doc:"PATH write the chosen rule (rules/model_rule.json)"
     and force = flag "--force" no_arg ~doc:" overwrite an existing frozen rule" in
     fun () ->
       let rows, fees, synthetic, since, until =
         if demo
         then
           ( Fade_synth.Synth_pit.rows (Fade_synth.Synth_pit.generate ())
           , Fees.Index.create
               { series =
                   [ { series_ticker = "SYN"
                     ; current_type = Quadratic
                     ; current_multiplier = 1.
                     ; changes = []
                     }
                   ]
               ; event_overrides = []
               }
           , true
           , "2025-06-01"
           , "2026-11-30" )
         else (
           let study = Frozen.of_string (In_channel.read_all frozen_path) |> ok_exn in
           let store = Fade_store.open_ db in
           let rows, _ = Fade_store.Pit_load.load store ~fraction:0.02 in
           let series_used =
             String.Set.of_list
               (List.map rows ~f:(fun (r : Pit.Row.t) -> r.series_ticker))
           in
           let fees = Fees.Index.create (Fade_store.Load.fees store ~series_used) in
           Fade_store.close store;
           rows, fees, false, study.config.data_since, study.config.data_until)
       in
       let workdir = if demo then "data/models/demo-work" else "data/models/work" in
       let config = Trainer.default_config ~workdir in
       let results = Trainer.run config rows in
       let ps = List.concat_map results ~f:fst in
       let months = List.map results ~f:(fun (_, c) -> c.month) in
       let models = preds_of ps in
       let variants =
         List.concat_map models ~f:(fun m ->
           List.map executions ~f:(variant ~fees ~months ~seed:config.explain.seed m))
       in
       let report : Backtest_report.t =
         { generated_at = Data_source.now_string ()
         ; synthetic
         ; max_spread
         ; thresholds = grid
         ; disagreement =
             List.map models ~f:(fun (m, preds) -> m, Backtest.disagreement ~fees preds)
         ; variants
         ; notes =
             [ sprintf
                 "Months %s and %s only choose the first threshold; trades start in %s."
                 (List.nth_exn months 0)
                 (List.nth_exn months 1)
                 (List.nth_exn months 2)
             ; "Size 10 fills at the ask plus the slippage shown: there is no historical \
                book depth."
             ]
         }
       in
       print report;
       List.iter variants ~f:(fun v ->
         if
           (not (String.equal v.model "baseline"))
           && v.size = 10
           && Float.equal v.slippage 1.
         then print_detail v);
       let out =
         Option.value
           out
           ~default:
             (if demo then "web/data/demo-backtest.json" else "web/data/backtest.json")
       in
       Core_unix.mkdir_p (Filename.dirname out);
       Out_channel.write_all
         out
         ~data:(Yojson.Safe.to_string (Backtest_report.yojson_of_t report) ^ "\n");
       printf "\nwrote %s\n" out;
       Option.iter freeze ~f:(fun path ->
         if Sys_unix.file_exists_exn path && not force
         then (
           eprintf
             "%s exists; refreezing after forward data defeats the test (--force)\n"
             path;
           exit 1);
         (* The final choice uses only the last [window] months: the model and threshold
            with the most profit there at the default execution (size 10, 1c slippage). *)
         let last = List.sub months ~pos:(List.length months - window) ~len:window in
         let candidates_for (_, preds) =
           let cs =
             Backtest.candidates
               ~fees
               ~params:{ max_spread; size = 10; slippage = 1. }
               preds
           in
           List.filter cs ~f:(fun c -> List.mem last c.pred.month ~equal:String.equal)
         in
         let choices =
           List.filter models ~f:(fun (m, _) -> not (String.equal m "baseline"))
           |> List.map ~f:(fun ((m, _) as model) ->
             let cs = candidates_for model in
             let threshold = Backtest.choose_threshold ~grid cs in
             ( m
             , threshold
             , List.sum (module Float) (Backtest.trades ~threshold cs) ~f:Backtest.profit
             ))
         in
         let model, threshold, validation_profit =
           List.max_elt choices ~compare:(fun (_, _, a) (_, _, b) -> Float.compare a b)
           |> Option.value_exn
         in
         let train =
           Train_report.t_of_yojson
             (Yojson.Safe.from_file
                (if demo then "web/data/demo-train.json" else "web/data/train.json"))
         in
         let file name =
           let s = List.find_exn train.saved ~f:(fun s -> String.equal s.model name) in
           { Model_rule.Model_file.name
           ; path = s.path
           ; sha256 = Model_rule_file.sha256_hex (In_channel.read_all s.path)
           }
         in
         let config : Model_rule.Config.t =
           { model = file model
           ; baseline = file "baseline"
           ; data_since = since
           ; data_until = until
           ; threshold
           ; max_spread
           ; sizes = [ 1; 10 ]
           ; slippage = 1.
           ; min_volume = Pit.min_volume
           ; horizons = Horizon.all
           ; one_trade_per_market = true
           ; selection =
               sprintf
                 "Model and EV threshold with the highest profit over the last %d \
                  walk-forward months (%s) at size 10 with 1c slippage, from the grid %s \
                  dollars."
                 window
                 (String.concat ~sep:", " last)
                 (String.concat ~sep:", " (List.map grid ~f:(sprintf "%g")))
           }
         in
         let chosen v = String.equal v.Backtest_report.Variant.model model in
         let rule =
           Model_rule_file.create
             ~frozen_at:(Data_source.now_string ())
             ~config
             ~backtest:
               (List.filter_map variants ~f:(fun v ->
                  Option.some_if
                    (chosen v)
                    (sprintf "size %d, slippage %.0fc" v.size v.slippage, v.total)))
             ~notes:
               [ sprintf
                   "Validation-window profit at the chosen threshold: $%.2f per \
                    10-contract order sequence."
                   validation_profit
               ]
         in
         Core_unix.mkdir_p (Filename.dirname path);
         Out_channel.write_all path ~data:(Model_rule_file.to_string rule);
         printf
           "froze %s: model %s, threshold %.1fc, sha256 %s\n"
           path
           model
           (100. *. threshold)
           rule.hash))
;;
