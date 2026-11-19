open! Core
open Fade_types
open Fade_analysis
module Trainer = Fade_model.Trainer
module Row = Pit.Row

(* `fade train`: the recalibrated price vs a hand-written logistic regression vs
   LightGBM, out of sample in walk-forward months. *)

let pct4 x = sprintf "%.4f" x

let diff_str (d : Train_report.Diff.t option) =
  match d with
  | None -> "-"
  | Some d ->
    sprintf
      "%+.4f (%s)"
      d.mean
      (match d.ci with
       | Some c -> sprintf "%+.4f..%+.4f" c.lo c.hi
       | None -> "no CI")
;;

let hash_of parts =
  Digestif.SHA256.digest_string (String.concat ~sep:"\n" parts) |> Digestif.SHA256.to_hex
;;

let build_report ~config ~synthetic ~data_since ~data_until ~models_dir rows =
  let results = Trainer.run config rows in
  let ps = List.concat_map results ~f:fst in
  let choices = List.map results ~f:snd in
  let folds =
    List.map results ~f:(fun (fold_ps, (c : Trainer.fold_choice)) ->
      let scored = Trainer.score_models config fold_ps ~key:c.month in
      { Train_report.Fold.month = c.month
      ; n_train = c.n_train
      ; n_test = List.length fold_ps
      ; events_test =
          List.map fold_ps ~f:(fun p -> p.row.event_ticker)
          |> List.dedup_and_sort ~compare:String.compare
          |> List.length
      ; models =
          List.map scored ~f:(fun (name, _, metrics, ll, br) ->
            { Train_report.Model_score.model = name
            ; metrics
            ; log_loss_vs_baseline = ll
            ; brier_vs_baseline = br
            ; chosen =
                (match name with
                 | "baseline" -> c.baseline_kind
                 | "logistic" -> sprintf "L2 lambda %g" c.lambda
                 | _ ->
                   Option.value_map c.gbm ~default:"" ~f:(fun (p, trees) ->
                     sprintf "%s, %d trees" (Fade_model.Lightgbm.Params.to_string p) trees))
            })
      })
  in
  let pooled =
    List.map
      (Trainer.score_models config ps ~key:"pooled")
      ~f:(fun (name, get, metrics, ll, br) ->
        { Train_report.Pooled.model = name
        ; metrics
        ; log_loss_vs_baseline = ll
        ; brier_vs_baseline = br
        ; calibration =
            Metrics.calibration (List.map ps ~f:(fun p -> get p, p.row.no_won))
            |> List.map ~f:(fun (b : Metrics.Bin.t) ->
              { Train_report.Calibration_bin.lo = b.lo
              ; hi = b.hi
              ; n = b.n
              ; mean_p = b.mean_p
              ; observed = b.observed
              })
        })
  in
  let beats =
    List.filter_map pooled ~f:(fun p ->
      match p.log_loss_vs_baseline with
      | Some d when Train_report.Diff.better d -> Some p.model
      | _ -> None)
  in
  let verdict =
    match beats with
    | [] ->
      "Neither model beats the recalibrated price out of sample: no pooled log-loss \
       difference has a 95% CI entirely below zero."
    | ms ->
      sprintf
        "%s beat%s the recalibrated price out of sample on log loss (pooled 95%% CI \
         below zero). Positive EV after fees is tested next."
        (String.concat ~sep:" and " ms)
        (if List.length ms = 1 then "s" else "")
  in
  (* Final models on every row, with the most recent fold's validation choices. *)
  let saved =
    match List.last choices, rows with
    | None, _ | _, [] -> []
    | Some last, _ ->
      Core_unix.mkdir_p models_dir;
      let design = Design.fit rows in
      let window = sprintf "%s..%s" data_since data_until in
      let features = String.concat ~sep:"," (Array.to_list design.names) in
      let save model ~params ~ext ~contents =
        let h = hash_of [ model; window; features; params ] in
        let path = models_dir ^/ sprintf "%s-%s.%s" model (String.prefix h 12) ext in
        Out_channel.write_all path ~data:contents;
        { Train_report.Saved.model; path; hash = h }
      in
      let cal =
        Calibrate.fit
          (if String.equal last.baseline_kind "Platt" then Platt else Isotonic)
          (Trainer.calib_rows rows)
      in
      let lr = Trainer.Logistic_model.fit ~lambda:last.lambda rows in
      [ save
          "baseline"
          ~params:last.baseline_kind
          ~ext:"sexp"
          ~contents:(Sexp.to_string_hum (Calibrate.sexp_of_t cal) ^ "\n")
      ; save
          "logistic"
          ~params:(sprintf "lambda=%g" last.lambda)
          ~ext:"sexp"
          ~contents:(Sexp.to_string_hum (Trainer.Logistic_model.sexp_of_t lr) ^ "\n")
      ]
      @
        (match last.gbm with
        | Some (params, trees) when config.use_gbm ->
          let _, model =
            Trainer.fit_gbm config ~dir:(config.workdir ^/ "final") ~params ~trees rows
          in
          [ save
              "lightgbm"
              ~params:
                (sprintf
                   "%s trees=%d"
                   (Sexp.to_string (Fade_model.Lightgbm.Params.sexp_of_t params))
                   trees)
              ~ext:"txt"
              ~contents:(In_channel.read_all model)
          ]
        | _ -> [])
  in
  let report : Train_report.t =
    { generated_at = Data_source.now_string ()
    ; synthetic
    ; data_since
    ; data_until
    ; features = Array.to_list (Design.fit rows).names
    ; rows = List.length rows
    ; test_rows = List.length ps
    ; folds
    ; pooled
    ; beats_baseline = beats
    ; verdict
    ; saved
    ; notes =
        [ "Each test month: baseline (Platt or isotonic), L2 strength and LightGBM \
           parameters chosen on the two previous months, then refit on all earlier \
           months. Differences are model minus baseline per row; negative is better. CIs \
           resample events."
        ]
    }
  in
  report, ps
;;

let print (r : Train_report.t) =
  printf
    "fade train%s: %d rows, %d out-of-sample test rows, %d features\n\n"
    (if r.synthetic then " (SYNTHETIC)" else "")
    r.rows
    r.test_rows
    (List.length r.features);
  printf
    "Log loss per walk-forward month (difference vs baseline, 95%% CI resampling events)\n";
  let model_names =
    List.hd r.folds
    |> Option.value_map ~default:[] ~f:(fun f -> List.map f.models ~f:(fun m -> m.model))
  in
  printf "  %-8s %6s" "month" "test";
  List.iter model_names ~f:(fun m -> printf " | %-36s" m);
  printf "\n";
  List.iter r.folds ~f:(fun f ->
    printf "  %-8s %6d" f.month f.n_test;
    List.iter f.models ~f:(fun m ->
      printf
        " | %s %-29s"
        (pct4 m.metrics.log_loss)
        (match m.log_loss_vs_baseline with
         | None -> sprintf "(%s)" m.chosen
         | Some _ -> diff_str m.log_loss_vs_baseline));
    printf "\n");
  printf "\nPooled over all test months\n";
  printf
    "  %-9s %8s %8s %8s  %-30s %-30s\n"
    "model"
    "log loss"
    "Brier"
    "ECE"
    "log loss vs baseline"
    "Brier vs baseline";
  List.iter r.pooled ~f:(fun p ->
    printf
      "  %-9s %8s %8s %8s  %-30s %-30s\n"
      p.model
      (pct4 p.metrics.log_loss)
      (pct4 p.metrics.brier)
      (pct4 p.metrics.ece)
      (diff_str p.log_loss_vs_baseline)
      (diff_str p.brier_vs_baseline));
  printf "\nCalibration (pooled): predicted P(NO) vs observed, by decile of prediction\n";
  List.iter r.pooled ~f:(fun p ->
    printf "  %s\n" p.model;
    List.iter p.calibration ~f:(fun b ->
      printf
        "    %.1f-%.1f  n=%6d  predicted %5.1f%%  observed %5.1f%%\n"
        b.lo
        b.hi
        b.n
        (100. *. b.mean_p)
        (100. *. b.observed)));
  printf "\nVERDICT: %s\n" r.verdict;
  List.iter r.saved ~f:(fun s ->
    printf "saved %s: %s (sha256 %s)\n" s.model s.path (String.prefix s.hash 16))
;;

let command =
  Command.basic
    ~summary:"Train and compare models against the recalibrated price (walk-forward)"
    (let%map_open.Command demo = flag "--demo" no_arg ~doc:" synthetic markets"
     and db =
       flag "--db" (optional_with_default "data/fade.db" string) ~doc:"PATH study cache"
     and frozen_path =
       flag
         "--frozen"
         (optional_with_default "rules/frozen.json" string)
         ~doc:"PATH study frozen rules (for the data window)"
     and out = flag "--out" (optional string) ~doc:"PATH (default web/data/train.json)"
     and models_dir =
       flag
         "--models"
         (optional string)
         ~doc:"DIR where final models go (default models/)"
     and no_gbm = flag "--no-gbm" no_arg ~doc:" skip LightGBM" in
     fun () ->
       let rows, synthetic, data_since, data_until =
         if demo
         then
           ( Fade_synth.Synth_pit.rows (Fade_synth.Synth_pit.generate ())
           , true
           , "2025-06-01"
           , "2026-11-30" )
         else (
           let study = Frozen.of_string (In_channel.read_all frozen_path) |> ok_exn in
           let store = Fade_store.open_ db in
           let rows, s = Fade_store.Pit_load.load store ~fraction:0.02 in
           Fade_store.close store;
           if s.leakage_failures > 0 then raise_s [%message "leakage check failed"];
           rows, false, study.config.data_since, study.config.data_until)
       in
       let workdir = if demo then "data/models/demo-work" else "data/models/work" in
       let config = Trainer.default_config ~workdir in
       let config = { config with use_gbm = config.use_gbm && not no_gbm } in
       if not config.use_gbm
       then eprintf "LightGBM not used (not installed, or --no-gbm)\n%!";
       let models_dir =
         Option.value models_dir ~default:(if demo then "data/models/demo" else "models")
       in
       let report, ps =
         build_report ~config ~synthetic ~data_since ~data_until ~models_dir rows
       in
       let out =
         Option.value
           out
           ~default:(if demo then "web/data/demo-train.json" else "web/data/train.json")
       in
       Core_unix.mkdir_p (Filename.dirname out);
       Out_channel.write_all
         out
         ~data:(Yojson.Safe.to_string (Train_report.yojson_of_t report) ^ "\n");
       (* Out-of-sample predictions for the Phase 9 backtest. Derived from Kalshi data,
          so they stay under data/ (not committed). *)
       let pred_path =
         if demo then "data/models/demo-oos.csv" else "data/models/oos.csv"
       in
       Core_unix.mkdir_p (Filename.dirname pred_path);
       Out_channel.with_file pred_path ~f:(fun oc ->
         Out_channel.output_string
           oc
           "ticker,event,horizon,month,no_ask,spread,no_won,baseline,logistic,lightgbm\n";
         List.iter ps ~f:(fun p ->
           let f = p.row.features in
           Out_channel.output_string
             oc
             (sprintf
                "%s,%s,%s,%s,%g,%s,%d,%.6f,%.6f,%s\n"
                p.row.ticker
                p.row.event_ticker
                (Horizon.to_string f.horizon)
                p.month
                f.no_ask
                (Option.value_map f.spread ~default:"" ~f:(sprintf "%g"))
                (if p.row.no_won then 1 else 0)
                p.baseline
                p.logistic
                (Option.value_map p.gbm ~default:"" ~f:(sprintf "%.6f")))));
       print report;
       printf "wrote %s and %s\n" out pred_path)
;;
