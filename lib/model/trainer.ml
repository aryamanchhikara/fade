open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Fade_types
open Fade_analysis
module Row = Pit.Row

(* Walk-forward training: in every test month, each model is tuned on the validation
   months, refit on all earlier months, and scored on the test month. The baseline is
   the recalibrated price. *)

type config =
  { explain : Explain_builder.config (** folds, embargo, validation months, seed *)
  ; lambdas : float list
  ; gbm_grid : Lightgbm.Params.t list
  ; workdir : string
  ; use_gbm : bool
  }

let gbm_params num_leaves min_data_in_leaf : Lightgbm.Params.t =
  { num_leaves
  ; min_data_in_leaf
  ; learning_rate = 0.05
  ; lambda_l2 = 1.
  ; feature_fraction = 0.9
  }
;;

let default_config ~workdir =
  { explain = Explain_builder.default_config
  ; lambdas = [ 1e-4; 1e-3; 1e-2; 1e-1; 1. ]
  ; gbm_grid =
      [ gbm_params 7 50
      ; gbm_params 7 200
      ; gbm_params 15 50
      ; gbm_params 15 200
      ; gbm_params 31 200
      ]
  ; workdir
  ; use_gbm = Lightgbm.available ()
  }
;;

type prediction =
  { row : Row.t
  ; month : string
  ; baseline : float
  ; logistic : float
  ; gbm : float option
  }

module Logistic_model = struct
  type t =
    { design : Design.t
    ; scaler : Logistic.Scaler.t
    ; model : Logistic.t
    ; lambda : float
    }
  [@@deriving sexp]

  let fit ?init ~lambda rows =
    let design = Design.fit rows in
    let x =
      Array.of_list
        (List.map rows ~f:(fun (r : Row.t) -> Design.encode design r.features))
    in
    let scaler = Logistic.Scaler.fit x in
    let x = Array.map x ~f:(Logistic.Scaler.apply scaler) in
    let y = Array.of_list (List.map rows ~f:(fun (r : Row.t) -> r.no_won)) in
    let model, _ = Logistic.fit_gd ~lambda ~max_iter:3000 ~tol:1e-5 ?init ~x ~y () in
    { design; scaler; model; lambda }
  ;;

  let predict t (f : Pit.Features.t) =
    Logistic.predict t.model (Logistic.Scaler.apply t.scaler (Design.encode t.design f))
  ;;
end

let log_loss_of predict rows =
  let p = Array.of_list (List.map rows ~f:(fun (r : Row.t) -> predict r.features)) in
  let y = Array.of_list (List.map rows ~f:(fun (r : Row.t) -> r.no_won)) in
  Logistic.log_loss ~p ~y
;;

let choose_lambda config ~fit ~validation =
  if List.is_empty validation || List.length fit < 500
  then 1e-2
  else
    (* Strongest penalty first, each fit warm-started from the previous one. *)
    List.sort config.lambdas ~compare:(fun a b -> Float.compare b a)
    |> List.fold ~init:(None, []) ~f:(fun (prev, acc) lambda ->
      let m = Logistic_model.fit ?init:prev ~lambda fit in
      Some m.model, (lambda, log_loss_of (Logistic_model.predict m) validation) :: acc)
    |> snd
    |> List.min_elt ~compare:(fun (_, a) (_, b) -> Float.compare a b)
    |> Option.value_exn
    |> fst
;;

let matrix design rows =
  Array.of_list (List.map rows ~f:(fun (r : Row.t) -> Design.encode design r.features))
;;

let labels rows = Array.of_list (List.map rows ~f:(fun (r : Row.t) -> r.no_won))

let monotone design =
  Array.mapi design.Design.names ~f:(fun i _ ->
    if List.mem Design.price_columns i ~equal:Int.equal then 1 else 0)
;;

(* Picks parameters and the number of trees on validation; returns them. *)
let choose_gbm config ~dir ~fit ~validation =
  let design = Design.fit fit in
  let x = matrix design fit
  and y = labels fit
  and vx = matrix design validation
  and vy = labels validation in
  List.mapi config.gbm_grid ~f:(fun i params ->
    let dir = dir ^/ sprintf "grid%d" i in
    let model, trees =
      Lightgbm.train
        ~dir
        ~params
        ~monotone:(monotone design)
        ~seed:config.explain.seed
        ~x
        ~y
        ~valid:(vx, vy)
        ()
    in
    let p = Lightgbm.predict ~dir ~model ~x:vx in
    params, trees, Logistic.log_loss ~p ~y:vy)
  |> List.min_elt ~compare:(fun (_, _, a) (_, _, b) -> Float.compare a b)
  |> Option.value_exn
;;

let fit_gbm config ~dir ~params ~trees rows =
  let design = Design.fit rows in
  let model, _ =
    Lightgbm.train
      ~dir
      ~params
      ~monotone:(monotone design)
      ~seed:config.explain.seed
      ~x:(matrix design rows)
      ~y:(labels rows)
      ~iterations:(Int.max 1 trees)
      ()
  in
  design, model
;;

type fold_choice =
  { month : string
  ; n_train : int
  ; baseline_kind : string
  ; lambda : float
  ; gbm : (Lightgbm.Params.t * int) option
  }

let calib_rows = Explain_builder.calib_rows

let run config (rows : Row.t list) =
  Explain_builder.folds config.explain rows
  |> List.filter_map ~f:(fun (f : Row.t Walk_forward.fold) ->
    if List.is_empty f.test || List.length f.train < 500
    then None
    else (
      eprintf
        "fold %s: %d train, %d test\n%!"
        f.month
        (List.length f.train)
        (List.length f.test);
      let has_validation =
        (not (List.is_empty f.validation)) && List.length f.fit >= 500
      in
      let cal, _ =
        if has_validation
        then
          Calibrate.choose
            ~fit_rows:(calib_rows f.fit)
            ~validation:(calib_rows f.validation)
            ~final_rows:(calib_rows f.train)
        else Calibrate.fit Platt (calib_rows f.train), (Float.nan, Float.nan)
      in
      let lambda = choose_lambda config ~fit:f.fit ~validation:f.validation in
      let lr = Logistic_model.fit ~lambda f.train in
      let dir = config.workdir ^/ f.month in
      let gbm =
        if config.use_gbm && has_validation
        then (
          let params, trees, _ =
            choose_gbm config ~dir ~fit:f.fit ~validation:f.validation
          in
          let design, model =
            fit_gbm config ~dir:(dir ^/ "final") ~params ~trees f.train
          in
          let p =
            Lightgbm.predict ~dir:(dir ^/ "final") ~model ~x:(matrix design f.test)
          in
          Some ((params, trees), p))
        else None
      in
      let preds =
        List.mapi f.test ~f:(fun i (r : Row.t) ->
          { row = r
          ; month = f.month
          ; baseline =
              Calibrate.predict cal ~horizon:r.features.horizon ~ask:r.features.no_ask
          ; logistic = Logistic_model.predict lr r.features
          ; gbm = Option.map gbm ~f:(fun (_, p) -> p.(i))
          })
      in
      Some
        ( preds
        , { month = f.month
          ; n_train = List.length f.train
          ; baseline_kind = Calibrate.kind_to_string cal.kind
          ; lambda
          ; gbm = Option.map gbm ~f:fst
          } )))
;;

(* ---- scoring ---------------------------------------------------------------------- *)

let models (ps : prediction list) =
  [ Some ("baseline", fun p -> p.baseline)
  ; Some ("logistic", fun p -> p.logistic)
  ; (if List.for_all ps ~f:(fun p -> Option.is_some p.gbm)
     then Some ("lightgbm", fun p -> Option.value_exn p.gbm)
     else None)
  ]
  |> List.filter_opt
;;

let diff ~rng ~resamples ~loss (ps : prediction list) get : Train_report.Diff.t =
  let mean, ci =
    Metrics.difference
      ~rng
      ~resamples
      ~loss
      (List.map ps ~f:(fun p -> p.row.event_ticker, get p, p.baseline, p.row.no_won))
  in
  { mean; ci }
;;

let metrics ps get : Train_report.Metrics.t =
  let s = Metrics.summary (List.map ps ~f:(fun p -> get p, p.row.no_won)) in
  { log_loss = s.log_loss; brier = s.brier; ece = s.ece }
;;

let score_models config (ps : prediction list) ~key =
  List.map (models ps) ~f:(fun (name, get) ->
    let rng k =
      Random.State.make [| config.explain.seed; String.hash (key ^ name); k |]
    in
    let vs loss k =
      if String.equal name "baseline"
      then None
      else Some (diff ~rng:(rng k) ~resamples:config.explain.resamples ~loss ps get)
    in
    name, get, metrics ps get, vs Metrics.row_log_loss 0, vs Metrics.row_brier 1)
;;
