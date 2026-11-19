open! Core
open Fade_types
module Row = Pit.Row
module F = Pit.Features

(* `fade explain`: which point-in-time factors predict NO outcomes beyond the
   recalibrated price. We explain RESIDUALS, 1[NO won] - baseline P(NO), computed out of
   sample in each walk-forward test month; raw winners vs losers would mostly restate
   the price. Pure: rows in, an [Explain.t] out. *)

type config =
  { first_test : string
  ; last_test : string
  ; embargo_days : int
  ; validation_months : int
  ; min_fold_rows : int
  ; resamples : int
  ; seed : int
  }

let default_config =
  { first_test = "2025-11"
  ; last_test = "2026-11"
  ; embargo_days = 2
  ; validation_months = 2
  ; min_fold_rows = 30
  ; resamples = 1000
  ; seed = 20260516
  }
;;

type scored =
  { row : Row.t
  ; month : string
  ; p : float (** recalibrated P(NO) *)
  ; residual : float
  }

let calib_rows rows =
  List.map rows ~f:(fun (r : Row.t) -> r.features.horizon, r.features.no_ask, r.no_won)
;;

let folds config rows =
  Walk_forward.folds
    ~first_test:config.first_test
    ~last_test:config.last_test
    ~embargo_days:config.embargo_days
    ~validation_months:config.validation_months
    ~event:(fun (r : Row.t) -> r.event_ticker)
    ~settle:(fun (r : Row.t) -> r.settle_time)
    ~decision:(fun (r : Row.t) -> r.features.decision)
    rows
;;

(* Out-of-sample residuals, one test month at a time. *)
let score config rows : scored list * Explain.Fold.t list =
  List.filter_map (folds config rows) ~f:(fun (f : Row.t Walk_forward.fold) ->
    if List.is_empty f.test || List.length f.train < 500
    then None
    else (
      let cal, (vp, vi) =
        if List.is_empty f.validation || List.length f.fit < 500
        then Calibrate.fit Platt (calib_rows f.train), (Float.nan, Float.nan)
        else
          Calibrate.choose
            ~fit_rows:(calib_rows f.fit)
            ~validation:(calib_rows f.validation)
            ~final_rows:(calib_rows f.train)
      in
      let scored =
        List.map f.test ~f:(fun (r : Row.t) ->
          let p =
            Calibrate.predict cal ~horizon:r.features.horizon ~ask:r.features.no_ask
          in
          { row = r; month = f.month; p; residual = (if r.no_won then 1. else 0.) -. p })
      in
      let y = Array.of_list (List.map scored ~f:(fun s -> s.row.no_won)) in
      let loss p = Logistic.log_loss ~p:(Array.of_list p) ~y in
      Some
        ( scored
        , { Explain.Fold.month = f.month
          ; n_train = List.length f.train
          ; n_test = List.length f.test
          ; events_test =
              List.map f.test ~f:(fun r -> r.event_ticker)
              |> List.dedup_and_sort ~compare:String.compare
              |> List.length
          ; baseline = Calibrate.kind_to_string cal.kind
          ; validation_platt = vp
          ; validation_isotonic = vi
          ; test_raw =
              loss
                (List.map scored ~f:(fun s ->
                   Calibrate.clip (s.row.features.no_ask /. 100.)))
          ; test_baseline = loss (List.map scored ~f:(fun s -> s.p))
          } )))
  |> List.unzip
  |> Tuple2.map_fst ~f:List.concat
;;

(* ---- features and their levels --------------------------------------------------- *)

type feature =
  { info : Explain.Feature.t
  ; level : F.t -> (int * string) option
  ; value : F.t -> float option (** for wins vs losses *)
  }

let bin edges labels x =
  (* [edges] ascending upper bounds; the last label takes everything above. *)
  let rec go i = function
    | [] -> i, List.nth_exn labels i
    | e :: rest -> if Float.( <= ) x e then i, List.nth_exn labels i else go (i + 1) rest
  in
  go 0 edges
;;

let move_level m =
  Some
    (match m with
     | None -> 0, "no trade before"
     | Some m when Float.( <= ) m (-5.) -> 1, "fell 5c+"
     | Some m when Float.( < ) m (-0.5) -> 2, "fell 1-4c"
     | Some m when Float.( <= ) m 0.5 -> 3, "flat"
     | Some m when Float.( < ) m 5. -> 4, "rose 1-4c"
     | Some _ -> 5, "rose 5c+")
;;

let kind (f : F.t) =
  if f.event_markets <= 1
  then 0, "single market"
  else if f.mutually_exclusive
  then 1, "bracket (one winner)"
  else 2, "other multi-market"
;;

let features : feature list =
  let info id name description ~numeric =
    { Explain.Feature.id; name; description; numeric }
  in
  [ { info =
        info
          "price"
          "NO ask"
          "Price bucket. After recalibration the residual should be near zero in every \
           bucket: a check, not a factor."
          ~numeric:false
    ; level =
        (fun f ->
          let b = Int.min 9 (Float.iround_down_exn (f.no_ask /. 10.)) in
          Some (b, sprintf "%d-%d¢" (b * 10) ((b + 1) * 10)))
    ; value = (fun _ -> None)
    }
  ; { info = info "spread" "Spread" "YES ask minus YES bid at the decision." ~numeric:true
    ; level =
        (fun f ->
          Some
            (match f.spread with
             | None -> 5, "one-sided"
             | Some s ->
               bin [ 1.; 2.; 5.; 10. ] [ "1¢"; "2¢"; "3-5¢"; "6-10¢"; "> 10¢" ] s))
    ; value = (fun f -> f.spread)
    }
  ; { info = info "book" "Book" "Whether both sides were quoted." ~numeric:true
    ; level = (fun f -> Some (if F.one_sided f then 1, "one-sided" else 0, "two-sided"))
    ; value = (fun f -> Some (if F.one_sided f then 1. else 0.))
    }
  ; { info =
        info
          "volume"
          "Volume to date"
          "Contracts traded before the decision (log10 in the win/loss table)."
          ~numeric:true
    ; level =
        (fun f ->
          Some
            (bin
               [ 1_000.; 3_000.; 10_000.; 30_000.; 100_000. ]
               [ "500-1k"; "1k-3k"; "3k-10k"; "10k-30k"; "30k-100k"; "100k+" ]
               f.volume_to_date))
    ; value = (fun f -> Some (Float.log10 f.volume_to_date))
    }
  ; { info =
        info
          "trades"
          "Trades to date"
          "Number of fills before the decision (log10 in the win/loss table)."
          ~numeric:true
    ; level =
        (fun f ->
          Some
            (bin
               [ 24.; 99.; 399.; 1599. ]
               [ "< 25"; "25-99"; "100-399"; "400-1,599"; "1,600+" ]
               (Float.of_int f.trades_to_date)))
    ; value = (fun f -> Some (Float.log10 (Float.of_int (Int.max 1 f.trades_to_date))))
    }
  ; { info =
        info
          "move_1h"
          "NO move, 1h"
          "Change in the last NO trade price over the hour before."
          ~numeric:true
    ; level = (fun f -> move_level f.move_1h)
    ; value = (fun f -> f.move_1h)
    }
  ; { info =
        info
          "move_6h"
          "NO move, 6h"
          "Change in the last NO trade price over the 6 hours before."
          ~numeric:true
    ; level = (fun f -> move_level f.move_6h)
    ; value = (fun f -> f.move_6h)
    }
  ; { info =
        info
          "move_24h"
          "NO move, 24h"
          "Change in the last NO trade price over the 24 hours before."
          ~numeric:true
    ; level = (fun f -> move_level f.move_24h)
    ; value = (fun f -> f.move_24h)
    }
  ; { info =
        info
          "time"
          "Time to scheduled close"
          "Hours from the decision to the expected expiration."
          ~numeric:false
    ; level =
        (fun f ->
          Some
            (match f.horizon with
             | H24 -> 0, "24h"
             | H6 -> 1, "6h"
             | H1 -> 2, "1h"))
    ; value = (fun _ -> None)
    }
  ; { info = info "category" "Category" "Kalshi's series category." ~numeric:false
    ; level = (fun f -> Some (0, f.category))
    ; value = (fun _ -> None)
    }
  ; { info =
        info
          "kind"
          "Bracket vs binary"
          "Single market, a mutually exclusive bracket, or another multi-market event."
          ~numeric:false
    ; level = (fun f -> Some (kind f))
    ; value = (fun _ -> None)
    }
  ; { info =
        info
          "event_markets"
          "Markets in the event"
          "How many markets the event had at the decision (log10 in the win/loss table)."
          ~numeric:true
    ; level =
        (fun f ->
          Some
            (bin
               [ 1.; 2.; 5.; 10.; 30. ]
               [ "1"; "2"; "3-5"; "6-10"; "11-30"; "31+" ]
               (Float.of_int f.event_markets)))
    ; value = (fun f -> Some (Float.log10 (Float.of_int (Int.max 1 f.event_markets))))
    }
  ]
;;

(* ---- statistics ------------------------------------------------------------------- *)

let clusters_of (xs : scored list) =
  let t = String.Table.create () in
  List.iter xs ~f:(fun s ->
    Hashtbl.update t s.row.event_ticker ~f:(fun prev ->
      let num, den = Option.value prev ~default:(0., 0.) in
      num +. s.residual, den +. 1.));
  Hashtbl.data t |> List.map ~f:(fun (num, den) -> { Stats.num; den }) |> Array.of_list
;;

let mean_residual xs =
  List.sum (module Float) xs ~f:(fun s -> s.residual) /. Float.of_int (List.length xs)
;;

let rng config key k = Random.State.make [| config.seed; String.hash key; k |]

let level_stats config ~feature ~level:(order, level) (xs : scored list) : Explain.Level.t
  =
  let key = feature ^ ": " ^ level in
  let mean = mean_residual xs in
  let ci =
    Stats.cluster_bootstrap
      ~rng:(rng config key 0)
      ~resamples:config.resamples
      (clusters_of xs)
  in
  let by_month =
    List.map xs ~f:(fun s -> s.month, s)
    |> String.Map.of_alist_multi
    |> Map.to_alist
    |> List.filter ~f:(fun (_, ys) -> List.length ys >= config.min_fold_rows)
  in
  let folds =
    List.map by_month ~f:(fun (month, ys) ->
      { Explain.Fold_value.month; n = List.length ys; mean = mean_residual ys })
  in
  let folds_agree =
    List.count folds ~f:(fun f ->
      Sign_or_nan.equal (Float.sign_or_nan f.mean) (Float.sign_or_nan mean))
  in
  let excludes_zero =
    Option.value_map ci ~default:false ~f:(fun c -> Ci.above_zero c || Ci.below_zero c)
  in
  let single_fold =
    excludes_zero
    && (List.length folds <= 1
        || List.exists by_month ~f:(fun (month, _) ->
          let rest = List.filter xs ~f:(fun s -> not (String.equal s.month month)) in
          match
            Stats.cluster_bootstrap
              ~rng:(rng config (key ^ month) 1)
              ~resamples:config.resamples
              (clusters_of rest)
          with
          | Some c -> not (Ci.above_zero c || Ci.below_zero c)
          | None -> true))
  in
  { feature
  ; level
  ; order
  ; n = List.length xs
  ; n_events =
      List.map xs ~f:(fun s -> s.row.event_ticker)
      |> List.dedup_and_sort ~compare:String.compare
      |> List.length
  ; mean_price_p =
      List.sum (module Float) xs ~f:(fun s -> s.p) /. Float.of_int (List.length xs)
  ; mean_residual = mean
  ; ci
  ; folds
  ; folds_agree
  ; single_fold
  }
;;

(* Percentile bootstrap of [stat] over sums of per-event vectors. *)
let bootstrap_vector ~rng ~resamples (clusters : float array array) ~stat =
  let k = Array.length clusters in
  if k = 0
  then None
  else (
    let d = Array.length clusters.(0) in
    let stats =
      Array.init resamples ~f:(fun _ ->
        let sum = Array.create ~len:d 0. in
        for _ = 1 to k do
          let c = clusters.(Random.State.int rng k) in
          Array.iteri c ~f:(fun j v -> sum.(j) <- sum.(j) +. v)
        done;
        stat sum)
      |> Array.filter_map ~f:Fn.id
    in
    if Array.length stats < resamples / 2
    then None
    else (
      Array.sort stats ~compare:Float.compare;
      Some { Ci.lo = Stats.percentile stats 0.025; hi = Stats.percentile stats 0.975 }))
;;

let min_win_loss = 20

let win_loss config (rows : Row.t list) : Explain.Win_loss.t list =
  List.concat_map features ~f:(fun feat ->
    if not feat.info.numeric
    then []
    else
      List.filter_map (List.range 0 10) ~f:(fun b ->
        let lo = Float.of_int (b * 10) in
        let hi = lo +. 10. in
        let in_bucket =
          List.filter_map rows ~f:(fun r ->
            if Float.( >= ) r.features.no_ask lo && Float.( < ) r.features.no_ask hi
            then Option.map (feat.value r.features) ~f:(fun v -> r, v)
            else None)
        in
        let wins = List.filter in_bucket ~f:(fun (r, _) -> r.no_won) in
        let losses = List.filter in_bucket ~f:(fun (r, _) -> not r.no_won) in
        if List.length wins < min_win_loss || List.length losses < min_win_loss
        then None
        else (
          let mean xs =
            List.sum (module Float) xs ~f:snd /. Float.of_int (List.length xs)
          in
          (* Per event: [sum over wins; wins; sum over losses; losses]. *)
          let t = String.Table.create () in
          List.iter in_bucket ~f:(fun ((r : Row.t), v) ->
            Hashtbl.update t r.event_ticker ~f:(fun prev ->
              let a = Option.value prev ~default:[| 0.; 0.; 0.; 0. |] in
              if r.no_won
              then [| a.(0) +. v; a.(1) +. 1.; a.(2); a.(3) |]
              else [| a.(0); a.(1); a.(2) +. v; a.(3) +. 1. |]));
          let stat s =
            if Float.( > ) s.(1) 0. && Float.( > ) s.(3) 0.
            then Some ((s.(0) /. s.(1)) -. (s.(2) /. s.(3)))
            else None
          in
          let mean_win = mean wins
          and mean_loss = mean losses in
          Some
            { Explain.Win_loss.feature = feat.info.id
            ; lo
            ; hi
            ; n_win = List.length wins
            ; n_loss = List.length losses
            ; mean_win
            ; mean_loss
            ; diff = mean_win -. mean_loss
            ; ci =
                bootstrap_vector
                  ~rng:(rng config (sprintf "%s-%d" feat.info.id b) 2)
                  ~resamples:config.resamples
                  (Array.of_list (Hashtbl.data t))
                  ~stat
            })))
;;

(* Factors whose pooled CI excludes zero, ranked by how many test months agree with the
   pooled sign, then by size relative to the CI. The price feature is a check, not a
   factor. *)
let ranking (levels : Explain.Level.t list) =
  List.filter levels ~f:(fun l ->
    (not (String.equal l.feature "price"))
    && Option.value_map l.ci ~default:false ~f:(fun c ->
      Ci.above_zero c || Ci.below_zero c))
  |> List.sort ~compare:(fun (a : Explain.Level.t) b ->
    let share (l : Explain.Level.t) =
      Float.of_int l.folds_agree /. Float.of_int (Int.max 1 (List.length l.folds))
    in
    let z (l : Explain.Level.t) =
      match l.ci with
      | Some c -> Float.abs l.mean_residual /. Float.max 1e-9 ((c.hi -. c.lo) /. 2.)
      | None -> 0.
    in
    match Bool.compare a.single_fold b.single_fold with
    | 0 ->
      (match Float.compare (share b) (share a) with
       | 0 -> Float.compare (z b) (z a)
       | c -> c)
    | c -> c)
  |> List.map ~f:Explain.Level.key
;;

let build
      ?(config = default_config)
      ~generated_at
      ~synthetic
      ~data_since
      ~data_until
      ~exclusions
      ~leakage_checked
      ~leakage_failures
      (rows : Row.t list)
  : Explain.t
  =
  let scored, fold_summaries = score config rows in
  let levels =
    List.concat_map features ~f:(fun feat ->
      List.filter_map scored ~f:(fun s ->
        Option.map (feat.level s.row.features) ~f:(fun lv -> lv, s))
      |> List.Assoc.sort_and_group ~compare:[%compare: int * string]
      |> List.filter ~f:(fun (_, xs) -> List.length xs >= config.min_fold_rows)
      |> List.map ~f:(fun (lv, xs) ->
        level_stats config ~feature:feat.info.id ~level:lv xs))
  in
  let distinct f =
    List.map rows ~f |> List.dedup_and_sort ~compare:String.compare |> List.length
  in
  { generated_at
  ; synthetic
  ; data_since
  ; data_until
  ; anchor = Anchor.to_string Expected
  ; min_volume = Pit.min_volume
  ; embargo_days = config.embargo_days
  ; validation_months = config.validation_months
  ; min_fold_rows = config.min_fold_rows
  ; rows = List.length rows
  ; markets = distinct (fun r -> r.ticker)
  ; events = distinct (fun r -> r.event_ticker)
  ; test_rows = List.length scored
  ; leakage_checked
  ; leakage_failures
  ; exclusions
  ; folds = fold_summaries
  ; features = List.map features ~f:(fun f -> f.info)
  ; levels
  ; ranking = ranking levels
  ; win_loss = win_loss config rows
  ; notes =
      [ "Residual = 1 if NO won, minus the recalibrated price's P(NO), computed out of \
         sample: each test month's baseline is chosen (Platt vs isotonic) on validation \
         months and fit on earlier months only."
      ; "Test months from 2026-05 on were already looked at in the study (Phases 3 to \
         6), so they are not a clean holdout. Markets settled from 2026-11-17 are \
         reserved for forward testing and are not in this data."
      ]
  }
;;
