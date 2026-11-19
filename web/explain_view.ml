open! Core
open Bonsai_web.Cont
open Fade_types
module Svg = Virtual_dom_svg

(* The Explain tab: out-of-sample residuals (1 if NO won, minus the recalibrated
   price's P(NO)) by feature level, with event-clustered CIs and how many test months
   agree. A pure function of the report and the selected feature. *)

let cls = Vdom.Attr.class_
let attr = Vdom.Attr.create
let pts x = sprintf "%+.1f" (100. *. x)

let excludes_zero (l : Explain.Level.t) =
  Option.value_map l.ci ~default:false ~f:(fun c -> Ci.above_zero c || Ci.below_zero c)
;;

let tone (l : Explain.Level.t) =
  match l.ci with
  | Some c when Ci.above_zero c -> "pos"
  | Some c when Ci.below_zero c -> "neg"
  | _ -> ""
;;

let levels_of (e : Explain.t) feature =
  List.filter e.levels ~f:(fun l -> String.equal l.feature feature)
  |> List.sort ~compare:(fun (a : Explain.Level.t) b ->
    match Int.compare a.order b.order with
    | 0 -> Int.compare b.n a.n
    | c -> c)
;;

(* One column per level: a dot at the mean residual, a whisker for the CI. *)
let chart (levels : Explain.Level.t list) =
  let width = 1100.
  and height = 300.
  and left = 64.
  and right = 20.
  and top = 16.
  and bottom = 64. in
  let ends =
    List.concat_map levels ~f:(fun l ->
      match l.ci with
      | Some c -> [ c.lo; c.hi ]
      | None -> [ l.mean_residual ])
  in
  let step = 0.02 in
  let lo =
    Float.round_down (Float.min (-0.04) (List.fold ends ~init:0. ~f:Float.min) /. step)
    *. step
  in
  let hi =
    Float.round_up (Float.max 0.04 (List.fold ends ~init:0. ~f:Float.max) /. step) *. step
  in
  let n = Int.max 1 (List.length levels) in
  let col_w = (width -. left -. right) /. Float.of_int n in
  let x i = left +. (col_w *. (Float.of_int i +. 0.5)) in
  let y v = top +. ((hi -. v) /. (hi -. lo) *. (height -. top -. bottom)) in
  let text ?(attrs = []) ~x ~y s =
    Svg.Node.text ~attrs:([ Svg.Attr.x x; Svg.Attr.y y ] @ attrs) [ Vdom.Node.text s ]
  in
  let hline ?(attrs = []) v =
    Svg.Node.line
      ~attrs:
        ([ Svg.Attr.x1 left
         ; Svg.Attr.x2 (width -. right)
         ; Svg.Attr.y1 (y v)
         ; Svg.Attr.y2 (y v)
         ]
         @ attrs)
      []
  in
  let n_ticks = Float.iround_nearest_exn ((hi -. lo) /. step) in
  let every = if n_ticks > 10 then 2 else 1 in
  let grid =
    List.init (n_ticks + 1) ~f:(fun i -> lo +. (Float.of_int i *. step))
    |> List.filteri ~f:(fun i _ -> i mod every = 0)
    |> List.concat_map ~f:(fun t ->
      [ hline ~attrs:[ cls "grid" ] t
      ; text
          ~attrs:[ cls "tick"; attr "text-anchor" "end" ]
          ~x:(left -. 8.)
          ~y:(y t +. 4.)
          (pts t)
      ])
  in
  let column i (l : Explain.Level.t) =
    let cx = x i in
    let mark =
      match l.ci with
      | Some c ->
        [ Svg.Node.line
            ~attrs:
              [ cls "fp-whisker"
              ; Svg.Attr.x1 cx
              ; Svg.Attr.x2 cx
              ; Svg.Attr.y1 (y c.lo)
              ; Svg.Attr.y2 (y c.hi)
              ]
            []
        ; Svg.Node.circle
            ~attrs:
              [ cls "fp-dot"
              ; Svg.Attr.cx cx
              ; Svg.Attr.cy (y l.mean_residual)
              ; Svg.Attr.r 5.
              ]
            []
        ]
      | None ->
        [ Svg.Node.circle
            ~attrs:
              [ cls "fp-dot thin"
              ; Svg.Attr.cx cx
              ; Svg.Attr.cy (y l.mean_residual)
              ; Svg.Attr.r 5.
              ]
            []
        ]
    in
    Svg.Node.g
      ~attrs:[ cls ("fp-row fwd " ^ tone l) ]
      (Svg.Node.title
         [ Vdom.Node.textf
             "%s: residual %s points (%s), n = %d in %d events, %d of %d months agree"
             l.level
             (pts l.mean_residual)
             (match l.ci with
              | Some c -> sprintf "%s to %s" (pts c.lo) (pts c.hi)
              | None -> "no CI")
             l.n
             l.n_events
             l.folds_agree
             (List.length l.folds)
         ]
       :: text
            ~attrs:[ cls "lvl-label"; attr "text-anchor" "middle" ]
            ~x:cx
            ~y:(height -. bottom +. 18.)
            l.level
       :: text
            ~attrs:[ cls "lvl-sub"; attr "text-anchor" "middle" ]
            ~x:cx
            ~y:(height -. bottom +. 34.)
            (sprintf
               "%d/%d mo%s"
               l.folds_agree
               (List.length l.folds)
               (if l.single_fold then " *" else ""))
       :: mark)
  in
  Vdom.Node.div
    ~attrs:[ cls "chart" ]
    [ Svg.Node.svg
        ~attrs:
          [ Svg.Attr.viewbox ~min_x:0. ~min_y:0. ~width ~height
          ; attr "role" "img"
          ; attr "aria-label" "Mean residual by level with 95% intervals"
          ]
        (grid
         @ [ hline ~attrs:[ cls "zero" ] 0.
           ; text
               ~attrs:
                 [ cls "axis-title"
                 ; attr "text-anchor" "middle"
                 ; attr
                     "transform"
                     (sprintf "rotate(-90 14 %f)" ((top +. height -. bottom) /. 2.))
                 ]
               ~x:14.
               ~y:((top +. height -. bottom) /. 2.)
               "residual, points"
           ]
         @ List.mapi levels ~f:column)
    ]
;;

let num s = Vdom.Node.td ~attrs:[ cls "num" ] [ Vdom.Node.text s ]
let th s = Vdom.Node.th [ Vdom.Node.text s ]

let win_loss_table (e : Explain.t) feature =
  let rows = List.filter e.win_loss ~f:(fun w -> String.equal w.feature feature) in
  if List.is_empty rows
  then Vdom.Node.none
  else
    Vdom.Node.section
      ~attrs:[ cls "table-section" ]
      [ Vdom.Node.h2 [ Vdom.Node.text "NO wins vs NO losses, within each price bucket" ]
      ; Vdom.Node.div
          ~attrs:[ cls "table-wrap" ]
          [ Vdom.Node.table
              [ Vdom.Node.thead
                  [ Vdom.Node.tr
                      [ th "NO ask"
                      ; th "Wins"
                      ; th "Losses"
                      ; th "Mean, wins"
                      ; th "Mean, losses"
                      ; th "Difference"
                      ; th "95% CI"
                      ]
                  ]
              ; Vdom.Node.tbody
                  (List.map rows ~f:(fun w ->
                     let tone =
                       match w.ci with
                       | Some c when Ci.above_zero c -> "pos"
                       | Some c when Ci.below_zero c -> "neg"
                       | _ -> ""
                     in
                     Vdom.Node.tr
                       ~attrs:[ cls tone ]
                       [ Vdom.Node.td
                           ~attrs:[ cls "label" ]
                           [ Vdom.Node.textf "%.0f-%.0f¢" w.lo w.hi ]
                       ; num (Fmt.int w.n_win)
                       ; num (Fmt.int w.n_loss)
                       ; num (sprintf "%.3f" w.mean_win)
                       ; num (sprintf "%.3f" w.mean_loss)
                       ; num (sprintf "%+.3f" w.diff)
                       ; num
                           (match w.ci with
                            | Some c -> sprintf "%+.3f to %+.3f" c.lo c.hi
                            | None -> "n/a")
                       ]))
              ]
          ]
      ]
;;

let ranking_table (e : Explain.t) =
  let find key =
    List.find e.levels ~f:(fun l -> String.equal (Explain.Level.key l) key)
  in
  Vdom.Node.section
    ~attrs:[ cls "table-section" ]
    [ Vdom.Node.h2 [ Vdom.Node.text "Ranked factors: pooled CI excludes zero" ]
    ; (if List.is_empty e.ranking
       then
         Vdom.Node.div
           ~attrs:[ cls "state empty" ]
           [ Vdom.Node.text "No factor's pooled residual CI excludes zero." ]
       else
         Vdom.Node.div
           ~attrs:[ cls "table-wrap" ]
           [ Vdom.Node.table
               [ Vdom.Node.thead
                   [ Vdom.Node.tr
                       [ th "#"
                       ; th "Factor"
                       ; th "n"
                       ; th "Residual"
                       ; th "95% CI"
                       ; th "Months agreeing"
                       ; th "Note"
                       ]
                   ]
               ; Vdom.Node.tbody
                   (List.filter_mapi e.ranking ~f:(fun i key ->
                      Option.map (find key) ~f:(fun l ->
                        Vdom.Node.tr
                          ~attrs:[ cls (if l.single_fold then "thin" else tone l) ]
                          [ num (Int.to_string (i + 1))
                          ; Vdom.Node.td ~attrs:[ cls "label" ] [ Vdom.Node.text key ]
                          ; num (Fmt.int l.n)
                          ; num (pts l.mean_residual)
                          ; num
                              (match l.ci with
                               | Some c -> sprintf "%s to %s" (pts c.lo) (pts c.hi)
                               | None -> "n/a")
                          ; num (sprintf "%d of %d" l.folds_agree (List.length l.folds))
                          ; Vdom.Node.td
                              [ Vdom.Node.text
                                  (if l.single_fold then "depends on one month" else "")
                              ]
                          ])))
               ]
           ])
    ]
;;

let folds_table (e : Explain.t) =
  Vdom.Node.section
    ~attrs:[ cls "table-section" ]
    [ Vdom.Node.h2 [ Vdom.Node.text "Walk-forward test months" ]
    ; Vdom.Node.div
        ~attrs:[ cls "table-wrap" ]
        [ Vdom.Node.table
            [ Vdom.Node.thead
                [ Vdom.Node.tr
                    [ th "Month"
                    ; th "Train rows"
                    ; th "Test rows"
                    ; th "Test events"
                    ; th "Baseline"
                    ; th "Log loss, raw ask"
                    ; th "Log loss, baseline"
                    ]
                ]
            ; Vdom.Node.tbody
                (List.map e.folds ~f:(fun f ->
                   Vdom.Node.tr
                     [ Vdom.Node.td ~attrs:[ cls "label" ] [ Vdom.Node.text f.month ]
                     ; num (Fmt.int f.n_train)
                     ; num (Fmt.int f.n_test)
                     ; num (Fmt.int f.events_test)
                     ; Vdom.Node.td [ Vdom.Node.text f.baseline ]
                     ; num (sprintf "%.4f" f.test_raw)
                     ; num (sprintf "%.4f" f.test_baseline)
                     ]))
            ]
        ]
    ]
;;

let view (e : Explain.t) ~selected ~select =
  let feature =
    List.find e.features ~f:(fun f -> String.equal f.id selected)
    |> Option.value ~default:(List.hd_exn e.features)
  in
  let levels = levels_of e feature.id in
  Vdom.Node.div
    [ (if e.synthetic
       then
         Vdom.Node.div
           ~attrs:[ cls "synthetic" ]
           [ Vdom.Node.text
               "SYNTHETIC DATA: generated markets with one planted effect (one-sided \
                books)."
           ]
       else Vdom.Node.none)
    ; Vdom.Node.section
        ~attrs:[ cls "verdict neutral"; Vdom.Attr.id "explain-status" ]
        [ Vdom.Node.div
            ~attrs:[ cls "verdict-tag" ]
            [ Vdom.Node.text "Explain: residuals, not winners" ]
        ; Vdom.Node.p
            ~attrs:[ cls "verdict-headline" ]
            [ Vdom.Node.textf
                "%d factor level%s with a pooled CI excluding zero, %d of them in more \
                 than half the test months"
                (List.length e.ranking)
                (if List.length e.ranking = 1 then "" else "s")
                (List.count e.ranking ~f:(fun key ->
                   match
                     List.find e.levels ~f:(fun l ->
                       String.equal (Explain.Level.key l) key)
                   with
                   | Some l ->
                     (not l.single_fold) && 2 * l.folds_agree > List.length l.folds
                   | None -> false))
            ]
        ; Vdom.Node.p
            ~attrs:[ cls "verdict-detail" ]
            [ Vdom.Node.textf
                "Residual = 1 if NO won, minus the recalibrated price's P(NO), out of \
                 sample in %d walk-forward months. %s rows from %s markets in %s events, \
                 decisions 24h, 6h and 1h before the expected expiration, volume to date \
                 >= %.0f. Leakage check: %d of %d markets changed when data after the \
                 decision was deleted."
                (List.length e.folds)
                (Fmt.int e.rows)
                (Fmt.int e.markets)
                (Fmt.int e.events)
                e.min_volume
                e.leakage_failures
                e.leakage_checked
            ]
        ]
    ; Vdom.Node.section
        ~attrs:[ cls "filters" ]
        [ Vdom.Node.div
            ~attrs:[ cls "control" ]
            [ Vdom.Node.span ~attrs:[ cls "control-label" ] [ Vdom.Node.text "Feature" ]
            ; Vdom.Node.div
                ~attrs:[ cls "segmented wrap"; attr "role" "group" ]
                (List.map e.features ~f:(fun f ->
                   let on = String.equal f.id feature.id in
                   Vdom.Node.button
                     ~attrs:
                       [ cls (if on then "seg on" else "seg")
                       ; Vdom.Attr.id ("feature-" ^ f.id)
                       ; attr "aria-pressed" (Bool.to_string on)
                       ; Vdom.Attr.on_click (fun _ -> select f.id)
                       ]
                     [ Vdom.Node.text f.name ]))
            ]
        ]
    ; Vdom.Node.p
        ~attrs:[ cls "explain" ]
        [ Vdom.Node.textf
            "%s Each column is one level: the dot is the mean residual in points (+2 \
             means NO won 2 percentage points more often than the recalibrated price \
             said), the bar its 95%% CI resampling whole events. Underneath: how many \
             test months have the same sign (* = the result depends on one month)."
            feature.description
        ]
    ; (if List.is_empty levels
       then
         Vdom.Node.div
           ~attrs:[ cls "state empty" ]
           [ Vdom.Node.textf
               "No level of this feature has %d or more test rows."
               e.min_fold_rows
           ]
       else chart levels)
    ; win_loss_table e feature.id
    ; ranking_table e
    ; folds_table e
    ; Vdom.Node.footer
        (List.map e.notes ~f:(fun n -> Vdom.Node.p [ Vdom.Node.text n ])
         @ List.map e.exclusions ~f:(fun (what, n) ->
           Vdom.Node.p [ Vdom.Node.textf "Excluded markets, %s: %s." what (Fmt.int n) ])
         @ [ Vdom.Node.p [ Vdom.Node.textf "Generated %s." e.generated_at ]
           ; Vdom.Node.p
               ~attrs:[ cls "disclaimer" ]
               [ Vdom.Node.text "Research only, not financial advice." ]
           ])
    ]
;;
