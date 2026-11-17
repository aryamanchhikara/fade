open! Core
open Bonsai_web.Cont
open Fade_types
module Svg = Virtual_dom_svg

(* The Forward test tab: the locked candidate scored on markets that settled after the
   study window. A pure function from the report to virtual DOM. *)

let cls = Vdom.Attr.class_

let tone : Forward.Status.t -> string = function
  | Supported -> "green"
  | Rejected -> "red"
  | Running | Inconclusive -> "neutral"
;;

let headline (f : Forward.t) =
  let supported =
    List.filter f.rules ~f:(fun r -> Forward.Status.equal r.status Supported)
  in
  let all_rejected =
    List.for_all f.rules ~f:(fun r -> Forward.Status.equal r.status Rejected)
  in
  let summary =
    match supported with
    | r :: _ ->
      sprintf
        "rule %s cleared zero on %s"
        r.rule.id
        (Option.value r.concluded_on ~default:"?")
    | [] when all_rejected -> "every rule lost money (CI below zero)"
    | [] when f.day >= f.config.forward_days -> "window over, no rule cleared zero"
    | [] -> "no rule has cleared zero"
  in
  if f.day = 0
  then sprintf "Starts %s" f.config.forward_start
  else sprintf "Day %d of %d: %s" f.day f.config.forward_days summary
;;

let overall_tone (f : Forward.t) =
  if List.exists f.rules ~f:(fun r -> Forward.Status.equal r.status Supported)
  then "green"
  else if List.for_all f.rules ~f:(fun r -> Forward.Status.equal r.status Rejected)
  then "red"
  else "neutral"
;;

(* ---- forest plot: each rule's 95% return interval, in-sample and forward ---------- *)

let forest (rules : Forward.Rule_report.t list) =
  let rows =
    List.concat_map rules ~f:(fun r ->
      [ r.rule.id, "in-sample", r.in_sample.result, "is"
      ; r.rule.id, "forward", r.forward.result, "fwd"
      ])
  in
  let width = 1100.
  and left = 210.
  and right = 24.
  and top = 12.
  and row_h = 30.
  and bottom = 40. in
  let height = top +. bottom +. (row_h *. Float.of_int (List.length rows)) in
  let ends =
    List.concat_map rows ~f:(fun (_, _, (b : Bucket_result.t), _) ->
      match b.return_ci with
      | Some ci -> [ ci.lo; ci.hi ]
      | None -> if b.n > 0 then [ b.mean_return ] else [])
  in
  let step = 0.05 in
  let lo =
    Float.round_down (Float.min (-0.1) (List.fold ends ~init:0. ~f:Float.min) /. step)
    *. step
  in
  let hi =
    Float.round_up (Float.max 0.1 (List.fold ends ~init:0. ~f:Float.max) /. step) *. step
  in
  let x v = left +. ((v -. lo) /. (hi -. lo) *. (width -. left -. right)) in
  let y i = top +. (row_h *. (Float.of_int i +. 0.5)) in
  let attr = Vdom.Attr.create in
  let text ?(attrs = []) ~x ~y s =
    Svg.Node.text ~attrs:([ Svg.Attr.x x; Svg.Attr.y y ] @ attrs) [ Vdom.Node.text s ]
  in
  let vline ?(attrs = []) v =
    Svg.Node.line
      ~attrs:
        ([ Svg.Attr.x1 (x v)
         ; Svg.Attr.x2 (x v)
         ; Svg.Attr.y1 top
         ; Svg.Attr.y2 (height -. bottom)
         ]
         @ attrs)
      []
  in
  let n_ticks = Float.iround_nearest_exn ((hi -. lo) /. step) in
  let tick_every = if n_ticks > 12 then 2 else 1 in
  let ticks =
    List.init (n_ticks + 1) ~f:(fun i -> lo +. (Float.of_int i *. step))
    |> List.filteri ~f:(fun i _ -> i mod tick_every = 0)
  in
  let grid =
    List.concat_map ticks ~f:(fun t ->
      [ vline ~attrs:[ cls "grid" ] t
      ; text
          ~attrs:[ cls "tick"; attr "text-anchor" "middle" ]
          ~x:(x t)
          ~y:(height -. bottom +. 16.)
          (Fmt.signed_pct ~digits:0 t)
      ])
  in
  let row i (id, sample, (b : Bucket_result.t), kind) =
    let state =
      if Bucket_result.proven_positive b
      then " pos"
      else if Bucket_result.proven_negative b
      then " neg"
      else ""
    in
    let label =
      Svg.Node.g
        [ (if String.equal kind "is"
           then text ~attrs:[ cls "fp-id" ] ~x:14. ~y:(y i +. 4.) id
           else Vdom.Node.none)
        ; text ~attrs:[ cls ("fp-label " ^ kind) ] ~x:96. ~y:(y i +. 4.) sample
        ]
    in
    let mark =
      match b.return_ci with
      | _ when b.n = 0 ->
        [ text ~attrs:[ cls "fp-note" ] ~x:(x lo +. 6.) ~y:(y i +. 4.) "no fills yet" ]
      | Some ci ->
        [ Svg.Node.line
            ~attrs:
              [ cls "fp-whisker"
              ; Svg.Attr.x1 (x ci.lo)
              ; Svg.Attr.x2 (x ci.hi)
              ; Svg.Attr.y1 (y i)
              ; Svg.Attr.y2 (y i)
              ]
            []
        ; Svg.Node.circle
            ~attrs:
              [ cls "fp-dot"
              ; Svg.Attr.cx (x b.mean_return)
              ; Svg.Attr.cy (y i)
              ; Svg.Attr.r 5.
              ]
            []
        ]
      | None ->
        [ Svg.Node.circle
            ~attrs:
              [ cls "fp-dot thin"
              ; Svg.Attr.cx (x b.mean_return)
              ; Svg.Attr.cy (y i)
              ; Svg.Attr.r 5.
              ]
            []
        ]
    in
    Svg.Node.g
      ~attrs:[ cls ("fp-row " ^ kind ^ state) ]
      (Svg.Node.title
         [ Vdom.Node.textf
             "%s %s: %s per $ (%s), %d fills in %d events"
             id
             sample
             (Fmt.signed_pct b.mean_return)
             (Fmt.return_ci b)
             b.n
             b.n_events
         ]
       :: label
       :: mark)
  in
  Vdom.Node.div
    ~attrs:[ cls "chart forest" ]
    [ Svg.Node.svg
        ~attrs:
          [ Svg.Attr.viewbox ~min_x:0. ~min_y:0. ~width ~height
          ; attr "role" "img"
          ; attr
              "aria-label"
              "Return per dollar with 95% intervals, in-sample and forward"
          ]
        (grid
         @ [ vline ~attrs:[ cls "zero" ] 0.
           ; text
               ~attrs:[ cls "axis-title"; attr "text-anchor" "middle" ]
               ~x:(x ((lo +. hi) /. 2.))
               ~y:(height -. 4.)
               "return per dollar after fees, 95% event-clustered interval"
           ]
         @ List.mapi rows ~f:row)
    ]
;;

(* ---- table ---------------------------------------------------------------------- *)

let num s = Vdom.Node.td ~attrs:[ cls "num" ] [ Vdom.Node.text s ]

let score_row ~label (s : Candidate.Score.t) =
  let b = s.result in
  let none = b.n = 0 in
  Vdom.Node.tr
    ~attrs:
      [ cls
          (if Bucket_result.proven_positive b
           then "pos"
           else if Bucket_result.proven_negative b
           then "neg"
           else "")
      ]
    [ Vdom.Node.td ~attrs:[ cls "label" ] [ Vdom.Node.text label ]
    ; num (Fmt.int s.orders)
    ; num (Fmt.int b.n)
    ; num (Fmt.int b.n_events)
    ; num (if none then "-" else Fmt.pct b.win_rate)
    ; num (if none then "-" else Fmt.pct b.breakeven)
    ; num (if none then "-" else Fmt.signed_pct b.mean_return)
    ; num (Fmt.return_ci b)
    ]
;;

let rule_rows (f : Forward.t) (r : Forward.Rule_report.t) =
  let progress =
    match r.status, r.more_events with
    | (Supported | Rejected), _ ->
      sprintf "Concluded %s." (Option.value r.concluded_on ~default:"")
    | Inconclusive, _ -> "The window ended without a conclusion."
    | Running, Some n ->
      sprintf
        "Need ~%s more events%s for the CI to exclude zero (projected from %s)."
        (Fmt.int n)
        (match r.rule.execution with
         | Taker_at_ask -> ""
         | Maker_bid _ -> " with a fill")
        r.projection
    | Running, None -> String.capitalize r.projection ^ "."
  in
  [ Vdom.Node.tr
      ~attrs:[ cls "rule-head" ]
      [ Vdom.Node.td
          ~attrs:[ Vdom.Attr.create "colspan" "8" ]
          [ Vdom.Node.span
              ~attrs:[ cls ("badge " ^ tone r.status) ]
              [ Vdom.Node.text (Forward.Status.to_string r.status) ]
          ; Vdom.Node.strong
              [ Vdom.Node.textf " %s: %s" r.rule.id (Candidate.Rule.short_name r.rule) ]
          ; Vdom.Node.div
              ~attrs:[ cls "rule-desc" ]
              [ Vdom.Node.text (Candidate.Rule.describe r.rule ^ ". " ^ progress) ]
          ]
      ]
  ; score_row ~label:(sprintf "in-sample (to %s)" f.config.in_sample_cutoff) r.in_sample
  ; score_row ~label:"forward" r.forward
  ]
;;

let table (f : Forward.t) =
  let th s = Vdom.Node.th [ Vdom.Node.text s ] in
  Vdom.Node.div
    ~attrs:[ cls "table-wrap" ]
    [ Vdom.Node.table
        [ Vdom.Node.thead
            [ Vdom.Node.tr
                [ th "Sample"
                ; th "Orders"
                ; th "Fills"
                ; th "Events"
                ; th "Win rate"
                ; th "Break-even"
                ; th "Return / $"
                ; th "Return 95% CI"
                ]
            ]
        ; Vdom.Node.tbody (List.concat_map f.rules ~f:(rule_rows f))
        ]
    ]
;;

let view (f : Forward.t) =
  let synthetic_banner =
    if f.synthetic
    then
      Vdom.Node.div
        ~attrs:[ cls "synthetic" ]
        [ Vdom.Node.text
            "SYNTHETIC DATA: the demo's out-of-time half stands in for the forward \
             window."
        ]
    else Vdom.Node.none
  in
  let status_card =
    Vdom.Node.section
      ~attrs:[ cls ("verdict " ^ overall_tone f); Vdom.Attr.id "forward-status" ]
      [ Vdom.Node.div
          ~attrs:[ cls "verdict-tag" ]
          [ Vdom.Node.text "Forward test, paper only" ]
      ; Vdom.Node.p ~attrs:[ cls "verdict-headline" ] [ Vdom.Node.text (headline f) ]
      ; Vdom.Node.p
          ~attrs:[ cls "verdict-detail" ]
          [ Vdom.Node.textf
              "The 90-95¢ NO candidate, locked before any forward data was fetched \
               (rules/candidate.json, sha256 %s). Markets settled %s to %s: %s in scope, \
               %s with a 1h quote, %s with trades. Latest settlement %s."
              (String.prefix f.candidate_hash 16)
              f.config.forward_start
              f.forward_end
              (Fmt.int f.listed_markets)
              (Fmt.int f.quoted_markets)
              (Fmt.int f.traded_markets)
              (Option.value f.last_settle ~default:"none yet")
          ]
      ]
  in
  let body =
    if f.listed_markets = 0
    then
      Vdom.Node.div
        ~attrs:[ cls "state empty" ]
        [ Vdom.Node.textf
            "No markets settled since %s have been fetched yet. Run fade forward."
            f.config.forward_start
        ]
    else
      Vdom.Node.div
        [ Vdom.Node.p
            ~attrs:[ cls "explain" ]
            [ Vdom.Node.text
                "Each row is one rule on one sample: the dot is the return per dollar \
                 after fees, the bar its 95% interval resampling whole events. A rule \
                 concludes when its forward bar clears zero on either side."
            ]
        ; forest f.rules
        ; Vdom.Node.section ~attrs:[ cls "table-section" ] [ table f ]
        ]
  in
  Vdom.Node.div
    [ synthetic_banner
    ; status_card
    ; body
    ; Vdom.Node.footer
        [ Vdom.Node.p [ Vdom.Node.text ("Stop rule: " ^ f.config.stop_rule) ]
        ; Vdom.Node.p
            [ Vdom.Node.textf
                "Horizons count back from the scheduled close. Volume >= %.0f. Generated \
                 %s."
                f.config.min_volume
                f.generated_at
            ]
        ; Vdom.Node.p
            ~attrs:[ cls "disclaimer" ]
            [ Vdom.Node.text "Paper trading only. Research only, not financial advice." ]
        ]
    ]
;;
