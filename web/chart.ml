open! Core
open Bonsai_web.Cont
open Fade_types
module Svg = Virtual_dom_svg

(* A hand-built SVG calibration chart.
   x: price paid (cents). y: how often that side actually won.
   - diagonal: a fair price (win rate equals price);
   - dashed accent line: break-even after fees (price + fee);
   - one point per 5c bucket with a Wilson 95% whisker; grey when n < min n.
   Points above the dashed line made money after fees. *)

type size =
  { width : float
  ; height : float
  ; margin_left : float
  ; margin_right : float
  ; margin_top : float
  ; margin_bottom : float
  ; tick_labels : bool
  }

let full =
  { width = 640.
  ; height = 460.
  ; margin_left = 56.
  ; margin_right = 18.
  ; margin_top = 16.
  ; margin_bottom = 48.
  ; tick_labels = true
  }
;;

let small =
  { width = 340.
  ; height = 300.
  ; margin_left = 44.
  ; margin_right = 22.
  ; margin_top = 10.
  ; margin_bottom = 34.
  ; tick_labels = true
  }
;;

let cls = Vdom.Attr.class_
let attr = Vdom.Attr.create
let plot_w s = s.width -. s.margin_left -. s.margin_right
let plot_h s = s.height -. s.margin_top -. s.margin_bottom

(* Map cents (0..100) and rates (0..1) to SVG coordinates. *)
let sx s price = s.margin_left +. (price /. 100. *. plot_w s)
let sy s rate = s.margin_top +. ((1. -. rate) *. plot_h s)

let line ?(attrs = []) s ~x1 ~y1 ~x2 ~y2 =
  Svg.Node.line
    ~attrs:
      ([ Svg.Attr.x1 (sx s x1)
       ; Svg.Attr.y1 (sy s y1)
       ; Svg.Attr.x2 (sx s x2)
       ; Svg.Attr.y2 (sy s y2)
       ]
       @ attrs)
    []
;;

let text ?(attrs = []) ~x ~y str =
  Svg.Node.text ~attrs:([ Svg.Attr.x x; Svg.Attr.y y ] @ attrs) [ Vdom.Node.text str ]
;;

let axes s =
  let ticks = [ 0.; 25.; 50.; 75.; 100. ] in
  let grid =
    List.concat_map ticks ~f:(fun t ->
      [ line s ~attrs:[ cls "grid" ] ~x1:t ~y1:0. ~x2:t ~y2:1.
      ; line s ~attrs:[ cls "grid" ] ~x1:0. ~y1:(t /. 100.) ~x2:100. ~y2:(t /. 100.)
      ])
  in
  let labels =
    if not s.tick_labels
    then []
    else
      List.concat_map ticks ~f:(fun t ->
        [ text
            ~attrs:[ cls "tick"; attr "text-anchor" "middle" ]
            ~x:(sx s t)
            ~y:(s.height -. s.margin_bottom +. 16.)
            (sprintf "%.0f¢" t)
        ; text
            ~attrs:[ cls "tick"; attr "text-anchor" "end" ]
            ~x:(s.margin_left -. 6.)
            ~y:(sy s (t /. 100.) +. 4.)
            (sprintf "%.0f%%" t)
        ])
  in
  let titles =
    [ text
        ~attrs:[ cls "axis-title"; attr "text-anchor" "middle" ]
        ~x:(s.margin_left +. (plot_w s /. 2.))
        ~y:(s.height -. 6.)
        "price paid"
    ; text
        ~attrs:
          [ cls "axis-title"
          ; attr "text-anchor" "middle"
          ; attr
              "transform"
              (sprintf "rotate(-90 %.1f %.1f)" 12. (s.margin_top +. (plot_h s /. 2.)))
          ]
        ~x:12.
        ~y:(s.margin_top +. (plot_h s /. 2.))
        "actual win rate"
    ]
  in
  grid @ labels @ titles
;;

let tooltip s ~(min_n : int) (b : Bucket_result.t) =
  let lines =
    [ sprintf
        "%s  n = %s in %s events"
        (Bucket_result.label b)
        (Fmt.int b.n)
        (Fmt.int b.n_events)
    ; sprintf "win rate %s (%s)" (Fmt.pct b.win_rate) (Fmt.ci_pct b.win_ci)
    ; sprintf "break-even %s incl. %s fee" (Fmt.pct b.breakeven) (Fmt.fee b.avg_fee)
    ; (if b.enough
       then sprintf "return %s (%s)" (Fmt.signed_pct b.mean_return) (Fmt.return_ci b)
       else sprintf "fewer than %d markets: not scored" min_n)
    ]
  in
  let box_w = 250. in
  let box_h = (16. *. Float.of_int (List.length lines)) +. 12. in
  let px = sx s b.avg_price in
  let py = sy s b.win_rate in
  (* Keep the box inside the plot. *)
  let x =
    if Float.( > ) (px +. 14. +. box_w) s.width then px -. 14. -. box_w else px +. 14.
  in
  let y =
    Float.max
      s.margin_top
      (Float.min (py -. (box_h /. 2.)) (s.height -. s.margin_bottom -. box_h))
  in
  Svg.Node.g
    ~attrs:[ cls "tooltip" ]
    (Svg.Node.rect
       ~attrs:
         [ Svg.Attr.x x
         ; Svg.Attr.y y
         ; Svg.Attr.width box_w
         ; Svg.Attr.height box_h
         ; Svg.Attr.rx 6.
         ]
       []
     :: List.mapi lines ~f:(fun i l ->
       text
         ~attrs:[ cls "tooltip-text" ]
         ~x:(x +. 10.)
         ~y:(y +. 20. +. (16. *. Float.of_int i))
         l))
;;

let view
      ?(title = "")
      ~(size : size)
      ~(min_n : int)
      ~(buckets : Bucket_result.t list)
      ~(hovered : float option)
      ~(set_hovered : float option -> unit Effect.t)
      ()
  =
  let s = size in
  let fair = line s ~attrs:[ cls "fair" ] ~x1:0. ~y1:0. ~x2:100. ~y2:1. in
  let breakeven =
    match buckets with
    | [] -> []
    | _ ->
      [ Svg.Node.polyline
          ~attrs:
            [ cls "breakeven"
            ; Svg.Attr.points
                (List.map buckets ~f:(fun b -> sx s b.avg_price, sy s b.breakeven))
            ]
          []
      ]
  in
  let points =
    List.map buckets ~f:(fun b ->
      let state =
        if not b.enough
        then "thin"
        else if Bucket_result.proven_positive b
        then "pos"
        else "ok"
      in
      let hover_attrs =
        [ Vdom.Attr.on_mouseenter (fun _ -> set_hovered (Some b.lo))
        ; Vdom.Attr.on_mouseleave (fun _ -> set_hovered None)
        ]
      in
      Svg.Node.g
        ~attrs:(cls ("bucket " ^ state) :: hover_attrs)
        [ line
            s
            ~attrs:[ cls "whisker" ]
            ~x1:b.avg_price
            ~y1:b.win_ci.lo
            ~x2:b.avg_price
            ~y2:b.win_ci.hi
        ; Svg.Node.circle
            ~attrs:
              [ Svg.Attr.cx (sx s b.avg_price)
              ; Svg.Attr.cy (sy s b.win_rate)
              ; Svg.Attr.r (if Float.( > ) s.width 400. then 5. else 4.)
              ]
            []
          (* An invisible, larger target so small points are easy to hover. *)
        ; Svg.Node.circle
            ~attrs:
              [ cls "hit"
              ; Svg.Attr.cx (sx s b.avg_price)
              ; Svg.Attr.cy (sy s b.win_rate)
              ; Svg.Attr.r 12.
              ]
            []
        ])
  in
  let tip =
    Option.bind hovered ~f:(fun lo -> List.find buckets ~f:(fun b -> Float.equal b.lo lo))
    |> Option.map ~f:(tooltip s ~min_n)
    |> Option.to_list
  in
  Vdom.Node.div
    ~attrs:[ cls "chart" ]
    [ (if String.is_empty title
       then Vdom.Node.none
       else Vdom.Node.h3 [ Vdom.Node.text title ])
    ; Svg.Node.svg
        ~attrs:
          [ Svg.Attr.viewbox ~min_x:0. ~min_y:0. ~width:s.width ~height:s.height
          ; attr "role" "img"
          ; attr "aria-label" ("Calibration chart " ^ title)
          ]
        (axes s @ [ fair ] @ breakeven @ points @ tip)
    ]
;;

let legend =
  Vdom.Node.div
    ~attrs:[ cls "legend" ]
    [ Vdom.Node.span ~attrs:[ cls "key fair-key" ] [ Vdom.Node.text "fair price" ]
    ; Vdom.Node.span
        ~attrs:[ cls "key be-key" ]
        [ Vdom.Node.text "break-even after fees" ]
    ; Vdom.Node.span
        ~attrs:[ cls "key pt-key" ]
        [ Vdom.Node.text "bucket (95% CI whisker)" ]
    ; Vdom.Node.span
        ~attrs:[ cls "key pos-key" ]
        [ Vdom.Node.text "return CI above zero" ]
    ; Vdom.Node.span ~attrs:[ cls "key thin-key" ] [ Vdom.Node.text "below min n" ]
    ]
;;
