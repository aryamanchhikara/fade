open! Core
open Bonsai_web.Cont
open Fade_types
open Js_of_ocaml

let cls = Vdom.Attr.class_

let columns =
  [ "Bucket"
  ; "n"
  ; "Events"
  ; "Win rate"
  ; "Win 95% CI"
  ; "Break-even"
  ; "Fee"
  ; "Return / $"
  ; "Return 95% CI"
  ]
;;

let row_cells (b : Bucket_result.t) =
  [ Bucket_result.label b
  ; Fmt.int b.n
  ; Fmt.int b.n_events
  ; Fmt.pct b.win_rate
  ; Fmt.ci_pct b.win_ci
  ; Fmt.pct b.breakeven
  ; Fmt.fee b.avg_fee
  ; Fmt.signed_pct b.mean_return
  ; Fmt.return_ci b
  ]
;;

let view (buckets : Bucket_result.t list) =
  let th = List.map columns ~f:(fun c -> Vdom.Node.th [ Vdom.Node.text c ]) in
  let rows =
    List.map buckets ~f:(fun b ->
      let state =
        if not b.enough
        then "thin"
        else if Bucket_result.proven_positive b
        then "pos"
        else ""
      in
      Vdom.Node.tr
        ~attrs:[ cls state ]
        (List.mapi (row_cells b) ~f:(fun i c ->
           Vdom.Node.td
             ~attrs:[ cls (if i = 0 then "label" else "num") ]
             [ Vdom.Node.text c ])))
  in
  Vdom.Node.div
    ~attrs:[ cls "table-wrap" ]
    [ Vdom.Node.table [ Vdom.Node.thead [ Vdom.Node.tr th ]; Vdom.Node.tbody rows ] ]
;;

(* CSV of the filtered table, with a header describing the filters. *)
let csv ~description (buckets : Bucket_result.t list) =
  let quote s =
    if String.exists s ~f:(fun c -> Char.equal c ',' || Char.equal c '"')
    then "\"" ^ String.substr_replace_all s ~pattern:"\"" ~with_:"\"\"" ^ "\""
    else s
  in
  let header =
    [ "bucket_lo_cents"
    ; "bucket_hi_cents"
    ; "n"
    ; "events"
    ; "contracts"
    ; "win_rate"
    ; "win_ci_lo"
    ; "win_ci_hi"
    ; "avg_price_cents"
    ; "avg_fee_cents"
    ; "breakeven"
    ; "return_per_dollar"
    ; "return_ci_lo"
    ; "return_ci_hi"
    ; "meets_min_n"
    ]
  in
  let f = sprintf "%.6g" in
  let line (b : Bucket_result.t) =
    [ f b.lo
    ; f b.hi
    ; Int.to_string b.n
    ; Int.to_string b.n_events
    ; f b.contracts
    ; f b.win_rate
    ; f b.win_ci.lo
    ; f b.win_ci.hi
    ; f b.avg_price
    ; f b.avg_fee
    ; f b.breakeven
    ; f b.mean_return
    ; Option.value_map b.return_ci ~default:"" ~f:(fun c -> f c.lo)
    ; Option.value_map b.return_ci ~default:"" ~f:(fun c -> f c.hi)
    ; Bool.to_string b.enough
    ]
  in
  String.concat
    ~sep:"\n"
    (("# " ^ description)
     :: String.concat ~sep:"," header
     :: List.map buckets ~f:(fun b -> String.concat ~sep:"," (List.map (line b) ~f:quote))
    )
  ^ "\n"
;;

(* Trigger a download by clicking a temporary <a download> with a data: URL. This is a
   side effect, so it runs inside [Effect.of_thunk] when the button is clicked. *)
let download ~filename ~contents =
  Effect.of_thunk (fun () ->
    let a = Dom_html.createA Dom_html.document in
    a##.href
    := Js.string
         ("data:text/csv;charset=utf-8,"
          ^ Js.to_string (Js.encodeURIComponent (Js.string contents)));
    a##setAttribute (Js.string "download") (Js.string filename);
    Dom.appendChild Dom_html.document##.body a;
    a##click;
    Dom.removeChild Dom_html.document##.body a)
;;
