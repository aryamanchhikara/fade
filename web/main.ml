open! Core
open Bonsai_web.Cont
open Bonsai.Let_syntax
open Fade_types

let cls = Vdom.Attr.class_

(* ---- loading report.json ---------------------------------------------------- *)

module Load = struct
  type t =
    | Loading
    | Loaded of Report.t
    | Failed of string
end

let report_url = "data/report.json"

(* Fetch and decode report.json once, when the app starts. [Effect.of_deferred_thunk]
   wraps an Async computation as a Bonsai effect; [Bonsai.Edge.lifecycle ~on_activate]
   runs it when the component first appears. The decoding uses the same
   [Report.t_of_yojson] the native CLI used to write the file (see [Json_js] for why
   the browser's JSON.parse does the parsing). *)
let load_report (graph : Bonsai.graph) =
  let state, set_state = Bonsai.state Load.Loading graph in
  let on_activate =
    let%arr set_state = set_state in
    match%bind.Effect
      Effect.of_deferred_thunk (fun () -> Async_js.Http.get report_url)
    with
    | Error e ->
      set_state
        (Failed (sprintf "Could not fetch %s: %s" report_url (Error.to_string_hum e)))
    | Ok body ->
      (match Report.t_of_yojson (Json_js.parse body) with
       | r -> set_state (Loaded r)
       | exception Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
         set_state
           (Failed
              (sprintf
                 "%s does not match this version of the dashboard (%s). Rebuild it with \
                  the same fade binary."
                 report_url
                 (Exn.to_string exn)))
       | exception exn ->
         set_state
           (Failed (sprintf "%s could not be read: %s" report_url (Exn.to_string exn))))
  in
  Bonsai.Edge.lifecycle ~on_activate graph;
  state
;;

(* ---- filters ------------------------------------------------------------------ *)

module Filters = struct
  type t =
    { side : Side.t
    ; horizon : Horizon.t
    ; segment : string (** [Segment.to_id] *)
    ; view : View.t
    ; sample : Sample.t
    ; compare_horizons : bool
    }
  [@@deriving sexp, equal]

  let default =
    { side = No
    ; horizon = H1
    ; segment = "all"
    ; view = Quotes
    ; sample = Out_of_time
    ; compare_horizons = false
    }
  ;;

  module Action = struct
    type t =
      | Side of Side.t
      | Horizon of Horizon.t
      | Segment of string
      | View of View.t
      | Sample of Sample.t
      | Toggle_compare
    [@@deriving sexp_of]
  end

  (* A state machine: every control sends an action, applied to the current filters. *)
  let apply (t : t) : Action.t -> t = function
    | Side side -> { t with side }
    | Horizon horizon -> { t with horizon }
    | Segment segment -> { t with segment }
    | View view -> { t with view }
    | Sample sample -> { t with sample }
    | Toggle_compare -> { t with compare_horizons = not t.compare_horizons }
  ;;
end

let filters_state graph =
  Bonsai.state_machine0
    ~sexp_of_model:Filters.sexp_of_t
    ~equal:Filters.equal
    ~sexp_of_action:Filters.Action.sexp_of_t
    ~default_model:Filters.default
    ~apply_action:(fun _ctx model action -> Filters.apply model action)
    graph
;;

(* ---- small view helpers ------------------------------------------------------- *)

let segmented ~label ~(options : (string * 'a) list) ~equal ~selected ~on_select =
  Vdom.Node.div
    ~attrs:[ cls "control" ]
    [ Vdom.Node.span ~attrs:[ cls "control-label" ] [ Vdom.Node.text label ]
    ; Vdom.Node.div
        ~attrs:[ cls "segmented"; Vdom.Attr.create "role" "group" ]
        (List.map options ~f:(fun (text, value) ->
           let on = equal value selected in
           Vdom.Node.button
             ~attrs:
               [ cls (if on then "seg on" else "seg")
               ; Vdom.Attr.create "aria-pressed" (Bool.to_string on)
               ; Vdom.Attr.on_click (fun _ -> on_select value)
               ]
             [ Vdom.Node.text text ]))
    ]
;;

let segment_select ~(segments : Segment.t list) ~selected ~on_select =
  let groups =
    List.group segments ~break:(fun a b ->
      not (String.equal (Segment.group a) (Segment.group b)))
  in
  Vdom.Node.div
    ~attrs:[ cls "control" ]
    [ Vdom.Node.label
        ~attrs:[ cls "control-label"; Vdom.Attr.for_ "segment" ]
        [ Vdom.Node.text "Segment" ]
    ; Vdom.Node.select
        ~attrs:[ Vdom.Attr.id "segment"; Vdom.Attr.on_change (fun _ v -> on_select v) ]
        (List.map groups ~f:(fun group ->
           Vdom.Node.create
             "optgroup"
             ~attrs:[ Vdom.Attr.create "label" (Segment.group (List.hd_exn group)) ]
             (List.map group ~f:(fun s ->
                let id = Segment.to_id s in
                Vdom.Node.option
                  ~attrs:
                    (Vdom.Attr.value id
                     :: (if String.equal id selected then [ Vdom.Attr.selected ] else [])
                    )
                  [ Vdom.Node.text (Segment.to_string s) ]))))
    ]
;;

let verdict_view (r : Report.t) =
  let tone =
    match r.verdict.tone with
    | Green -> "green"
    | Red -> "red"
    | Neutral -> "neutral"
  in
  Vdom.Node.section
    ~attrs:[ cls ("verdict " ^ tone); Vdom.Attr.id "verdict" ]
    [ Vdom.Node.div ~attrs:[ cls "verdict-tag" ] [ Vdom.Node.text "Out-of-time verdict" ]
    ; Vdom.Node.p ~attrs:[ cls "verdict-headline" ] [ Vdom.Node.text r.verdict.headline ]
    ; Vdom.Node.p ~attrs:[ cls "verdict-detail" ] [ Vdom.Node.text r.verdict.detail ]
    ]
;;

let explain (r : Report.t) (f : Filters.t) segment_name =
  let who =
    match f.view with
    | Quotes ->
      sprintf
        "markets where %s's ask was in each 5¢ range %s, bought as a taker at that ask"
        (Side.to_string f.side)
        (Fmt.horizon_phrase f.view f.horizon)
    | Trades role ->
      sprintf
        "real fills where the %s ended up holding %s (%s), weighted by contracts"
        (Role.to_string role)
        (Side.to_string f.side)
        (Fmt.horizon_phrase f.view f.horizon)
  in
  sprintf
    "%s, %s: each point is a bucket of %s. Up is how often %s actually won. Points above \
     the dashed line made money after fees%s."
    (String.capitalize (Sample.to_string f.sample))
    segment_name
    who
    (Side.to_string f.side)
    (match r.config.anchor with
     | Schedule -> " (horizons count back from the scheduled close)"
     | Close -> "")
;;

(* ---- the app ------------------------------------------------------------------ *)

let table_index (r : Report.t) =
  let t = Hashtbl.create (module Table.Key) in
  List.iter r.tables ~f:(fun table -> Hashtbl.set t ~key:table.key ~data:table.buckets);
  t
;;

let footer (r : Report.t) =
  let data_line =
    if r.synthetic
    then
      sprintf
        "Synthetic data: %s generated markets in %s events, %s trades, settling %s to %s."
        (Fmt.int r.window.n_markets)
        (Fmt.int r.window.n_events)
        (Fmt.int r.window.n_trades)
        r.window.first_settle
        r.window.last_settle
    else
      sprintf
        "Kalshi public API: markets settled %s to %s with volume >= 500. Population %s \
         markets; uniform random sample of %s with hourly quotes and %s with trades (%s \
         markets in %s events, %s trades)."
        r.window.first_settle
        r.window.last_settle
        (Fmt.int r.window.population)
        (Fmt.int r.window.quote_sample)
        (Fmt.int r.window.trade_sample)
        (Fmt.int r.window.n_markets)
        (Fmt.int r.window.n_events)
        (Fmt.int r.window.n_trades)
  in
  Vdom.Node.footer
    [ Vdom.Node.p [ Vdom.Node.text data_line ]
    ; Vdom.Node.p
        [ Vdom.Node.textf
            "Cutoff %s: %s in-sample events, %s out-of-time, %s embargoed. %d in-sample \
             cells examined, %d rules frozen."
            r.config.cutoff
            (Fmt.int r.window.n_in_sample_events)
            (Fmt.int r.window.n_out_of_time_events)
            (Fmt.int r.window.n_embargoed_events)
            r.examined_in_sample
            (List.length r.rules)
        ]
    ; Vdom.Node.p
        [ Vdom.Node.textf
            "Fees modelled for %.0f-contract taker orders. Config sha256 %s. Generated \
             %s."
            r.config.contracts_per_order
            (String.prefix r.config_hash 16)
            r.generated_at
        ]
    ; Vdom.Node.p
        ~attrs:[ cls "disclaimer" ]
        [ Vdom.Node.text "Research only, not financial advice." ]
    ]
;;

let dashboard (graph : Bonsai.graph) =
  let load = load_report graph in
  let filters, inject = filters_state graph in
  let hovered, set_hovered = Bonsai.state (None : (Horizon.t * float) option) graph in
  (* Build the table index once per loaded report, not on every filter change: this
     [let%arr] only depends on [load], so Bonsai recomputes it only when [load] changes. *)
  let index =
    let%arr load = load in
    match load with
    | Load.Loaded r -> Some (r, table_index r)
    | Loading | Failed _ -> None
  in
  let%arr load = load
  and index = index
  and filters = filters
  and inject = inject
  and hovered = hovered
  and set_hovered = set_hovered in
  match load, index with
  | Loading, _ | Loaded _, None ->
    Vdom.Node.div ~attrs:[ cls "state loading" ] [ Vdom.Node.text "Loading report..." ]
  | Failed msg, _ ->
    Vdom.Node.div
      ~attrs:[ cls "state error" ]
      [ Vdom.Node.h2 [ Vdom.Node.text "Could not load the report" ]
      ; Vdom.Node.p [ Vdom.Node.text msg ]
      ; Vdom.Node.p
          [ Vdom.Node.text "Build one with "
          ; Vdom.Node.code [ Vdom.Node.text "fade report" ]
          ; Vdom.Node.text " (or run "
          ; Vdom.Node.code [ Vdom.Node.text "fade serve --demo" ]
          ; Vdom.Node.text " for synthetic data)."
          ]
      ]
  | Loaded _, Some (r, index) ->
    let segment =
      List.find r.segments ~f:(fun s -> String.equal (Segment.to_id s) filters.segment)
      |> Option.value ~default:Segment.All
    in
    let key horizon : Table.Key.t =
      { sample = filters.sample
      ; view = filters.view
      ; side = filters.side
      ; horizon
      ; segment
      }
    in
    let buckets horizon = Hashtbl.find index (key horizon) |> Option.value ~default:[] in
    let current = buckets filters.horizon in
    let has_enough bs = List.exists bs ~f:(fun (b : Bucket_result.t) -> b.enough) in
    let empty_note =
      Vdom.Node.div
        ~attrs:[ cls "state empty" ]
        [ Vdom.Node.textf "No buckets with n >= %d for these filters." r.config.min_n ]
    in
    let chart ~size ~title horizon =
      let bs = buckets horizon in
      if not (has_enough bs)
      then
        Vdom.Node.div
          ~attrs:[ cls "chart" ]
          [ (if String.is_empty title
             then Vdom.Node.none
             else Vdom.Node.h3 [ Vdom.Node.text title ])
          ; empty_note
          ]
      else
        Chart.view
          ~title
          ~size
          ~min_n:r.config.min_n
          ~buckets:bs
          ~hovered:
            (Option.bind hovered ~f:(fun (h, lo) ->
               Option.some_if (Horizon.equal h horizon) lo))
          ~set_hovered:(fun lo -> set_hovered (Option.map lo ~f:(fun lo -> horizon, lo)))
          ()
    in
    let charts =
      if filters.compare_horizons
      then
        Vdom.Node.div
          ~attrs:[ cls "multiples" ]
          (List.map Horizon.all ~f:(fun h ->
             chart ~size:Chart.small ~title:(Fmt.horizon_phrase filters.view h) h))
      else chart ~size:Chart.full ~title:"" filters.horizon
    in
    let description =
      sprintf
        "fade %s, %s, %s, buy %s, %s"
        (Sample.to_string filters.sample)
        (View.to_string filters.view)
        (Segment.to_string segment)
        (Side.to_string filters.side)
        (Fmt.horizon_phrase filters.view filters.horizon)
    in
    let filename =
      sprintf
        "fade-%s-%s-%s-%s-%s.csv"
        (match filters.sample with
         | In_sample -> "in-sample"
         | Out_of_time -> "out-of-time")
        (match filters.view with
         | Quotes -> "quotes"
         | Trades Taker -> "taker-trades"
         | Trades Maker -> "maker-trades")
        (String.lowercase (Side.to_string filters.side))
        (Horizon.to_string filters.horizon)
        (String.map filters.segment ~f:(fun c -> if Char.is_alphanum c then c else '-'))
    in
    let synthetic_banner =
      if r.synthetic
      then
        Vdom.Node.div
          ~attrs:[ cls "synthetic" ]
          [ Vdom.Node.text
              "SYNTHETIC DATA: generated fixtures for demo mode, not Kalshi markets."
          ]
      else Vdom.Node.none
    in
    let filters_view =
      Vdom.Node.section
        ~attrs:[ cls "filters" ]
        [ segmented
            ~label:"Side"
            ~options:[ "NO", Side.No; "YES", Yes ]
            ~equal:Side.equal
            ~selected:filters.side
            ~on_select:(fun s -> inject (Side s))
        ; segmented
            ~label:"Horizon"
            ~options:(List.map Horizon.all ~f:(fun h -> Horizon.to_string h, h))
            ~equal:Horizon.equal
            ~selected:filters.horizon
            ~on_select:(fun h -> inject (Horizon h))
        ; segmented
            ~label:"Who"
            ~options:
              [ "Taker at ask", View.Quotes
              ; "Takers (trades)", Trades Taker
              ; "Makers (trades)", Trades Maker
              ]
            ~equal:View.equal
            ~selected:filters.view
            ~on_select:(fun v -> inject (View v))
        ; segmented
            ~label:"Sample"
            ~options:[ "Out-of-time", Sample.Out_of_time; "In-sample", In_sample ]
            ~equal:Sample.equal
            ~selected:filters.sample
            ~on_select:(fun s -> inject (Sample s))
        ; segment_select
            ~segments:r.segments
            ~selected:filters.segment
            ~on_select:(fun s -> inject (Segment s))
        ; Vdom.Node.div
            ~attrs:[ cls "control" ]
            [ Vdom.Node.span ~attrs:[ cls "control-label" ] [ Vdom.Node.text "Layout" ]
            ; Vdom.Node.button
                ~attrs:
                  [ cls (if filters.compare_horizons then "seg on solo" else "seg solo")
                  ; Vdom.Attr.id "compare"
                  ; Vdom.Attr.create
                      "aria-pressed"
                      (Bool.to_string filters.compare_horizons)
                  ; Vdom.Attr.on_click (fun _ -> inject Toggle_compare)
                  ]
                [ Vdom.Node.text "Compare 24h / 6h / 1h" ]
            ]
        ]
    in
    Vdom.Node.div
      ~attrs:[ cls "app" ]
      [ synthetic_banner
      ; Vdom.Node.header
          [ Vdom.Node.h1 [ Vdom.Node.text "fade" ]
          ; Vdom.Node.p
              ~attrs:[ cls "tagline" ]
              [ Vdom.Node.text
                  "Does buying NO on Kalshi win more often than its price implies, after \
                   fees?"
              ]
          ]
      ; verdict_view r
      ; filters_view
      ; Vdom.Node.p
          ~attrs:[ cls "explain" ]
          [ Vdom.Node.text (explain r filters (Segment.to_string segment)) ]
      ; charts
      ; Chart.legend
      ; Vdom.Node.section
          ~attrs:[ cls "table-section" ]
          [ Vdom.Node.div
              ~attrs:[ cls "table-head" ]
              [ Vdom.Node.h2
                  [ Vdom.Node.textf
                      "Buckets: buy %s, %s"
                      (Side.to_string filters.side)
                      (Fmt.horizon_phrase filters.view filters.horizon)
                  ]
              ; Vdom.Node.button
                  ~attrs:
                    [ cls "export"
                    ; Vdom.Attr.id "export"
                    ; (if List.is_empty current
                       then Vdom.Attr.disabled
                       else Vdom.Attr.empty)
                    ; Vdom.Attr.on_click (fun _ ->
                        Bucket_table.download
                          ~filename
                          ~contents:(Bucket_table.csv ~description current))
                    ]
                  [ Vdom.Node.text "Export CSV" ]
              ]
          ; (if has_enough current then Bucket_table.view current else empty_note)
          ]
      ; footer r
      ]
;;

let () =
  Async_js.init ();
  Bonsai_web.Start.start dashboard
;;
