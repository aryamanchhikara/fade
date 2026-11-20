open! Core
open Fade_types

(* The model-gated maker rule: post instead of crossing.

   At a decision, with a two-sided book quoted at the quote time, rest a 1-contract NO
   bid one cent above the best NO bid (so still below the NO ask, and never crossing)
   when the model says it is worth it:

     P_model(NO) - bid - maker fee > threshold      (dollars)

   The order rests until the market closes. It fills only if a later trade prints
   through it (strict) or at or through it (loose), the same fill model as rule B. A
   filled order pays the bid plus the maker fee and returns $1 if NO wins; an unfilled
   order costs nothing. The catch is adverse selection: a NO bid tends to fill when NO
   is falling, which is when NO tends to lose. *)

type order =
  { pred : Backtest.prediction
  ; bid : float (** cents *)
  ; fee : float (** maker fee, cents per contract *)
  ; ev : float (** dollars per contract, if filled *)
  ; strict : bool (** filled under the strict model *)
  ; loose : bool
  }

type fill_model =
  | Strict
  | Loose

let fill_model_to_string = function
  | Strict -> "strict"
  | Loose -> "loose"
;;

let filled fill o =
  match fill with
  | Strict -> o.strict
  | Loose -> o.loose
;;

(* Every row where an order inside the spread is possible, and how many two-sided rows
   had no room (a 1c spread). *)
let orders ~fees (preds : Backtest.prediction list) =
  let no_room = ref 0 in
  let os =
    List.filter_map preds ~f:(fun pr ->
      let f = pr.row.features in
      match f.spread with
      | None -> None
      | Some s when Float.( <= ) s 1. ->
        incr no_room;
        None
      | Some s ->
        let bid = f.no_ask -. s +. 1. in
        if Float.( < ) bid 1. || Float.( >= ) bid f.no_ask
        then None
        else
          Fees.Index.lookup
            fees
            ~series_ticker:pr.row.series_ticker
            ~event_ticker:pr.row.event_ticker
            ~ts:f.asof
          |> Option.bind ~f:(fun spec ->
            Fees.fee_per_contract_cents spec ~role:Maker ~contracts:1. ~price_cents:bid)
          |> Option.map ~f:(fun fee ->
            let fills cmp =
              Option.value_map pr.row.min_no_after ~default:false ~f:(fun m -> cmp m bid)
            in
            { pred = pr
            ; bid
            ; fee
            ; ev = pr.p -. ((bid +. fee) /. 100.)
            ; strict = fills Float.( < )
            ; loose = fills Float.( <= )
            }))
  in
  os, !no_room
;;

type horizon_choice =
  | Only of Horizon.t
  | First

let horizon_choice_to_string = function
  | Only h -> Horizon.to_string h
  | First -> "first"
;;

let horizon_choices = [ Only H24; Only H6; Only H1; First ]

(* The orders placed: EV above the threshold at the chosen horizon, the first such
   decision per market. *)
let placed ~threshold ~horizon (os : order list) =
  List.filter os ~f:(fun o ->
    Float.( > ) o.ev threshold
    &&
    match horizon with
    | Only h -> Horizon.equal o.pred.row.features.horizon h
    | First -> true)
  |> List.sort_and_group ~compare:(fun a b ->
    String.compare a.pred.row.ticker b.pred.row.ticker)
  |> List.map ~f:(fun group ->
    List.min_elt group ~compare:(fun a b ->
      Int.compare a.pred.row.features.asof b.pred.row.features.asof)
    |> Option.value_exn)
;;

let to_candidate o : Backtest.candidate =
  { pred = o.pred; price = o.bid; fee = o.fee; ev = o.ev }
;;

let profit fill os =
  List.sum
    (module Float)
    os
    ~f:(fun o -> if filled fill o then Backtest.profit (to_candidate o) else 0.)
;;

(* Walk-forward: each month, the (threshold, horizon) with the most profit over the
   [window] months before it, using that month's own fill model. Ties go to the higher
   threshold. *)
let walk_forward ~grid ~window ~fill ~(months : string list) (os : order list) =
  let by_month = List.map os ~f:(fun o -> o.pred.month, o) |> String.Map.of_alist_multi in
  let get m = Map.find by_month m |> Option.value ~default:[] in
  List.filter_mapi months ~f:(fun i m ->
    if i < window
    then None
    else (
      let validation =
        List.concat_map (List.sub months ~pos:(i - window) ~len:window) ~f:get
      in
      let choices =
        List.concat_map (List.rev grid) ~f:(fun threshold ->
          List.map horizon_choices ~f:(fun horizon -> threshold, horizon))
      in
      let threshold, horizon =
        List.max_elt choices ~compare:(fun (t1, h1) (t2, h2) ->
          Float.compare
            (profit fill (placed ~threshold:t1 ~horizon:h1 validation))
            (profit fill (placed ~threshold:t2 ~horizon:h2 validation)))
        |> Option.value_exn
      in
      Some (m, threshold, horizon, placed ~threshold ~horizon (get m))))
;;

let adverse fill (os : order list) : Maker_report.Adverse.t =
  let f, u = List.partition_tf os ~f:(filled fill) in
  let mean xs g =
    if List.is_empty xs
    then 0.
    else List.sum (module Float) xs ~f:g /. Float.of_int (List.length xs)
  in
  let won o = if o.pred.row.no_won then 1. else 0. in
  { orders = List.length os
  ; filled = List.length f
  ; win_rate_filled = mean f won
  ; win_rate_unfilled = mean u won
  ; model_p_filled = mean f (fun o -> o.pred.p)
  ; model_p_unfilled = mean u (fun o -> o.pred.p)
  }
;;

let%expect_test "bid inside the spread, maker fee, strict vs loose fills" =
  let row ticker ~ask ~spread ~min_after ~won : Pit.Row.t =
    { ticker
    ; event_ticker = "E" ^ ticker
    ; series_ticker = "S"
    ; settle_time = 1000
    ; no_won = won
    ; min_no_after = min_after
    ; features =
        { horizon = H1
        ; decision = 100
        ; asof = 100
        ; no_ask = ask
        ; spread
        ; volume_to_date = 1000.
        ; trades_to_date = 10
        ; move_1h = None
        ; move_6h = None
        ; move_24h = None
        ; hours_to_close = 1.
        ; category = "Test"
        ; mutually_exclusive = false
        ; event_markets = 1
        }
    }
  in
  let fees =
    Fees.Index.create
      { series =
          [ { series_ticker = "S"
            ; current_type = Quadratic_with_maker_fees
            ; current_multiplier = 1.
            ; changes = []
            }
          ]
      ; event_overrides = []
      }
  in
  let pred r p = { Backtest.row = r; month = "m"; p } in
  let preds =
    [ (* NO bid 56, ask 60: we bid 57. A later trade at 55 fills us under both models. *)
      pred (row "A" ~ask:60. ~spread:(Some 4.) ~min_after:(Some 55.) ~won:true) 0.70
    ; (* A trade exactly at our 57: loose only. *)
      pred (row "B" ~ask:60. ~spread:(Some 4.) ~min_after:(Some 57.) ~won:false) 0.70
    ; (* 1c spread: no room inside. *)
      pred (row "C" ~ask:60. ~spread:(Some 1.) ~min_after:(Some 10.) ~won:true) 0.90
    ; (* One-sided: no NO bid to improve on. *)
      pred (row "D" ~ask:60. ~spread:None ~min_after:(Some 10.) ~won:true) 0.90
    ]
  in
  let os, no_room = orders ~fees preds in
  printf "no room: %d\n" no_room;
  List.iter os ~f:(fun o ->
    printf
      "%s bid %.0f fee %.0fc ev %+.2f strict %b loose %b\n"
      o.pred.row.ticker
      o.bid
      o.fee
      o.ev
      o.strict
      o.loose);
  printf "profit strict %+.2f, loose %+.2f\n" (profit Strict os) (profit Loose os);
  [%expect
    {|
    no room: 1
    A bid 57 fee 1c ev +0.12 strict true loose true
    B bid 57 fee 1c ev +0.12 strict false loose true
    profit strict +0.42, loose -0.16
    |}]
;;
