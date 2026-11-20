open! Core
open Fade_types
module Row = Pit.Row

(* The EV rule, backtested on out-of-sample predictions.

   At a decision, with the NO ask at the quote time, buying one contract of NO costs
   ask + fee and pays $1 if NO wins. With the model's probability p:

     EV per contract = p - (ask + fee) / 100     (dollars)

   Trade when EV exceeds a threshold, as a taker, only on two-sided books with a spread
   of at most [max_spread], at most once per market (the first horizon that qualifies).
   A 10-contract order is assumed to fill [slippage] cents above the ask: there is no
   historical book depth to say otherwise. *)

type params =
  { max_spread : float
  ; size : int
  ; slippage : float
  }

type prediction =
  { row : Row.t
  ; month : string
  ; p : float
  }

type candidate =
  { pred : prediction
  ; price : float (** cents paid per contract *)
  ; fee : float (** cents per contract *)
  ; ev : float (** dollars per contract *)
  }

let candidates ~fees ~params (preds : prediction list) =
  List.filter_map preds ~f:(fun pr ->
    let f = pr.row.features in
    match f.spread with
    | Some s when Float.( <= ) s params.max_spread ->
      let price = f.no_ask +. if params.size > 1 then params.slippage else 0. in
      if Float.( >= ) price 100.
      then None
      else
        Fees.Index.lookup
          fees
          ~series_ticker:pr.row.series_ticker
          ~event_ticker:pr.row.event_ticker
          ~ts:f.asof
        |> Option.bind ~f:(fun spec ->
          Fees.fee_per_contract_cents
            spec
            ~role:Taker
            ~contracts:(Float.of_int params.size)
            ~price_cents:price)
        |> Option.map ~f:(fun fee ->
          { pred = pr; price; fee; ev = pr.p -. ((price +. fee) /. 100.) })
    | _ -> None)
;;

(* Trades at [threshold]: EV above it, the earliest qualifying decision per market. *)
let trades ~threshold (cs : candidate list) =
  List.filter cs ~f:(fun c -> Float.( > ) c.ev threshold)
  |> List.sort_and_group ~compare:(fun a b ->
    String.compare a.pred.row.ticker b.pred.row.ticker)
  |> List.map ~f:(fun group ->
    List.min_elt group ~compare:(fun a b ->
      Int.compare a.pred.row.features.asof b.pred.row.features.asof)
    |> Option.value_exn)
;;

let cost c = (c.price +. c.fee) /. 100.
let profit c = (if c.pred.row.no_won then 1. else 0.) -. cost c

let summary ~rng ~resamples ~size (ts : candidate list) : Backtest_report.Summary.t =
  let n = List.length ts in
  let nf = Float.of_int (Int.max 1 n) in
  let sum f = List.sum (module Float) ts ~f in
  let by_event = String.Table.create () in
  List.iter ts ~f:(fun c ->
    Hashtbl.update by_event c.pred.row.event_ticker ~f:(fun prev ->
      let num, den = Option.value prev ~default:(0., 0.) in
      num +. profit c, den +. cost c));
  let clusters =
    Hashtbl.data by_event
    |> List.map ~f:(fun (num, den) -> { Stats.num; den })
    |> Array.of_list
  in
  (* Max drawdown of cumulative P&L, in settlement order. *)
  let size = Float.of_int size in
  let _, _, max_dd =
    List.sort ts ~compare:(fun a b ->
      Int.compare a.pred.row.settle_time b.pred.row.settle_time)
    |> List.fold ~init:(0., 0., 0.) ~f:(fun (cum, peak, dd) c ->
      let cum = cum +. (size *. profit c) in
      let peak = Float.max peak cum in
      cum, peak, Float.max dd (peak -. cum))
  in
  { trades = n
  ; events = Hashtbl.length by_event
  ; hit_rate = sum (fun c -> if c.pred.row.no_won then 1. else 0.) /. nf
  ; avg_price = sum (fun c -> c.price) /. nf
  ; avg_fee = sum (fun c -> c.fee) /. nf
  ; avg_ev = sum (fun c -> c.ev) /. nf
  ; return_per_dollar =
      (let c = sum cost in
       if Float.( > ) c 0. then sum profit /. c else 0.)
  ; ci = (if n >= 2 then Stats.cluster_bootstrap ~rng ~resamples clusters else None)
  ; profit = size *. sum profit
  ; max_drawdown = max_dd
  }
;;

(* Walk-forward threshold: for each month, the grid value with the highest total profit
   on the [window] months before it (their own out-of-sample predictions); ties go to
   the higher threshold. The first [window] months are validation only. *)
let choose_threshold ~grid (validation : candidate list) =
  List.max_elt
    (List.rev grid)
    ~compare:
      (Comparable.lift Float.compare ~f:(fun threshold ->
         List.sum (module Float) (trades ~threshold validation) ~f:profit))
  |> Option.value_exn
;;

let walk_forward ~grid ~window ~(months : string list) (cs : candidate list) =
  let by_month = List.map cs ~f:(fun c -> c.pred.month, c) |> String.Map.of_alist_multi in
  let get m = Map.find by_month m |> Option.value ~default:[] in
  List.filter_mapi months ~f:(fun i m ->
    if i < window
    then None
    else (
      let validation =
        List.concat_map (List.sub months ~pos:(i - window) ~len:window) ~f:get
      in
      let threshold = choose_threshold ~grid validation in
      Some (m, threshold, trades ~threshold (get m))))
;;

(* Share of predictions where the model and the ask differ by more than the fee. *)
let disagreement ~fees (preds : prediction list) =
  let cs =
    candidates ~fees ~params:{ max_spread = 100.; size = 1; slippage = 0. } preds
  in
  let n = List.length cs in
  if n = 0
  then 0.
  else
    Float.of_int
      (List.count cs ~f:(fun c ->
         Float.( > )
           (Float.abs (c.pred.p -. (c.pred.row.features.no_ask /. 100.)))
           (c.fee /. 100.)))
    /. Float.of_int n
;;

let%expect_test "EV, one trade per market, threshold choice" =
  let row ticker ~ask ~asof ~won : Row.t =
    { ticker
    ; event_ticker = "E" ^ ticker
    ; series_ticker = "S"
    ; settle_time = asof + 3600
    ; no_won = won
    ; features =
        { horizon = H1
        ; decision = asof
        ; asof
        ; no_ask = ask
        ; spread = Some 2.
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
            ; current_type = Quadratic
            ; current_multiplier = 1.
            ; changes = []
            }
          ]
      ; event_overrides = []
      }
  in
  let preds =
    [ { row = row "A" ~ask:60. ~asof:100 ~won:true; month = "m"; p = 0.70 }
    ; (* Same market later: only the earliest qualifying decision trades. *)
      { row = row "A" ~ask:65. ~asof:200 ~won:true; month = "m"; p = 0.75 }
    ; { row = row "B" ~ask:90. ~asof:100 ~won:false; month = "m"; p = 0.91 }
    ]
  in
  let cs = candidates ~fees ~params:{ max_spread = 5.; size = 1; slippage = 0. } preds in
  List.iter cs ~f:(fun c ->
    printf "%s ask %.0f fee %.2fc ev %+.4f\n" c.pred.row.ticker c.price c.fee c.ev);
  List.iter [ 0.; 0.02 ] ~f:(fun threshold ->
    let ts = trades ~threshold cs in
    printf
      "threshold %.2f: %s, profit %+.4f\n"
      threshold
      (String.concat
         ~sep:" "
         (List.map ts ~f:(fun c ->
            sprintf "%s@%d" c.pred.row.ticker c.pred.row.features.asof)))
      (List.sum (module Float) ts ~f:profit));
  [%expect
    {|
    A ask 60 fee 2.00c ev +0.0800
    A ask 65 fee 2.00c ev +0.0800
    B ask 90 fee 1.00c ev +0.0000
    threshold 0.00: A@100, profit +0.3800
    threshold 0.02: A@100, profit +0.3800
    |}]
;;
