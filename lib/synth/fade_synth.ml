open! Core
open Fade_types

(* A small synthetic market with a known, deliberate mispricing, so tests and the demo
   have something to find. The generator:
   1. draws a true YES probability for each market 24h before close, and lets it drift
      to 6h and 1h (brackets stay normalized so exactly one market can win);
   2. draws the outcome from the 1h probability;
   3. quotes prices with a favorite-longshot bias: the quoted probability is the true
      one pulled toward 50% in log-odds space, so cheap YES is overpriced and
      expensive NO is underpriced. *)

module Params = struct
  type t =
    { seed : int
    ; n_events : int
    ; first_settle : Unix_time.t
    ; last_settle : Unix_time.t
    ; shrink : float (** < 1 means longshots are overpriced *)
    }

  let default =
    { seed = 20260516
    ; n_events = 700
    ; first_settle = Unix_time.of_date_string_exn "2025-05-16"
    ; last_settle = Unix_time.of_date_string_exn "2026-09-30"
    ; shrink = 0.86
    }
  ;;
end

let categories =
  [| "Sports", "SPT"
   ; "Climate and Weather", "WX"
   ; "Economics", "ECO"
   ; "Politics", "POL"
   ; "Crypto", "CRY"
   ; "Financials", "FIN"
  |]
;;

let logit p = Float.log (p /. (1. -. p))
let expit x = 1. /. (1. +. Float.exp (-.x))
let clamp ~lo ~hi x = Float.max lo (Float.min hi x)

(* Standard normal draw by Box-Muller. [Random.State.t] is an explicit generator
   threaded through every call, so the output depends only on the seed. *)
let gaussian st =
  let u1 = Float.max 1e-12 (Random.State.float st 1.) in
  let u2 = Random.State.float st 1. in
  Float.sqrt (-2. *. Float.log u1) *. Float.cos (2. *. Float.pi *. u2)
;;

let normalize ps =
  let total = List.sum (module Float) ps ~f:Fn.id in
  List.map ps ~f:(fun p -> p /. total)
;;

(* Pick index i with probability ps.(i). *)
let categorical st ps =
  let r = Random.State.float st 1. in
  let rec go i acc = function
    | [] -> i - 1
    | p :: rest -> if Float.( < ) r (acc +. p) then i else go (i + 1) (acc +. p) rest
  in
  go 0 0. ps
;;

(* True probabilities at 24h, 6h and 1h for every market of one event. *)
let draw_paths st ~(kind : Event_kind.t) ~n =
  let drift ~sd ps =
    let moved =
      List.map ps ~f:(fun p ->
        expit (logit (clamp ~lo:0.002 ~hi:0.998 p) +. (sd *. gaussian st)))
    in
    match kind with
    | Bracket -> normalize moved
    | Single | Multi -> moved
  in
  let p24 =
    match kind with
    | Bracket -> normalize (List.init n ~f:(fun _ -> Float.exp (1.3 *. gaussian st)))
    | Single | Multi -> List.init n ~f:(fun _ -> expit (1.9 *. gaussian st))
  in
  let p6 = drift ~sd:0.55 p24 in
  let p1 = drift ~sd:0.45 p6 in
  p24, p6, p1
;;

let quote st ~shrink ~p =
  let q = expit (shrink *. logit (clamp ~lo:0.002 ~hi:0.998 p)) in
  let mid = 100. *. q in
  let half_spread = 0.5 +. Float.of_int (Random.State.int st 2) in
  let bid = Float.round_down (mid -. half_spread) in
  let ask = Float.round_up (mid +. half_spread) in
  let bid = if Float.( < ) bid 1. then None else Some bid in
  let ask = if Float.( > ) ask 99. then None else Some ask in
  bid, ask
;;

let event st ~(params : Params.t) ~idx =
  let category, abbrev = categories.(Random.State.int st (Array.length categories)) in
  let series_ticker = sprintf "SYN%s%d" abbrev (1 + Random.State.int st 2) in
  let event_ticker = sprintf "%s-%04d" series_ticker idx in
  let kind : Event_kind.t =
    let r = Random.State.float st 1. in
    if Float.( < ) r 0.45 then Bracket else if Float.( < ) r 0.65 then Multi else Single
  in
  let n =
    match kind with
    | Single -> 1
    | Bracket -> 3 + Random.State.int st 4
    | Multi -> 2 + Random.State.int st 3
  in
  let span = params.last_settle - params.first_settle in
  let settle = params.first_settle + Random.State.int st span in
  let close_time = settle - (60 + Random.State.int st 3000) in
  let open_time = close_time - (86_400 * (2 + Random.State.int st 10)) in
  let p24, p6, p1 = draw_paths st ~kind ~n in
  let wins =
    match kind with
    | Bracket ->
      let w = categorical st p1 in
      List.init n ~f:(fun i -> i = w)
    | Single | Multi -> List.map p1 ~f:(fun p -> Float.( < ) (Random.State.float st 1.) p)
  in
  (* [List.zip_exn] pairs up two lists of equal length; [List.mapi] passes the index. *)
  let paths = List.zip_exn (List.zip_exn (List.zip_exn p24 p6) p1) wins in
  List.mapi paths ~f:(fun i (((p24, p6), p1), won) ->
    let ticker = sprintf "%s-M%d" event_ticker i in
    let market : Market.t =
      { ticker
      ; event_ticker
      ; series_ticker
      ; category
      ; open_time
      ; close_time
      ; settle_time = settle
      ; result = (if won then Yes else No)
      ; volume =
          Float.round_decimal
            ~decimal_digits:2
            (500. +. Float.exp (6.5 +. (1.3 *. gaussian st)))
      ; can_close_early = String.equal category "Sports"
      ; event_kind = kind
      }
    in
    let snapshots =
      List.filter_map
        [ Horizon.H24, p24; H6, p6; H1, p1 ]
        ~f:(fun (horizon, p) ->
          (* About 4% of horizons have no candle at all. *)
          if Float.( < ) (Random.State.float st 1.) 0.04
          then None
          else (
            let yes_bid, yes_ask = quote st ~shrink:params.shrink ~p in
            let ts = close_time - Horizon.seconds horizon in
            let ts = ts - (ts mod 3600) in
            Some { Snapshot.ticker; horizon; ts; yes_bid; yes_ask }))
    in
    let trades =
      if Float.( < ) (Random.State.float st 1.) 0.55
      then []
      else
        List.init
          (2 + Random.State.int st 6)
          ~f:(fun j ->
            let secs_before = 60 + Random.State.int st (26 * 3600) in
            let p =
              if secs_before <= 3600
              then p1
              else if secs_before <= 6 * 3600
              then p6
              else p24
            in
            let bid, ask = quote st ~shrink:params.shrink ~p in
            let q = Option.value bid ~default:1. in
            (* Takers lean toward buying cheap YES, the classic longshot habit. *)
            let taker_yes_prob = if Float.( < ) q 30. then 0.62 else 0.5 in
            let taker_side : Side.t =
              if Float.( < ) (Random.State.float st 1.) taker_yes_prob then Yes else No
            in
            let yes_price =
              match taker_side with
              | Yes -> Option.value ask ~default:99.
              | No -> Option.value bid ~default:1.
            in
            { Trade.trade_id = sprintf "%s-T%d" ticker j
            ; ticker
            ; ts = close_time - secs_before
            ; yes_price
            ; count = Float.of_int (1 + Random.State.int st 250)
            ; taker_side
            ; is_block = false
            })
    in
    market, snapshots, trades)
;;

let fees : Fee_schedule.t =
  let quadratic series_ticker : Fee_schedule.Series.t =
    { series_ticker; current_type = Quadratic; current_multiplier = 1.; changes = [] }
  in
  let all_series =
    Array.to_list categories
    |> List.concat_map ~f:(fun (_, abbrev) ->
      [ sprintf "SYN%s1" abbrev; sprintf "SYN%s2" abbrev ])
  in
  { series =
      List.map all_series ~f:(fun s ->
        if String.equal s "SYNECO1"
        then
          (* Gains maker fees on 2026-01-01, like real series did in 2026. *)
          { (quadratic s) with
            current_type = Quadratic_with_maker_fees
          ; changes =
              [ { ts = Unix_time.of_date_string_exn "2026-01-01"
                ; fee_type = Some Quadratic_with_maker_fees
                ; multiplier = Some 1.
                }
              ]
          }
        else quadratic s)
  ; event_overrides = []
  }
;;

let generate ?(params = Params.default) () : Dataset.t =
  let st = Random.State.make [| params.seed |] in
  let rows =
    List.concat (List.init params.n_events ~f:(fun idx -> event st ~params ~idx))
  in
  let markets = List.map rows ~f:(fun (m, _, _) -> m) in
  let snapshots = List.concat_map rows ~f:(fun (_, s, _) -> s) in
  let trades = List.concat_map rows ~f:(fun (_, _, t) -> t) in
  { markets = List.sort markets ~compare:Market.compare
  ; snapshots = List.sort snapshots ~compare:Snapshot.compare
  ; trades = List.sort trades ~compare:Trade.compare
  ; fees
  }
;;

(* One JSON value per line inside each array, so fixture diffs stay readable. *)
let to_json_string (d : Dataset.t) =
  let lines to_json xs =
    List.map xs ~f:(fun x -> Yojson.Safe.to_string (to_json x))
    |> String.concat ~sep:",\n"
  in
  sprintf
    "{\"markets\":[\n\
     %s\n\
     ],\n\
     \"snapshots\":[\n\
     %s\n\
     ],\n\
     \"trades\":[\n\
     %s\n\
     ],\n\
     \"fees\":%s}\n"
    (lines Market.yojson_of_t d.markets)
    (lines Snapshot.yojson_of_t d.snapshots)
    (lines Trade.yojson_of_t d.trades)
    (Yojson.Safe.to_string (Fee_schedule.yojson_of_t d.fees))
;;

let of_json_string s = Dataset.t_of_yojson (Yojson.Safe.from_string s)
