open! Core
open Fade_types

type spec =
  { fee_type : Fee_type.t
  ; multiplier : float
  }
[@@deriving sexp, compare, equal]

let coefficient (fee_type : Fee_type.t) (role : Role.t) =
  match fee_type, role with
  | Flat, _ -> None
  | (Quadratic | Quadratic_with_maker_fees | Quadratic_with_combo_maker_fees), Taker ->
    Some 0.07
  | Quadratic, Maker -> Some 0.
  | Quadratic_with_maker_fees, Maker -> Some 0.0175
  | Quadratic_with_combo_maker_fees, Maker -> Some 0.035
;;

(* Money is done in integer micro-dollars ($0.000001), the finest unit Kalshi uses, so
   that "round up" is exact. In floating point, 0.07 *. 100. *. 0.25 is
   1.7500000000000002, and naively rounding that up gives $1.76 instead of $1.75. *)
let micros_per_dollar = 1_000_000
let micros_per_cent = 10_000

(* Ceiling of a non-negative float, treating values within 1e-6 of an integer as that
   integer (float noise, not a real fraction of a micro-dollar). *)
let ceil_tolerant x =
  let r = Float.round_nearest x in
  if Float.( < ) (Float.abs (x -. r)) 1e-6 then Float.to_int r else Float.iround_up_exn x
;;

let ceil_div a b = (a + b - 1) / b

let order_fee_dollars spec ~role ~contracts ~price_cents =
  coefficient spec.fee_type role
  |> Option.map ~f:(fun coef ->
    let p = price_cents /. 100. in
    (* Model fee in micro-dollars, rounded up to a whole micro-dollar. *)
    let fee =
      ceil_tolerant
        (spec.multiplier
         *. coef
         *. contracts
         *. p
         *. (1. -. p)
         *. Float.of_int micros_per_dollar)
    in
    let cost = ceil_tolerant (contracts *. price_cents *. Float.of_int micros_per_cent) in
    (* The balance moves in whole cents: cost plus fee is rounded up to the next cent, and
       whatever that adds on top of [cost] is fee too. *)
    let total = ceil_div (cost + fee) micros_per_cent * micros_per_cent in
    Float.of_int (total - cost) /. Float.of_int micros_per_dollar)
;;

let fee_per_contract_cents spec ~role ~contracts ~price_cents =
  order_fee_dollars spec ~role ~contracts ~price_cents
  |> Option.map ~f:(fun d -> d *. 100. /. contracts)
;;

let general = { fee_type = Quadratic; multiplier = 1. }

let latest_at changes ~ts =
  List.filter changes ~f:(fun (c : Fee_schedule.Change.t) -> c.ts <= ts)
  |> List.max_elt ~compare:(fun (a : Fee_schedule.Change.t) b -> Int.compare a.ts b.ts)
;;

let series_spec (s : Fee_schedule.Series.t) ~ts =
  match s.changes with
  | [] -> { fee_type = s.current_type; multiplier = s.current_multiplier }
  | changes ->
    (match latest_at changes ~ts with
     | None -> general
     | Some c ->
       { fee_type = Option.value c.fee_type ~default:s.current_type
       ; multiplier = Option.value c.multiplier ~default:s.current_multiplier
       })
;;

let with_override base (o : Fee_schedule.Event_override.t option) ~ts =
  match Option.bind o ~f:(fun o -> latest_at o.changes ~ts) with
  | None -> base
  | Some { fee_type = None; multiplier = None; _ } -> base
  | Some c ->
    { fee_type = Option.value c.fee_type ~default:base.fee_type
    ; multiplier = Option.value c.multiplier ~default:base.multiplier
    }
;;

module Index = struct
  type t =
    { series : Fee_schedule.Series.t String.Table.t
    ; events : Fee_schedule.Event_override.t String.Table.t
    }

  (* A hash table keyed by ticker; the first entry wins if a ticker repeats. *)
  let table items ~key =
    let t = String.Table.create () in
    List.iter items ~f:(fun x ->
      ignore (Hashtbl.add t ~key:(key x) ~data:x : [ `Ok | `Duplicate ]));
    t
  ;;

  let create (s : Fee_schedule.t) =
    { series = table s.series ~key:(fun x -> x.series_ticker)
    ; events = table s.event_overrides ~key:(fun x -> x.event_ticker)
    }
  ;;

  let lookup t ~series_ticker ~event_ticker ~ts =
    Hashtbl.find t.series series_ticker
    |> Option.map ~f:(fun s ->
      with_override (series_spec s ~ts) (Hashtbl.find t.events event_ticker) ~ts)
  ;;
end

let lookup schedule ~series_ticker ~event_ticker ~ts =
  Index.lookup (Index.create schedule) ~series_ticker ~event_ticker ~ts
;;

(* ---- tests -------------------------------------------------------------------- *)

let show ?(spec = general) ~role ~contracts ~price_cents () =
  let fee = Option.value_exn (order_fee_dollars spec ~role ~contracts ~price_cents) in
  printf
    "%s %g @ %g¢: fee $%.6f, total $%.2f\n"
    (Role.to_string role)
    contracts
    price_cents
    fee
    ((contracts *. price_cents /. 100.) +. fee)
;;

let%expect_test "docs.kalshi.com fee rounding example: 1 contract at $0.055" =
  (* "trade fee = ceil_6dp($0.00363825) = $0.003639; ... The balance changes by -$0.06;
     the trade fee plus rounding fee is exactly $0.005." *)
  show ~role:Taker ~contracts:1. ~price_cents:5.5 ();
  [%expect {| taker 1 @ 5.5¢: fee $0.005000, total $0.06 |}]
;;

let%expect_test "general schedule: 0.07 x C x P x (1 - P), rounded up" =
  show ~role:Taker ~contracts:100. ~price_cents:50. ();
  (* exactly $1.75: the float product is 1.7500000000000002 *)
  show ~role:Taker ~contracts:1. ~price_cents:50. ();
  (* $0.0175 rounds up to 2 cents *)
  show ~role:Taker ~contracts:100. ~price_cents:95. ();
  show ~role:Taker ~contracts:1. ~price_cents:95. ();
  [%expect
    {|
    taker 100 @ 50¢: fee $1.750000, total $51.75
    taker 1 @ 50¢: fee $0.020000, total $0.52
    taker 100 @ 95¢: fee $0.340000, total $95.34
    taker 1 @ 95¢: fee $0.010000, total $0.96
    |}]
;;

let%expect_test "maker fees: none, a quarter, or a half of the taker coefficient" =
  List.iter
    [ Fee_type.Quadratic; Quadratic_with_maker_fees; Quadratic_with_combo_maker_fees ]
    ~f:(fun fee_type ->
      printf "%-32s " (Fee_type.to_kalshi_string fee_type);
      show
        ~spec:{ fee_type; multiplier = 1. }
        ~role:Maker
        ~contracts:100.
        ~price_cents:50.
        ());
  [%expect
    {|
    quadratic                        maker 100 @ 50¢: fee $0.000000, total $50.00
    quadratic_with_maker_fees        maker 100 @ 50¢: fee $0.440000, total $50.44
    quadratic_with_combo_maker_fees  maker 100 @ 50¢: fee $0.880000, total $50.88
    |}]
;;

let%expect_test "multiplier scales the fee; flat is not modelled" =
  show
    ~spec:{ fee_type = Quadratic; multiplier = 0.5 }
    ~role:Taker
    ~contracts:100.
    ~price_cents:50.
    ();
  show
    ~spec:{ fee_type = Quadratic; multiplier = 0. }
    ~role:Taker
    ~contracts:100.
    ~price_cents:50.
    ();
  print_s
    [%sexp
      (order_fee_dollars
         { fee_type = Flat; multiplier = 1. }
         ~role:Taker
         ~contracts:1.
         ~price_cents:50.
       : float option)];
  [%expect
    {|
    taker 100 @ 50¢: fee $0.880000, total $50.88
    taker 100 @ 50¢: fee $0.000000, total $50.00
    ()
    |}]
;;

let%expect_test "per-contract fee depends on order size" =
  List.iter [ 1.; 10.; 100.; 1000. ] ~f:(fun contracts ->
    printf
      "%5g contracts at 92¢: %.4f¢ per contract\n"
      contracts
      (Option.value_exn
         (fee_per_contract_cents general ~role:Taker ~contracts ~price_cents:92.)));
  [%expect
    {|
        1 contracts at 92¢: 1.0000¢ per contract
       10 contracts at 92¢: 0.6000¢ per contract
      100 contracts at 92¢: 0.5200¢ per contract
     1000 contracts at 92¢: 0.5160¢ per contract
    |}]
;;

let%expect_test "schedule lookup: series changes and event overrides" =
  let day d = Unix_time.of_date_string_exn d in
  let schedule : Fee_schedule.t =
    { series =
        [ { series_ticker = "S"
          ; current_type = Quadratic_with_maker_fees
          ; current_multiplier = 1.
          ; changes =
              [ { ts = day "2026-01-01"
                ; fee_type = Some Quadratic_with_maker_fees
                ; multiplier = Some 1.
                }
              ]
          }
        ; { series_ticker = "HALF"
          ; current_type = Quadratic
          ; current_multiplier = 0.5
          ; changes = []
          }
        ]
    ; event_overrides =
        [ { event_ticker = "S-PROMO"
          ; changes =
              [ { ts = day "2026-03-01"; fee_type = None; multiplier = Some 0. }
              ; { ts = day "2026-03-08"; fee_type = None; multiplier = None }
              ]
          }
        ]
    }
  in
  let at series event d =
    let spec = lookup schedule ~series_ticker:series ~event_ticker:event ~ts:(day d) in
    print_s [%message d (spec : spec option)]
  in
  at "S" "S-X" "2025-12-31";
  at "S" "S-X" "2026-01-01";
  at "HALF" "HALF-X" "2025-06-01";
  at "S" "S-PROMO" "2026-03-02";
  at "S" "S-PROMO" "2026-03-09";
  at "NOPE" "NOPE-X" "2026-01-01";
  [%expect
    {|
    (2025-12-31 (spec (((fee_type Quadratic) (multiplier 1)))))
    (2026-01-01 (spec (((fee_type Quadratic_with_maker_fees) (multiplier 1)))))
    (2025-06-01 (spec (((fee_type Quadratic) (multiplier 0.5)))))
    (2026-03-02 (spec (((fee_type Quadratic_with_maker_fees) (multiplier 0)))))
    (2026-03-09 (spec (((fee_type Quadratic_with_maker_fees) (multiplier 1)))))
    (2026-01-01 (spec ()))
    |}]
;;
