(** Kalshi trading fees.

    Source: docs.kalshi.com (Get Series: fee types; Fee Rounding guide) and the fee
    schedule's general formula [round_up(M x 0.07 x C x P x (1 - P))], with makers paying
    0.0175 (a quarter) under [quadratic_with_maker_fees] and 0.035 (a half) under
    [quadratic_with_combo_maker_fees]. *)

open! Core
open Fade_types

(** Fee structure in force for one market at one moment. *)
type spec =
  { fee_type : Fee_type.t
  ; multiplier : float
  }
[@@deriving sexp, compare, equal]

(** The coefficient multiplying [C x P x (1 - P)], before the multiplier. [None] for
    [Flat], whose table is not modelled. *)
val coefficient : Fee_type.t -> Role.t -> float option

(** Total fee in dollars for one order of [contracts] at [price_cents], after Kalshi's
    rounding for a member whose balance is kept in whole cents: the model fee is rounded
    up to $0.000001, then cost plus fee is rounded up to the next cent. [None] when the
    fee type is not modelled. *)
val order_fee_dollars
  :  spec
  -> role:Role.t
  -> contracts:float
  -> price_cents:float
  -> float option

(** [order_fee_dollars] spread over the order, in cents per contract. *)
val fee_per_contract_cents
  :  spec
  -> role:Role.t
  -> contracts:float
  -> price_cents:float
  -> float option

(** The fee structure in force for [series_ticker]/[event_ticker] at [ts]:
    - an event override, if one is active (a change with both fields null clears it);
    - otherwise the latest series change at or before [ts];
    - before a series' first recorded change, the general schedule (quadratic, x1);
    - a series with no recorded changes uses its current settings.
    [None] if the series is unknown. *)
val lookup
  :  Fee_schedule.t
  -> series_ticker:string
  -> event_ticker:string
  -> ts:Unix_time.t
  -> spec option

(** Precomputed index for repeated [lookup]s. *)
module Index : sig
  type t

  val create : Fee_schedule.t -> t

  val lookup
    :  t
    -> series_ticker:string
    -> event_ticker:string
    -> ts:Unix_time.t
    -> spec option
end
