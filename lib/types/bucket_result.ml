open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Results for one price bucket of one table. Prices are cents; rates and returns are
   fractions (0.05 = 5%). *)
type t =
  { lo : float (** bucket covers prices paid in [lo, hi) *)
  ; hi : float
  ; n : int (** markets (quote view) or trades (trade view) *)
  ; n_events : int
  ; contracts : float (** quote view: equals [n]; trade view: total contracts *)
  ; wins : float (** markets or contracts whose side won *)
  ; win_rate : float
  ; win_ci : Ci.t (** Wilson score interval; ignores event clustering *)
  ; avg_price : float (** average price paid, cents, before fees *)
  ; avg_fee : float (** average fee per contract, cents *)
  ; breakeven : float (** win rate needed to break even after fees *)
  ; mean_return : float (** profit per dollar staked, after fees *)
  ; return_ci : Ci.t option
    (** 95% CI: the wider of an event-clustered bootstrap and an events-as-n Wilson
          interval mapped to returns. [None] when [n < min_n]. *)
  ; enough : bool (** [n >= min_n] *)
  }
[@@deriving sexp, yojson, compare, equal]

let label t = sprintf "%.0f-%.0f¢" t.lo t.hi

(* A row is only called a winner if its whole return CI sits above zero. *)
let proven_positive t =
  t.enough && Option.value_map t.return_ci ~default:false ~f:Ci.above_zero
;;

let proven_negative t =
  t.enough && Option.value_map t.return_ci ~default:false ~f:Ci.below_zero
;;
