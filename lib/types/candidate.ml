open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* The candidate that came out of the study's exploratory trade view (whoever held NO
   at 90-95c in the last hour made money), pre-registered for a forward test on markets
   that settle after the study window. rules/candidate.json freezes these records with
   a SHA-256; see [Fade_analysis.Candidate_file]. *)

module Fill_model = struct
  type t =
    | Through (** a later trade printed strictly below our bid *)
    | At_or_through (** a later trade printed at or below our bid *)
  [@@deriving sexp, yojson, compare, equal]
end

module Execution = struct
  (* A constructor can carry a value: [Maker_bid Through] is a maker rule that uses the
     strict fill model. *)
  type t =
    | Taker_at_ask
    | Maker_bid of Fill_model.t
  [@@deriving sexp, yojson, compare, equal]
end

module Rule = struct
  type t =
    { id : string
    ; execution : Execution.t
    ; side : Side.t
    ; horizon : Horizon.t
    ; lo : float (** price range [lo, hi), cents *)
    ; hi : float
    ; max_spread : float option (** cents, YES ask minus YES bid at the decision *)
    ; contracts : float (** order size, for fee rounding *)
    }
  [@@deriving sexp, yojson, compare, equal]

  let describe t =
    let side = Side.to_string t.side in
    let range = sprintf "%.0f-%.0f¢" t.lo t.hi in
    let horizon = Horizon.to_string t.horizon in
    match t.execution with
    | Taker_at_ask ->
      sprintf
        "Buy %s as a taker at the ask when it is %s, %s before close%s (%.0f-contract \
         orders)"
        side
        range
        horizon
        (match t.max_spread with
         | Some s -> sprintf ", spread <= %.0f¢" s
         | None -> "")
        t.contracts
    | Maker_bid fill ->
      sprintf
        "Rest a %.0f-contract %s bid at the best %s bid when it is %s, from %s before \
         close; filled %s"
        t.contracts
        side
        side
        range
        horizon
        (match fill with
         | Through -> "only if a later trade prints below it"
         | At_or_through -> "if a later trade prints at or below it")
  ;;

  let short_name t =
    match t.execution with
    | Taker_at_ask -> "Taker at the ask"
    | Maker_bid Through -> "Maker bid, strict fills"
    | Maker_bid At_or_through -> "Maker bid, loose fills"
  ;;
end

module Config = struct
  type t =
    { study_hash : string (** sha256 of the study's rules/frozen.json *)
    ; in_sample_since : string
    ; in_sample_cutoff : string (** in-sample: events fully settled before this date *)
    ; forward_start : string (** forward test: markets settled on or after this date *)
    ; forward_days : int
    ; min_volume : float
    ; anchor : Anchor.t
    ; min_n : int
    ; bootstrap_resamples : int
    ; seed : int
    ; stop_rule : string
    }
  [@@deriving sexp, yojson, compare, equal]
end

(* One rule's numbers on one sample. [orders] counts markets where the rule placed an
   order; [result] covers the orders that filled (all of them for a taker). *)
module Score = struct
  type t =
    { rule_id : string
    ; orders : int
    ; result : Bucket_result.t
    }
  [@@deriving sexp, yojson, compare, equal]
end

type t =
  { frozen_at : string
  ; config : Config.t
  ; rules : Rule.t list
  ; in_sample : Score.t list
  ; hash : string
  ; notes : string list (** not hashed *)
  }
[@@deriving sexp, yojson, compare, equal]
