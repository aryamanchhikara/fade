open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* web/data/forward.json: the forward test of rules/candidate.json, written by
   `fade forward` and read by the dashboard's Forward test tab. *)

module Status = struct
  type t =
    | Running
    | Supported (** the return CI cleared zero: the rule made money *)
    | Rejected (** the return CI is entirely below zero *)
    | Inconclusive (** the window ended without either *)
  [@@deriving sexp, yojson, compare, equal]

  let to_string = function
    | Running -> "Running"
    | Supported -> "Edge supported"
    | Rejected -> "Rejected"
    | Inconclusive -> "Inconclusive"
  ;;
end

module Rule_report = struct
  type t =
    { rule : Candidate.Rule.t
    ; in_sample : Candidate.Score.t
    ; forward : Candidate.Score.t
    ; status : Status.t
    ; concluded_on : string option (** the first run at which the CI excluded zero *)
    ; more_events : int option
      (** rough number of further events (with a fill) until the CI could exclude zero
          at the current effect size; [None] when there is no estimate *)
    ; projection : string (** what [more_events] was projected from *)
    }
  [@@deriving sexp, yojson, compare, equal]
end

type t =
  { generated_at : string
  ; synthetic : bool
  ; candidate_hash : string
  ; config : Candidate.Config.t
  ; forward_end : string (** last settlement date in the window *)
  ; day : int (** day of the window the test is on, 0 before it starts *)
  ; listed_markets : int (** in scope and in the window *)
  ; quoted_markets : int (** with a quote at the rules' horizon *)
  ; traded_markets : int (** with complete trades (maker rules can be judged) *)
  ; first_settle : string option
  ; last_settle : string option
  ; rules : Rule_report.t list
  ; notes : string list
  }
[@@deriving sexp, yojson, compare, equal]
