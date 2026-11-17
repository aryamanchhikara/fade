open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Fade_types

(* rules/candidate.json: the forward test's pre-registration. Like rules/frozen.json,
   the hash is SHA-256 over the canonical JSON of everything that decides results (the
   config, the rules) plus the in-sample numbers known when it was locked. Notes are
   not hashed. *)

let rules : Candidate.Rule.t list =
  let taker : Candidate.Rule.t =
    { id = "A"
    ; execution = Taker_at_ask
    ; side = No
    ; horizon = H1
    ; lo = 90.
    ; hi = 95.
    ; max_spread = Some 5.
    ; contracts = 100.
    }
  in
  let maker fill id : Candidate.Rule.t =
    { taker with id; execution = Maker_bid fill; max_spread = None; contracts = 1. }
  in
  [ taker; maker Through "B"; maker At_or_through "B-loose" ]
;;

let stop_rule =
  "Each rule is checked after every forward run. It stops at the first check where its \
   95% event-clustered return CI (needs n >= min_n fills) is entirely above or below \
   zero, or when the forward window of forward_days ends; otherwise it is inconclusive. \
   Checking repeatedly inflates the false-positive rate above 5%."
;;

let config ~(study : Frozen.t) ~forward_start ~forward_days : Candidate.Config.t =
  { study_hash = study.hash
  ; in_sample_since = study.config.data_since
  ; in_sample_cutoff = study.config.cutoff
  ; forward_start
  ; forward_days
  ; min_volume = 500.
  ; anchor = study.config.anchor
  ; min_n = study.config.min_n
  ; bootstrap_resamples = study.config.bootstrap_resamples
  ; seed = study.config.seed
  ; stop_rule
  }
;;

module Hashed = struct
  type t =
    { config : Candidate.Config.t
    ; rules : Candidate.Rule.t list
    ; in_sample : Candidate.Score.t list
    }
  [@@deriving yojson]
end

let compute_hash ~config ~rules ~in_sample =
  Hashed.yojson_of_t { config; rules; in_sample }
  |> Yojson.Safe.to_string
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
;;

let create ~frozen_at ~config ~rules ~in_sample ~notes : Candidate.t =
  { frozen_at
  ; config
  ; rules
  ; in_sample
  ; hash = compute_hash ~config ~rules ~in_sample
  ; notes
  }
;;

let to_string t = Yojson.Safe.pretty_to_string (Candidate.yojson_of_t t) ^ "\n"

let of_string s =
  let open Or_error.Let_syntax in
  let%bind (t : Candidate.t) =
    Or_error.try_with (fun () -> Candidate.t_of_yojson (Yojson.Safe.from_string s))
    |> Or_error.tag ~tag:"parsing the candidate file"
  in
  let expected = compute_hash ~config:t.config ~rules:t.rules ~in_sample:t.in_sample in
  if String.equal expected t.hash
  then Ok t
  else
    Or_error.error_s
      [%message
        "the candidate file does not match its hash; it was edited after it was locked"
          ~stored:(t.hash : string)
          ~recomputed:(expected : string)]
;;

let%expect_test "hash covers config, rules and in-sample numbers, not notes" =
  let config : Candidate.Config.t =
    { study_hash = "abc"
    ; in_sample_since = "2025-05-16"
    ; in_sample_cutoff = "2026-05-16"
    ; forward_start = "2026-11-17"
    ; forward_days = 60
    ; min_volume = 500.
    ; anchor = Schedule
    ; min_n = 30
    ; bootstrap_resamples = 1000
    ; seed = 1
    ; stop_rule
    }
  in
  let t = create ~frozen_at:"now" ~config ~rules ~in_sample:[] ~notes:[ "x" ] in
  let ok t = Or_error.is_ok (of_string (to_string t)) in
  printf
    "%b %b %b %b\n"
    (ok t)
    (ok { t with notes = [ "y" ] })
    (ok { t with config = { config with forward_days = 90 } })
    (ok { t with rules = List.tl_exn rules });
  [%expect {| true true false false |}]
;;
