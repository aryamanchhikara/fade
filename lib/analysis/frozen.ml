open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Fade_types

(* Rules chosen in-sample and frozen before anything out-of-time is looked at, stored in
   rules/frozen.json. As in edgecheck, the hash is SHA-256 over the canonical JSON of
   the parts that decide results (config and rules); notes are not hashed. Editing a
   hashed field makes [load] refuse the file. *)

type t =
  { frozen_at : string
  ; config : Config.t
  ; rules : Rule.t list
  ; hash : string
  ; examined : int (** in-sample cells with n >= min_n that could have been selected *)
  ; notes : string list (** not hashed *)
  }
[@@deriving sexp, yojson]

module Hashed = struct
  type t =
    { config : Config.t
    ; rules : Rule.t list
    }
  [@@deriving yojson]
end

let compute_hash ~config ~rules =
  Hashed.yojson_of_t { config; rules }
  |> Yojson.Safe.to_string
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
;;

let create ~frozen_at ~config ~rules ~examined ~notes =
  { frozen_at; config; rules; hash = compute_hash ~config ~rules; examined; notes }
;;

let to_string t = Yojson.Safe.pretty_to_string (yojson_of_t t) ^ "\n"

let of_string s =
  let open Or_error.Let_syntax in
  let%bind t =
    Or_error.try_with (fun () -> t_of_yojson (Yojson.Safe.from_string s))
    |> Or_error.tag ~tag:"parsing frozen rules"
  in
  let expected = compute_hash ~config:t.config ~rules:t.rules in
  if String.equal expected t.hash
  then Ok t
  else
    Or_error.error_s
      [%message
        "frozen rules do not match their hash; they were edited after freezing. Restore \
         the file, or freeze again (which starts the out-of-time count over)."
          ~stored:(t.hash : string)
          ~recomputed:(expected : string)]
;;

(* The selection rule: an in-sample cell of the NO-side quote view is selected if it
   has at least [min_n] markets and its whole bootstrap return CI is above zero. *)
let selection_text =
  "in-sample, NO side, taker at quoted ask: every (horizon, segment, 5c bucket) cell \
   with n >= min_n whose 95% event-clustered bootstrap return CI is entirely above zero"
;;

let select ~(in_sample_tables : Table.t list) =
  let candidates =
    List.concat_map in_sample_tables ~f:(fun (t : Table.t) ->
      if
        Sample.equal t.key.sample In_sample
        && View.equal t.key.view Quotes
        && Side.equal t.key.side No
      then List.map t.buckets ~f:(fun b -> t.key, b)
      else [])
  in
  let examined = List.count candidates ~f:(fun (_, b) -> b.enough) in
  let rules =
    List.filter_map candidates ~f:(fun ((k : Table.Key.t), b) ->
      if Bucket_result.proven_positive b
      then
        Some
          { Rule.side = k.side
          ; horizon = k.horizon
          ; segment = k.segment
          ; lo = b.lo
          ; hi = b.hi
          }
      else None)
    |> List.sort ~compare:Rule.compare
  in
  rules, examined
;;

let%expect_test "hash is stable and detects edits" =
  let config : Config.t =
    { data_since = "2025-05-16"
    ; data_until = "2026-11-15"
    ; cutoff = "2026-03-01"
    ; min_n = 30
    ; bucket_width = 5.
    ; contracts_per_order = 100.
    ; bootstrap_resamples = 1000
    ; seed = 1
    ; volume_tercile_bounds = [ 1000.; 5000. ]
    ; anchor = Schedule
    ; selection = selection_text
    }
  in
  let rules = [ { Rule.side = No; horizon = H1; segment = All; lo = 90.; hi = 95. } ] in
  let t = create ~frozen_at:"2026-11-16" ~config ~rules ~examined:12 ~notes:[ "x" ] in
  print_endline t.hash;
  let ok = of_string (to_string t) in
  print_s [%sexp (Or_error.is_ok ok : bool)];
  (* Notes are not hashed: changing them is fine. *)
  print_s
    [%sexp (Or_error.is_ok (of_string (to_string { t with notes = [ "y" ] })) : bool)];
  (* Widening a rule after the fact is caught. *)
  let edited = { t with rules = [ { (List.hd_exn rules) with lo = 85. } ] } in
  print_s [%sexp (Or_error.is_ok (of_string (to_string edited)) : bool)];
  [%expect
    {|
    d2e59a8f67a343e268ab8c65355568b5bb05a40deb0d74d91197acd2ddc1e9c6
    true
    true
    false
    |}]
;;
