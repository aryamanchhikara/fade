open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Fade_types

(* Hashing and verifying rules/model_rule.json, like the other frozen files: SHA-256
   over the canonical JSON of the config (including each model file's own SHA-256) and
   the backtest numbers known when it was frozen. Notes are not hashed. *)

module Hashed = struct
  type t =
    { config : Model_rule.Config.t
    ; backtest : (string * Backtest_report.Summary.t) list
    }
  [@@deriving yojson]
end

let sha256_hex s = Digestif.SHA256.digest_string s |> Digestif.SHA256.to_hex

let compute_hash ~config ~backtest =
  sha256_hex (Yojson.Safe.to_string (Hashed.yojson_of_t { config; backtest }))
;;

let create ~frozen_at ~config ~backtest ~notes : Model_rule.t =
  { frozen_at; config; backtest; hash = compute_hash ~config ~backtest; notes }
;;

let to_string t = Yojson.Safe.pretty_to_string (Model_rule.yojson_of_t t) ^ "\n"

let of_string s =
  let open Or_error.Let_syntax in
  let%bind (t : Model_rule.t) =
    Or_error.try_with (fun () -> Model_rule.t_of_yojson (Yojson.Safe.from_string s))
    |> Or_error.tag ~tag:"parsing the model rule"
  in
  let expected = compute_hash ~config:t.config ~backtest:t.backtest in
  if String.equal expected t.hash
  then Ok t
  else
    Or_error.error_s
      [%message
        "the model rule does not match its hash; it was edited after it was frozen"
          ~stored:(t.hash : string)
          ~recomputed:(expected : string)]
;;

(* A model file's contents must still be what was frozen. *)
let check_file ~read (f : Model_rule.Model_file.t) =
  let actual = sha256_hex (read f.path) in
  if String.equal actual f.sha256
  then Ok ()
  else
    Or_error.error_s
      [%message
        "model file changed since the rule was frozen" f.path ~expected:f.sha256 actual]
;;
