(* Every file is a module: this one is [Fade_types.Side]. dune wraps the library so
   callers write [Fade_types.Side.t], or [Side.t] after [open Fade_types].

   [open! Core] swaps OCaml's standard library for Jane Street's Core. The [!]
   silences the "unused open" warning in files that happen not to use it. *)
open! Core

(* Brings [int_of_yojson], [string_of_yojson], ... into scope for [@@deriving yojson]. *)
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* A variant type: a [Side.t] is either [Yes] or [No], nothing else. [@@deriving ...]
   runs ppx code generators at compile time:
   - sexp: [sexp_of_t]/[t_of_sexp] (S-expressions, Jane Street's text format)
   - yojson: [yojson_of_t]/[t_of_yojson] (JSON, used for report.json)
   - compare/equal/hash: structural comparison without polymorphic [=]
   - enumerate: [all : t list], handy for UI dropdowns and loops *)
type t =
  | Yes
  | No
[@@deriving sexp, yojson, compare, equal, hash, enumerate]

let opposite = function
  | Yes -> No
  | No -> Yes
;;

let to_string = function
  | Yes -> "YES"
  | No -> "NO"
;;

let of_kalshi_string = function
  | "yes" -> Some Yes
  | "no" -> Some No
  | _ -> None
;;
