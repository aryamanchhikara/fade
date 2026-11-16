open! Core
open Js_of_ocaml

(* Parse JSON with the browser's native JSON.parse, then convert the JS value into a
   [Yojson.Safe.t] so the shared [t_of_yojson] decoders work unchanged.

   Why not [Yojson.Safe.from_string]? Its lexer is fine natively, but under
   js_of_ocaml every token goes through OCaml byte buffers that are converted to JS
   strings, which makes a 2MB report take minutes. JSON.parse takes milliseconds. *)

let rec to_yojson (v : Js.Unsafe.any) : Yojson.Safe.t =
  match Js.to_string (Js.typeof v) with
  | "string" -> `String (Js.to_string (Js.Unsafe.coerce v : Js.js_string Js.t))
  | "boolean" -> `Bool (Js.to_bool (Js.Unsafe.coerce v : bool Js.t))
  | "number" ->
    let f = Js.float_of_number (Js.Unsafe.coerce v : Js.number Js.t) in
    (* JSON has one number type; integral values that fit become [`Int] (decoders for
       float fields accept [`Int] too). *)
    if Float.is_integer f && Float.( < ) (Float.abs f) 1e9
    then `Int (Float.to_int f)
    else `Float f
  | "object" ->
    if Js.Unsafe.equals v Js.null
    then `Null
    else if Js.to_bool (Js.Unsafe.fun_call (Js.Unsafe.js_expr "Array.isArray") [| v |])
    then (
      let arr : Js.Unsafe.any Js.js_array Js.t = Js.Unsafe.coerce v in
      `List (List.init arr##.length ~f:(fun i -> to_yojson (Js.Unsafe.get arr i))))
    else (
      let keys = Js.to_array (Js.object_keys (Js.Unsafe.coerce v : < > Js.t)) in
      `Assoc
        (Array.to_list keys
         |> List.map ~f:(fun k -> Js.to_string k, to_yojson (Js.Unsafe.get v k))))
  | _ -> `Null
;;

let parse (s : string) : Yojson.Safe.t = to_yojson (Js._JSON##parse (Js.string s))
