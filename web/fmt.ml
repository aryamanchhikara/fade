open! Core
open Fade_types

(* Number formatting shared by the chart, the table and the CSV. *)

let pct ?(digits = 1) x = sprintf "%.*f%%" digits (100. *. x)
let signed_pct ?(digits = 1) x = sprintf "%+.*f%%" digits (100. *. x)
let cents x = sprintf "%.0f¢" x
let fee x = sprintf "%.2f¢" x

let int n =
  (* 1234567 -> "1,234,567" *)
  let s = Int.to_string (abs n) in
  let len = String.length s in
  let b = Buffer.create (len + (len / 3)) in
  String.iteri s ~f:(fun i c ->
    if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char b ',';
    Buffer.add_char b c);
  (if n < 0 then "-" else "") ^ Buffer.contents b
;;

let ci_pct ?(signed = false) (ci : Ci.t) =
  let f = if signed then signed_pct ?digits:None else pct ?digits:None in
  sprintf "%s to %s" (f ci.lo) (f ci.hi)
;;

let return_ci (b : Bucket_result.t) =
  match b.return_ci with
  | Some ci -> ci_pct ~signed:true ci
  | None -> "n/a"
;;

let horizon_phrase (view : View.t) (h : Horizon.t) =
  match view, h with
  | Quotes, h -> sprintf "%s before close" (Horizon.to_string h)
  | Trades _, H1 -> "trades in the last hour"
  | Trades _, H6 -> "trades 1 to 6 hours out"
  | Trades _, H24 -> "trades 6 to 24 hours out"
;;
