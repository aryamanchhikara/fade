open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Seconds since 1970-01-01 UTC. A plain [int] keeps the type usable from both native
   code and JavaScript. In js_of_ocaml an OCaml [int] is 32 bits, which holds
   timestamps up to January 2038; everything here is 2025 or 2026. *)
type t = int [@@deriving sexp, yojson, compare, equal, hash]

(* ISO-8601 date ("2026-05-16") for a timestamp. Pure arithmetic, so it also works in
   the browser, where [Core.Time_float] would pull in time-zone tables. *)
let to_date_string t =
  let days = t / 86_400 in
  (* Civil-from-days, Howard Hinnant's algorithm. *)
  let z = days + 719_468 in
  let era = (if z >= 0 then z else z - 146_096) / 146_097 in
  let doe = z - (era * 146_097) in
  let yoe = (doe - (doe / 1460) + (doe / 36_524) - (doe / 146_096)) / 365 in
  let y = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = if mp < 10 then mp + 3 else mp - 9 in
  let y = if m <= 2 then y + 1 else y in
  sprintf "%04d-%02d-%02d" y m d
;;

let of_date_string s =
  (* Inverse of [to_date_string]: days-from-civil. *)
  match String.split s ~on:'-' |> List.map ~f:Int.of_string_opt with
  | [ Some y; Some m; Some d ] ->
    let y = if m <= 2 then y - 1 else y in
    let era = (if y >= 0 then y else y - 399) / 400 in
    let yoe = y - (era * 400) in
    let mp = if m > 2 then m - 3 else m + 9 in
    let doy = (((153 * mp) + 2) / 5) + d - 1 in
    let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
    Some (((era * 146_097) + doe - 719_468) * 86_400)
  | _ -> None
;;

let of_date_string_exn s =
  match of_date_string s with
  | Some t -> t
  | None -> raise_s [%message "expected YYYY-MM-DD" (s : string)]
;;

(* Kalshi timestamps look like "2026-09-17T00:00:00Z" or "2026-09-16T23:58:48.396619Z".
   Fractional seconds are dropped. Only UTC ("Z") is accepted, which is all Kalshi
   sends. *)
let of_rfc3339 s =
  match String.lsplit2 s ~on:'T' with
  | None -> None
  | Some (date, rest) ->
    let rest = String.chop_suffix rest ~suffix:"Z" in
    (match of_date_string date, rest with
     | Some day, Some clock ->
       let clock =
         match String.lsplit2 clock ~on:'.' with
         | Some (whole, _) -> whole
         | None -> clock
       in
       (match String.split clock ~on:':' |> List.map ~f:Int.of_string_opt with
        | [ Some h; Some m; Some sec ] -> Some (day + (h * 3600) + (m * 60) + sec)
        | _ -> None)
     | _ -> None)
;;

let to_rfc3339 t =
  let day = t - (t mod 86_400) in
  let secs = t - day in
  sprintf
    "%sT%02d:%02d:%02dZ"
    (to_date_string t)
    (secs / 3600)
    (secs mod 3600 / 60)
    (secs mod 60)
;;
