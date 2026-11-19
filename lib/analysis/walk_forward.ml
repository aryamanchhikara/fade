open! Core
open Fade_types

(* Walk-forward splits by settlement month, grouped by event.

   Each event is placed by its last settlement and its first decision. For a test month
   [s, e):
   - test: events whose first decision is at or after s and that settle before e;
   - train: events that settled before s - embargo;
   - validation (for choosing the baseline and hyperparameters): the last
     [validation_months] of the training period, split the same way, with its own
     training rows ([fit]) settled before its start - embargo.
   An event that straddles a boundary is in neither side of it, so no event is ever in
   both train and test. The test month is never used to choose anything. *)

type 'a fold =
  { month : string (** "2026-03" *)
  ; train : 'a list
  ; fit : 'a list (** train rows before the validation window *)
  ; validation : 'a list
  ; test : 'a list
  }

let month_start ~year ~month =
  Unix_time.of_date_string_exn (sprintf "%04d-%02d-01" year month)
;;

(* (year, month) [k] months after [year]-[month]. *)
let add_months ~year ~month k =
  let i = (year * 12) + (month - 1) + k in
  i / 12, (i mod 12) + 1
;;

let parse_month s =
  match String.split s ~on:'-' with
  | [ y; m ] -> Int.of_string y, Int.of_string m
  | _ -> raise_s [%message "expected YYYY-MM" s]
;;

let folds
      ~first_test
      ~last_test
      ~embargo_days
      ~validation_months
      ~(event : 'a -> string)
      ~(settle : 'a -> Unix_time.t)
      ~(decision : 'a -> Unix_time.t)
      (rows : 'a list)
  =
  let by_event = String.Table.create () in
  List.iter rows ~f:(fun r ->
    Hashtbl.update by_event (event r) ~f:(fun prev ->
      let s, d = Option.value prev ~default:(Int.min_value, Int.max_value) in
      Int.max s (settle r), Int.min d (decision r)));
  let span r = Hashtbl.find_exn by_event (event r) in
  let embargo = embargo_days * 86_400 in
  let settled_before t r = fst (span r) < t in
  let inside ~lo ~hi r =
    let s, d = span r in
    d >= lo && s < hi
  in
  let y0, m0 = parse_month first_test in
  let y1, m1 = parse_month last_test in
  let n = (y1 * 12) + m1 - ((y0 * 12) + m0) + 1 in
  List.init n ~f:(fun k ->
    let year, month = add_months ~year:y0 ~month:m0 k in
    let s = month_start ~year ~month in
    let e =
      let year, month = add_months ~year ~month 1 in
      month_start ~year ~month
    in
    let v =
      let year, month = add_months ~year ~month (-validation_months) in
      month_start ~year ~month
    in
    { month = sprintf "%04d-%02d" year month
    ; train = List.filter rows ~f:(settled_before (s - embargo))
    ; fit = List.filter rows ~f:(settled_before (v - embargo))
    ; validation = List.filter rows ~f:(inside ~lo:v ~hi:(s - embargo))
    ; test = List.filter rows ~f:(inside ~lo:s ~hi:e)
    })
;;

let%expect_test "no event is in both train and test; straddlers are dropped" =
  let day = 86_400 in
  let t date = Unix_time.of_date_string_exn date in
  (* (event, decision, settle) *)
  let rows =
    [ "jan", t "2026-01-10", t "2026-01-11"
    ; "feb-a", t "2026-02-03", t "2026-02-04"
    ; "feb-b", t "2026-02-27", t "2026-02-28" (* inside the embargo before March *)
    ; "straddle", t "2026-02-28", t "2026-03-02"
    ; "mar", t "2026-03-05", t "2026-03-06"
    ; "mar", t "2026-03-04", t "2026-03-06"
    ; "apr", t "2026-04-02", t "2026-04-03"
    ]
  in
  let fs =
    folds
      ~first_test:"2026-03"
      ~last_test:"2026-04"
      ~embargo_days:2
      ~validation_months:1
      ~event:(fun (e, _, _) -> e)
      ~decision:(fun (_, d, _) -> d)
      ~settle:(fun (_, _, s) -> s)
      rows
  in
  ignore day;
  List.iter fs ~f:(fun f ->
    let names l =
      List.map l ~f:(fun (e, _, _) -> e)
      |> List.dedup_and_sort ~compare:String.compare
      |> String.concat ~sep:","
    in
    printf
      "%s train=[%s] fit=[%s] validation=[%s] test=[%s]\n"
      f.month
      (names f.train)
      (names f.fit)
      (names f.validation)
      (names f.test));
  [%expect
    {|
    2026-03 train=[feb-a,jan] fit=[jan] validation=[feb-a] test=[mar]
    2026-04 train=[feb-a,feb-b,jan,mar,straddle] fit=[feb-a,jan] validation=[mar] test=[apr]
    |}]
;;
