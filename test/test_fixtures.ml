open! Core
open Fade_types

let load () = Fade_synth.of_json_string (In_channel.read_all "fixtures/synthetic.json")

let%expect_test "fixture file is exactly what the generator produces" =
  let on_disk = In_channel.read_all "fixtures/synthetic.json" in
  let fresh = Fade_synth.to_json_string (Fade_synth.generate ()) in
  print_s [%sexp (String.equal on_disk fresh : bool)];
  [%expect {| true |}]
;;

let%expect_test "fixture shape" =
  let d = load () in
  let count_by f xs =
    List.map xs ~f
    |> List.sort_and_group ~compare:String.compare
    |> List.map ~f:(fun g -> List.hd_exn g, List.length g)
  in
  print_s
    [%message
      ""
        ~markets:(List.length d.markets : int)
        ~events:
          (List.map d.markets ~f:(fun m -> m.event_ticker)
           |> List.dedup_and_sort ~compare:String.compare
           |> List.length
           : int)
        ~snapshots:(List.length d.snapshots : int)
        ~trades:(List.length d.trades : int)
        ~kinds:
          (count_by (fun (m : Market.t) -> Event_kind.to_string m.event_kind) d.markets
           : (string * int) list)
        ~yes_rate:
          (let yes = List.count d.markets ~f:(fun m -> Side.equal m.result Yes) in
           Float.round_decimal
             ~decimal_digits:3
             (Float.of_int yes /. Float.of_int (List.length d.markets))
           : float)];
  [%expect
    {|
    ((markets 2590) (events 900) (snapshots 7087) (trades 5223)
     (kinds
      (("bracket event" 1728) ("multi-market event" 517) ("single market" 345)))
     (yes_rate 0.315))
    |}]
;;

let%expect_test "every bracket event has exactly one YES" =
  let d = load () in
  let bad =
    List.filter d.markets ~f:(fun m -> Event_kind.equal m.event_kind Bracket)
    |> List.sort_and_group ~compare:(fun (a : Market.t) b ->
      String.compare a.event_ticker b.event_ticker)
    |> List.count ~f:(fun ms -> List.count ms ~f:(fun m -> Side.equal m.result Yes) <> 1)
  in
  print_s [%sexp (bad : int)];
  [%expect {| 0 |}]
;;

let%expect_test "types round-trip through JSON and sexp" =
  let d = load () in
  let m = List.hd_exn d.markets in
  let via_json = Market.t_of_yojson (Market.yojson_of_t m) in
  let via_sexp = Market.t_of_sexp (Market.sexp_of_t m) in
  print_s [%sexp (Market.equal m via_json && Market.equal m via_sexp : bool)];
  let s = List.hd_exn d.snapshots in
  print_s [%sexp (s : Snapshot.t)];
  print_s
    [%sexp
      { no_ask = (Snapshot.ask s No : float option)
      ; no_bid = (Snapshot.bid s No : float option)
      }];
  [%expect
    {|
    true
    ((ticker SYNCRY1-0009-M0) (horizon H24) (ts 1747803600) (yes_bid (65))
     (yes_ask (67)))
    ((no_ask (35)) (no_bid (33)))
    |}]
;;
