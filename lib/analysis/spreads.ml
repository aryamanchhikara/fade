open! Core
open Fade_types

(* How wide the book was at one horizon, by category: the share of quoted markets with
   only one side quoted, and of two-sided books wider than [max_spread] cents. *)

module Row = struct
  type t =
    { category : string
    ; quoted : int (** markets with a quote at the horizon *)
    ; one_sided : int (** no bid or no ask *)
    ; wide : int (** two-sided, spread > max_spread *)
    }
  [@@deriving sexp_of]

  let two_sided t = t.quoted - t.one_sided
  let share a b = if b = 0 then 0. else Float.of_int a /. Float.of_int b

  (* Not tradeable at a tight spread: one-sided, or two-sided but wide. *)
  let not_tight t = share (t.one_sided + t.wide) t.quoted
end

let by_category ~max_spread ~horizon (d : Dataset.t) =
  let markets = Observation.market_index d in
  let rows = String.Table.create () in
  let bump category ~one_sided ~wide =
    Hashtbl.update rows category ~f:(fun prev ->
      let r =
        Option.value prev ~default:{ Row.category; quoted = 0; one_sided = 0; wide = 0 }
      in
      { r with
        quoted = r.quoted + 1
      ; one_sided = (r.one_sided + if one_sided then 1 else 0)
      ; wide = (r.wide + if wide then 1 else 0)
      })
  in
  List.iter d.snapshots ~f:(fun (s : Snapshot.t) ->
    match Hashtbl.find markets s.ticker with
    | Some m when Horizon.equal s.horizon horizon ->
      let one_sided, wide =
        match Candidate_score.spread s with
        | None -> true, false
        | Some sp -> false, Float.( > ) sp max_spread
      in
      bump m.category ~one_sided ~wide;
      bump "All categories" ~one_sided ~wide
    | _ -> ());
  Hashtbl.data rows
  |> List.sort ~compare:(fun (a : Row.t) b ->
    match
      String.equal a.category "All categories", String.equal b.category "All categories"
    with
    | true, false -> -1
    | false, true -> 1
    | _ -> Int.compare b.quoted a.quoted)
;;

let%expect_test "one-sided and wide books by category" =
  let market ticker category : Market.t =
    { ticker
    ; event_ticker = ticker
    ; series_ticker = "S"
    ; category
    ; open_time = 0
    ; close_time = 10
    ; settle_time = 10
    ; result = No
    ; volume = 500.
    ; can_close_early = false
    ; expected_expiration = None
    ; close_kind = Scheduled
    ; event_kind = Single
    }
  in
  let snap ticker ?bid ?ask () : Snapshot.t =
    { ticker; horizon = H1; ts = 0; yes_bid = bid; yes_ask = ask }
  in
  let d : Dataset.t =
    { markets = [ market "a" "Sports"; market "b" "Sports"; market "c" "Crypto" ]
    ; snapshots =
        [ snap "a" ~bid:40. ~ask:43. ()
        ; snap "b" ~bid:40. ~ask:60. ()
        ; snap "c" ~ask:5. ()
        ; { (snap "a" ~bid:1. ~ask:99. ()) with horizon = H6 }
        ]
    ; trades = []
    ; fees = { series = []; event_overrides = [] }
    }
  in
  List.iter (by_category ~max_spread:5. ~horizon:H1 d) ~f:(fun r ->
    printf
      "%-15s quoted %d, one-sided %d, wide %d, not tight %.2f\n"
      r.category
      r.quoted
      r.one_sided
      r.wide
      (Row.not_tight r));
  [%expect
    {|
    All categories  quoted 3, one-sided 1, wide 1, not tight 0.67
    Sports          quoted 2, one-sided 0, wide 1, not tight 0.50
    Crypto          quoted 1, one-sided 1, wide 0, not tight 1.00
    |}]
;;
