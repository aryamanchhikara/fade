open! Core
open Fade_types

(* Build every table: sample x view x side x horizon x segment, each a list of price
   buckets. Splits are one dimension at a time. *)

let tercile ~bounds volume =
  match bounds with
  | [ b1; b2 ] ->
    if Float.( < ) volume b1
    then Some 1
    else if Float.( < ) volume b2
    then Some 2
    else Some 3
  | _ -> None
;;

let segments ~(config : Config.t) (m : Market.t) : Segment.t list =
  [ Some Segment.All
  ; Some (Category m.category)
  ; Option.map (tercile ~bounds:config.volume_tercile_bounds m.volume) ~f:(fun k ->
      Segment.Volume_tercile k)
  ; Some (Event_kind m.event_kind)
  ; Some (Close_kind m.close_kind)
  ]
  |> List.filter_opt
;;

(* Volume tercile bounds from in-sample markets only, so the out-of-time split uses
   thresholds fixed in advance. *)
let tercile_bounds volumes =
  let a = Array.of_list volumes in
  Array.sort a ~compare:Float.compare;
  if Array.length a < 3
  then []
  else [ Stats.percentile a (1. /. 3.); Stats.percentile a (2. /. 3.) ]
;;

module Group_key = struct
  type t =
    { key : Table.Key.t
    ; lo : float
    }
  [@@deriving sexp, compare, equal, hash]
end

(* A per-group generator seeded from the group itself, so results do not depend on the
   order tables are built in. *)
let rng ~(config : Config.t) (g : Group_key.t) =
  Random.State.make [| config.seed; Group_key.hash g |]
;;

let build
      ?(bootstrap = true)
      ~(config : Config.t)
      ~(split : Split.t)
      ~(observations : (View.t * Observation.t list) list)
      ()
  : Table.t list
  =
  let groups = Hashtbl.create (module Group_key) in
  List.iter observations ~f:(fun (view, obs) ->
    List.iter obs ~f:(fun (o : Observation.t) ->
      match Split.sample split o.market with
      | None -> ()
      | Some sample ->
        let lo = Bucket.lo_of ~width:config.bucket_width o.price in
        List.iter (segments ~config o.market) ~f:(fun segment ->
          let key =
            { Table.Key.sample; view; side = o.side; horizon = o.horizon; segment }
          in
          Hashtbl.add_multi groups ~key:{ Group_key.key; lo } ~data:o)));
  let results =
    Hashtbl.to_alist groups
    |> List.map ~f:(fun ((g : Group_key.t), obs) ->
      let rng = if bootstrap then Some (rng ~config g) else None in
      g.key, Bucket.result ?rng ~config ~lo:g.lo obs)
  in
  List.map results ~f:(fun (k, b) -> k, b)
  |> List.sort_and_group ~compare:(fun (a, _) (b, _) -> Table.Key.compare a b)
  |> List.map ~f:(fun group ->
    let key = fst (List.hd_exn group) in
    { Table.key
    ; buckets =
        List.map group ~f:snd
        |> List.sort ~compare:(fun (a : Bucket_result.t) b -> Float.compare a.lo b.lo)
    })
;;

let find tables (key : Table.Key.t) =
  List.find tables ~f:(fun (t : Table.t) -> Table.Key.equal t.key key)
;;

let find_bucket tables (key : Table.Key.t) ~lo =
  Option.bind (find tables key) ~f:(fun t ->
    List.find t.buckets ~f:(fun b -> Float.equal b.lo lo))
;;
