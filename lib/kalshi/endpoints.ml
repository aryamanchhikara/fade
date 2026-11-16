open! Core
open! Async
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Fade_types

(* Raw response shapes. [@@yojson.allow_extra_fields] ignores fields we don't model,
   so new API fields don't break decoding. Prices arrive as dollar strings ("0.5600")
   and counts as fixed-point strings ("10.00"). *)

module Cutoff = struct
  type t =
    { market_settled_ts : string
    ; trades_created_ts : string
    }
  [@@deriving sexp, yojson] [@@yojson.allow_extra_fields]
end

let decode name of_yojson json =
  Or_error.try_with (fun () -> of_yojson json) |> Or_error.tag ~tag:("decoding " ^ name)
;;

let ts_exn s =
  match Unix_time.of_rfc3339 s with
  | Some t -> t
  | None -> raise_s [%message "bad timestamp" (s : string)]
;;

let cutoff client =
  let%map json = Client.get_json client "/historical/cutoff" in
  Or_error.bind json ~f:(decode "cutoff" Cutoff.t_of_yojson)
;;
