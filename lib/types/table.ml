open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Key = struct
  type t =
    { sample : Sample.t
    ; view : View.t
    ; side : Side.t
    ; horizon : Horizon.t
    ; segment : Segment.t
    }
  [@@deriving sexp, yojson, compare, equal, hash]
end

type t =
  { key : Key.t
  ; buckets : Bucket_result.t list
  }
[@@deriving sexp, yojson, compare, equal]
