open! Core
open! Async
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Fade_types

(* Raw response shapes, decoded with ppx_yojson_conv. [@@yojson.allow_extra_fields]
   ignores fields we don't model, so new API fields don't break decoding. A field
   typed [_ option] with [@default None] may be missing or null. Prices arrive as
   dollar strings ("0.5600") and counts as fixed-point strings ("10.00"). *)

module Cutoff = struct
  type t =
    { market_settled_ts : string
    ; trades_created_ts : string
    }
  [@@deriving sexp, yojson] [@@yojson.allow_extra_fields]
end

module Market_raw = struct
  type t =
    { ticker : string
    ; event_ticker : string
    ; market_type : string
    ; created_time : string
    ; open_time : string
    ; close_time : string
    ; settlement_ts : string option [@default None]
    ; result : string
    ; volume_fp : string
    ; can_close_early : bool [@default false]
    ; expected_expiration_time : string option [@default None]
    ; latest_expiration_time : string option [@default None]
    ; occurrence_datetime : string option [@default None]
    }
  [@@deriving sexp, yojson] [@@yojson.allow_extra_fields]
end

module Markets_page = struct
  type t =
    { markets : Market_raw.t list
    ; cursor : string [@default ""]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Event_raw = struct
  type t =
    { event_ticker : string
    ; series_ticker : string
    ; mutually_exclusive : bool [@default false]
    ; title : string [@default ""]
    }
  [@@deriving sexp, yojson] [@@yojson.allow_extra_fields]
end

module Events_page = struct
  type t =
    { events : Event_raw.t list
    ; cursor : string [@default ""]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Series_raw = struct
  type t =
    { ticker : string
    ; category : string [@default ""]
    ; fee_type : string
    ; fee_multiplier : float
    }
  [@@deriving sexp, yojson] [@@yojson.allow_extra_fields]
end

module Series_list = struct
  type t = { series : Series_raw.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Series_fee_change_raw = struct
  type t =
    { id : string
    ; series_ticker : string
    ; fee_type : string
    ; fee_multiplier : float
    ; scheduled_ts : string
    }
  [@@deriving sexp, yojson] [@@yojson.allow_extra_fields]
end

module Series_fee_changes = struct
  type t = { series_fee_change_arr : Series_fee_change_raw.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Event_fee_change_raw = struct
  type t =
    { id : string
    ; event_ticker : string
    ; fee_type_override : string option [@default None]
    ; fee_multiplier_override : float option [@default None]
    ; scheduled_ts : string
    }
  [@@deriving sexp, yojson] [@@yojson.allow_extra_fields]
end

module Event_fee_changes_page = struct
  type t =
    { event_fee_changes : Event_fee_change_raw.t list
    ; cursor : string [@default ""]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

(* Historical candlesticks name their OHLC fields [close]; live ones [close_dollars]. *)
module Ohlc_hist = struct
  type t = { close : string option [@default None] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Candle_hist = struct
  type t =
    { end_period_ts : int
    ; yes_bid : Ohlc_hist.t
    ; yes_ask : Ohlc_hist.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Candles_hist = struct
  type t = { candlesticks : Candle_hist.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Ohlc_live = struct
  type t = { close_dollars : string option [@default None] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Candle_live = struct
  type t =
    { end_period_ts : int
    ; yes_bid : Ohlc_live.t
    ; yes_ask : Ohlc_live.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Market_candles_live = struct
  type t =
    { market_ticker : string
    ; candlesticks : Candle_live.t list
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Batch_candles = struct
  type t = { markets : Market_candles_live.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Trade_raw = struct
  type t =
    { trade_id : string
    ; ticker : string
    ; count_fp : string
    ; yes_price_dollars : string
    ; taker_outcome_side : string
    ; created_time : string
    ; is_block_trade : bool [@default false]
    }
  [@@deriving sexp, yojson] [@@yojson.allow_extra_fields]
end

module Trades_page = struct
  type t =
    { trades : Trade_raw.t list
    ; cursor : string [@default ""]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

(* Conversions shared by the fetcher. *)

let ts_exn s =
  match Unix_time.of_rfc3339 s with
  | Some t -> t
  | None -> raise_s [%message "bad timestamp" (s : string)]
;;

(* "0.5600" dollars -> 56. cents, rounded to 1/10000 of a cent to shed float noise. *)
let cents_of_dollars s = Float.round_decimal ~decimal_digits:4 (Float.of_string s *. 100.)

(* A YES bid of 0 means nobody is bidding; a YES ask of 1.00 means nobody is offering. *)
let bid_of_string = function
  | None -> None
  | Some s ->
    let c = cents_of_dollars s in
    if Float.( <= ) c 0. then None else Some c
;;

let ask_of_string = function
  | None -> None
  | Some s ->
    let c = cents_of_dollars s in
    if Float.( >= ) c 100. then None else Some c
;;

let candle_of_hist (c : Candle_hist.t) : Fade_types.Candle.t =
  { end_ts = c.end_period_ts
  ; yes_bid = bid_of_string c.yes_bid.close
  ; yes_ask = ask_of_string c.yes_ask.close
  }
;;

let candle_of_live (c : Candle_live.t) : Fade_types.Candle.t =
  { end_ts = c.end_period_ts
  ; yes_bid = bid_of_string c.yes_bid.close_dollars
  ; yes_ask = ask_of_string c.yes_ask.close_dollars
  }
;;

let trade_of_raw (r : Trade_raw.t) : Trade.t option =
  match Side.of_kalshi_string r.taker_outcome_side with
  | None -> None
  | Some taker_side ->
    Some
      { trade_id = r.trade_id
      ; ticker = r.ticker
      ; ts = ts_exn r.created_time
      ; yes_price = cents_of_dollars r.yes_price_dollars
      ; count = Float.of_string r.count_fp
      ; taker_side
      ; is_block = r.is_block_trade
      }
;;

(* Endpoint calls. Each returns [Or_error]: decode failures carry the endpoint name. *)

let decode name of_yojson json =
  Or_error.try_with (fun () -> of_yojson json) |> Or_error.tag ~tag:("decoding " ^ name)
;;

let get client ?query path name of_yojson =
  let%map json = Client.get_json client ?query path in
  Or_error.bind json ~f:(decode name of_yojson)
;;

let cutoff client = get client "/historical/cutoff" "cutoff" Cutoff.t_of_yojson
let opt_cursor cursor = if String.is_empty cursor then [] else [ "cursor", cursor ]

let historical_markets client ~cursor =
  get
    client
    "/historical/markets"
    "historical markets"
    Markets_page.t_of_yojson
    ~query:([ "limit", "1000"; "mve_filter", "exclude" ] @ opt_cursor cursor)
;;

let settled_markets client ~min_settled_ts ~cursor =
  get
    client
    "/markets"
    "markets"
    Markets_page.t_of_yojson
    ~query:
      ([ "limit", "1000"
       ; "status", "settled"
       ; "mve_filter", "exclude"
       ; "min_settled_ts", Int.to_string min_settled_ts
       ]
       @ opt_cursor cursor)
;;

let events client ~tickers =
  get
    client
    "/events"
    "events"
    Events_page.t_of_yojson
    ~query:[ "tickers", String.concat ~sep:"," tickers; "limit", "200" ]
;;

let series client = get client "/series" "series" Series_list.t_of_yojson

let series_fee_changes client =
  get
    client
    "/series/fee_changes"
    "series fee changes"
    Series_fee_changes.t_of_yojson
    ~query:[ "show_historical", "true" ]
;;

let event_fee_changes client ~cursor =
  get
    client
    "/events/fee_changes"
    "event fee changes"
    Event_fee_changes_page.t_of_yojson
    ~query:([ "limit", "1000" ] @ opt_cursor cursor)
;;

let historical_candles client ~ticker ~start_ts ~end_ts =
  let%map r =
    get
      client
      (sprintf "/historical/markets/%s/candlesticks" ticker)
      "historical candlesticks"
      Candles_hist.t_of_yojson
      ~query:
        [ "start_ts", Int.to_string start_ts
        ; "end_ts", Int.to_string end_ts
        ; "period_interval", "60"
        ]
  in
  Or_error.map r ~f:(fun r -> List.map r.candlesticks ~f:candle_of_hist)
;;

let batch_candles client ~tickers ~start_ts ~end_ts =
  let%map r =
    get
      client
      "/markets/candlesticks"
      "batch candlesticks"
      Batch_candles.t_of_yojson
      ~query:
        [ "market_tickers", String.concat ~sep:"," tickers
        ; "start_ts", Int.to_string start_ts
        ; "end_ts", Int.to_string end_ts
        ; "period_interval", "60"
        ]
  in
  Or_error.map r ~f:(fun r ->
    List.map r.markets ~f:(fun m ->
      m.market_ticker, List.map m.candlesticks ~f:candle_of_live))
;;

let trades client ~historical ~ticker ~cursor =
  get
    client
    (if historical then "/historical/trades" else "/markets/trades")
    "trades"
    Trades_page.t_of_yojson
    ~query:([ "ticker", ticker; "limit", "1000" ] @ opt_cursor cursor)
;;
