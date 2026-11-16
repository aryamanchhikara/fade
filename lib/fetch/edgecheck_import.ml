open! Core
open! Async
open Fade_types
module Store = Fade_store

(* `fade import-edgecheck`: reuse the NFL markets that ../betting (edgecheck) already
   cached, so `fade fetch` doesn't have to download them again.

   Read-only by construction: the duckdb CLI opens the file with -readonly and we only
   run COPY (SELECT ...) TO a temp file in our own directory. Existing rows in our cache
   always win (INSERT OR IGNORE): edgecheck only fills gaps. *)

let text s = Sqlite3.Data.TEXT s
let int i = Sqlite3.Data.INT (Int64.of_int i)
let float f = Sqlite3.Data.FLOAT f

let opt f = function
  | None -> Sqlite3.Data.NULL
  | Some x -> f x
;;

(* Export one SELECT as tab-separated rows and read them back. *)
let export ~duckdb ~tmp_dir ~name sql =
  let out = tmp_dir ^/ name ^ ".tsv" in
  let copy =
    sprintf "COPY (%s) TO '%s' (HEADER false, DELIMITER '\t', NULLSTR '')" sql out
  in
  let%map result =
    Process.run ~prog:"duckdb" ~args:[ "-no-agent"; "-readonly"; duckdb; "-c"; copy ] ()
  in
  Or_error.map result ~f:(fun (_ : string) ->
    let rows = In_channel.read_lines out |> List.map ~f:(String.split ~on:'\t') in
    Core_unix.unlink out;
    rows)
;;

let float_opt s = if String.is_empty s then None else Some (Float.of_string s)

let run ~duckdb ~db_path ~trade_sample =
  if not (Sys_unix.file_exists_exn duckdb)
  then return (Or_error.error_s [%message "no edgecheck cache found" (duckdb : string)])
  else (
    let store = Store.open_ db_path in
    let tmp_dir = Filename.dirname db_path ^/ "edgecheck_import" in
    Core_unix.mkdir_p tmp_dir;
    let open Deferred.Or_error.Let_syntax in
    (* 1. Markets, joined with their open/close times. *)
    let%bind markets =
      export
        ~duckdb
        ~tmp_dir
        ~name:"markets"
        "SELECT m.ticker, m.event_ticker, mm.open_ts, mm.close_ts, m.result, \
         m.volume_total FROM markets m JOIN market_meta mm USING (ticker) WHERE m.result \
         IN ('yes', 'no')"
    in
    let added_markets =
      Store.transaction store ~f:(fun () ->
        List.fold markets ~init:0 ~f:(fun added row ->
          match row with
          | [ ticker; event_ticker; open_ts; close_ts; result; volume ] ->
            Store.exec
              store
              "INSERT OR IGNORE INTO listed(ticker, event_ticker) VALUES (?,?)"
              [ text ticker; text event_ticker ];
            Store.exec
              store
              "INSERT OR IGNORE INTO markets(ticker, event_ticker, open_ts, close_ts, \
               settle_ts, result, volume, can_close_early, source, sample_key) VALUES \
               (?,?,?,?,?,?,?,1,'edgecheck',?)"
              [ text ticker
              ; text event_ticker
              ; int (Int.of_string open_ts)
              ; int (Int.of_string close_ts)
              ; int (Int.of_string close_ts)
              ; text result
              ; float (Option.value (float_opt volume) ~default:0.)
              ; int (Fetch.sample_key ticker)
              ];
            (* [changes()] is 1 if the INSERT OR IGNORE actually inserted. *)
            added + Store.count store "SELECT changes()" []
          | _ -> added))
    in
    (* 2. Hourly candles -> snapshots, for markets whose candles we don't have yet. *)
    let%bind candles =
      export
        ~duckdb
        ~tmp_dir
        ~name:"candles"
        "SELECT ticker, end_ts, bid, ask FROM candles WHERE period = 60 ORDER BY ticker, \
         end_ts"
    in
    let times =
      List.filter_map markets ~f:(function
        | [ ticker; _; open_ts; close_ts; _; _ ] ->
          Some (ticker, (Int.of_string open_ts, Int.of_string close_ts))
        | _ -> None)
      |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
    in
    let by_ticker =
      List.filter_map candles ~f:(function
        | [ ticker; end_ts; bid; ask ] ->
          let cents s =
            Option.map (float_opt s) ~f:(fun d ->
              Float.round_decimal ~decimal_digits:4 (d *. 100.))
          in
          let yes_bid = Option.filter (cents bid) ~f:(fun c -> Float.( > ) c 0.) in
          let yes_ask = Option.filter (cents ask) ~f:(fun c -> Float.( < ) c 100.) in
          Some
            (ticker, { Fade_types.Candle.end_ts = Int.of_string end_ts; yes_bid; yes_ask })
        | _ -> None)
      |> String.Map.of_alist_multi
    in
    let added_candles =
      Store.transaction store ~f:(fun () ->
        Map.fold by_ticker ~init:0 ~f:(fun ~key:ticker ~data:candles added ->
          let already =
            Store.count
              store
              "SELECT COUNT(*) FROM candles_done WHERE ticker = ?"
              [ text ticker ]
            > 0
          in
          (* Edgecheck often stopped recording at kickoff. Only trust its candles if
             they reach the last two hours, otherwise the 1h horizon would be missing
             for exactly these markets; `fade fetch` gets the full window instead. *)
          let covers_close close_time =
            List.exists candles ~f:(fun c -> c.end_ts >= close_time - 7200)
          in
          match Map.find times ticker with
          | Some (open_time, close_time) when (not already) && covers_close close_time ->
            let start, _ = Fade_analysis.Snapshots.window ~open_time ~close_time () in
            List.iter candles ~f:(fun (c : Fade_types.Candle.t) ->
              if c.end_ts >= start && c.end_ts <= close_time
              then
                Store.exec
                  store
                  "INSERT OR IGNORE INTO candles(ticker, end_ts, yes_bid, yes_ask) \
                   VALUES (?,?,?,?)"
                  [ text ticker; int c.end_ts; opt float c.yes_bid; opt float c.yes_ask ]);
            Store.exec
              store
              "INSERT OR IGNORE INTO candles_done(ticker, n_candles) VALUES (?,?)"
              [ text ticker; int (List.length candles) ];
            added + 1
          | _ -> added))
    in
    (* 3. Trades, only for markets inside fade's random trade sample, so the import
       doesn't tilt the trade view toward NFL. *)
    let sample =
      Store.rows
        store
        "SELECT ticker FROM markets ORDER BY sample_key LIMIT ?"
        [ int trade_sample ]
      |> List.filter_map ~f:(function
        | [| Sqlite3.Data.TEXT t |] -> Some t
        | _ -> None)
    in
    let sample_file = tmp_dir ^/ "sample.csv" in
    Out_channel.write_lines sample_file ("ticker" :: sample);
    let%bind trades =
      export
        ~duckdb
        ~tmp_dir
        ~name:"trades"
        (sprintf
           "SELECT t.trade_id, t.ticker, t.created_ts, t.yes_price, t.count, \
            t.taker_side FROM trades t WHERE t.ticker IN (SELECT ticker FROM \
            read_csv('%s', header = true))"
           sample_file)
    in
    let fetched_already =
      Store.rows store "SELECT ticker FROM trades_done" []
      |> List.filter_map ~f:(function
        | [| Sqlite3.Data.TEXT t |] -> Some t
        | _ -> None)
      |> String.Set.of_list
    in
    let trades_by_ticker =
      List.filter_map trades ~f:(function
        | [ trade_id; ticker; ts; yes_price; count; side ]
          when not (Set.mem fetched_already ticker) ->
          Option.map (Side.of_kalshi_string side) ~f:(fun taker_side ->
            ( ticker
            , { Trade.trade_id
              ; ticker
              ; ts = Int.of_string ts
              ; yes_price =
                  Float.round_decimal ~decimal_digits:4 (Float.of_string yes_price *. 100.)
              ; count = Float.of_string count
              ; taker_side
              ; is_block = false
              } ))
        | _ -> None)
      |> String.Map.of_alist_multi
    in
    Store.transaction store ~f:(fun () ->
      Map.iteri trades_by_ticker ~f:(fun ~key:ticker ~data:ts ->
        List.iter ts ~f:(fun (t : Trade.t) ->
          Store.exec
            store
            "INSERT OR IGNORE INTO trades(trade_id, ticker, ts, yes_price, count, \
             taker_side, is_block) VALUES (?,?,?,?,?,?,0)"
            [ text t.trade_id
            ; text t.ticker
            ; int t.ts
            ; float t.yes_price
            ; float t.count
            ; text (Side.to_string t.taker_side)
            ]);
        Store.exec
          store
          "INSERT OR IGNORE INTO trades_done(ticker, n_trades, truncated) VALUES (?,?,0)"
          [ text ticker; int (List.length ts) ]));
    Core_unix.unlink sample_file;
    Core_unix.rmdir tmp_dir;
    Store.close store;
    return
      (sprintf
         "edgecheck import: %d markets read, %d new; candles for %d markets; trades for \
          %d sampled markets"
         (List.length markets)
         added_markets
         added_candles
         (Map.length trades_by_ticker)))
;;
