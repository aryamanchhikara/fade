open! Core
open! Async

let command =
  Command.async_or_error
    ~summary:"Import edgecheck's cached NFL markets from ../betting (read-only)"
    ~readme:(fun () ->
      "Uses the duckdb CLI with -readonly; nothing in ../betting is modified. Rows \
       already in fade's cache are kept; edgecheck only fills gaps. Run `fade fetch \
       --stages events` afterwards to resolve event metadata for imported markets.")
    (let%map_open.Command duckdb =
       flag
         "--duckdb"
         (optional_with_default "../betting/data/kalshi.duckdb" string)
         ~doc:"PATH edgecheck DuckDB cache (default ../betting/data/kalshi.duckdb)"
     and db =
       flag "--db" (optional_with_default "data/fade.db" string) ~doc:"PATH SQLite cache"
     and trade_sample =
       flag
         "--trade-sample"
         (optional_with_default 20_000 int)
         ~doc:"N same trade sample size as `fade fetch` (default 20000)"
     in
     fun () ->
       let%map.Deferred.Or_error summary =
         Fade_fetch.Edgecheck_import.run ~duckdb ~db_path:db ~trade_sample
       in
       print_endline summary)
;;
