(* The cache schema. Every write is an upsert keyed on Kalshi's own ids, so re-running
   a fetch never duplicates rows (idempotent), and a crash loses at most the batch in
   flight (resumable). *)

let statements =
  [ {|CREATE TABLE IF NOT EXISTS meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)|}
  ; (* Every market seen while listing, qualifying or not: used to count how many
       markets each event has (single vs multi-market). *)
    {|CREATE TABLE IF NOT EXISTS listed (
        ticker TEXT PRIMARY KEY, event_ticker TEXT NOT NULL)|}
  ; {|CREATE INDEX IF NOT EXISTS listed_event ON listed(event_ticker)|}
  ; (* Markets in scope: settled in the window, volume >= threshold, result yes/no. *)
    {|CREATE TABLE IF NOT EXISTS markets (
        ticker TEXT PRIMARY KEY,
        event_ticker TEXT NOT NULL,
        open_ts INTEGER NOT NULL,
        close_ts INTEGER NOT NULL,
        settle_ts INTEGER NOT NULL,
        result TEXT NOT NULL,
        volume REAL NOT NULL,
        can_close_early INTEGER NOT NULL,
        expected_expiration_ts INTEGER,
        latest_expiration_ts INTEGER,
        occurrence_ts INTEGER,
        source TEXT NOT NULL,
        sample_key INTEGER NOT NULL)|}
  ; {|CREATE INDEX IF NOT EXISTS markets_event ON markets(event_ticker)|}
  ; (* Every market of each sampled event with its creation and open times, whatever
       its volume, so "markets in the event at time t" can be counted point-in-time. *)
    {|CREATE TABLE IF NOT EXISTS event_markets (
        ticker TEXT PRIMARY KEY,
        event_ticker TEXT NOT NULL,
        created_ts INTEGER NOT NULL,
        open_ts INTEGER NOT NULL)|}
  ; {|CREATE INDEX IF NOT EXISTS event_markets_event ON event_markets(event_ticker)|}
  ; {|CREATE TABLE IF NOT EXISTS event_markets_done (
        event_ticker TEXT PRIMARY KEY, n_markets INTEGER NOT NULL)|}
  ; (* Trades fetched for the predictive model. Kept apart from [trades] so that the
       study's trade sample (and so its report) never changes: the model reads both. *)
    {|CREATE TABLE IF NOT EXISTS model_trades_extra (
        trade_id TEXT PRIMARY KEY,
        ticker TEXT NOT NULL,
        ts INTEGER NOT NULL,
        yes_price REAL NOT NULL,
        count REAL NOT NULL,
        taker_side TEXT NOT NULL,
        is_block INTEGER NOT NULL)|}
  ; {|CREATE INDEX IF NOT EXISTS model_trades_extra_ticker ON model_trades_extra(ticker)|}
  ; {|CREATE TABLE IF NOT EXISTS model_trades_done (
        ticker TEXT PRIMARY KEY, n_trades INTEGER NOT NULL, truncated INTEGER NOT NULL)|}
  ; {|CREATE INDEX IF NOT EXISTS markets_key ON markets(sample_key)|}
  ; {|CREATE TABLE IF NOT EXISTS events (
        event_ticker TEXT PRIMARY KEY,
        series_ticker TEXT NOT NULL,
        mutually_exclusive INTEGER NOT NULL,
        title TEXT)|}
  ; {|CREATE TABLE IF NOT EXISTS series (
        ticker TEXT PRIMARY KEY,
        category TEXT NOT NULL,
        fee_type TEXT NOT NULL,
        fee_multiplier REAL NOT NULL)|}
  ; {|CREATE TABLE IF NOT EXISTS series_fee_changes (
        id TEXT PRIMARY KEY,
        series_ticker TEXT NOT NULL,
        fee_type TEXT NOT NULL,
        fee_multiplier REAL NOT NULL,
        ts INTEGER NOT NULL)|}
  ; {|CREATE TABLE IF NOT EXISTS event_fee_changes (
        id TEXT PRIMARY KEY,
        event_ticker TEXT NOT NULL,
        fee_type TEXT,
        fee_multiplier REAL,
        ts INTEGER NOT NULL)|}
  ; (* Raw hourly candles (YES bid/ask at the end of each hour, cents) for the window
       before close. Snapshots are derived from these at load time, so the choice of
       horizon anchor is an analysis setting, not baked into the cache. *)
    {|CREATE TABLE IF NOT EXISTS candles (
        ticker TEXT NOT NULL,
        end_ts INTEGER NOT NULL,
        yes_bid REAL,
        yes_ask REAL,
        PRIMARY KEY (ticker, end_ts)) WITHOUT ROWID|}
  ; {|CREATE TABLE IF NOT EXISTS snapshots (
        ticker TEXT NOT NULL,
        horizon TEXT NOT NULL,
        ts INTEGER NOT NULL,
        yes_bid REAL,
        yes_ask REAL,
        PRIMARY KEY (ticker, horizon))|}
  ; (* A market whose candles were fetched, even if it yielded no snapshot. *)
    {|CREATE TABLE IF NOT EXISTS candles_done (
        ticker TEXT PRIMARY KEY, n_candles INTEGER NOT NULL)|}
  ; {|CREATE TABLE IF NOT EXISTS trades (
        trade_id TEXT PRIMARY KEY,
        ticker TEXT NOT NULL,
        ts INTEGER NOT NULL,
        yes_price REAL NOT NULL,
        count REAL NOT NULL,
        taker_side TEXT NOT NULL,
        is_block INTEGER NOT NULL)|}
  ; {|CREATE INDEX IF NOT EXISTS trades_ticker ON trades(ticker)|}
  ; {|CREATE TABLE IF NOT EXISTS trades_done (
        ticker TEXT PRIMARY KEY, n_trades INTEGER NOT NULL, truncated INTEGER NOT NULL)|}
  ]
;;
