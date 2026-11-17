# Architecture

fade is one dune project with a native command-line tool and a browser dashboard.
Both are written in OCaml. They share a single library of types, compiled twice: once
to native code, once to JavaScript with js_of_ocaml.

```
                 native (bin/fade.exe)                         browser (web/main.bc.js)
 Kalshi API ──> lib/kalshi ──> lib/fetch ──> lib/store ──> lib/analysis ──> report.json ──> web/
                                   (SQLite cache)                │                 ▲
                                                                 └── lib/types ────┘
                                                               (same .ml files, both targets)
```

## Libraries

| Directory | dune library | Targets | What it does |
| --- | --- | --- | --- |
| `lib/types` | `fade_types` | native + JS | Records and variants shared by everything: `Market`, `Snapshot`, `Trade`, `Bucket_result`, `Report`, `Rule`, `Config`, the fee schedule. All derive `sexp`, `yojson` and `compare`. No IO, no native-only dependencies. |
| `lib/analysis` | `fade_analysis` | native | Pure functions from a `Dataset.t` to numbers: fee model, quote and trade observations, bucket statistics, the out-of-time split, rule freezing, the report builder. No IO, no clock, no global random state. |
| `lib/kalshi` | `kalshi` | native | Rate-limited HTTP client (cohttp-async, TLS via async_ssl, gzip) and decoders for the Kalshi endpoints used. |
| `lib/store` | `fade_store` | native | SQLite cache (`data/fade.db`), schema, and `Load.dataset`, which turns the cache into a `Dataset.t`. |
| `lib/fetch` | `fade_fetch` | native | The `fade fetch` stages and the read-only edgecheck import. |
| `lib/synth` | `fade_synth` | native + JS | Deterministic synthetic data for tests and `fade serve --demo`. |
| `bin` | `fade.exe` | native | The CLI: `fetch`, `import-edgecheck`, `freeze`, `report`, `candidate`, `forward`, `spreads`, `serve`, `synth`, `cutoff`. |
| `web` | `main.bc.js` | JS | The Bonsai dashboard. |
| `test` | `fade_tests` | native | Integration expect tests over `test/fixtures/synthetic.json`. |

Unit tests are inline `let%expect_test`s next to the code they test (`lib/analysis/fees.ml`,
`lib/analysis/stats.ml`, `lib/analysis/snapshots.ml`, ...). No test touches the network.

## How the types are shared

`lib/types/dune` declares an ordinary library:

```
(library
 (name fade_types)
 (libraries core ppx_yojson_conv_lib yojson)
 (preprocess (pps ppx_jane ppx_yojson_conv)))
```

Nothing in it marks it as "JavaScript". dune compiles every library to bytecode, and
js_of_ocaml translates bytecode to JavaScript when an executable with `(modes js)` links
it. The only rule is that every dependency must also work under js_of_ocaml. Core,
sexplib and yojson are pure OCaml, so they do. Async's Unix layer, cohttp-async and
sqlite3 do not, which is why they live in `kalshi`, `store` and `fetch`, never in `types`.

Two consequences:

1. **One definition, two runtimes.** `Report.t` is written once. The CLI calls
   `Report.yojson_of_t` to write `web/data/report.json`; the dashboard calls
   `Report.t_of_yojson` on the same file. If a field is added or renamed, both sides
   change together and the compiler checks every use. A stale `report.json` from an
   older build fails to decode, and the dashboard says so instead of rendering garbage.
2. **Plain `int` timestamps.** In js_of_ocaml an OCaml `int` is 32 bits, so
   `Unix_time.t` (seconds since 1970) is fine until January 2038. It is an `int` rather
   than `Time_ns.t` so the browser bundle doesn't pull in time-zone tables.

One js_of_ocaml-specific detail lives in `web/json_js.ml`: the browser parses
`report.json` with its native `JSON.parse` and converts the result to a
`Yojson.Safe.t`. Yojson's own lexer is correct under js_of_ocaml but very slow there,
because each token round-trips through OCaml byte buffers.

## Data flow

1. **`fade fetch`** fills `data/fade.db` (never committed; Kalshi's data terms).
   - `GET /historical/cutoff` splits the archive: markets settled before
     `market_settled_ts` come from `/historical/markets`, newer ones from
     `/markets?status=settled`.
   - Every in-scope market gets a stable pseudo-random `sample_key` (MD5 of the ticker).
     Hourly candles are fetched for markets whose key is below a threshold (a Bernoulli
     sample, so it can be drawn while listing is still running), trades for the first N
     keys. `Load.dataset` only uses the complete key prefix, so a fetch stopped at any
     point still yields a uniform random sample.
   - Raw hourly candles are stored; quotes at each horizon are derived at load time.
   - Once a listing completes, the population is pinned: later runs fetch missing
     candles and trades but never list again. `--until` bounds a fresh listing to the
     study window.
2. **`Load.dataset`** classifies each series' close timing from timestamps alone,
   derives snapshots with the configured horizon anchor, and assembles a `Dataset.t`.
3. **`fade freeze`** builds in-sample tables only, selects rules, and writes
   `rules/frozen.json` with a SHA-256 over the canonical JSON of config and rules.
4. **`fade report`** verifies the hash, builds every table for both samples, scores the
   frozen rules out-of-time, and writes `report.json`.
5. **`fade serve`** serves `web/index.html`, `web/style.css`, the JS bundle,
   `report.json` and `forward.json` on localhost.

## The forward test

- **`fade candidate`** scores the rules in `Fade_analysis.Candidate_file` on the
  study's in-sample events (`Candidate_score`, pure) and writes `rules/candidate.json`
  with a SHA-256 over the config, rules and those numbers. It was committed before any
  forward data existed.
- **`fade forward`** (`Fade_fetch.Forward_fetch`) keeps its own cache,
  `data/forward.db`, tied to the candidate's hash. Each run lists markets settled in
  the forward window from two days before the previous listing, fetches candles for
  all of them (batch endpoint, 100 markets a request) and trades only where a maker
  rule would have rested a bid. `Load.dataset ~complete_prefix:false` loads the whole
  cache, since it is the population rather than a sample.
- `Forward_builder` (pure) scores each rule, projects how many more events its CI
  needs, and decides its status. A rule that has concluded is recorded in the
  cache's `meta` table, so later runs cannot overturn the conclusion.
- The dashboard's Forward test tab (`web/forward_view.ml`) reads
  `web/data/forward.json` through the same `Forward.t_of_yojson` the CLI wrote it
  with.

## The dashboard (Bonsai)

`web/main.ml` uses Bonsai's `Cont` API. A component is a function
`Bonsai.graph -> Vdom.Node.t Bonsai.t`:

- `load_report` holds `Loading | Loaded of Report.t | Failed of string` in
  `Bonsai.state`, and starts the HTTP request from `Bonsai.Edge.lifecycle ~on_activate`.
- The filters are one `Bonsai.state_machine0`: every control sends an action that is
  applied to the current filters, so fast clicks never act on stale values.
- The table index (a hash table from `Table.Key.t` to buckets) is a separate `let%arr`
  that depends only on the loaded report. Bonsai recomputes it once per report, not on
  every filter change; the view `let%arr` depends on the index, the filters and the
  hovered point.
- `web/chart.ml` draws the calibration chart as SVG with `Virtual_dom_svg`; there is
  no JavaScript charting library.
