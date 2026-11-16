# fade: common tasks. `make help` lists them.
SWITCH_ENV = eval "$$(opam env --switch=. --set-switch)" &&
FADE = ./_build/default/bin/fade.exe

# The pinned study. Changing any of these is a different study, not a rerun.
SINCE = 2025-05-16
CANDLE_FRACTION = 0.02
TRADE_SAMPLE = 20000
FROZEN = rules/frozen.json
FROZEN_SHA = PENDING

.PHONY: help build test web demo serve fmt fetch reproduce figures

help:
	@echo "make build      build everything (native CLI + dev JS)"
	@echo "make test       run all expect tests"
	@echo "make web        release build of the CLI and dashboard JS"
	@echo "make demo       serve the dashboard on synthetic data"
	@echo "make serve      serve the dashboard on web/data/report.json"
	@echo "make reproduce  fetch (or reuse the cache), report, regenerate README figures"

build:
	$(SWITCH_ENV) dune build

test:
	$(SWITCH_ENV) dune runtest

web:
	$(SWITCH_ENV) dune build --profile release ./web/main.bc.js ./bin/fade.exe

demo: web
	$(FADE) serve --demo

serve: web
	$(FADE) serve

fmt:
	$(SWITCH_ENV) dune fmt

# Resumable: an existing data/fade.db is reused and only missing pieces are fetched.
fetch: web
	$(FADE) fetch --since $(SINCE) --candle-fraction $(CANDLE_FRACTION) --trade-sample $(TRADE_SAMPLE)

# The frozen rules are committed and never regenerated here: freezing again after seeing
# out-of-time results would defeat the test. `fade report` checks the file's hash, and
# this target checks it is the hash pinned in the README.
reproduce: fetch
	@grep -q '"hash": "$(FROZEN_SHA)"' $(FROZEN) || \
	  (echo "$(FROZEN) does not have the pinned hash $(FROZEN_SHA)"; exit 1)
	$(FADE) report --frozen $(FROZEN)
	./scripts/figures.sh

figures: web
	./scripts/figures.sh
