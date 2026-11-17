# fade: common tasks. `make help` lists them.
SWITCH_ENV = eval "$$(opam env --switch=. --set-switch)" &&
FADE = ./_build/default/bin/fade.exe

# The pinned study. Changing any of these is a different study, not a rerun.
SINCE = 2025-05-16
UNTIL = 2026-11-16
CANDLE_FRACTION = 0.02
TRADE_SAMPLE = 20000
FROZEN = rules/frozen.json
FROZEN_SHA = 4a9021cb1f5d09d5ca53951ad7b6468f2baa7b0336dc5e8d4efc8e0e00498e54

# The forward test of the candidate, locked before any forward data was fetched.
CANDIDATE = rules/candidate.json
CANDIDATE_SHA = ceca2b0c97a2953ba0b7dbf23e5026b0bcfd7273379133ea2d5ab492b9fa467a
AGENT = $(HOME)/Library/LaunchAgents/com.fade.forward.plist

.PHONY: help build test web demo serve fmt fetch reproduce figures forward \
  forward-install forward-uninstall

help:
	@echo "make build      build everything (native CLI + dev JS)"
	@echo "make test       run all expect tests"
	@echo "make web        release build of the CLI and dashboard JS"
	@echo "make demo       serve the dashboard on synthetic data"
	@echo "make serve      serve the dashboard on web/data/report.json"
	@echo "make reproduce  fetch (or reuse the cache), report, regenerate README figures"
	@echo "make forward    fetch newly settled markets and score the locked candidate"
	@echo "make forward-install / forward-uninstall   daily launchd agent (macOS)"

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
	$(FADE) fetch --since $(SINCE) --until $(UNTIL) --candle-fraction $(CANDLE_FRACTION) --trade-sample $(TRADE_SAMPLE)

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

# Paper-trade the locked candidate on markets settled after the study window.
forward: web
	@grep -q '"hash": "$(CANDIDATE_SHA)"' $(CANDIDATE) || \
	  (echo "$(CANDIDATE) does not have the pinned hash $(CANDIDATE_SHA)"; exit 1)
	$(FADE) forward --candidate $(CANDIDATE)

# A daily launchd agent (06:30 local) that runs scripts/forward-job.sh, like edgecheck's
# `schedule install`. Logs go to logs/forward.log and logs/forward-runs.jsonl.
forward-install: web
	@mkdir -p logs "$(dir $(AGENT))"
	sed -e 's|@REPO@|$(CURDIR)|g' \
	  -e 's|@PATH@|/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin|g' \
	  scripts/launchd/com.fade.forward.plist.in > "$(AGENT)"
	-launchctl bootout gui/$$(id -u)/com.fade.forward 2>/dev/null
	launchctl bootstrap gui/$$(id -u) "$(AGENT)"
	@echo "installed $(AGENT): runs daily at 06:30 (if the Mac is asleep, on wake)"

# Unloads the agent and deletes its one plist file.
forward-uninstall:
	-launchctl bootout gui/$$(id -u)/com.fade.forward
	test -n "$(AGENT)" && rm -f "$(AGENT)"
