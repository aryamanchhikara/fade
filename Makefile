# fade: common tasks. `make help` lists them.
SWITCH_ENV = eval "$$(opam env --switch=. --set-switch)" &&
FADE = ./_build/default/bin/fade.exe

.PHONY: help build test web demo serve fmt

help:
	@echo "make build    build everything (native CLI + dev JS)"
	@echo "make test     run all expect tests"
	@echo "make web      release build of the CLI and dashboard JS"
	@echo "make demo     serve the dashboard on synthetic data"
	@echo "make serve    serve the dashboard on web/data/report.json"

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
