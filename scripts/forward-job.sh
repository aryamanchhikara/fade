#!/usr/bin/env bash
# What the com.fade.forward launchd agent runs once a day: fetch newly settled markets,
# score the locked candidate, write web/data/forward.json, and log one line per run to
# logs/forward-runs.jsonl. Uses the already-built binary (`make web`).
set -uo pipefail
cd "$(dirname "$0")/.."
mkdir -p logs
started=$(date -u +%Y-%m-%dT%H:%M:%SZ)
echo "== fade forward $started"
./_build/default/bin/fade.exe forward
status=$?
printf '{"started":"%s","finished":"%s","exit":%d}\n' \
  "$started" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$status" >> logs/forward-runs.jsonl
exit "$status"
