#!/usr/bin/env bash
# Regenerate the README screenshots in docs/assets from the current web/data/report.json.
# Needs: a release build (`make web`), `fade forward` output for the Forward tab,
# Google Chrome, Node 22+, and Pillow for compression.
set -euo pipefail
cd "$(dirname "$0")/.."
PORT="${PORT:-8097}"
./_build/default/bin/fade.exe serve --port "$PORT" > /dev/null 2>&1 &
SERVER=$!
trap 'kill "$SERVER" 2>/dev/null || true' EXIT
for _ in $(seq 1 50); do
  curl -sf "http://127.0.0.1:$PORT/" > /dev/null && break
  sleep 0.2
done
mkdir -p docs/assets
for steps in scripts/figures/*.json; do
  node scripts/shoot.mjs "http://127.0.0.1:$PORT/" "$steps" --width 1280 --height 900 2>/dev/null | grep '^wrote' || true
done
# Keep each image well under 500 KB for the README.
if python3 -c "import PIL" 2>/dev/null; then
  python3 scripts/compress_png.py docs/assets/*.png
else
  echo "Pillow not found: screenshots left uncompressed (pip install pillow)"
fi
