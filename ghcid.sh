#!/usr/bin/env bash
set -euo pipefail

ghcid -c "cabal v2-repl test:json-parser-haskell-test" \
  -T=Main.main \
  --setup ":set args --hide-successes --color always"
