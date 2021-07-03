#!/usr/bin/env bash
set -euo pipefail

ghcid -c "cabal v2-repl lib:json-parser-haskell \
  --repl-options=-fno-break-on-exception \
  --repl-options=-fno-break-on-error \
  --repl-options=-v1 \
  --repl-options=-ferror-spans \
  --repl-options=-fobject-code \
  "
