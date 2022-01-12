#!/bin/bash
set -eo pipefail

ormolu --mode inplace -ce \
$(find . -name "*.hs" \
  -not -path "./*.stack-work/*" \
  -not -path "./.git/*")

stack build --fast --test \
--ghc-options "-j6 +RTS -A128m -n2m -RTS -fwarn-incomplete-patterns"
