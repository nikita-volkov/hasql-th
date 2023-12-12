#!/bin/bash
set -eo pipefail

function format {
  for path in $(git diff --staged --name-only -- '*.cabal') $(git ls-files -om --exclude-standard -- '*.cabal'); do if test -f $path; then cabal-fmt --no-tabular -c $path 2> /dev/null || cabal-fmt --no-tabular -i $path; fi; done
  for path in $(git diff --staged --name-only -- '*.hs') $(git ls-files -om --exclude-standard -- '*.hs'); do if test -f $path; then ormolu -ic $path; fi; done
}

cabal build --enable-tests --ghc-options="-Werror -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wredundant-constraints -Wunused-packages -Wno-name-shadowing -Wno-unused-matches -Wno-unused-do-bind -Wno-type-defaults"

# cabal haddock

format
