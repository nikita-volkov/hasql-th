module Main (main) where

import qualified Test.DocTest

import System.IO (IO)

main :: IO ()
main = Test.DocTest.doctest
  [ "src"
  , "-XFlexibleInstances"
  , "-XLambdaCase"
  , "-XNoImplicitPrelude"
  , "-XNoMonomorphismRestriction"
  , "-XOverloadedStrings"
  , "-XQuasiQuotes"
  , "-XScopedTypeVariables"
  , "-XTemplateHaskell"
  , "-XTupleSections"
  ]
