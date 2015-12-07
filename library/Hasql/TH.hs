module Hasql.TH where

import Hasql.TH.Prelude
import Language.Haskell.TH
import qualified Hasql.TH.Parsers as Parsers
import qualified Hasql.TH.Renderers as Renderers
import qualified Data.Text.IO


-- |
-- Read an SQL-file, containing multiple statements,
-- and produce a transaction expression.
-- 
-- Allows to store plain SQL in external files and read it at compile time.
-- 
-- E.g.,
-- 
-- >migration1 :: Transaction ()
-- >migration1 =
-- >  $(Hasql.TH.readFileAsTransaction "sql/migration-1.sql")
-- 
readFileAsTransaction :: String -> Q Exp
readFileAsTransaction path =
  do
    contents <-
      runIO $ Data.Text.IO.readFile path
    statements <- 
      either (fail . showString ("Parsing failure: ")) return $
      Parsers.run Parsers.statements contents
    return $
      Renderers.statementsTransactionExp statements
