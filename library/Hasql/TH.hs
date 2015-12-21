module Hasql.TH where

import Hasql.TH.Prelude
import Language.Haskell.TH
import qualified Hasql.TH.Renderers as Renderers
import qualified Data.ByteString


-- |
-- Read an SQL-file, containing multiple statements,
-- and produce an expression of type `ByteString`.
-- 
-- Allows to store plain SQL in external files and read it at compile time.
-- 
-- E.g.,
-- 
-- >migration1 :: Hasql.Session.Session ()
-- >migration1 =
-- >  Hasql.Session.sql $(Hasql.TH.readFileAsSQL "sql/migration-1.sql")
-- 
readFileAsSQL :: String -> Q Exp
readFileAsSQL path =
  fmap Renderers.byteStringExp $
  runIO $ Data.ByteString.readFile path

