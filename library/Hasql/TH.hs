module Hasql.TH
where

import Hasql.TH.Prelude
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Hasql.TH.Exp as A
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as C


sql :: QuasiQuoter
sql =
  QuasiQuoter exp undefined undefined undefined
  where
    exp =
      return . A.byteStringExp . C.encodeUtf8 . fromString

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
sqlFile :: QuasiQuoter
sqlFile =
  quoteFile sql
