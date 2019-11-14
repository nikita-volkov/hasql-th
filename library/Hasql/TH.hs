module Hasql.TH
where

import Hasql.TH.Prelude
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Hasql.TH.Exp as Exp
import qualified Data.Text.Encoding as Text


sql :: QuasiQuoter
sql = let
  exp = return . Exp.byteString . Text.encodeUtf8 . fromString
  unsupported _ = fail "Unsupported"
  in QuasiQuoter exp unsupported unsupported unsupported

{-|
Read an SQL-file, containing multiple statements,
and produce an expression of type `ByteString`.

Allows to store plain SQL in external files and read it at compile time.

E.g.,

>migration1 :: Hasql.Session.Session ()
>migration1 = Hasql.Session.sql [sqlFile|sql/migration-1.sql|]
-}
sqlFile :: QuasiQuoter
sqlFile = quoteFile sql
