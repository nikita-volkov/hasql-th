module Hasql.TH
where

import Hasql.TH.Prelude hiding (exp)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Hasql.TH.Exp as Exp
import qualified Hasql.TH.Syntax.Extraction as Extraction
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


-- * Helpers
-------------------------

exp :: (String -> Q Exp) -> QuasiQuoter
exp = let
  _unsupported _ = fail "Unsupported"
  in \ _exp -> QuasiQuoter _exp _unsupported _unsupported _unsupported

statementExp :: (Extraction.Statement -> Exp) -> QuasiQuoter
statementExp _statement = exp (either (fail . Text.unpack) (return . _statement) . Extraction.statement . fromString)


-- * Statements
-------------------------

{-|
>>> :t [resultlessStatement|select 1 :: int2|]
[resultlessStatement|select 1 :: int2|]
  :: Hasql.Statement.Statement () ()
-}
resultlessStatement :: QuasiQuoter
resultlessStatement = statementExp Exp.resultlessStatement

{-|
>>> :t [rowsAffectedStatement|select 1 :: int2|]
[rowsAffectedStatement|select 1 :: int2|]
  :: Hasql.Statement.Statement () Int64
-}
rowsAffectedStatement :: QuasiQuoter
rowsAffectedStatement = statementExp Exp.rowsAffectedStatement

{-|
>>> :t [singletonStatement|select 1 :: int2|]
[singletonStatement|select 1 :: int2|]
  :: Hasql.Statement.Statement () Int16
-}
singletonStatement :: QuasiQuoter
singletonStatement = statementExp Exp.singletonStatement

{-|
>>> :t [maybeStatement|select 1 :: int2|]
[maybeStatement|select 1 :: int2|]
  :: Hasql.Statement.Statement () (Maybe Int16)
-}
maybeStatement :: QuasiQuoter
maybeStatement = statementExp Exp.maybeStatement

{-|
>>> :t [vectorStatement|select 1 :: int2|]
[vectorStatement|select 1 :: int2|]
  :: Hasql.Statement.Statement () (Data.Vector.Vector Int16)
-}
vectorStatement :: QuasiQuoter
vectorStatement = statementExp Exp.vectorStatement

{-|
>>> :t [foldStatement|select 1 :: int2|]
[foldStatement|select 1 :: int2|]
  :: Fold Int16 b -> Hasql.Statement.Statement () b
-}
foldStatement :: QuasiQuoter
foldStatement = statementExp Exp.foldStatement


-- * Plain SQL
-------------------------

sql :: QuasiQuoter
sql = exp $ return . Exp.byteString . Text.encodeUtf8 . fromString

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
