module Hasql.TH
(
  -- * Statements
  {-|
  Quasiquoters in this category produce Hasql `Statement`s,
  checking the correctness of SQL at compile-time.

  To achieve this a custom parser is used,
  which for now ports a part of functionality
  from the parser used in Postgres itself.

  Because it is a partial port,
  you may bump into situations,
  where a correct statement won't pass the checker.
  In such cases you can always downgrade to implementing `Statement`
  and its codecs explicitly.
  Please report such cases at the project\'s issue tracker.
  -}
  -- ** Row-parsing statements
  singletonStatement,
  maybeStatement,
  vectorStatement,
  foldStatement,
  -- ** Row-ignoring statements
  resultlessStatement,
  rowsAffectedStatement,
  -- * SQL ByteStrings
  {-|
  ByteString-producing quasiquoters.

  For now they perform no compile-time checking.
  -}
  uncheckedSql,
  uncheckedSqlFile,
)
where

import Hasql.TH.Prelude hiding (exp)
import Hasql.Statement (Statement)
import Data.Vector (Vector)
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

statementExp :: (Extraction.Statement -> Exp) -> (Text -> Either Text Extraction.Statement) -> QuasiQuoter
statementExp _exp _extract = exp (either (fail . Text.unpack) (return . _exp) . _extract . fromString)


-- * Statements
-------------------------

{-|
Statement producing exactly one result row.

Will raise the `Hasql.Session.UnexpectedAmountOfRows` error if it's any other.

=== Examples

>>> :t [singletonStatement|select 1 :: int2|]
[singletonStatement|select 1 :: int2|] :: Statement () Int16

Incorrect SQL:

>>> :t [singletonStatement|elect 1|]
...
  |
1 | elect 1
  | ^^^^^^
unexpected "elect "
...
-}
singletonStatement :: QuasiQuoter
singletonStatement = statementExp Exp.singletonStatement Extraction.statement

{-|
>>> :t [maybeStatement|select 1 :: int2|]
[maybeStatement|select 1 :: int2|] :: Statement () (Maybe Int16)
-}
maybeStatement :: QuasiQuoter
maybeStatement = statementExp Exp.maybeStatement Extraction.statement

{-|
>>> :t [vectorStatement|select 1 :: int2|]
[vectorStatement|select 1 :: int2|] :: Statement () (Vector Int16)
-}
vectorStatement :: QuasiQuoter
vectorStatement = statementExp Exp.vectorStatement Extraction.statement

{-|
>>> :t [foldStatement|select 1 :: int2|]
[foldStatement|select 1 :: int2|] :: Fold Int16 b -> Statement () b
-}
foldStatement :: QuasiQuoter
foldStatement = statementExp Exp.foldStatement Extraction.statement

{-|
>>> :t [resultlessStatement|select 1|]
[resultlessStatement|select 1|] :: Statement () ()
-}
resultlessStatement :: QuasiQuoter
resultlessStatement = statementExp Exp.resultlessStatement Extraction.rowlessStatement

{-|
>>> :t [rowsAffectedStatement|select 1|]
[rowsAffectedStatement|select 1|] :: Statement () Int64
-}
rowsAffectedStatement :: QuasiQuoter
rowsAffectedStatement = statementExp Exp.rowsAffectedStatement Extraction.rowlessStatement


-- * SQL ByteStrings
-------------------------

{-|
Quoter of a multiline Unicode SQL string,
which gets converted into a format ready to be used for declaration of statements.
-}
uncheckedSql :: QuasiQuoter
uncheckedSql = exp $ return . Exp.byteString . Text.encodeUtf8 . fromString

{-|
Read an SQL-file, containing multiple statements,
and produce an expression of type `ByteString`.

Allows to store plain SQL in external files and read it at compile time.

E.g.,

>migration1 :: Hasql.Session.Session ()
>migration1 = Hasql.Session.sql [uncheckedSqlFile|migrations/1.sql|]
-}
uncheckedSqlFile :: QuasiQuoter
uncheckedSqlFile = quoteFile uncheckedSql


-- * Tests
-------------------------

{- $
>>> :t [maybeStatement| select (password = $2 :: bytea) :: bool, id :: int4 from "user" where "email" = $1 :: text |]
...
... Statement (Text, ByteString) (Maybe (Bool, Int32))

>>> :t [maybeStatement| select id :: int4 from application where pub_key = $1 :: uuid and sec_key_pt1 = $2 :: int8 and sec_key_pt2 = $3 :: int8 |]
...
... Statement (UUID, Int64, Int64) (Maybe Int32)

>>> :t [singletonStatement| select 1 :: int4 from a left join b on b.id = a.id |]
...
... Statement () Int32
-}
