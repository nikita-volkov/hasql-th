module Hasql.TH
(
  -- * Statements
  {-|
  Quasiquoters in this category produce Hasql `Statement`s,
  checking the correctness of SQL at compile-time.
  
  To extract the information about parameters and results of the statement,
  the quoter requires you to explicitly specify the Postgres types for placeholders and results.
  
  Here's an example of how to use it:

  >selectUserDetails :: Statement Int32 (Maybe (Text, Text, Maybe Text))
  >selectUserDetails =
  >  [maybeStatement|
  >    select name :: text, email :: text, phone :: text?
  >    from "user"
  >    where id = $1 :: int4
  >    |]
  
  As you can see, it completely eliminates the need to mess with codecs.
  The quasiquoters directly produce `Statement`,
  which you can then `dimap` over using its `Profunctor` instance to get to your domain types.

  === Nullability

  As you might have noticed in the example,
  we introduce one change to the Postgres syntax in the way
  the typesignatures are parsed:
  we interpret question-marks in them as specification of nullability.
  Here's more examples of that:

  >>> :t [singletonStatement| select a :: int4? |]
  ...
    :: Statement () (Maybe Int32)
  
  You can use it to specify the nullability of array elements:

  >>> :t [singletonStatement| select a :: int4?[] |]
  ...
    :: Data.Vector.Generic.Base.Vector v (Maybe Int32) =>
       Statement () (v (Maybe Int32))

  And of arrays themselves:

  >>> :t [singletonStatement| select a :: int4?[]? |]
  ...
    :: Data.Vector.Generic.Base.Vector v (Maybe Int32) =>
       Statement () (Maybe (v (Maybe Int32)))
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

Will raise `Hasql.Session.UnexpectedAmountOfRows` error if it's any other.

=== Examples

>>> :t [singletonStatement|select 1 :: int2|]
... :: Statement () Int16

>>> :{
  :t [singletonStatement|
       insert into "user" (email, name)
       values ($1 :: text, $2 :: text)
       returning id :: int4
       |]
:}
...
... :: Statement (Text, Text) Int32

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
Statement producing one row or none.

>>> :t [maybeStatement|select 1 :: int2|]
... :: Statement () (Maybe Int16)
-}
maybeStatement :: QuasiQuoter
maybeStatement = statementExp Exp.maybeStatement Extraction.statement

{-|
Statement producing a vector of rows.

>>> :t [vectorStatement|select 1 :: int2|]
... :: Statement () (Vector Int16)
-}
vectorStatement :: QuasiQuoter
vectorStatement = statementExp Exp.vectorStatement Extraction.statement

{-|
Function from `Fold` over rows to a statement producing the result of folding.
Use this when you need to aggregate rows customly.

>>> :t [foldStatement|select 1 :: int2|]
... :: Fold Int16 b -> Statement () b
-}
foldStatement :: QuasiQuoter
foldStatement = statementExp Exp.foldStatement Extraction.statement

{-|
Statement producing no results.

>>> :t [resultlessStatement|insert into "user" (name, email) values ($1 :: text, $2 :: text)|]
...
... :: Statement (Text, Text) ()
-}
resultlessStatement :: QuasiQuoter
resultlessStatement = statementExp Exp.resultlessStatement Extraction.rowlessStatement

{-|
Statement counting the rows it affects.

>>> :t [rowsAffectedStatement|delete from "user" where password is null|]
...
... :: Statement () Int64
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
