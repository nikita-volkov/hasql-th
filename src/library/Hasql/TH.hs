module Hasql.TH
  ( -- * Statements

    -- |
    --  Quasiquoters in this category produce Hasql `Statement`s,
    --  checking the correctness of SQL at compile-time.
    --
    --  To extract the information about parameters and results of the statement,
    --  the quoter requires you to explicitly specify the Postgres types for placeholders and results.
    --
    --  Here's an example of how to use it:
    --
    --  >selectUserDetails :: Statement Int32 (Maybe (Text, Text, Maybe Text))
    --  >selectUserDetails =
    --  >  [maybeStatement|
    --  >    select name :: text, email :: text, phone :: text?
    --  >    from "user"
    --  >    where id = $1 :: int4
    --  >    |]
    --
    --  As you can see, it completely eliminates the need to mess with codecs.
    --  The quasiquoters directly produce `Statement`,
    --  which you can then `Data.Profunctor.dimap` over using its `Data.Profunctor.Profunctor` instance to get to your domain types.
    --
    --  == Type mappings
    --
    --  === Primitives
    --
    --  Following is a list of supported Postgres types and their according types on the Haskell end.
    --
    --  - @bool@ - `Bool`
    --  - @int2@ - `Int16`
    --  - @int4@ - `Int32`
    --  - @int8@ - `Int64`
    --  - @float4@ - `Float`
    --  - @float8@ - `Double`
    --  - @numeric@ - `Data.Scientific.Scientific`
    --  - @char@ - `Char`
    --  - @text@ - `Data.Text.Text`
    --  - @bytea@ - `Data.ByteString.ByteString`
    --  - @date@ - `Data.Time.Day`
    --  - @timestamp@ - `Data.Time.LocalTime`
    --  - @timestamptz@ - `Data.Time.UTCTime`
    --  - @time@ - `Data.Time.TimeOfDay`
    --  - @timetz@ - @(`Data.Time.TimeOfDay`, `Data.Time.TimeZone`)@
    --  - @interval@ - `Data.Time.DiffTime`
    --  - @uuid@ - `Data.UUID.UUID`
    --  - @inet@ - @(`Network.IP.Addr.NetAddr` `Network.IP.Addr.IP`)@
    --  - @json@ - `Data.Aeson.Value`
    --  - @jsonb@ - `Data.Aeson.Value`
    --
    --  === Arrays
    --
    --  Array mappings are also supported.
    --  They are specified according to Postgres syntax: by appending one or more @[]@ to the primitive type,
    --  depending on how many dimensions the array has.
    --  On the Haskell end array is mapped to generic `Data.Vector.Generic.Vector`,
    --  allowing you to choose which particular vector implementation to map to.
    --
    --  === Nulls
    --
    --  As you might have noticed in the example,
    --  we introduce one change to the Postgres syntax in the way
    --  the typesignatures are parsed:
    --  we interpret question-marks in them as specification of nullability.
    --  Here's more examples of that:
    --
    --  >>> :t [singletonStatement| select a :: int4? |]
    --  ...
    --    :: Statement () (Maybe Int32)
    --
    --  You can use it to specify the nullability of array elements:
    --
    --  >>> :t [singletonStatement| select a :: int4?[] |]
    --  ...
    --    :: Data.Vector.Generic.Base.Vector v (Maybe Int32) =>
    --       Statement () (v (Maybe Int32))
    --
    --  And of arrays themselves:
    --
    --  >>> :t [singletonStatement| select a :: int4?[]? |]
    --  ...
    --    :: Data.Vector.Generic.Base.Vector v (Maybe Int32) =>
    --       Statement () (Maybe (v (Maybe Int32)))

    -- ** Row-parsing statements
    singletonStatement,
    maybeStatement,
    vectorStatement,
    foldStatement,

    -- ** Row-ignoring statements
    resultlessStatement,
    rowsAffectedStatement,

    -- * SQL Strings

    -- |
    -- Text-producing quasiquoters performing no compile-time checking.
    uncheckedSql,
    uncheckedSqlFile,
  )
where

import qualified Data.Text as Text
import qualified Hasql.TH.Construction.Exp as Exp
import qualified Hasql.TH.Extraction.Exp as ExpExtraction
import Hasql.TH.Prelude hiding (exp)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import qualified PostgresqlSyntax.Ast as Ast
import qualified PostgresqlSyntax.Parsing as Parsing

-- * Helpers

exp :: (String -> Q Exp) -> QuasiQuoter
exp =
  let _unsupported _ = fail "Unsupported"
   in \_exp -> QuasiQuoter _exp _unsupported _unsupported _unsupported

expParser :: (Text -> Either Text Exp) -> QuasiQuoter
expParser _parser =
  exp $ \_inputString -> either (fail . Text.unpack) return $ _parser $ fromString _inputString

expPreparableStmtAstParser :: (Ast.PreparableStmt -> Either Text Exp) -> QuasiQuoter
expPreparableStmtAstParser _parser =
  expParser $ \_input -> do
    _ast <- first fromString $ Parsing.run (Parsing.atEnd Parsing.preparableStmt) _input
    _parser _ast

-- * Statement

-- |
-- @
-- :: `Statement` params row
-- @
--
-- Statement producing exactly one result row.
--
-- Will cause the running session to fail with the
-- `Hasql.Session.UnexpectedAmountOfRows` error if it's any other.
--
-- === __Examples__
--
-- >>> :t [singletonStatement|select 1 :: int2|]
-- ... :: Statement () Int16
--
-- >>> :{
--   :t [singletonStatement|
--        insert into "user" (email, name)
--        values ($1 :: text, $2 :: text)
--        returning id :: int4
--        |]
-- :}
-- ...
-- ... :: Statement (Text, Text) Int32
--
-- Incorrect SQL:
--
-- >>> :t [singletonStatement|elect 1|]
-- ...
--   |
-- 1 | elect 1
--   |      ^
-- ...
singletonStatement :: QuasiQuoter
singletonStatement = expPreparableStmtAstParser (ExpExtraction.undecodedStatement Exp.singleRowResultDecoder)

-- |
-- @
-- :: `Statement` params (Maybe row)
-- @
--
-- Statement producing one row or none.
--
-- === __Examples__
--
-- >>> :t [maybeStatement|select 1 :: int2|]
-- ... :: Statement () (Maybe Int16)
maybeStatement :: QuasiQuoter
maybeStatement = expPreparableStmtAstParser (ExpExtraction.undecodedStatement Exp.rowMaybeResultDecoder)

-- |
-- @
-- :: `Statement` params (`Vector` row)
-- @
--
-- Statement producing a vector of rows.
--
-- === __Examples__
--
-- >>> :t [vectorStatement|select 1 :: int2|]
-- ... :: Statement () (Vector Int16)
vectorStatement :: QuasiQuoter
vectorStatement = expPreparableStmtAstParser (ExpExtraction.undecodedStatement Exp.rowVectorResultDecoder)

-- |
-- @
-- :: `Fold` row folding -> `Statement` params folding
-- @
--
-- Function from `Fold` over rows to a statement producing the result of folding.
-- Use this when you need to aggregate rows customly.
--
-- === __Examples__
--
-- >>> :t [foldStatement|select 1 :: int2|]
-- ... :: Fold Int16 b -> Statement () b
foldStatement :: QuasiQuoter
foldStatement = expPreparableStmtAstParser ExpExtraction.foldStatement

-- |
-- @
-- :: `Statement` params ()
-- @
--
-- Statement producing no results.
--
-- === __Examples__
--
-- >>> :t [resultlessStatement|insert into "user" (name, email) values ($1 :: text, $2 :: text)|]
-- ...
-- ... :: Statement (Text, Text) ()
resultlessStatement :: QuasiQuoter
resultlessStatement = expPreparableStmtAstParser (ExpExtraction.undecodedStatement (const Exp.noResultResultDecoder))

-- |
-- @
-- :: `Statement` params Int64
-- @
--
-- Statement counting the rows it affects.
--
-- === __Examples__
--
-- >>> :t [rowsAffectedStatement|delete from "user" where password is null|]
-- ...
-- ... :: Statement () Int64
rowsAffectedStatement :: QuasiQuoter
rowsAffectedStatement = expPreparableStmtAstParser (ExpExtraction.undecodedStatement (const Exp.rowsAffectedResultDecoder))

-- * SQL Strings

-- |
-- Quoter of a multiline Unicode SQL string,
-- which gets converted into a format ready to be used for declaration of statements.
uncheckedSql :: QuasiQuoter
uncheckedSql = exp $ return . Exp.text . fromString

-- |
-- Read an SQL-file, containing multiple statements,
-- and produce an expression of type 'Text'.
--
-- Allows to store plain SQL in external files and read it at compile time.
--
-- E.g.,
--
-- >migration1 :: Hasql.Session.Session ()
-- >migration1 = Hasql.Session.script [uncheckedSqlFile|migrations/1.sql|]
uncheckedSqlFile :: QuasiQuoter
uncheckedSqlFile = quoteFile uncheckedSql

-- * Tests

-- $
-- >>> :t [maybeStatement| select (password = $2 :: text) :: bool, id :: int4 from "user" where "email" = $1 :: text |]
-- ...
-- ... Statement (Text, Text) (Maybe (Bool, Int32))
--
-- >>> :t [maybeStatement| select id :: int4 from application where pub_key = $1 :: uuid and sec_key_pt1 = $2 :: int8 and sec_key_pt2 = $3 :: int8 |]
-- ...
-- ... Statement (UUID, Int64, Int64) (Maybe Int32)
--
-- >>> :t [singletonStatement| select 1 :: int4 from a left join b on b.id = a.id |]
-- ...
-- ... Statement () Int32
