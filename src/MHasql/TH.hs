module MHasql.TH
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
    --  >selectUserDetails :: Statement Int32 (Maybe (Maybe Text, Maybe Text, Maybe Text))
    --  >selectUserDetails =
    --  >  [maybeStatement|
    --  >    select name :: text, email :: text, phone :: text
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
    --  As you might have noticed in the example, that all types by default come with a `Maybe` wrapper.
    --  This will be improved in future commits to allow explicit non nullability while not having to
    --  derivate from PG native syntax.
    --
    -- ** Row-parsing statements
    singletonStatement,
    maybeStatement,
    vectorStatement,
    foldStatement,

    -- ** Row-ignoring statements
    resultlessStatement,
    rowsAffectedStatement,

    -- * SQL ByteStrings

    -- |
    --  ByteString-producing quasiquoters.
    --
    --  For now they perform no compile-time checking.
    uncheckedSql,
    uncheckedSqlFile,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified MHasql.TH.Construction.Exp as Exp
import qualified MHasql.TH.Extraction.Exp as ExpExtraction
import MHasql.TH.Prelude
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import qualified PostgresqlSyntax.Ast as Ast
import qualified PostgresqlSyntax.Parsing as Parsing

-- * Helpers

exp :: (String -> Q Exp) -> QuasiQuoter
exp =
  let unsupported _ = fail "Unsupported"
   in \exp' -> QuasiQuoter exp' unsupported unsupported unsupported

expParser :: (Text -> Either Text Exp) -> QuasiQuoter
expParser parser =
  exp $ \inputString -> either (fail . Text.unpack) return $ parser $ fromString inputString

expPreparableStmtAstParser :: (Text -> Ast.PreparableStmt -> Either Text Exp) -> QuasiQuoter
expPreparableStmtAstParser parser =
  expParser $ \input -> do
    ast <- first fromString $ Parsing.run (Parsing.atEnd Parsing.preparableStmt) input
    parser input ast

-- $setup
-- >>> import Data.Int
-- >>> import Data.Vector
-- >>> import Hasql.Statement

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
-- ...
-- ... :: Statement () (Maybe Int16)
--
-- >>> :{
--   :t [singletonStatement|
--        insert into "user" (email, name)
--        values ($1 :: text, $2 :: text)
--        returning id :: int4
--        |]
-- :}
-- ...
-- ... :: Statement (Maybe Text, Maybe Text) (Maybe Int32)
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
-- ...
-- ... :: Statement () (Maybe (Maybe Int16))
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
-- ...
-- ... :: Statement () (Vector (Maybe Int16))
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
-- >>> :t [foldStatement|SELECT 1 :: int2|]
-- ...
-- ... :: Fold (Maybe Int16) b -> Statement () b
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
-- ... :: Statement (Maybe Text, Maybe Text) ()
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

-- * SQL ByteStrings

-- |
-- Quoter of a multiline Unicode SQL string,
-- which gets converted into a format ready to be used for declaration of statements.
uncheckedSql :: QuasiQuoter
uncheckedSql = exp $ return . Exp.byteString . Text.encodeUtf8 . fromString

-- |
-- Read an SQL-file, containing multiple statements,
-- and produce an expression of type `ByteString`.
--
-- Allows to store plain SQL in external files and read it at compile time.
--
-- E.g.,
--
-- >migration1 :: Hasql.Session.Session ()
-- >migration1 = Hasql.Session.sql [uncheckedSqlFile|migrations/1.sql|]
uncheckedSqlFile :: QuasiQuoter
uncheckedSqlFile = quoteFile uncheckedSql

-- * Tests

-- $
-- >>> :t [maybeStatement| select (password = $2 :: bytea) :: bool, id :: int4 from "user" where "email" = $1 :: text |]
-- ...
-- ... Statement
-- ...   (Maybe Text, Maybe ByteString) (Maybe (Maybe Bool, Maybe Int32))
--
-- >>> :t [maybeStatement| select id :: int4 from application where pub_key = $1 :: uuid and sec_key_pt1 = $2 :: int8 and sec_key_pt2 = $3 :: int8 |]
-- ...
-- ... Statement
-- ...   (Maybe UUID, Maybe Int64, Maybe Int64) (Maybe (Maybe Int32))
--
-- >>> :t [singletonStatement| select 1 :: int4 from a left join b on b.id = a.id |]
-- ...
-- ... Statement () (Maybe Int32)
