module Hasql.TH.Syntax.Parser where

import Hasql.TH.Prelude hiding (expr, try, option, many)
import Text.Megaparsec hiding (some, endBy1, someTill, sepBy1, sepEndBy1)
import Text.Megaparsec.Char
import Control.Applicative.Combinators.NonEmpty
import Hasql.TH.Syntax.Ast
import qualified Text.Megaparsec.Char.Lexer as Lex
import qualified Hasql.TH.Syntax.Predicate as Predicate
import qualified Hasql.TH.Syntax.HashSet as HashSet
import qualified Data.Text as Text
import qualified Text.Builder as TextBuilder


{- $setup
>>> test parser = parseTest (parser <* eof)
-}


type Parser = Parsec Void Text


-- * Helpers
-------------------------

commaSeparator :: Parser ()
commaSeparator = space *> char ',' *> space

dotSeparator :: Parser ()
dotSeparator = space *> char '.' *> space

inParenthesis :: Parser a -> Parser a
inParenthesis p = char '(' *> space *> p <* space <* char ')'

{-|
>>> test (quotedString '\'') "'abc''d'"
"abc'd"
-}
quotedString :: Char -> Parser Text
quotedString q = do
  char q
  let
    collectChunks !bdr = do
      chunk <- takeWhileP Nothing (/= q)
      let bdr' = bdr <> TextBuilder.text chunk
      try (consumeEscapedQuote bdr') <|> finish bdr'
    consumeEscapedQuote bdr = do
      char q
      char q
      collectChunks (bdr <> TextBuilder.char q)
    finish bdr = do
      char q
      return (TextBuilder.run bdr)
    in collectChunks mempty

tryList :: [Parser a] -> Parser a
tryList = \ case
  a : [] -> a
  a : b -> try a <|> tryList b
  _ -> empty


-- * Select
-------------------------

{-|
>>> test select "select"
Select Nothing Nothing Nothing

>>> test select "select distinct"
Select (Just (DistinctAllOrDistinctSelectClause Nothing)) Nothing Nothing

>>> test select "select $1"
Select Nothing (Just (ExprSelection (PlaceholderExpr 1) Nothing :| [])) Nothing

>>> test select "select $1 + $2"
Select Nothing (Just (ExprSelection (BinOpExpr "+" (PlaceholderExpr 1) (PlaceholderExpr 2)) Nothing :| [])) Nothing

>>> test select "select a, b"
Select Nothing (Just (ExprSelection (ColumnRefExpr (Ref Nothing (UnquotedName "a"))) Nothing :| [ExprSelection (ColumnRefExpr (Ref Nothing (UnquotedName "b"))) Nothing])) Nothing

>>> test select "select $1 :: text"
Select Nothing (Just (ExprSelection (TypecastExpr (PlaceholderExpr 1) (Type False "text" 0)) Nothing :| [])) Nothing

>>> test select "select 1"
Select Nothing (Just (ExprSelection (LiteralExpr (IntLiteral 1)) Nothing :| [])) Nothing
-}
select :: Parser Select
select = label "select" $ do
  string' "select"
  _allOrDistinct <- optional $ try $ space1 *> allOrDistinctSelectClause
  _selections <- optional $ try $ space1 *> sepBy1 selection commaSeparator
  _from <- optional $ try $ fromClause
  return (Select _allOrDistinct _selections _from)

allOrDistinctSelectClause :: Parser AllOrDistinctSelectClause
allOrDistinctSelectClause =
  AllAllOrDistinctSelectClause <$ string' "all" <|>
  DistinctAllOrDistinctSelectClause <$> (string' "distinct" *> optional (space1 *> onExpressionsClause))

onExpressionsClause :: Parser (NonEmpty Expr)
onExpressionsClause = do
  string' "on"
  space1
  sepBy1 expr commaSeparator

selection :: Parser Selection
selection =
  AllSelection <$ char '*' <|>
  ExprSelection <$> expr <*> optional (space1 *> asAliasClause name)

asAliasClause :: Parser a -> Parser a
asAliasClause name =
  try (string' "as" *> space1 *> name) <|>
  name

{-|
>>> test fromClause "from a as b, c"
TableRefFromItem False (Ref Nothing (UnquotedName "a")) False (Just (UnquotedName "b",Nothing)) :| [TableRefFromItem False (Ref Nothing (UnquotedName "c")) False Nothing]
-}
fromClause :: Parser (NonEmpty FromItem)
fromClause = label "from clause" $ string' "from" *> space1 *> sepBy1 fromItem commaSeparator

{-
TODO: Add support for joins, inner selects and ctes.
-}
fromItem :: Parser FromItem
fromItem = tableRefFromItem

{-
TODO: Add support for TABLESAMPLE.
-}
tableRefFromItem :: Parser FromItem
tableRefFromItem = label "table reference" $ do
  _only <- option False $ try $ True <$ string' "only" <* space1
  _tableRef <- ref
  _asterisk <- option False $ try $ True <$ space1 <* char '*'
  _aliasing <- optional $ try $ do
    space1
    _alias <- asAliasClause name
    _columnAliases <- optional $ try $ space1 *> columnAliasList
    return (_alias, _columnAliases)
  return (TableRefFromItem _only _tableRef _asterisk _aliasing)

columnAliasList :: Parser (NonEmpty Name)
columnAliasList = label "column alias list" $ inParenthesis (sepBy1 name commaSeparator)


-- * References & Names
-------------------------

{-|
>>> test ref "a"
Ref Nothing (UnquotedName "a")

>>> test ref "a.b"
Ref (Just (UnquotedName "a")) (UnquotedName "b")

>>> test ref "a.\"b\""
Ref (Just (UnquotedName "a")) (QuotedName "b")

>>> test ref "user"
1:5:
  |
1 | user
  |     ^
Reserved keyword. You have to put it in quotes
-}
ref :: Parser Ref
ref = do
  _a <- name
  _dot <- option False (try dotSeparator $> True)
  if _dot
    then do
      _b <- name
      return (Ref (Just _a) _b)
    else return (Ref Nothing _a)

name :: Parser Name
name = label "name" $ try unquotedName <|> quotedName

unquotedName :: Parser Name
unquotedName = label "unquoted name" $ do
  _firstChar <- satisfy Predicate.firstIdentifierChar
  _remainder <- takeWhileP Nothing Predicate.notFirstIdentifierChar
  let _name = Text.cons _firstChar _remainder
  if Predicate.reservedKeyword _name
    then fail "Reserved keyword. You have to put it in quotes"
    else return (UnquotedName _name)

quotedName :: Parser Name
quotedName = label "quoted name" $ do
  _contents <- quotedString '"'
  if Text.null _contents
    then fail "Empty name"
    else return (QuotedName _contents)


-- * Expressions
-------------------------

expr :: Parser Expr
expr = try loopingExpr <|> nonLoopingExpr

{-|
Expr, which does not start with another expression.
-}
nonLoopingExpr :: Parser Expr
nonLoopingExpr = 
  tryList
    [
      placeholderExpr,
      defaultExpr,
      columnRefExpr,
      literalExpr,
      inParenthesisExpr,
      caseExpr,
      funcExpr,
      selectExpr,
      existsSelectExpr,
      arraySelectExpr,
      groupingExpr
    ]

loopingExpr :: Parser Expr
loopingExpr = 
  tryList
    [
      typecastExpr,
      escapableBinOpExpr,
      binOpExpr
    ]

placeholderExpr :: Parser Expr
placeholderExpr = PlaceholderExpr <$> (char '$' *> Lex.decimal)

inParenthesisExpr :: Parser Expr
inParenthesisExpr = fmap InParenthesisExpr (inParenthesis expr)

typecastExpr :: Parser Expr
typecastExpr = do
  _a <- nonLoopingExpr
  space
  string "::"
  space
  _type <- type_
  return (TypecastExpr _a _type)

binOpExpr :: Parser Expr
binOpExpr = do
  _a <- nonLoopingExpr
  _binOp <- try (space *> symbolicBinOp <* space) <|> (space1 *> lexicalBinOp <* space1)
  _b <- expr
  return (BinOpExpr _binOp _a _b)

symbolicBinOp :: Parser Text
symbolicBinOp = do
  _text <- takeWhile1P Nothing Predicate.symbolicBinOpChar
  if Predicate.inSet HashSet.symbolicBinOp _text
    then return _text
    else fail ("Unknown binary operator: " <> show _text)

lexicalBinOp :: Parser Text
lexicalBinOp = asum $ fmap string' $ ["and", "or", "is distinct from", "is not distinct from"]

escapableBinOpExpr :: Parser Expr
escapableBinOpExpr = do
  _a <- nonLoopingExpr
  space1
  _not <- option False $ True <$ string' "not" <* space1
  _op <- asum $ fmap string' $ ["like", "ilike", "similar to"]
  space1
  _b <- expr
  _escaping <- optional $ try $ do
    string' "escape"
    space1
    expr
  return (EscapableBinOpExpr _not _op _a _b _escaping)

defaultExpr :: Parser Expr
defaultExpr = DefaultExpr <$ string' "default"

columnRefExpr :: Parser Expr
columnRefExpr = ColumnRefExpr <$> ref

literalExpr :: Parser Expr
literalExpr = LiteralExpr <$> literal

{-|
Full specification:

>>> test caseExpr "CASE WHEN a = b THEN c WHEN d THEN e ELSE f END"
CaseExpr Nothing (WhenClause (BinOpExpr "=" (ColumnRefExpr (Ref Nothing (UnquotedName "a"))) (ColumnRefExpr (Ref Nothing (UnquotedName "b")))) (ColumnRefExpr (Ref Nothing (UnquotedName "c"))) :| [WhenClause (ColumnRefExpr (Ref Nothing (UnquotedName "d"))) (ColumnRefExpr (Ref Nothing (UnquotedName "e")))]) (Just (ColumnRefExpr (Ref Nothing (UnquotedName "f"))))

Implicit argument:

>>> test caseExpr "CASE a WHEN b THEN c ELSE d END"
CaseExpr (Just (ColumnRefExpr (Ref Nothing (UnquotedName "a")))) (WhenClause (ColumnRefExpr (Ref Nothing (UnquotedName "b"))) (ColumnRefExpr (Ref Nothing (UnquotedName "c"))) :| []) (Just (ColumnRefExpr (Ref Nothing (UnquotedName "d"))))
-}
caseExpr :: Parser Expr
caseExpr = label "case expression" $ do
  string' "case"
  space1
  (_arg, _whenClauses) <-
    (Nothing,) <$> sepEndBy1 whenClause space1 <|>
    (,) <$> (Just <$> expr <* space1) <*> sepEndBy1 whenClause space1
  _default <- optional $ try $ do
    string' "else"
    space1
    expr <* space1
  string' "end"
  return $ CaseExpr _arg _whenClauses _default

whenClause :: Parser WhenClause
whenClause = do
  string' "when"
  space1
  _a <- expr
  space1
  string' "then"
  space1
  _b <- expr
  return (WhenClause _a _b)

funcExpr :: Parser Expr
funcExpr = FuncExpr <$> funcApplication

funcApplication :: Parser FuncApplication
funcApplication = do
  _name <- name
  space
  _params <- inParenthesis funcApplicationParams
  return (FuncApplication _name _params)

funcApplicationParams :: Parser FuncApplicationParams
funcApplicationParams =
  asum
    [
      normalFuncApplicationParams,
      singleVariadicFuncApplicationParams,
      listVariadicFuncApplicationParams,
      pure NoFuncApplicationParams
    ]

normalFuncApplicationParams :: Parser FuncApplicationParams
normalFuncApplicationParams = do
  _optAllOrDistinct <- optional ((string' "all" $> AllAllOrDistinct <|> string' "distinct" $> DistinctAllOrDistinct) <* space1)
  _argList <- sepBy1 funcArg commaSeparator
  _optSortClause <- optional (space1 *> sortClause)
  return (NormalFuncApplicationParams _optAllOrDistinct _argList _optSortClause)

singleVariadicFuncApplicationParams :: Parser FuncApplicationParams
singleVariadicFuncApplicationParams = do
  string' "variadic"
  space1
  _arg <- funcArg
  _optSortClause <- optional (space1 *> sortClause)
  return (VariadicFuncApplicationParams Nothing _arg _optSortClause)

listVariadicFuncApplicationParams :: Parser FuncApplicationParams
listVariadicFuncApplicationParams = do
  _argList <- sepBy1 funcArg commaSeparator
  commaSeparator
  string' "variadic"
  space1
  _arg <- funcArg
  _optSortClause <- optional (space1 *> sortClause)
  return (VariadicFuncApplicationParams (Just _argList) _arg _optSortClause)

funcArg :: Parser FuncArg
funcArg = ExprFuncArg <$> expr

sortClause :: Parser (NonEmpty OrderByItem)
sortClause = do
  string' "order by"
  space1
  sepBy1 orderByItem commaSeparator

orderByItem :: Parser OrderByItem
orderByItem = do
  _expr <- expr
  _optOrder <- optional (space1 *> order)
  return (OrderByItem _expr _optOrder)

order :: Parser Order
order = string' "asc" $> AscOrder <|> string' "desc" $> DescOrder

selectExpr :: Parser Expr
selectExpr = SelectExpr <$> inParenthesis select

existsSelectExpr :: Parser Expr
existsSelectExpr = do
  string' "exists"
  space
  ExistsSelectExpr <$> inParenthesis select

arraySelectExpr :: Parser Expr
arraySelectExpr = do
  string' "array"
  space
  ExistsSelectExpr <$> inParenthesis select

groupingExpr :: Parser Expr
groupingExpr = do
  string' "grouping"
  space
  GroupingExpr <$> inParenthesis (sepBy1 expr commaSeparator)


-- * Literals
-------------------------

{-|
@
AexprConst: Iconst
      | FCONST
      | Sconst
      | BCONST
      | XCONST
      | func_name Sconst
      | func_name '(' func_arg_list opt_sort_clause ')' Sconst
      | ConstTypename Sconst
      | ConstInterval Sconst opt_interval
      | ConstInterval '(' Iconst ')' Sconst
      | TRUE_P
      | FALSE_P
      | NULL_P
@

>>> test literal "- 324098320984320480392480923842"
IntLiteral (-324098320984320480392480923842)

>>> test literal "'abc''de'"
StringLiteral "abc'de"

>>> test literal "23.43234"
FloatLiteral 23.43234

>>> test literal "-32423423.3243248732492739847923874"
FloatLiteral -3.24234233243248732492739847923874e7

>>> test literal "NULL"
NullLiteral
-}
literal :: Parser Literal
literal = label "literal" $ tryList [numericLiteral, stringLiteral, boolLiteral, nullLiteral]

numericLiteral :: Parser Literal
numericLiteral = label "numeric literal" $ do
  (_input, _scientific) <- match $ Lex.signed space Lex.scientific
  case parseMaybe (Lex.signed space Lex.decimal <* eof :: Parser Integer) _input of
    Just _int -> return (IntLiteral _int)
    Nothing -> return (FloatLiteral _scientific)

stringLiteral :: Parser Literal
stringLiteral = quotedString '\'' <&> StringLiteral <?> "string literal"

boolLiteral :: Parser Literal
boolLiteral = BoolLiteral True <$ string' "true" <|> BoolLiteral False <$ string' "false" <?> "bool literal"

nullLiteral :: Parser Literal
nullLiteral = NullLiteral <$ string' "null" <?> "null literal"


-- * Types
-------------------------

{-|
>>> test type_ "int4"
Type False "int4" 0

>>> test type_ "int4[]"
Type False "int4" 1

>>> test type_ "int4[ ] []"
Type False "int4" 2

>>> test type_ "int4?[][]"
Type True "int4" 2
-}
type_ :: Parser Type
type_ = do
  _baseName <- fmap Text.toLower $ takeWhile1P Nothing isAlphaNum
  _nullable <- option False (try (True <$ space <* char '?'))
  _arrayLevels <- fmap length $ many $ space *> char '[' *> space *> char ']'
  return (Type _nullable _baseName _arrayLevels)
