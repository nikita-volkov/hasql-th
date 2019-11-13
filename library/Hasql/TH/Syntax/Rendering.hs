module Hasql.TH.Syntax.Rendering where

import Hasql.TH.Prelude hiding (expr, try, option, many)
import Hasql.TH.Syntax.Ast
import Data.ByteString.FastBuilder
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text


-- * Helpers
-------------------------

text :: Text -> Builder
text = stringUtf8 . Text.unpack


-- * Select
-------------------------

select :: Select -> Builder
select (Select a b c) =
  "SELECT" <>
  foldMap (mappend " " . allOrDistinctClause) a <>
  foldMap (mappend " " . intersperseFoldMap1 ", " selection) b

allOrDistinctClause :: AllOrDistinctSelectClause -> Builder
allOrDistinctClause = \ case
  AllAllOrDistinctSelectClause -> "ALL"
  DistinctAllOrDistinctSelectClause a -> "DISTINCT" <> foldMap (mappend " " . onExpressionsClause) a

onExpressionsClause :: NonEmpty Expr -> Builder
onExpressionsClause a = "ON (" <> intersperseFoldMap1 ", " expr a <> ")"

selection :: Selection -> Builder
selection = \ case
  AllSelection -> "*"
  ExprSelection a b -> expr a <> foldMap (mappend " " . name) b

expr :: Expr -> Builder
expr = \ case
  PlaceholderExpr a -> "$" <> intDec a
  TypecastExpr a b -> expr a <> " :: " <> type_ b
  BinOpExpr a b c -> expr b <> " " <> text a <> " " <> expr c
  ColumnRefExpr a -> ref a
  LiteralExpr a -> literal a
  InParenthesisExpr a -> "(" <> expr a <> ")"

type_ :: Type -> Builder
type_ (Type a _ b _) =
  text a <>
  fold (replicate b "[]")

ref :: Ref -> Builder
ref (Ref a b) = foldMap (\ c -> name c <> ".") a <> name b

name :: Name -> Builder
name = \ case
  QuotedName a -> char7 '"' <> text a <> char7 '"'
  UnquotedName a -> text a

literal :: Literal -> Builder
literal = \ case
  IntLiteral a -> integerDec a
