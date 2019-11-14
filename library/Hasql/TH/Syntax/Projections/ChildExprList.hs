module Hasql.TH.Syntax.Projections.ChildExprList where

import Hasql.TH.Prelude
import Hasql.TH.Syntax.Ast
import qualified Data.Foldable as Foldable


foldable :: (Foldable f, Functor f) => (a -> [Expr]) -> f a -> [Expr]
foldable a = concat . Foldable.toList . fmap a

select :: Select -> [Expr]
select (Select a b c) =
  foldable allOrDistinctSelectClause a <>
  foldable (foldable selection) b <>
  foldable (foldable fromItem) c

allOrDistinctSelectClause :: AllOrDistinctSelectClause -> [Expr]
allOrDistinctSelectClause = \ case
  AllAllOrDistinctSelectClause -> []
  DistinctAllOrDistinctSelectClause a -> foldable toList a

selection :: Selection -> [Expr]
selection = \ case
  AllSelection -> []
  ExprSelection a _ -> [a]

fromItem :: TableRef -> [Expr]
fromItem = \ case
  RelationExprTableRef _ _ -> []

expr :: Expr -> [Expr]
expr = \ case
  PlaceholderExpr _ -> []
  TypecastExpr a _ -> [a]
  BinOpExpr _ a b -> [a, b]
  EscapableBinOpExpr _ _ a b c -> [a, b] <> maybeToList c
  BetweenExpr _ a b -> [a, b]
  DefaultExpr -> []
  ColumnRefExpr _ -> []
  LiteralExpr _ -> []
  InParenthesisExpr a -> [a]
  CaseExpr a b c -> maybeToList a <> foldable whenClause b <> maybeToList c
  FuncExpr a -> funcApplication a
  SelectExpr a -> select a
  ExistsSelectExpr a -> select a
  ArraySelectExpr a -> select a
  GroupingExpr a -> toList a

whenClause :: WhenClause -> [Expr]
whenClause (WhenClause a b) = [a, b]

funcApplication :: FuncApplication -> [Expr]
funcApplication (FuncApplication _ a) = funcApplicationParams a

funcApplicationParams :: FuncApplicationParams -> [Expr]
funcApplicationParams = \ case
  NoFuncApplicationParams -> []
  NormalFuncApplicationParams _ a b -> foldable funcArg a <> foldable (foldable orderByItem) b
  VariadicFuncApplicationParams a b c -> foldable (foldable funcArg) a <> funcArg b <> foldable (foldable orderByItem) c
  StarFuncApplicationParams -> []

funcArg :: FuncArg -> [Expr]
funcArg = \ case
  ExprFuncArg a -> [a]

orderByItem :: OrderByItem -> [Expr]
orderByItem (OrderByItem a _) = [a]
