{-|
AST traversal extracting output types.
-}
module Hasql.TH.Syntax.Projections.OutputTypeList where

import Hasql.TH.Prelude
import Hasql.TH.Syntax.Ast

{- $setup
>>> import qualified Hasql.TH.Syntax.Parsing as P
>>> parse parser = either (error . show) id . Text.Megaparsec.parse parser ""
-}

foldable :: Foldable f => (a -> Either Text [TypecastTypename]) -> f a -> Either Text [TypecastTypename]
foldable fn = fmap join . traverse fn . toList

preparableStmt = \ case
  SelectPreparableStmt a -> selectStmt a

selectStmt = \ case
  Left a -> selectNoParens a
  Right a -> selectWithParens a

selectNoParens (SelectNoParens _ a _ _ _) = selectClause a

selectWithParens = \ case
  NoParensSelectWithParens a -> selectNoParens a
  WithParensSelectWithParens a -> selectWithParens a

selectClause = either simpleSelect selectWithParens

simpleSelect = \ case
  NormalSimpleSelect a _ _ _ _ _ _ -> foldable targeting a
  ValuesSimpleSelect a -> valuesClause a
  BinSimpleSelect _ a _ b -> do
    c <- selectClause a
    d <- selectClause b
    if c == d
      then return c
      else Left "Merged queries produce results of incompatible types"

targeting = \ case
  NormalTargeting a -> foldable targetEl a
  AllTargeting a -> foldable (foldable targetEl) a
  DistinctTargeting _ b -> foldable targetEl b

targetEl = \ case
  AliasedExprTargetEl a _ -> aExpr a
  ImplicitlyAliasedExprTargetEl a _ -> aExpr a
  ExprTargetEl a -> aExpr a
  AsteriskTargetEl -> Left "Target of all fields is not allowed, \
    \because it leaves the output types unspecified. \
    \You have to be specific."

valuesClause = foldable (foldable aExpr)

aExpr = \ case
  CExprAExpr a -> cExpr a
  TypecastAExpr _ a -> Right [a]
  a -> Left "Result expression is missing a typecast"

cExpr = \ case
  InParensCExpr a Nothing -> aExpr a
  a -> Left "Result expression is missing a typecast"
