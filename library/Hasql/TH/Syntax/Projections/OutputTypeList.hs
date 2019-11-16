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

foldable :: Foldable f => (a -> Either Text [Type]) -> f a -> Either Text [Type]
foldable fn = fmap join . traverse fn . toList

preparableStmt :: PreparableStmt -> Either Text [Type]
preparableStmt = \ case
  SelectPreparableStmt a -> selectStmt a

selectStmt :: SelectStmt -> Either Text [Type]
selectStmt = \ case
  InParensSelectStmt a -> selectStmt a
  NoParensSelectStmt a -> selectNoParens a

selectNoParens :: SelectNoParens -> Either Text [Type]
selectNoParens (SelectNoParens _ a _ _ _) = selectClause a

selectClause :: SelectClause -> Either Text [Type]
selectClause = either simpleSelect selectNoParens

simpleSelect :: SimpleSelect -> Either Text [Type]
simpleSelect = \ case
  NormalSimpleSelect a _ _ _ _ _ _ -> foldable targeting a
  ValuesSimpleSelect a -> valuesClause a
  BinSimpleSelect _ a _ b -> do
    c <- selectClause a
    d <- selectClause b
    if c == d
      then return c
      else Left "Merged queries produce results of incompatible types"

targeting :: Targeting -> Either Text [Type]
targeting = \ case
  NormalTargeting a -> foldable target a
  AllTargeting a -> foldable (foldable target) a
  DistinctTargeting _ b -> foldable target b

target :: Target -> Either Text [Type]
target = \ case
  ExprTarget a _ -> expr a
  AllTarget -> Left "Target of all fields is not allowed, \
    \because it leaves the output types unspecified. \
    \You have to be specific."

valuesClause :: ValuesClause -> Either Text [Type]
valuesClause = foldable (foldable expr)

expr :: Expr -> Either Text [Type]
expr = \ case
  TypecastExpr _ a -> Right [a]
  InParensExpr a _ -> expr a
  a -> Left "Result expression is missing a typecast"
