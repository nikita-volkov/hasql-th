-- |
-- AST traversal extracting output types.
module Hasql.TH.Extraction.OutputTypeList where

import Hasql.TH.Prelude
import PostgresqlSyntax.Ast

foldable :: Foldable f => (a -> Either Text [Typename]) -> f a -> Either Text [Typename]
foldable fn = fmap join . traverse fn . toList

preparableStmt = \case
  SelectPreparableStmt a -> selectStmt a
  InsertPreparableStmt a -> insertStmt a
  UpdatePreparableStmt a -> updateStmt a
  DeletePreparableStmt a -> deleteStmt a

-- * Insert

insertStmt (InsertStmt a b c d e) = foldable returningClause e

returningClause = targetList

-- * Update

updateStmt (UpdateStmt _ _ _ _ _ a) = foldable returningClause a

-- * Delete

deleteStmt (DeleteStmt _ _ _ _ a) = foldable returningClause a

-- * Select

selectStmt = \case
  Left a -> selectNoParens a
  Right a -> selectWithParens a

selectNoParens (SelectNoParens _ a _ _ _) = selectClause a

selectWithParens = \case
  NoParensSelectWithParens a -> selectNoParens a
  WithParensSelectWithParens a -> selectWithParens a

selectClause = either simpleSelect selectWithParens

simpleSelect = \case
  NormalSimpleSelect a _ _ _ _ _ _ -> foldable targeting a
  ValuesSimpleSelect a -> valuesClause a
  TableSimpleSelect _ -> Left "TABLE cannot be used as a final statement, since it's impossible to specify the output types"
  BinSimpleSelect _ a _ b -> do
    c <- selectClause a
    d <- selectClause b
    if c == d
      then return c
      else Left "Merged queries produce results of incompatible types"

targeting = \case
  NormalTargeting a -> targetList a
  AllTargeting a -> foldable targetList a
  DistinctTargeting _ b -> targetList b

targetList = foldable targetEl

targetEl = \case
  AliasedExprTargetEl a _ -> aExpr a
  ImplicitlyAliasedExprTargetEl a _ -> aExpr a
  ExprTargetEl a -> aExpr a
  AsteriskTargetEl ->
    Left
      "Target of all fields is not allowed, \
      \because it leaves the output types unspecified. \
      \You have to be specific."

valuesClause = foldable (foldable aExpr)

aExpr = \case
  CExprAExpr a -> cExpr a
  TypecastAExpr _ a -> Right [a]
  a -> Left "Result expression is missing a typecast"

cExpr = \case
  InParensCExpr a Nothing -> aExpr a
  a -> Left "Result expression is missing a typecast"
