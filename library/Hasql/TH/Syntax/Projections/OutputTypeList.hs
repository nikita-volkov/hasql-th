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

traverse' :: (a -> Either Text [Type]) -> [a] -> Either Text [Type]
traverse' fn = fmap join . traverse fn

{-|
>>> "select 1 :: int4, b :: text" & parse P.select & select
Right [Type "int4" False 0 False,Type "text" False 0 False]

>>> "select 1 :: int4, b" & parse P.select & select
Left "Result expression is missing a typecast"
-}
select :: Select -> Either Text [Type]
select (Select _ a _) = traverse' selection (foldMap toList a)

selection :: Selection -> Either Text [Type]
selection = \ case
  ExprSelection a _ -> expr a
  AllSelection -> Left "Selection of all fields is not allowed, \
    \because it leaves the output types unspecified. \
    \You have to be specific."

expr :: Expr -> Either Text [Type]
expr = \ case
  TypecastExpr _ a -> Right [a]
  InParenthesisExpr a -> expr a
  a -> Left "Result expression is missing a typecast"
