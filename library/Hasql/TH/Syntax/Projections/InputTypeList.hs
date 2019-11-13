{-|
AST traversal extracting input types.
-}
module Hasql.TH.Syntax.Projections.InputTypeList where

import Hasql.TH.Prelude
import Hasql.TH.Syntax.Ast
import qualified Hasql.TH.Syntax.Projections.PlaceholderTypeMap as PlaceholderTypeMap
import qualified Data.IntMap.Strict as IntMap

{- $setup
>>> import qualified Hasql.TH.Syntax.Parsing as P
>>> parse parser = either (error . show) id . Text.Megaparsec.parse parser ""
-}

{-|
>>> "select $1 :: INT4" & parse P.select & select
Right [Type False "int4" 0]

>>> "select $1 :: int4, a + $2 :: text?[]" & parse P.select & select
Right [Type False "int4" 0,Type True "text" 1]

>>> "select $1" & parse P.select & select
Left "Placeholder $1 misses an explicit typecast"

>>> "select $2 :: int4, $1 :: int4, $2 :: int4" & parse P.select & select
Right [Type False "int4" 0,Type False "int4" 0]

>>> "select $1 :: int4, $1 :: text" & parse P.select & select
Left "Placeholder $1 has conflicting type annotations"

>>> "select $2 :: int4, $2 :: text" & parse P.select & select
Left "Placeholder $2 has conflicting type annotations"

>>> "select $3 :: int4, $1 :: int4" & parse P.select & select
Left "You've missed placeholder $2"

-}
select :: Select -> Either Text [Type]
select = placeholderTypeMap <=< PlaceholderTypeMap.select

placeholderTypeMap :: IntMap Type -> Either Text [Type]
placeholderTypeMap a = do
  zipWithM (\ a b -> if a == b then Right () else Left ("You've missed placeholder $" <> showAsText b))
    (IntMap.keys a) [1..]
  return (IntMap.elems a)
