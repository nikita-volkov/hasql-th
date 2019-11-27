module Hasql.TH.Syntax.Projections.PlaceholderTypeMap where

import Hasql.TH.Prelude hiding (union)
import Hasql.TH.Syntax.Ast
import Hasql.TH.Syntax.Projections.ChildExprList (ChildExpr(..))
import qualified Data.IntMap.Strict as IntMap
import qualified Hasql.TH.Syntax.Projections.ChildExprList as ChildExprList


preparableStmt :: PreparableStmt -> Either Text (IntMap TypecastTypename)
preparableStmt = childExprList . ChildExprList.preparableStmt

childExprList :: [ChildExpr] -> Either Text (IntMap TypecastTypename)
childExprList = foldM union IntMap.empty <=< traverse childExpr

union :: IntMap TypecastTypename -> IntMap TypecastTypename -> Either Text (IntMap TypecastTypename)
union a b = IntMap.mergeWithKey merge (fmap Right) (fmap Right) a b & sequence where
  merge index a b = if a == b
    then Just (Right a)
    else Just (Left ("Placeholder $" <> (fromString . show) index <> " has conflicting type annotations"))

childExpr :: ChildExpr -> Either Text (IntMap TypecastTypename)
childExpr = \ case
  AChildExpr a -> aExpr a
  BChildExpr a -> bExpr a
  CChildExpr a -> cExpr a

aExpr = \ case
  CExprAExpr a -> cExpr a
  TypecastAExpr a b -> castedAExpr b a
  a -> childExprList (ChildExprList.aChildExpr a)

bExpr = \ case
  CExprBExpr a -> cExpr a
  TypecastBExpr a b -> castedBExpr b a
  a -> childExprList (ChildExprList.bChildExpr a)

cExpr = \ case
  ParamCExpr a _ -> Left ("Placeholder $" <> (fromString . show) a <> " misses an explicit typecast")
  a -> childExprList (ChildExprList.cChildExpr a)

castedAExpr a = \ case
  CExprAExpr b -> castedCExpr a b
  TypecastAExpr b c -> castedAExpr c b
  b -> aExpr b

castedBExpr a = \ case
  CExprBExpr b -> castedCExpr a b
  TypecastBExpr b c -> castedBExpr c b
  b -> bExpr b

castedCExpr a = \ case
  ParamCExpr b _ -> Right (IntMap.singleton b a)
  InParensCExpr b _ -> castedAExpr a b
  b -> cExpr b
