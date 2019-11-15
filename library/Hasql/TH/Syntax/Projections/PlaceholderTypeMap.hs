module Hasql.TH.Syntax.Projections.PlaceholderTypeMap where

import Hasql.TH.Prelude hiding (union)
import Hasql.TH.Syntax.Ast
import qualified Data.IntMap.Strict as IntMap
import qualified Hasql.TH.Syntax.Projections.ChildExprList as ChildExprList


preparableStmt :: PreparableStmt -> Either Text (IntMap Type)
preparableStmt = exprList . ChildExprList.preparableStmt

exprList :: [Expr] -> Either Text (IntMap Type)
exprList = foldM union IntMap.empty <=< traverse expr

union :: IntMap Type -> IntMap Type -> Either Text (IntMap Type)
union a b = IntMap.mergeWithKey merge (fmap Right) (fmap Right) a b & sequence where
  merge index a b = if a == b
    then Just (Right a)
    else Just (Left ("Placeholder $" <> (fromString . show) index <> " has conflicting type annotations"))

expr :: Expr -> Either Text (IntMap Type)
expr = \ case
  PlaceholderExpr _index -> Left ("Placeholder $" <> (fromString . show) _index <> " misses an explicit typecast")
  TypecastExpr _expr _type -> castedExpr _type _expr
  _expr -> exprList (ChildExprList.expr _expr)

castedExpr :: Type -> Expr -> Either Text (IntMap Type)
castedExpr _type = \ case
  PlaceholderExpr _index -> Right $ IntMap.singleton _index _type
  TypecastExpr _expr _type' -> castedExpr _type' _expr
  InParensExpr _expr -> castedExpr _type _expr
  _expr -> exprList (ChildExprList.expr _expr)
