{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Hasql.TH.Extraction.PlaceholderTypeMap where

import qualified Data.IntMap.Strict as IntMap
import Hasql.TH.Extraction.ChildExprList (ChildExpr (..))
import qualified Hasql.TH.Extraction.ChildExprList as ChildExprList
import Hasql.TH.Prelude hiding (union)
import PostgresqlSyntax.Ast

preparableStmt :: PreparableStmt -> Either Text (IntMap Typename)
preparableStmt = childExprList . ChildExprList.preparableStmt

childExprList :: [ChildExpr] -> Either Text (IntMap Typename)
childExprList = foldM union IntMap.empty <=< traverse childExpr

union :: IntMap Typename -> IntMap Typename -> Either Text (IntMap Typename)
union a b = IntMap.mergeWithKey merge (fmap Right) (fmap Right) a b & sequence
  where
    merge index a b =
      if a == b
        then Just (Right a)
        else Just (Left ("Placeholder $" <> (fromString . show) index <> " has conflicting type annotations"))

childExpr :: ChildExpr -> Either Text (IntMap Typename)
childExpr = \case
  AChildExpr a -> aExpr a
  BChildExpr a -> bExpr a
  CChildExpr a -> cExpr a

aExpr = \case
  CExprAExpr a -> cExpr a
  TypecastAExpr a b -> castedAExpr b a
  a -> childExprList (ChildExprList.aChildExpr a)

bExpr = \case
  CExprBExpr a -> cExpr a
  TypecastBExpr a b -> castedBExpr b a
  a -> childExprList (ChildExprList.bChildExpr a)

cExpr = \case
  ParamCExpr a _ -> Left ("Placeholder $" <> (fromString . show) a <> " misses an explicit typecast")
  a -> childExprList (ChildExprList.cChildExpr a)

castedAExpr a = \case
  CExprAExpr b -> castedCExpr a b
  TypecastAExpr b c -> castedAExpr c b
  b -> aExpr b

castedBExpr a = \case
  CExprBExpr b -> castedCExpr a b
  TypecastBExpr b c -> castedBExpr c b
  b -> bExpr b

castedCExpr a = \case
  ParamCExpr b _ -> Right (IntMap.singleton b a)
  InParensCExpr b _ -> castedAExpr a b
  b -> cExpr b
