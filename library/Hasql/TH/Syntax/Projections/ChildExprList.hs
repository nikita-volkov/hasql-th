module Hasql.TH.Syntax.Projections.ChildExprList where

import Hasql.TH.Prelude hiding (sortBy)
import Hasql.TH.Syntax.Ast


foldable :: (Foldable f, Functor f) => (a -> [Expr]) -> f a -> [Expr]
foldable a = concat . toList . fmap a

preparableStmt :: PreparableStmt -> [Expr]
preparableStmt = \ case
  SelectPreparableStmt a -> selectStmt a

selectStmt :: SelectStmt -> [Expr]
selectStmt = \ case
  InParensSelectStmt a -> selectStmt a
  NoParensSelectStmt a -> selectNoParens a

selectNoParens :: SelectNoParens -> [Expr]
selectNoParens = \ case
  SimpleSelectNoParens a -> simpleSelect a

selectClause :: SelectClause -> [Expr]
selectClause = either simpleSelect selectNoParens

simpleSelect :: SimpleSelect -> [Expr]
simpleSelect = \ case
  NormalSimpleSelect a b c d e f g ->
    foldable targeting a <> foldable intoClause b <> foldable fromClause c <>
    foldable whereClause d <> foldable groupClause e <> foldable havingClause f <>
    foldable windowClause g
  ValuesSimpleSelect a -> valuesClause a
  BinSimpleSelect _ a _ b -> selectClause a <> selectClause b

targeting :: Targeting -> [Expr]
targeting = \ case
  NormalTargeting a -> foldable target a
  AllTargeting a -> foldable (foldable target) a
  DistinctTargeting a b -> foldable toList a <> foldable target b

target :: Target -> [Expr]
target = \ case
  AllTarget -> []
  ExprTarget a _ -> [a]

intoClause :: IntoClause -> [Expr]
intoClause = optTempTableName

fromClause :: FromClause -> [Expr]
fromClause = foldable tableRef

whereClause :: WhereClause -> [Expr]
whereClause = pure

groupClause :: GroupClause -> [Expr]
groupClause = foldable groupByItem

havingClause :: HavingClause -> [Expr]
havingClause = pure

windowClause :: WindowClause -> [Expr]
windowClause = foldable windowDefinition

valuesClause :: ValuesClause -> [Expr]
valuesClause = foldable toList

optTempTableName :: OptTempTableName -> [Expr]
optTempTableName (OptTempTableName _ _ _) = []

groupByItem :: GroupByItem -> [Expr]
groupByItem = \ case
  ExprGroupByItem a -> [a]
  EmptyGroupingSetGroupByItem -> []
  RollupGroupByItem a -> toList a
  CubeGroupByItem a -> toList a
  GroupingSetsGroupByItem a -> foldable groupByItem a

windowDefinition :: WindowDefinition -> [Expr]
windowDefinition (WindowDefinition _ a) = windowSpecification a

windowSpecification :: WindowSpecification -> [Expr]
windowSpecification (WindowSpecification _ a b c) = foldable toList a <> foldable sortClause b <> foldable frameClause c

frameClause :: FrameClause -> [Expr]
frameClause (FrameClause _ a _) = frameExtent a

frameExtent :: FrameExtent -> [Expr]
frameExtent = \ case
  SingularFrameExtent a -> frameBound a
  BetweenFrameExtent a b -> frameBound a <> frameBound b

frameBound :: FrameBound -> [Expr]
frameBound = \ case
  UnboundedPrecedingFrameBound -> []
  UnboundedFollowingFrameBound -> []
  CurrentRowFrameBound -> []
  PrecedingFrameBound a -> [a]
  FollowingFrameBound a -> [a]

sortClause :: SortClause -> [Expr]
sortClause = foldable sortBy

sortBy :: SortBy -> [Expr]
sortBy (SortBy a _) = [a]

tableRef :: TableRef -> [Expr]
tableRef = \ case
  RelationExprTableRef a _ -> relationExpr a
  SelectTableRef _ a _ -> selectNoParens a
  JoinTableRef a _ -> joinedTable a

relationExpr = \ case
  SimpleRelationExpr a _ -> qualifiedName a
  OnlyRelationExpr a _ -> qualifiedName a

joinedTable :: JoinedTable -> [Expr]
joinedTable = \ case
  InParensJoinedTable a -> joinedTable a
  MethJoinedTable a b c -> joinMeth a <> tableRef b <> tableRef c

joinMeth :: JoinMeth -> [Expr]
joinMeth = \ case
  CrossJoinMeth -> []
  QualJoinMeth _ a -> joinQual a
  NaturalJoinMeth _ -> []

joinQual :: JoinQual -> [Expr]
joinQual = \ case
  UsingJoinQual _ -> []
  OnJoinQual a -> [a]

expr :: Expr -> [Expr]
expr = \ case
  PlaceholderExpr _ -> []
  TypecastExpr a _ -> [a]
  BinOpExpr _ a b -> [a, b]
  EscapableBinOpExpr _ _ a b c -> [a, b] <> maybeToList c
  BetweenExpr _ a b -> [a, b]
  DefaultExpr -> []
  QualifiedNameExpr a -> qualifiedName a
  LiteralExpr _ -> []
  InParensExpr a b -> [a] <> foldable indirection b
  CaseExpr a b c -> maybeToList a <> foldable whenClause b <> maybeToList c
  FuncExpr a -> funcApplication a
  SelectExpr a -> selectNoParens a
  ExistsSelectExpr a -> selectNoParens a
  ArraySelectExpr a -> selectNoParens a
  GroupingExpr a -> toList a

whenClause :: WhenClause -> [Expr]
whenClause (WhenClause a b) = [a, b]

funcApplication :: FuncApplication -> [Expr]
funcApplication (FuncApplication a b) = qualifiedName a <> foldable funcApplicationParams b

funcApplicationParams :: FuncApplicationParams -> [Expr]
funcApplicationParams = \ case
  NormalFuncApplicationParams _ a b -> foldable funcArgExpr a <> foldable (foldable sortBy) b
  VariadicFuncApplicationParams a b c -> foldable (foldable funcArgExpr) a <> funcArgExpr b <> foldable (foldable sortBy) c
  StarFuncApplicationParams -> []

funcArgExpr :: FuncArgExpr -> [Expr]
funcArgExpr = \ case
  ExprFuncArgExpr a -> [a]
  ColonEqualsFuncArgExpr _ a -> [a]
  EqualsGreaterFuncArgExpr _ a -> [a]

qualifiedName :: QualifiedName -> [Expr]
qualifiedName = \ case
  SimpleQualifiedName _ -> []
  IndirectedQualifiedName _ a -> indirection a

indirection :: Indirection -> [Expr]
indirection = foldable indirectionEl

indirectionEl :: IndirectionEl -> [Expr]
indirectionEl = \ case
  AttrNameIndirectionEl _ -> []
  AllIndirectionEl -> []
  ExprIndirectionEl a -> [a]
  SliceIndirectionEl a b -> toList a <> toList b
