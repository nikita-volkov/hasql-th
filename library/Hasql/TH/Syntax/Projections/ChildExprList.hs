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
selectNoParens (SelectNoParens a b c d e) =
  foldable withClause a <>
  selectClause b <>
  foldable sortClause c <>
  foldable selectLimit d <>
  foldable forLockingClause e

withClause (WithClause _ a) = foldable commonTableExpr a

commonTableExpr (CommonTableExpr a b c d) = preparableStmt d

selectLimit = \ case
  LimitOffsetSelectLimit a b -> limitClause a <> offsetClause b
  OffsetLimitSelectLimit a b -> offsetClause a <> limitClause b
  LimitSelectLimit a -> limitClause a
  OffsetSelectLimit a -> offsetClause a

limitClause = \ case
  LimitLimitClause a b -> selectLimitValue a <> toList b
  FetchOnlyLimitClause a b c -> foldable selectFetchFirstValue b

offsetClause = \ case
  ExprOffsetClause a -> [a]
  FetchFirstOffsetClause a b -> selectFetchFirstValue a

selectFetchFirstValue = \ case
  ExprSelectFetchFirstValue a -> [a]
  NumSelectFetchFirstValue _ _ -> []

selectLimitValue = \ case
  ExprSelectLimitValue a -> [a]
  AllSelectLimitValue -> []

forLockingClause = \ case
  ItemsForLockingClause a -> foldable forLockingItem a
  ReadOnlyForLockingClause -> []

forLockingItem (ForLockingItem a b c) =
  foldable (foldable qualifiedName) b

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
optTempTableName _ = []

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
  DefaultExpr -> []
  QualifiedNameExpr a -> qualifiedName a
  LiteralExpr a -> literal a
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


-- * Literals
-------------------------

literal = \ case
  IntLiteral _ -> []
  FloatLiteral _ -> []
  StringLiteral _ -> []
  BitLiteral _ -> []
  HexLiteral _ -> []
  FuncLiteral a b _ -> qualifiedName a <> foldable funcLiteralArgList b
  ConstTypenameLiteral a _ -> constTypename a
  StringIntervalLiteral _ a -> foldable interval a
  IntIntervalLiteral _ _ -> []
  BoolLiteral _ -> []
  NullLiteral -> []

funcLiteralArgList (FuncLiteralArgList a b) = foldable funcArgExpr a <> foldable sortClause b

constTypename = \ case
  NumericConstTypename a -> numeric a
  ConstBitConstTypename a -> constBit a
  ConstCharacterConstTypename a -> constCharacter a
  ConstDatetimeConstTypename a -> constDatetime a

numeric = \ case
  IntNumeric -> []
  IntegerNumeric -> []
  SmallintNumeric -> []
  BigintNumeric -> []
  RealNumeric -> []
  FloatNumeric _ -> []
  DoublePrecisionNumeric -> []
  DecimalNumeric a -> foldable toList a
  DecNumeric a -> foldable toList a
  NumericNumeric a -> foldable toList a
  BooleanNumeric -> []

constBit (ConstBit _ a) = foldable toList a

constCharacter (ConstCharacter _ _) = []

constDatetime _ = []

interval _ = []


-- * Names
-------------------------

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
