module Hasql.TH.Syntax.Projections.ChildExprList where

import Hasql.TH.Prelude hiding (sortBy, bit)
import Hasql.TH.Syntax.Ast


-- * Types
-------------------------

data ChildExpr = AChildExpr AExpr | BChildExpr BExpr | CChildExpr CExpr
  deriving (Show, Eq, Ord)


-- * Helpers
-------------------------

foldable :: (Foldable f, Functor f) => (a -> [ChildExpr]) -> f a -> [ChildExpr]
foldable a = concat . toList . fmap a


-- *
-------------------------

{-|
Dives one level of recursion.
-}
childExpr = \ case
  AChildExpr a -> aChildExpr a
  BChildExpr a -> bChildExpr a
  CChildExpr a -> cChildExpr a

aChildExpr = \ case
  CExprAExpr a -> cChildExpr a
  TypecastAExpr a b -> aExpr a <> typecastTypename b
  CollateAExpr a b -> aExpr a <> anyName b
  AtTimeZoneAExpr a b -> aExpr a <> aExpr b
  PlusAExpr a -> aExpr a
  MinusAExpr a -> aExpr a
  SymbolicBinOpAExpr a b c -> aExpr a <> symbolicExprBinOp b <> aExpr c
  PrefixQualOpAExpr a b -> qualOp a <> aExpr b
  SuffixQualOpAExpr a b -> aExpr a <> qualOp b
  AndAExpr a b -> aExpr a <> aExpr b
  OrAExpr a b -> aExpr a <> aExpr b
  NotAExpr a -> aExpr a
  VerbalExprBinOpAExpr a b c d e -> aExpr a <> verbalExprBinOp c <> aExpr d <> foldable aExpr e
  ReversableOpAExpr a b c -> aExpr a <> aExprReversableOp c
  IsnullAExpr a -> aExpr a
  NotnullAExpr a -> aExpr a
  OverlapsAExpr a b -> row a <> row b
  SubqueryAExpr a b c d -> aExpr a <> subqueryOp b <> subType c <> either selectWithParens aExpr d
  UniqueAExpr a -> selectWithParens a
  DefaultAExpr -> []

bChildExpr = \ case
  CExprBExpr a -> cChildExpr a
  TypecastBExpr a b -> bExpr a <> typecastTypename b
  PlusBExpr a -> bExpr a
  MinusBExpr a -> bExpr a
  SymbolicBinOpBExpr a b c -> bExpr a <> symbolicExprBinOp b <> bExpr c
  QualOpBExpr a b -> qualOp a <> bExpr b
  IsOpBExpr a b c -> bExpr a <> bExprIsOp c

cChildExpr = \ case
  ColumnrefCExpr a -> columnref a
  AexprConstCExpr a -> aexprConst a
  ParamCExpr a b -> foldable indirection b
  InParensCExpr a b -> aExpr a <> foldable indirection b
  CaseCExpr a -> caseExpr a
  FuncCExpr a -> funcExpr a
  SelectWithParensCExpr a b -> selectWithParens a <> foldable indirection b
  ExistsCExpr a -> selectWithParens a
  ArrayCExpr a -> either selectWithParens arrayExpr a
  ExplicitRowCExpr a -> explicitRow a
  ImplicitRowCExpr a -> implicitRow a
  GroupingCExpr a -> exprList a


-- *
-------------------------

preparableStmt = \ case
  SelectPreparableStmt a -> selectStmt a

selectStmt = \ case
  Left a -> selectNoParens a
  Right a -> selectWithParens a

selectNoParens (SelectNoParens a b c d e) =
  foldable withClause a <>
  selectClause b <>
  foldable sortClause c <>
  foldable selectLimit d <>
  foldable forLockingClause e

selectWithParens = \ case
  NoParensSelectWithParens a -> selectNoParens a
  WithParensSelectWithParens a -> selectWithParens a

withClause (WithClause _ a) = foldable commonTableExpr a

commonTableExpr (CommonTableExpr a b c d) = preparableStmt d

selectLimit = \ case
  LimitOffsetSelectLimit a b -> limitClause a <> offsetClause b
  OffsetLimitSelectLimit a b -> offsetClause a <> limitClause b
  LimitSelectLimit a -> limitClause a
  OffsetSelectLimit a -> offsetClause a

limitClause = \ case
  LimitLimitClause a b -> selectLimitValue a <> exprList b
  FetchOnlyLimitClause a b c -> foldable selectFetchFirstValue b

offsetClause = \ case
  ExprOffsetClause a -> aExpr a
  FetchFirstOffsetClause a b -> selectFetchFirstValue a

selectFetchFirstValue = \ case
  ExprSelectFetchFirstValue a -> cExpr a
  NumSelectFetchFirstValue _ _ -> []

selectLimitValue = \ case
  ExprSelectLimitValue a -> aExpr a
  AllSelectLimitValue -> []

forLockingClause = \ case
  ItemsForLockingClause a -> foldable forLockingItem a
  ReadOnlyForLockingClause -> []

forLockingItem (ForLockingItem a b c) =
  foldable (foldable qualifiedName) b

selectClause = either simpleSelect selectWithParens

simpleSelect = \ case
  NormalSimpleSelect a b c d e f g ->
    foldable targeting a <> foldable intoClause b <> foldable fromClause c <>
    foldable whereClause d <> foldable groupClause e <> foldable havingClause f <>
    foldable windowClause g
  ValuesSimpleSelect a -> valuesClause a
  BinSimpleSelect _ a _ b -> selectClause a <> selectClause b

targeting = \ case
  NormalTargeting a -> foldable targetEl a
  AllTargeting a -> foldable (foldable targetEl) a
  DistinctTargeting a b -> foldable exprList a <> foldable targetEl b

targetEl = \ case
  AliasedExprTargetEl a _ -> aExpr a
  ImplicitlyAliasedExprTargetEl a _ -> aExpr a
  ExprTargetEl a -> aExpr a
  AsteriskTargetEl -> []

intoClause = optTempTableName

fromClause = foldable tableRef

whereClause = aExpr

groupClause = foldable groupByItem

havingClause = aExpr

windowClause = foldable windowDefinition

valuesClause = foldable exprList

optTempTableName _ = []

groupByItem = \ case
  ExprGroupByItem a -> aExpr a
  EmptyGroupingSetGroupByItem -> []
  RollupGroupByItem a -> exprList a
  CubeGroupByItem a -> exprList a
  GroupingSetsGroupByItem a -> foldable groupByItem a

windowDefinition (WindowDefinition _ a) = windowSpecification a

windowSpecification (WindowSpecification _ a b c) = foldable (foldMap aExpr) a <> foldable sortClause b <> foldable frameClause c

frameClause (FrameClause _ a _) = frameExtent a

frameExtent = \ case
  SingularFrameExtent a -> frameBound a
  BetweenFrameExtent a b -> frameBound a <> frameBound b

frameBound = \ case
  UnboundedPrecedingFrameBound -> []
  UnboundedFollowingFrameBound -> []
  CurrentRowFrameBound -> []
  PrecedingFrameBound a -> aExpr a
  FollowingFrameBound a -> aExpr a

sortClause = foldable sortBy

sortBy (SortBy a _) = aExpr a

tableRef = \ case
  RelationExprTableRef a _ -> relationExpr a
  SelectTableRef _ a _ -> selectWithParens a
  JoinTableRef a _ -> joinedTable a

relationExpr = \ case
  SimpleRelationExpr a _ -> qualifiedName a
  OnlyRelationExpr a _ -> qualifiedName a

joinedTable = \ case
  InParensJoinedTable a -> joinedTable a
  MethJoinedTable a b c -> joinMeth a <> tableRef b <> tableRef c

joinMeth = \ case
  CrossJoinMeth -> []
  QualJoinMeth _ a -> joinQual a
  NaturalJoinMeth _ -> []

joinQual = \ case
  UsingJoinQual _ -> []
  OnJoinQual a -> aExpr a


-- *
-------------------------

exprList = fmap AChildExpr . toList

aExpr = pure . AChildExpr
bExpr = pure . BChildExpr
cExpr = pure . CChildExpr

funcExpr = \ case
  ApplicationFuncExpr a b c d -> funcApplication a <> foldable withinGroupClause b <> foldable filterClause c <> foldable overClause d
  SubexprFuncExpr a -> funcExprCommonSubexpr a

withinGroupClause = sortClause

filterClause a = aExpr a

overClause = \ case
  WindowOverClause a -> windowSpecification a
  ColIdOverClause _ -> []

funcExprCommonSubexpr = \ case
  CollationForFuncExprCommonSubexpr a -> aExpr a
  CurrentDateFuncExprCommonSubexpr -> []
  CurrentTimeFuncExprCommonSubexpr _ -> []
  CurrentTimestampFuncExprCommonSubexpr _ -> []
  LocalTimeFuncExprCommonSubexpr _ -> []
  LocalTimestampFuncExprCommonSubexpr _ -> []
  CurrentRoleFuncExprCommonSubexpr -> []
  CurrentUserFuncExprCommonSubexpr -> []
  SessionUserFuncExprCommonSubexpr -> []
  UserFuncExprCommonSubexpr -> []
  CurrentCatalogFuncExprCommonSubexpr -> []
  CurrentSchemaFuncExprCommonSubexpr -> []
  CastFuncExprCommonSubexpr a b -> aExpr a <> typename b
  ExtractFuncExprCommonSubexpr a -> foldable extractList a
  OverlayFuncExprCommonSubexpr a -> overlayList a
  PositionFuncExprCommonSubexpr a -> foldable positionList a
  SubstringFuncExprCommonSubexpr a -> foldable substrList a
  TreatFuncExprCommonSubexpr a b -> aExpr a <> typename b
  TrimFuncExprCommonSubexpr a b -> foldable trimModifier a <> trimList b
  NullIfFuncExprCommonSubexpr a b -> aExpr a <> aExpr b
  CoalesceFuncExprCommonSubexpr a -> exprList a
  GreatestFuncExprCommonSubexpr a -> exprList a
  LeastFuncExprCommonSubexpr a -> exprList a

extractList (ExtractList a b) = extractArg a <> aExpr b

extractArg _ = []

overlayList (OverlayList a b c d) = foldable aExpr ([a, b, c] <> toList d)

positionList (PositionList a b) = bExpr a <> bExpr b

substrList = \ case
  ExprSubstrList a b -> aExpr a <> substrListFromFor b
  ExprListSubstrList a -> exprList a

substrListFromFor = \ case
  FromForSubstrListFromFor a b -> aExpr a <> aExpr b
  ForFromSubstrListFromFor a b -> aExpr a <> aExpr b
  FromSubstrListFromFor a -> aExpr a
  ForSubstrListFromFor a -> aExpr a

trimModifier _ = []

trimList = \ case
  ExprFromExprListTrimList a b -> aExpr a <> exprList b
  FromExprListTrimList a -> exprList a
  ExprListTrimList a -> exprList a  

whenClause (WhenClause a b) = aExpr a <> aExpr b

funcApplication (FuncApplication a b) = funcName a <> foldable funcApplicationParams b

funcApplicationParams = \ case
  NormalFuncApplicationParams _ a b -> foldable funcArgExpr a <> foldable (foldable sortBy) b
  VariadicFuncApplicationParams a b c -> foldable (foldable funcArgExpr) a <> funcArgExpr b <> foldable (foldable sortBy) c
  StarFuncApplicationParams -> []

funcArgExpr = \ case
  ExprFuncArgExpr a -> aExpr a
  ColonEqualsFuncArgExpr _ a -> aExpr a
  EqualsGreaterFuncArgExpr _ a -> aExpr a

caseExpr (CaseExpr a b c) = foldable aExpr a <> whenClauseList b <> foldable aExpr c

whenClauseList = foldable whenClause

arrayExpr = \ case
  ExprListArrayExpr a -> exprList a
  ArrayExprListArrayExpr a -> arrayExprList a
  EmptyArrayExpr -> []

arrayExprList = foldable arrayExpr

inExpr = \ case
  SelectInExpr a -> selectWithParens a
  ExprListInExpr a -> exprList a


-- * Operators
-------------------------

symbolicExprBinOp = \ case
  MathSymbolicExprBinOp a -> mathOp a
  QualSymbolicExprBinOp a -> qualOp a

qualOp = \ case
  OpQualOp a -> op a
  OperatorQualOp a -> anyOperator a

verbalExprBinOp = const []

aExprReversableOp = \ case
  NullAExprReversableOp -> []
  TrueAExprReversableOp -> []
  FalseAExprReversableOp -> []
  UnknownAExprReversableOp -> []
  DistinctFromAExprReversableOp a -> aExpr a
  OfAExprReversableOp a -> typeList a
  BetweenAExprReversableOp a b c -> bExpr b <> aExpr c
  BetweenSymmetricAExprReversableOp a b -> bExpr a <> aExpr b
  InAExprReversableOp a -> inExpr a
  DocumentAExprReversableOp -> []

subqueryOp = \ case
  AllSubqueryOp a -> allOp a
  AnySubqueryOp a -> anyOperator a
  LikeSubqueryOp _ -> []
  IlikeSubqueryOp _ -> []

bExprIsOp = \ case
  DistinctFromBExprIsOp a -> bExpr a
  OfBExprIsOp a -> typeList a
  DocumentBExprIsOp -> []

allOp = \ case
  OpAllOp a -> op a
  MathAllOp a -> mathOp a

anyOperator = \ case
  AllOpAnyOperator a -> allOp a
  QualifiedAnyOperator a b -> colId a <> anyOperator b

op = const []

mathOp = const []


-- * Rows
-------------------------

row = \ case
  ExplicitRowRow a -> explicitRow a
  ImplicitRowRow a -> implicitRow a

explicitRow = foldable exprList

implicitRow (ImplicitRow a b) = exprList a <> aExpr b


-- * Constants
-------------------------

aexprConst = \ case
  IAexprConst _ -> []
  FAexprConst _ -> []
  SAexprConst _ -> []
  BAexprConst _ -> []
  XAexprConst _ -> []
  FuncAexprConst a b _ -> funcName a <> foldable funcConstArgs b
  ConstTypenameAexprConst a _ -> constTypename a
  StringIntervalAexprConst _ a -> foldable interval a
  IntIntervalAexprConst _ _ -> []
  BoolAexprConst _ -> []
  NullAexprConst -> []

funcConstArgs (FuncConstArgs a b) = foldable funcArgExpr a <> foldable sortClause b

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
  DecimalNumeric a -> foldable exprList a
  DecNumeric a -> foldable exprList a
  NumericNumeric a -> foldable exprList a
  BooleanNumeric -> []

bit (Bit _ a) = foldable exprList a

constBit = bit

constCharacter (ConstCharacter _ _) = []

constDatetime _ = []

interval _ = []


-- * Names
-------------------------

ident _ = []

colId = ident

anyName (AnyName a b) = colId a <> foldable attrs b

columnref (Columnref a b) = colId a <> foldable indirection b

funcName = \ case
  TypeFuncName a -> typeFunctionName a
  IndirectedFuncName a b -> colId a <> indirection b

qualifiedName = \ case
  SimpleQualifiedName _ -> []
  IndirectedQualifiedName _ a -> indirection a

indirection = foldable indirectionEl

indirectionEl = \ case
  AttrNameIndirectionEl _ -> []
  AllIndirectionEl -> []
  ExprIndirectionEl a -> aExpr a
  SliceIndirectionEl a b -> exprList a <> exprList b


-- * Types
-------------------------

typeList = foldable typename

typecastTypename _ = []

typename = \ case
  ArrayBoundsTypename _ a b -> simpleTypename a <> arrayBounds b
  ArrayDimTypename _ a _ -> simpleTypename a

simpleTypename = \ case
  GenericTypeSimpleTypename a -> genericType a
  NumericSimpleTypename a -> numeric a
  BitSimpleTypename a -> bit a
  CharacterSimpleTypename a -> character a
  ConstDatetimeSimpleTypename a -> constDatetime a
  ConstIntervalSimpleTypename a -> either (foldable interval) (const []) a

arrayBounds _ = []

genericType (GenericType a b c) = typeFunctionName a <> foldable attrs b <> foldable typeModifiers c

typeFunctionName = ident

attrs = foldable attrName

attrName _ = []

typeModifiers = exprList

character _ = []

subType _ = []
