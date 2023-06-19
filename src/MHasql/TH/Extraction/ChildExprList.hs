{-# OPTIONS -Wno-missing-signatures #-}
module MHasql.TH.Extraction.ChildExprList where

import MHasql.TH.Prelude
import PostgresqlSyntax.Ast

-- * Types

data ChildExpr = AChildExpr AExpr | BChildExpr BExpr | CChildExpr CExpr
  deriving (Show, Eq, Ord)

-- *

-- |
-- Dives one level of recursion.
childExpr = \case
  AChildExpr a -> aChildExpr a
  BChildExpr a -> bChildExpr a
  CChildExpr a -> cChildExpr a

aChildExpr = \case
  CExprAExpr a -> cChildExpr a
  TypecastAExpr a b -> aExpr a <> typename b
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
  VerbalExprBinOpAExpr a _b c d e -> aExpr a <> verbalExprBinOp c <> aExpr d <> foldMap aExpr e
  ReversableOpAExpr a _b c -> aExpr a <> aExprReversableOp c
  IsnullAExpr a -> aExpr a
  NotnullAExpr a -> aExpr a
  OverlapsAExpr a b -> row a <> row b
  SubqueryAExpr a b c d -> aExpr a <> subqueryOp b <> subType c <> either selectWithParens aExpr d
  UniqueAExpr a -> selectWithParens a
  DefaultAExpr -> []

bChildExpr = \case
  CExprBExpr a -> cChildExpr a
  TypecastBExpr a b -> bExpr a <> typename b
  PlusBExpr a -> bExpr a
  MinusBExpr a -> bExpr a
  SymbolicBinOpBExpr a b c -> bExpr a <> symbolicExprBinOp b <> bExpr c
  QualOpBExpr a b -> qualOp a <> bExpr b
  IsOpBExpr a _b c -> bExpr a <> bExprIsOp c

cChildExpr = \case
  ColumnrefCExpr a -> columnref a
  AexprConstCExpr a -> aexprConst a
  ParamCExpr _a b -> foldMap indirection b
  InParensCExpr a b -> aExpr a <> foldMap indirection b
  CaseCExpr a -> caseExpr a
  FuncCExpr a -> funcExpr a
  SelectWithParensCExpr a b -> selectWithParens a <> foldMap indirection b
  ExistsCExpr a -> selectWithParens a
  ArrayCExpr a -> either selectWithParens arrayExpr a
  ExplicitRowCExpr a -> explicitRow a
  ImplicitRowCExpr a -> implicitRow a
  GroupingCExpr a -> exprList a

-- *

preparableStmt = \case
  SelectPreparableStmt a -> selectStmt a
  InsertPreparableStmt a -> insertStmt a
  UpdatePreparableStmt a -> updateStmt a
  DeletePreparableStmt a -> deleteStmt a
  CallPreparableStmt a -> callStmt a

-- * Call

callStmt (CallStmt a) = funcApplication a

-- * Insert

insertStmt (InsertStmt a b c d e) =
  foldMap withClause a
    <> insertTarget b
    <> insertRest c
    <> foldMap onConflict d
    <> foldMap returningClause e

insertTarget (InsertTarget a b) = qualifiedName a <> colId b

insertRest = \case
  SelectInsertRest a b c -> foldMap insertColumnList a <> foldMap overrideKind b <> selectStmt c
  DefaultValuesInsertRest -> []

overrideKind _ = []

insertColumnList = foldMap insertColumnItem

insertColumnItem (InsertColumnItem a b) = colId a <> foldMap indirection b

onConflict (OnConflict a b) = foldMap confExpr a <> onConflictDo b

onConflictDo = \case
  UpdateOnConflictDo b c -> setClauseList b <> foldMap whereClause c
  NothingOnConflictDo -> []

confExpr = \case
  WhereConfExpr a b -> indexParams a <> foldMap whereClause b
  ConstraintConfExpr a -> name a

returningClause = targetList

-- * Update

updateStmt (UpdateStmt a b c d e f) =
  foldMap withClause a
    <> relationExprOptAlias b
    <> setClauseList c
    <> foldMap fromClause d
    <> foldMap whereOrCurrentClause e
    <> foldMap returningClause f

setClauseList = foldMap setClause

setClause = \case
  TargetSetClause a b -> setTarget a <> aExpr b
  TargetListSetClause a b -> setTargetList a <> aExpr b

setTarget (SetTarget a b) = colId a <> foldMap indirection b

setTargetList = foldMap setTarget

-- * Delete

deleteStmt (DeleteStmt a b c d e) =
  foldMap withClause a
    <> relationExprOptAlias b
    <> foldMap usingClause c
    <> foldMap whereOrCurrentClause d
    <> foldMap returningClause e

usingClause = fromList

-- * Select

selectStmt = \case
  Left a -> selectNoParens a
  Right a -> selectWithParens a

selectNoParens (SelectNoParens a b c d e) =
  foldMap withClause a
    <> selectClause b
    <> foldMap sortClause c
    <> foldMap selectLimit d
    <> foldMap forLockingClause e

selectWithParens = \case
  NoParensSelectWithParens a -> selectNoParens a
  WithParensSelectWithParens a -> selectWithParens a

withClause (WithClause _ a) = foldMap commonTableExpr a

commonTableExpr (CommonTableExpr _a _b _c d) = preparableStmt d

selectLimit = \case
  LimitOffsetSelectLimit a b -> limitClause a <> offsetClause b
  OffsetLimitSelectLimit a b -> offsetClause a <> limitClause b
  LimitSelectLimit a -> limitClause a
  OffsetSelectLimit a -> offsetClause a

limitClause = \case
  LimitLimitClause a b -> selectLimitValue a <> exprList b
  FetchOnlyLimitClause _a b _c -> foldMap selectFetchFirstValue b

offsetClause = \case
  ExprOffsetClause a -> aExpr a
  FetchFirstOffsetClause a _b -> selectFetchFirstValue a

selectFetchFirstValue = \case
  ExprSelectFetchFirstValue a -> cExpr a
  NumSelectFetchFirstValue _ _ -> []

selectLimitValue = \case
  ExprSelectLimitValue a -> aExpr a
  AllSelectLimitValue -> []

forLockingClause = \case
  ItemsForLockingClause a -> foldMap forLockingItem a
  ReadOnlyForLockingClause -> []

forLockingItem (ForLockingItem _a b _c) =
  foldMap (foldMap qualifiedName) b

selectClause = either simpleSelect selectWithParens

simpleSelect = \case
  NormalSimpleSelect a b c d e f g ->
    foldMap targeting a <> foldMap intoClause b <> foldMap fromClause c
      <> foldMap whereClause d
      <> foldMap groupClause e
      <> foldMap havingClause f
      <> foldMap windowClause g
  ValuesSimpleSelect a -> valuesClause a
  TableSimpleSelect a -> relationExpr a
  BinSimpleSelect _ a _ b -> selectClause a <> selectClause b

targeting = \case
  NormalTargeting a -> foldMap targetEl a
  AllTargeting a -> foldMap (foldMap targetEl) a
  DistinctTargeting a b -> foldMap exprList a <> foldMap targetEl b

targetList = foldMap targetEl

targetEl = \case
  AliasedExprTargetEl a _ -> aExpr a
  ImplicitlyAliasedExprTargetEl a _ -> aExpr a
  ExprTargetEl a -> aExpr a
  AsteriskTargetEl -> []

intoClause = optTempTableName

fromClause = fromList

fromList = foldMap tableRef

whereClause = aExpr

whereOrCurrentClause = \case
  ExprWhereOrCurrentClause a -> aExpr a
  CursorWhereOrCurrentClause a -> cursorName a

groupClause = foldMap groupByItem

havingClause = aExpr

windowClause = foldMap windowDefinition

valuesClause = foldMap exprList

optTempTableName _ = []

groupByItem = \case
  ExprGroupByItem a -> aExpr a
  EmptyGroupingSetGroupByItem -> []
  RollupGroupByItem a -> exprList a
  CubeGroupByItem a -> exprList a
  GroupingSetsGroupByItem a -> foldMap groupByItem a

windowDefinition (WindowDefinition _ a) = windowSpecification a

windowSpecification (WindowSpecification _ a b c) = foldMap (foldMap aExpr) a <> foldMap sortClause b <> foldMap frameClause c

frameClause (FrameClause _ a _) = frameExtent a

frameExtent = \case
  SingularFrameExtent a -> frameBound a
  BetweenFrameExtent a b -> frameBound a <> frameBound b

frameBound = \case
  UnboundedPrecedingFrameBound -> []
  UnboundedFollowingFrameBound -> []
  CurrentRowFrameBound -> []
  PrecedingFrameBound a -> aExpr a
  FollowingFrameBound a -> aExpr a

sortClause = foldMap sortBy

sortBy = \case
  UsingSortBy a b c -> aExpr a <> qualAllOp b <> foldMap nullsOrder c
  AscDescSortBy a b c -> aExpr a <> foldMap ascDesc b <> foldMap nullsOrder c

-- * Table refs

tableRef = \case
  RelationExprTableRef a b c -> relationExpr a <> foldMap aliasClause b <> foldMap tablesampleClause c
  FuncTableRef _a b c -> funcTable b <> foldMap funcAliasClause c
  SelectTableRef _ a _ -> selectWithParens a
  JoinTableRef a _ -> joinedTable a

relationExpr = \case
  SimpleRelationExpr a _ -> qualifiedName a
  OnlyRelationExpr a _ -> qualifiedName a

relationExprOptAlias (RelationExprOptAlias a b) = relationExpr a <> foldMap (colId . snd) b

tablesampleClause (TablesampleClause a b c) = funcName a <> exprList b <> foldMap repeatableClause c

repeatableClause = aExpr

funcTable = \case
  FuncExprFuncTable a b -> funcExprWindowless a <> optOrdinality b
  RowsFromFuncTable a b -> rowsfromList a <> optOrdinality b

rowsfromItem (RowsfromItem a b) = funcExprWindowless a <> foldMap colDefList b

rowsfromList = foldMap rowsfromItem

colDefList = tableFuncElementList

optOrdinality = const []

tableFuncElementList = foldMap tableFuncElement

tableFuncElement (TableFuncElement a b c) = colId a <> typename b <> foldMap collateClause c

collateClause = anyName

aliasClause = const []

funcAliasClause = \case
  AliasFuncAliasClause a -> aliasClause a
  AsFuncAliasClause a -> tableFuncElementList a
  AsColIdFuncAliasClause a b -> colId a <> tableFuncElementList b
  ColIdFuncAliasClause a b -> colId a <> tableFuncElementList b

joinedTable = \case
  InParensJoinedTable a -> joinedTable a
  MethJoinedTable a b c -> joinMeth a <> tableRef b <> tableRef c

joinMeth = \case
  CrossJoinMeth -> []
  QualJoinMeth _ a -> joinQual a
  NaturalJoinMeth _ -> []

joinQual = \case
  UsingJoinQual _ -> []
  OnJoinQual a -> aExpr a

-- *

exprList = fmap AChildExpr . toList

aExpr = pure . AChildExpr

bExpr = pure . BChildExpr

cExpr = pure . CChildExpr

funcExpr = \case
  ApplicationFuncExpr a b c d -> funcApplication a <> foldMap withinGroupClause b <> foldMap filterClause c <> foldMap overClause d
  SubexprFuncExpr a -> funcExprCommonSubexpr a

funcExprWindowless = \case
  ApplicationFuncExprWindowless a -> funcApplication a
  CommonSubexprFuncExprWindowless a -> funcExprCommonSubexpr a

withinGroupClause = sortClause

filterClause = aExpr

overClause = \case
  WindowOverClause a -> windowSpecification a
  ColIdOverClause _ -> []

funcExprCommonSubexpr = \case
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
  ExtractFuncExprCommonSubexpr a -> foldMap extractList a
  OverlayFuncExprCommonSubexpr a -> overlayList a
  PositionFuncExprCommonSubexpr a -> foldMap positionList a
  SubstringFuncExprCommonSubexpr a -> foldMap substrList a
  TreatFuncExprCommonSubexpr a b -> aExpr a <> typename b
  TrimFuncExprCommonSubexpr a b -> foldMap trimModifier a <> trimList b
  NullIfFuncExprCommonSubexpr a b -> aExpr a <> aExpr b
  CoalesceFuncExprCommonSubexpr a -> exprList a
  GreatestFuncExprCommonSubexpr a -> exprList a
  LeastFuncExprCommonSubexpr a -> exprList a

extractList (ExtractList a b) = extractArg a <> aExpr b

extractArg _ = []

overlayList (OverlayList a b c d) = foldMap aExpr ([a, b, c] <> toList d)

positionList (PositionList a b) = bExpr a <> bExpr b

substrList = \case
  ExprSubstrList a b -> aExpr a <> substrListFromFor b
  ExprListSubstrList a -> exprList a

substrListFromFor = \case
  FromForSubstrListFromFor a b -> aExpr a <> aExpr b
  ForFromSubstrListFromFor a b -> aExpr a <> aExpr b
  FromSubstrListFromFor a -> aExpr a
  ForSubstrListFromFor a -> aExpr a

trimModifier _ = []

trimList = \case
  ExprFromExprListTrimList a b -> aExpr a <> exprList b
  FromExprListTrimList a -> exprList a
  ExprListTrimList a -> exprList a

whenClause (WhenClause a b) = aExpr a <> aExpr b

funcApplication (FuncApplication a b) = funcName a <> foldMap funcApplicationParams b

funcApplicationParams = \case
  NormalFuncApplicationParams _ a b -> foldMap funcArgExpr a <> foldMap (foldMap sortBy) b
  VariadicFuncApplicationParams a b c -> foldMap (foldMap funcArgExpr) a <> funcArgExpr b <> foldMap (foldMap sortBy) c
  StarFuncApplicationParams -> []

funcArgExpr = \case
  ExprFuncArgExpr a -> aExpr a
  ColonEqualsFuncArgExpr _ a -> aExpr a
  EqualsGreaterFuncArgExpr _ a -> aExpr a

caseExpr (CaseExpr a b c) = foldMap aExpr a <> whenClauseList b <> foldMap aExpr c

whenClauseList = foldMap whenClause

arrayExpr = \case
  ExprListArrayExpr a -> exprList a
  ArrayExprListArrayExpr a -> arrayExprList a
  EmptyArrayExpr -> []

arrayExprList = foldMap arrayExpr

inExpr = \case
  SelectInExpr a -> selectWithParens a
  ExprListInExpr a -> exprList a

-- * Operators

symbolicExprBinOp = \case
  MathSymbolicExprBinOp a -> mathOp a
  QualSymbolicExprBinOp a -> qualOp a

qualOp = \case
  OpQualOp a -> op a
  OperatorQualOp a -> anyOperator a

qualAllOp = \case
  AllQualAllOp a -> allOp a
  AnyQualAllOp a -> anyOperator a

verbalExprBinOp = const []

aExprReversableOp = \case
  NullAExprReversableOp -> []
  TrueAExprReversableOp -> []
  FalseAExprReversableOp -> []
  UnknownAExprReversableOp -> []
  DistinctFromAExprReversableOp a -> aExpr a
  OfAExprReversableOp a -> typeList a
  BetweenAExprReversableOp _a b c -> bExpr b <> aExpr c
  BetweenSymmetricAExprReversableOp a b -> bExpr a <> aExpr b
  InAExprReversableOp a -> inExpr a
  DocumentAExprReversableOp -> []

subqueryOp = \case
  AllSubqueryOp a -> allOp a
  AnySubqueryOp a -> anyOperator a
  LikeSubqueryOp _ -> []
  IlikeSubqueryOp _ -> []

bExprIsOp = \case
  DistinctFromBExprIsOp a -> bExpr a
  OfBExprIsOp a -> typeList a
  DocumentBExprIsOp -> []

allOp = \case
  OpAllOp a -> op a
  MathAllOp a -> mathOp a

anyOperator = \case
  AllOpAnyOperator a -> allOp a
  QualifiedAnyOperator a b -> colId a <> anyOperator b

op = const []

mathOp = const []

-- * Rows

row = \case
  ExplicitRowRow a -> explicitRow a
  ImplicitRowRow a -> implicitRow a

explicitRow = foldMap exprList

implicitRow (ImplicitRow a b) = exprList a <> aExpr b

-- * Constants

aexprConst = \case
  IAexprConst _ -> []
  FAexprConst _ -> []
  SAexprConst _ -> []
  BAexprConst _ -> []
  XAexprConst _ -> []
  FuncAexprConst a b _ -> funcName a <> foldMap funcConstArgs b
  ConstTypenameAexprConst a _ -> constTypename a
  StringIntervalAexprConst _ a -> foldMap interval a
  IntIntervalAexprConst _ _ -> []
  BoolAexprConst _ -> []
  NullAexprConst -> []

funcConstArgs (FuncConstArgs a b) = foldMap funcArgExpr a <> foldMap sortClause b

constTypename = \case
  NumericConstTypename a -> numeric a
  ConstBitConstTypename a -> constBit a
  ConstCharacterConstTypename a -> constCharacter a
  ConstDatetimeConstTypename a -> constDatetime a

numeric = \case
  IntNumeric -> []
  IntegerNumeric -> []
  SmallintNumeric -> []
  BigintNumeric -> []
  RealNumeric -> []
  FloatNumeric _ -> []
  DoublePrecisionNumeric -> []
  DecimalNumeric a -> foldMap exprList a
  DecNumeric a -> foldMap exprList a
  NumericNumeric a -> foldMap exprList a
  BooleanNumeric -> []

bit (Bit _ a) = foldMap exprList a

constBit = bit

constCharacter (ConstCharacter _ _) = []

constDatetime _ = []

interval _ = []

-- * Names

ident _ = []

colId = ident

name = colId

cursorName = name

anyName (AnyName a b) = colId a <> foldMap attrs b

columnref (Columnref a b) = colId a <> foldMap indirection b

funcName = \case
  TypeFuncName a -> typeFunctionName a
  IndirectedFuncName a b -> colId a <> indirection b

qualifiedName = \case
  SimpleQualifiedName _ -> []
  IndirectedQualifiedName _ a -> indirection a

indirection = foldMap indirectionEl

indirectionEl = \case
  AttrNameIndirectionEl _ -> []
  AllIndirectionEl -> []
  ExprIndirectionEl a -> aExpr a
  SliceIndirectionEl a b -> exprList a <> exprList b

-- * Types

typeList = foldMap typename

typename (Typename _a b _c) =
  simpleTypename b

simpleTypename = \case
  GenericTypeSimpleTypename a -> genericType a
  NumericSimpleTypename a -> numeric a
  BitSimpleTypename a -> bit a
  CharacterSimpleTypename a -> character a
  ConstDatetimeSimpleTypename a -> constDatetime a
  ConstIntervalSimpleTypename a -> either (foldMap interval) (const []) a

arrayBounds _ = []

genericType (GenericType a b c) = typeFunctionName a <> foldMap attrs b <> foldMap typeModifiers c

typeFunctionName = ident

attrs = foldMap attrName

attrName _ = []

typeModifiers = exprList

character _ = []

subType _ = []

-- * Indexes

indexParams = foldMap indexElem

indexElem (IndexElem a b c _d _e) = indexElemDef a <> foldMap anyName b <> foldMap anyName c

indexElemDef = \case
  IdIndexElemDef a -> colId a
  FuncIndexElemDef a -> funcExprWindowless a
  ExprIndexElemDef a -> aExpr a

ascDesc = const []

nullsOrder = const []
