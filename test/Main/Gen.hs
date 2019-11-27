module Main.Gen where

import Hasql.TH.Prelude hiding (maybe, bool, sortBy, filter, bit)
import Hasql.TH.Syntax.Ast
import Hedgehog (Gen, MonadGen)
import Hedgehog.Gen
import qualified Hedgehog.Range as Range
import qualified Data.Text as Text
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import qualified Hasql.TH.Syntax.HashSet as HashSet
import qualified Hasql.TH.Syntax.Predicate as Predicate
import qualified Hasql.TH.Syntax.Validator as Validator



-- * Generic
-------------------------

inSet _set = filter (flip HashSet.member _set)

notInSet _set = filter (not . flip HashSet.member _set)


-- * Statements
-------------------------

preparableStmt = choice [
    SelectPreparableStmt <$> selectStmt
  ]


-- * Select
-------------------------

selectStmt = Left <$> selectNoParens

-- ** selectNoParens
-------------------------

selectNoParens = frequency [
    (90, SelectNoParens <$> maybe withClause <*> (Left <$> simpleSelect) <*> maybe sortClause <*> maybe selectLimit <*> maybe forLockingClause)
    ,
    (10, SelectNoParens <$> fmap Just withClause <*> selectClause <*> fmap Just sortClause <*> fmap Just selectLimit <*> fmap Just forLockingClause)
  ]

terminalSelectNoParens = 
  SelectNoParens <$> pure Nothing <*> (Left <$> terminalSimpleSelect) <*> pure Nothing <*> pure Nothing <*> pure Nothing

-- ** selectWithParens
-------------------------

selectWithParens = sized $ \ _size -> if _size <= 1
  then discard
  else frequency [
    (95, NoParensSelectWithParens <$> selectNoParens)
    ,
    (5, WithParensSelectWithParens <$> selectWithParens)
  ]

terminalSelectWithParens = NoParensSelectWithParens <$> terminalSelectNoParens

-- ** selectClause
-------------------------

selectClause = choice [
    Left <$> simpleSelect,
    Right <$> small selectWithParens
  ]

nonTrailingSelectClause = Left <$> nonTrailingSimpleSelect

-- ** simpleSelect
-------------------------

simpleSelect = choice [
    normalSimpleSelect,
    valuesSimpleSelect,
    small nonTrailingSelectClause >>= binSimpleSelect
  ]

nonTrailingSimpleSelect = choice [normalSimpleSelect, valuesSimpleSelect]

normalSimpleSelect = NormalSimpleSelect <$> maybe targeting <*> maybe intoClause <*> maybe fromClause <*> maybe whereClause <*> maybe groupClause <*> maybe havingClause <*> maybe windowClause

valuesSimpleSelect = ValuesSimpleSelect <$> valuesClause

binSimpleSelect _leftSelect = 
  BinSimpleSelect <$> selectBinOp <*> pure _leftSelect <*> maybe allOrDistinct <*> small selectClause

terminalSimpleSelect = pure (NormalSimpleSelect Nothing Nothing Nothing Nothing Nothing Nothing Nothing)


-- * Targeting
-------------------------

targeting = choice [
    NormalTargeting <$> targets,
    AllTargeting <$> maybe targets,
    DistinctTargeting <$> maybe (nonEmpty (Range.exponential 1 8) aExpr) <*> targets
  ]

targets = nonEmpty (Range.exponential 1 8) target

target = choice [
    pure AsteriskTarget,
    AliasedExprTarget <$> aExpr <*> name,
    ImplicitlyAliasedExprTarget <$> aExpr <*> name,
    ExprTarget <$> aExpr
  ]


-- * BinSimpleSelect
-------------------------

selectBinOp = element [UnionSelectBinOp, IntersectSelectBinOp, ExceptSelectBinOp]


-- * With Clause
-------------------------

withClause = WithClause <$> bool <*> nonEmpty (Range.exponential 1 7) commonTableExpr

commonTableExpr = CommonTableExpr <$> name <*> maybe (nonEmpty (Range.exponential 1 8) name) <*> maybe bool <*> small preparableStmt


-- * Into Clause
-------------------------

intoClause = optTempTableName

optTempTableName = choice [
    TemporaryOptTempTableName <$> bool <*> qualifiedName,
    TempOptTempTableName <$> bool <*> qualifiedName,
    LocalTemporaryOptTempTableName <$> bool <*> qualifiedName,
    LocalTempOptTempTableName <$> bool <*> qualifiedName,
    GlobalTemporaryOptTempTableName <$> bool <*> qualifiedName,
    GlobalTempOptTempTableName <$> bool <*> qualifiedName,
    UnloggedOptTempTableName <$> bool <*> qualifiedName,
    TableOptTempTableName <$> qualifiedName,
    QualifedOptTempTableName <$> qualifiedName
  ]


-- * From Clause
-------------------------

fromClause = nonEmpty (Range.exponential 1 8) tableRef

tableRef = choice [relationExprTableRef, selectTableRef, joinTableRef]
relationExprTableRef = RelationExprTableRef <$> relationExpr <*> maybe aliasClause
selectTableRef = SelectTableRef <$> bool <*> small selectWithParens <*> maybe aliasClause
joinTableRef = JoinTableRef <$> joinedTable <*> maybe aliasClause

relationExpr = choice [
    SimpleRelationExpr <$> qualifiedName <*> bool,
    OnlyRelationExpr <$> qualifiedName <*> bool
  ]

aliasClause = AliasClause <$> name <*> maybe (nonEmpty (Range.exponential 1 8) name)

joinedTable = frequency [
    (5,) $ InParensJoinedTable <$> joinedTable,
    (95,) $ MethJoinedTable <$> joinMeth <*> choice [relationExprTableRef, selectTableRef] <*> tableRef
  ]

joinMeth = choice [
    pure CrossJoinMeth,
    QualJoinMeth <$> maybe joinType <*> joinQual,
    NaturalJoinMeth <$> maybe joinType
  ]

joinType = choice [
    FullJoinType <$> bool,
    LeftJoinType <$> bool,
    RightJoinType <$> bool,
    pure InnerJoinType
  ]

joinQual = choice [
    UsingJoinQual <$> nonEmpty (Range.exponential 1 8) name,
    OnJoinQual <$> aExpr
  ]


-- * Group Clause
-------------------------

groupClause = nonEmpty (Range.exponential 1 8) groupByItem

groupByItem = choice [
    ExprGroupByItem <$> aExpr,
    pure EmptyGroupingSetGroupByItem,
    RollupGroupByItem <$> nonEmpty (Range.exponential 1 8) aExpr,
    CubeGroupByItem <$> nonEmpty (Range.exponential 1 8) aExpr,
    GroupingSetsGroupByItem <$> nonEmpty (Range.exponential 1 3) groupByItem
  ]


-- * Having Clause
-------------------------

havingClause = aExpr


-- * Where Clause
-------------------------

whereClause = aExpr


-- * Window Clause
-------------------------

windowClause = nonEmpty (Range.exponential 1 8) windowDefinition

windowDefinition = WindowDefinition <$> name <*> windowSpecification

windowSpecification = WindowSpecification <$> maybe name <*> maybe (nonEmpty (Range.exponential 1 8) aExpr) <*> maybe sortClause <*> maybe frameClause

frameClause = FrameClause <$> frameClauseMode <*> frameExtent <*> maybe windowExclusionClause

frameClauseMode = element [RangeFrameClauseMode, RowsFrameClauseMode, GroupsFrameClauseMode]

frameExtent = choice [
    SingularFrameExtent <$> frameBound,
    BetweenFrameExtent <$> frameBound <*> frameBound
  ]

frameBound = choice [
    pure UnboundedPrecedingFrameBound,
    pure UnboundedFollowingFrameBound,
    pure CurrentRowFrameBound,
    PrecedingFrameBound <$> aExpr,
    FollowingFrameBound <$> aExpr
  ]

windowExclusionClause = element [CurrentRowWindowExclusionClause, GroupWindowExclusionClause, TiesWindowExclusionClause, NoOthersWindowExclusionClause]


-- * Values Clause
-------------------------

valuesClause = nonEmpty (Range.exponential 1 8) (nonEmpty (Range.exponential 1 8) aExpr)


-- * Sort Clause
-------------------------

sortClause = nonEmpty (Range.exponential 1 8) sortBy

sortBy = SortBy <$> aExpr <*> maybe order

order = element [AscOrder, DescOrder]


-- * All or distinct
-------------------------

allOrDistinct = bool


-- * Limit
-------------------------

selectLimit = choice [
    LimitOffsetSelectLimit <$> limitClause <*> offsetClause,
    OffsetLimitSelectLimit <$> offsetClause <*> limitClause,
    LimitSelectLimit <$> limitClause,
    OffsetSelectLimit <$> offsetClause
  ]

limitClause = choice [
    LimitLimitClause <$> selectLimitValue <*> maybe aExpr,
    FetchOnlyLimitClause <$> bool <*> maybe selectFetchFirstValue <*> bool
  ]

selectFetchFirstValue = choice [
    ExprSelectFetchFirstValue <$> cExpr,
    NumSelectFetchFirstValue <$> bool <*> iconstOrFconst
  ]

selectLimitValue = choice [
    ExprSelectLimitValue <$> aExpr,
    pure AllSelectLimitValue
  ]

offsetClause = choice [
    ExprOffsetClause <$> aExpr,
    FetchFirstOffsetClause <$> selectFetchFirstValue <*> bool
  ]


-- * For Locking
-------------------------

forLockingClause = choice [
    ItemsForLockingClause <$> nonEmpty (Range.exponential 1 8) forLockingItem,
    pure ReadOnlyForLockingClause
  ]

forLockingItem = ForLockingItem <$> forLockingStrength <*> maybe (nonEmpty (Range.exponential 1 8) qualifiedName) <*> maybe bool

forLockingStrength = element [
    UpdateForLockingStrength,
    NoKeyUpdateForLockingStrength,
    ShareForLockingStrength,
    KeyForLockingStrength
  ]


-- * Expressions
-------------------------

exprList = nonEmpty (Range.exponential 1 7) aExpr

aExpr = recursive choice [
    CExprAExpr <$> cExpr,
    pure DefaultAExpr
  ] [
    TypecastAExpr <$> prefixAExpr <*> typecastTypename,
    CollateAExpr <$> prefixAExpr <*> anyName,
    AtTimeZoneAExpr <$> prefixAExpr <*> aExpr,
    PlusAExpr <$> aExpr,
    MinusAExpr <$> aExpr,
    SymbolicBinOpAExpr <$> prefixAExpr <*> symbolicExprBinOp <*> aExpr,
    PrefixQualOpAExpr <$> qualOp <*> aExpr,
    SuffixQualOpAExpr <$> prefixAExpr <*> qualOp,
    AndAExpr <$> prefixAExpr <*> aExpr,
    OrAExpr <$> prefixAExpr <*> aExpr,
    NotAExpr <$> aExpr,
    VerbalExprBinOpAExpr <$> prefixAExpr <*> bool <*> verbalExprBinOp <*> aExpr <*> maybe aExpr,
    ReversableOpAExpr <$> prefixAExpr <*> bool <*> aExprReversableOp,
    IsnullAExpr <$> prefixAExpr,
    NotnullAExpr <$> prefixAExpr,
    OverlapsAExpr <$> row <*> row,
    SubqueryAExpr <$> prefixAExpr <*> subqueryOp <*> subType <*> choice [Left <$> selectWithParens, Right <$> aExpr],
    UniqueAExpr <$> selectWithParens
  ]

prefixAExpr = choice [
    CExprAExpr <$> cExpr,
    pure DefaultAExpr,
    OverlapsAExpr <$> row <*> row,
    UniqueAExpr <$> selectWithParens
  ]

bExpr = recursive choice [
    CExprBExpr <$> cExpr
  ] [
    TypecastBExpr <$> prefixBExpr <*> typecastTypename,
    PlusBExpr <$> bExpr,
    MinusBExpr <$> bExpr,
    SymbolicBinOpBExpr <$> prefixBExpr <*> symbolicExprBinOp <*> bExpr,
    QualOpBExpr <$> qualOp <*> bExpr,
    IsOpBExpr <$> prefixBExpr <*> bool <*> bExprIsOp
  ]

prefixBExpr = choice [
    CExprBExpr <$> cExpr
  ]

cExpr = recursive choice [
    ColumnrefCExpr <$> columnref
  ] [
    AexprConstCExpr <$> aexprConst,
    ParamCExpr <$> integral (Range.linear 1 19) <*> maybe indirection,
    InParensCExpr <$> aExpr <*> maybe indirection,
    CaseCExpr <$> caseExpr,
    FuncCExpr <$> funcExpr,
    SelectWithParensCExpr <$> selectWithParens <*> maybe indirection,
    ExistsCExpr <$> selectWithParens,
    ArrayCExpr <$> choice [Left <$> selectWithParens, Right <$> arrayExpr],
    ExplicitRowCExpr <$> explicitRow,
    ImplicitRowCExpr <$> implicitRow,
    GroupingCExpr <$> exprList
  ]

-- **
-------------------------

caseExpr = CaseExpr <$> maybe aExpr <*> whenClauseList <*> maybe aExpr

whenClauseList = nonEmpty (Range.exponential 1 7) whenClause

whenClause = WhenClause <$> small aExpr <*> small aExpr

inExpr = choice [
    SelectInExpr <$> selectWithParens,
    ExprListInExpr <$> exprList
  ]

arrayExpr = small $ choice [
    ExprListArrayExpr <$> exprList,
    ArrayExprListArrayExpr <$> arrayExprList,
    pure EmptyArrayExpr
  ]

arrayExprList = nonEmpty (Range.exponential 1 4) arrayExpr

row = choice [
    ExplicitRowRow <$> explicitRow,
    ImplicitRowRow <$> implicitRow
  ]

explicitRow = maybe exprList

implicitRow = ImplicitRow <$> exprList <*> aExpr

-- ** FuncExpr
-------------------------

funcExpr = choice [
    ApplicationFuncExpr <$> funcApplication <*> maybe withinGroupClause <*> maybe filterClause <*> maybe overClause,
    SubexprFuncExpr <$> funcExprCommonSubExpr
  ]

funcApplication = FuncApplication <$> funcName <*> maybe funcApplicationParams

funcApplicationParams = choice [
    NormalFuncApplicationParams <$> maybe allOrDistinct <*> nonEmpty (Range.exponential 1 8) funcArgExpr <*> maybe sortClause,
    VariadicFuncApplicationParams <$> maybe (nonEmpty (Range.exponential 1 8) funcArgExpr) <*> funcArgExpr <*> maybe sortClause,
    pure StarFuncApplicationParams
  ]

funcArgExpr = choice [
    ExprFuncArgExpr <$> small aExpr,
    ColonEqualsFuncArgExpr <$> name <*> small aExpr,
    EqualsGreaterFuncArgExpr <$> name <*> small aExpr
  ]

withinGroupClause = sortClause

filterClause = aExpr

overClause = choice [WindowOverClause <$> windowSpecification, ColIdOverClause <$> colId]

funcExprCommonSubExpr = choice [
    CollationForFuncExprCommonSubExpr <$> aExpr,
    pure CurrentDateFuncExprCommonSubExpr,
    CurrentTimeFuncExprCommonSubExpr <$> maybe iconst,
    CurrentTimestampFuncExprCommonSubExpr <$> maybe iconst,
    LocalTimeFuncExprCommonSubExpr <$> maybe iconst,
    LocalTimestampFuncExprCommonSubExpr <$> maybe iconst,
    pure CurrentRoleFuncExprCommonSubExpr,
    pure CurrentUserFuncExprCommonSubExpr,
    pure SessionUserFuncExprCommonSubExpr,
    pure UserFuncExprCommonSubExpr,
    pure CurrentCatalogFuncExprCommonSubExpr,
    pure CurrentSchemaFuncExprCommonSubExpr,
    CastFuncExprCommonSubExpr <$> aExpr <*> typename,
    ExtractFuncExprCommonSubExpr <$> maybe extractList,
    OverlayFuncExprCommonSubExpr <$> overlayList,
    PositionFuncExprCommonSubExpr <$> maybe positionList,
    SubstringFuncExprCommonSubExpr <$> maybe substrList,
    TreatFuncExprCommonSubExpr <$> aExpr <*> typename,
    TrimFuncExprCommonSubExpr <$> maybe trimModifier <*> trimList,
    NullIfFuncExprCommonSubExpr <$> aExpr <*> aExpr,
    CoalesceFuncExprCommonSubExpr <$> exprList,
    GreatestFuncExprCommonSubExpr <$> exprList,
    LeastFuncExprCommonSubExpr <$> exprList
  ]

extractList = ExtractList <$> extractArg <*> aExpr

extractArg = choice [
    IdentExtractArg <$> ident,
    pure YearExtractArg,
    pure MonthExtractArg,
    pure DayExtractArg,
    pure HourExtractArg,
    pure MinuteExtractArg,
    pure SecondExtractArg,
    SconstExtractArg <$> sconst
  ]

overlayList = OverlayList <$> aExpr <*> overlayPlacing <*> substrFrom <*> maybe substrFor

overlayPlacing = aExpr

positionList = PositionList <$> bExpr <*> bExpr

substrList = choice [
    ExprSubstrList <$> aExpr <*> substrListFromFor,
    ExprListSubstrList <$> exprList
  ]

substrListFromFor = choice [
    FromForSubstrListFromFor <$> substrFrom <*> substrFor,
    ForFromSubstrListFromFor <$> substrFor <*> substrFrom,
    FromSubstrListFromFor <$> substrFrom,
    ForSubstrListFromFor <$> substrFor
  ]

substrFrom = aExpr

substrFor = aExpr

trimModifier = enumBounded

trimList = choice [
    ExprFromExprListTrimList <$> aExpr <*> exprList,
    FromExprListTrimList <$> exprList,
    ExprListTrimList <$> exprList
  ]


-- * Operators
-------------------------

qualOp = choice [OpQualOp <$> op, OperatorQualOp <$> anyOperator]

op = do
  a <- text (Range.exponential 1 7) (element "+-*/<>=~!@#%^&|`?")
  case Validator.op a of
    Nothing -> return a
    _ -> discard

anyOperator = recursive choice [
    AllOpAnyOperator <$> allOp
  ] [
    QualifiedAnyOperator <$> colId <*> anyOperator
  ]

allOp = choice [OpAllOp <$> op, MathAllOp <$> mathOp]

mathOp = enumBounded

symbolicExprBinOp = choice [
    MathSymbolicExprBinOp <$> mathOp,
    QualSymbolicExprBinOp <$> qualOp
  ]

binOp = element (toList HashSet.symbolicBinOp <> ["AND", "OR", "IS DISTINCT FROM", "IS NOT DISTINCT FROM"])

escapableBinOp = element ["LIKE", "ILIKE", "SIMILAR TO"]

verbalExprBinOp = enumBounded

aExprReversableOp = choice [
    pure NullAExprReversableOp,
    pure TrueAExprReversableOp,
    pure FalseAExprReversableOp,
    pure UnknownAExprReversableOp,
    DistinctFromAExprReversableOp <$> aExpr,
    OfAExprReversableOp <$> typeList,
    BetweenAExprReversableOp <$> bool <*> bExpr <*> aExpr,
    BetweenSymmetricAExprReversableOp <$> bExpr <*> aExpr,
    InAExprReversableOp <$> inExpr,
    pure DocumentAExprReversableOp
  ]

bExprIsOp = choice [
    DistinctFromBExprIsOp <$> bExpr,
    OfBExprIsOp <$> typeList,
    pure DocumentBExprIsOp
  ]

subqueryOp = choice [
    AllSubqueryOp <$> allOp,
    AnySubqueryOp <$> anyOperator,
    LikeSubqueryOp <$> bool,
    IlikeSubqueryOp <$> bool
  ]


-- * Constants
-------------------------

aexprConst = choice [
    IAexprConst <$> iconst,
    FAexprConst <$> fconst,
    SAexprConst <$> sconst,
    BAexprConst <$> text (Range.exponential 1 100) (element "01"),
    XAexprConst <$> text (Range.exponential 1 100) (element "0123456789abcdefABCDEF"),
    FuncAexprConst <$> funcName <*> maybe funcConstArgs <*> sconst,
    ConstTypenameAexprConst <$> constTypename <*> sconst,
    StringIntervalAexprConst <$> sconst <*> maybe interval,
    IntIntervalAexprConst <$> integral (Range.exponential 0 2309482309483029) <*> sconst,
    BoolAexprConst <$> bool,
    pure NullAexprConst
  ]

funcConstArgs = FuncConstArgs <$> nonEmpty (Range.exponential 1 7) funcArgExpr <*> maybe sortClause

constTypename = choice [
    NumericConstTypename <$> numeric,
    ConstBitConstTypename <$> constBit,
    ConstCharacterConstTypename <$> constCharacter,
    ConstDatetimeConstTypename <$> constDatetime
  ]

numeric = choice [
    pure IntNumeric,
    pure IntegerNumeric,
    pure SmallintNumeric,
    pure BigintNumeric,
    pure RealNumeric,
    FloatNumeric <$> maybe iconst,
    pure DoublePrecisionNumeric,
    DecimalNumeric <$> maybe (nonEmpty (Range.exponential 1 7) (small aExpr)),
    DecNumeric <$> maybe (nonEmpty (Range.exponential 1 7) (small aExpr)),
    NumericNumeric <$> maybe (nonEmpty (Range.exponential 1 7) (small aExpr)),
    pure BooleanNumeric
  ]

bit = Bit <$> bool <*> maybe (nonEmpty (Range.exponential 1 7) (small aExpr))

constBit = bit

constCharacter = ConstCharacter <$> character <*> maybe iconst

character = choice [
    CharacterCharacter <$> bool,
    CharCharacter <$> bool,
    pure VarcharCharacter,
    NationalCharacterCharacter <$> bool,
    NationalCharCharacter <$> bool,
    NcharCharacter <$> bool
  ]

constDatetime = choice [
    TimestampConstDatetime <$> maybe iconst <*> maybe bool,
    TimeConstDatetime <$> maybe iconst <*> maybe bool
  ]

interval = choice [
    pure YearInterval,
    pure MonthInterval,
    pure DayInterval,
    pure HourInterval,
    pure MinuteInterval,
    SecondInterval <$> intervalSecond,
    pure YearToMonthInterval,
    pure DayToHourInterval,
    pure DayToMinuteInterval,
    DayToSecondInterval <$> intervalSecond,
    pure HourToMinuteInterval,
    HourToSecondInterval <$> intervalSecond,
    MinuteToSecondInterval <$> intervalSecond
  ]

intervalSecond = maybe iconst

sconst = text (Range.exponential 0 1000) unicode

iconstOrFconst = choice [Left <$> iconst <|> Right <$> fconst]

fconst = realFrac_ (Range.exponentialFloat 0 309457394857984375983475943)

iconst = integral (Range.exponential 0 maxBound)


-- * Types
-------------------------

typecastTypename = TypecastTypename <$> typeName <*> nullable <*> arrayDimensionsAmount <*> nullable

nullable = pure False

arrayDimensionsAmount = int (Range.exponential 0 4)

-- ** Typename
-------------------------

typename = choice [
    ArrayBoundsTypename <$> bool <*> simpleTypename <*> maybe arrayBounds,
    ArrayDimTypename <$> bool <*> simpleTypename <*> maybe iconst
  ]

arrayBounds = nonEmpty (Range.exponential 1 4) (maybe iconst)

simpleTypename = choice [
    GenericTypeSimpleTypename <$> genericType,
    NumericSimpleTypename <$> numeric,
    BitSimpleTypename <$> bit,
    CharacterSimpleTypename <$> character,
    ConstDatetimeSimpleTypename <$> constDatetime,
    ConstIntervalSimpleTypename <$> choice [Left <$> maybe interval, Right <$> iconst]
  ]

genericType = GenericType <$> typeFunctionName <*> maybe attrs <*> maybe typeModifiers

attrs = nonEmpty (Range.exponential 1 10) attrName

typeModifiers = exprList

typeList = nonEmpty (Range.exponential 1 7) typename

subType = enumBounded


-- * Names
-------------------------

columnref = Columnref <$> colId <*> maybe indirection

keywordNotInSet = \ set -> notInSet set $ do
  a <- element startList
  b <- text (Range.linear 1 29) (element contList)
  return (Text.cons a b)
  where
    startList = "abcdefghijklmnopqrstuvwxyz_" <> List.filter isLower (enumFromTo '\200' '\377')
    contList = startList <> "0123456789$"

ident = choice [
    QuotedIdent <$> text (Range.linear 1 30) quotedChar,
    UnquotedIdent <$> keywordNotInSet HashSet.keyword
  ]

typeName = nameWithSet HashSet.typeFunctionName

name = nameWithSet HashSet.colId

nameWithSet set = choice [
    QuotedIdent <$> text (Range.linear 1 30) quotedChar,
    UnquotedIdent <$> (keywordNotInSet . HashSet.difference HashSet.keyword) set
  ]

qualifiedName = choice [
    SimpleQualifiedName <$> name,
    IndirectedQualifiedName <$> name <*> indirection
  ]

indirection = nonEmpty (Range.linear 1 3) indirectionEl

indirectionEl = choice [
    AttrNameIndirectionEl <$> name,
    pure AllIndirectionEl,
    ExprIndirectionEl <$> (small aExpr),
    SliceIndirectionEl <$> maybe (small aExpr) <*> maybe (small aExpr)
  ]

quotedChar = filter (not . isControl) unicode

colId = name

colLabel = name

attrName = colLabel

typeFunctionName = name

funcName = choice [
    TypeFuncName <$> typeFunctionName,
    IndirectedFuncName <$> colId <*> indirection
  ]

anyName = AnyName <$> colId <*> maybe attrs
