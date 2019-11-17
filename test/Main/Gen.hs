module Main.Gen where

import Hasql.TH.Prelude hiding (maybe, bool, sortBy)
import Hasql.TH.Syntax.Ast
import Hedgehog.Gen
import qualified Hedgehog.Range as Range
import qualified Data.Text as Text
import qualified Hasql.TH.Syntax.HashSet as HashSet



-- * Statements
-------------------------

preparableStmt = choice [
    SelectPreparableStmt <$> selectStmt
  ]


-- * Select
-------------------------

selectStmt = choice [
    InParensSelectStmt <$> selectStmt,
    NoParensSelectStmt <$> selectNoParens
  ]

selectNoParens = SelectNoParens <$> maybe withClause <*> selectClause <*> maybe sortClause <*> maybe selectLimit <*> maybe forLockingClause

selectClause = choice [
    Left <$> simpleSelect,
    Right <$> selectNoParens
  ]

simpleSelect = choice [
    NormalSimpleSelect <$> maybe targeting <*> maybe intoClause <*> maybe fromClause <*> maybe whereClause <*> maybe groupClause <*> maybe havingClause <*> maybe windowClause,
    ValuesSimpleSelect <$> valuesClause,
    BinSimpleSelect <$> selectBinOp <*> selectClause <*> allOrDistinct <*> selectClause
  ]


-- * Targeting
-------------------------

targeting = choice [
    NormalTargeting <$> targets,
    AllTargeting <$> maybe targets,
    DistinctTargeting <$> maybe (nonEmpty (Range.exponential 1 20) expr) <*> targets
  ]

targets = nonEmpty (Range.exponential 1 20) target

target = choice [
    pure AllTarget,
    ExprTarget <$> expr <*> maybe name
  ]


-- * BinSimpleSelect
-------------------------

selectBinOp = element [UnionSelectBinOp, IntersectSelectBinOp, ExceptSelectBinOp]


-- * With Clause
-------------------------

withClause = WithClause <$> bool <*> nonEmpty (Range.exponential 1 10) commonTableExpr

commonTableExpr = CommonTableExpr <$> name <*> maybe (nonEmpty (Range.exponential 1 20) name) <*> maybe bool <*> preparableStmt


-- * Into Clause
-------------------------

intoClause = optTempTableName

optTempTableName = OptTempTableName <$> bool <*> bool <*> qualifiedName


-- * From Clause
-------------------------

fromClause = nonEmpty (Range.exponential 1 20) tableRef

tableRef = choice [
    RelationExprTableRef <$> relationExpr <*> maybe aliasClause,
    SelectTableRef <$> bool <*> selectNoParens <*> maybe aliasClause,
    JoinTableRef <$> joinedTable <*> maybe aliasClause
  ]

relationExpr = choice [
    SimpleRelationExpr <$> qualifiedName <*> bool,
    OnlyRelationExpr <$> qualifiedName <*> bool
  ]

aliasClause = AliasClause <$> name <*> maybe (nonEmpty (Range.exponential 1 20) name)

joinedTable = choice [
    InParensJoinedTable <$> joinedTable,
    MethJoinedTable <$> joinMeth <*> tableRef <*> tableRef
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
    UsingJoinQual <$> nonEmpty (Range.exponential 1 20) name,
    OnJoinQual <$> expr
  ]


-- * Group Clause
-------------------------

groupClause = nonEmpty (Range.exponential 1 20) groupByItem

groupByItem = choice [
    ExprGroupByItem <$> expr,
    pure EmptyGroupingSetGroupByItem,
    RollupGroupByItem <$> nonEmpty (Range.exponential 1 20) expr,
    CubeGroupByItem <$> nonEmpty (Range.exponential 1 20) expr,
    GroupingSetsGroupByItem <$> nonEmpty (Range.exponential 1 5) groupByItem
  ]


-- * Having Clause
-------------------------

havingClause = expr


-- * Where Clause
-------------------------

whereClause = expr


-- * Window Clause
-------------------------

windowClause = nonEmpty (Range.exponential 1 20) windowDefinition

windowDefinition = WindowDefinition <$> name <*> windowSpecification

windowSpecification = WindowSpecification <$> maybe name <*> maybe (nonEmpty (Range.exponential 1 20) expr) <*> maybe sortClause <*> maybe frameClause

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
    PrecedingFrameBound <$> expr,
    FollowingFrameBound <$> expr
  ]

windowExclusionClause = element [CurrentRowWindowExclusionClause, GroupWindowExclusionClause, TiesWindowExclusionClause, NoOthersWindowExclusionClause]


-- * Values Clause
-------------------------

valuesClause = nonEmpty (Range.exponential 1 20) (nonEmpty (Range.exponential 1 20) expr)


-- * Sort Clause
-------------------------

sortClause = nonEmpty (Range.exponential 1 20) sortBy

sortBy = SortBy <$> expr <*> maybe order

order = element [AscOrder, DescOrder]


-- * All or distinct
-------------------------

allOrDistinct = element [AllAllOrDistinct, DistinctAllOrDistinct]


-- * Limit
-------------------------

selectLimit = choice [
    LimitOffsetSelectLimit <$> limitClause <*> offsetClause,
    OffsetLimitSelectLimit <$> offsetClause <*> limitClause,
    LimitSelectLimit <$> limitClause,
    OffsetSelectLimit <$> offsetClause
  ]

limitClause = choice [
    LimitLimitClause <$> selectLimitValue <*> maybe expr,
    FetchOnlyLimitClause <$> bool <*> maybe selectFetchFirstValue <*> bool
  ]

selectFetchFirstValue = choice [
    ExprSelectFetchFirstValue <$> expr,
    NumSelectFetchFirstValue <$> bool <*> intOrFloatLiteral
  ]

selectLimitValue = choice [
    ExprSelectLimitValue <$> expr,
    pure AllSelectLimitValue
  ]

offsetClause = choice [
    ExprOffsetClause <$> expr,
    FetchFirstOffsetClause <$> selectFetchFirstValue <*> bool
  ]


-- * For Locking
-------------------------

forLockingClause = choice [
    ItemsForLockingClause <$> nonEmpty (Range.exponential 1 20) forLockingItem,
    pure ReadOnlyForLockingClause
  ]

forLockingItem = ForLockingItem <$> forLockingStrength <*> maybe (nonEmpty (Range.exponential 1 20) qualifiedName) <*> maybe bool

forLockingStrength = element [
    UpdateForLockingStrength,
    NoKeyUpdateForLockingStrength,
    ShareForLockingStrength,
    KeyForLockingStrength
  ]


-- * Expressions
-------------------------

expr = choice [
    PlaceholderExpr <$> int (Range.linear 1 5),
    TypecastExpr <$> expr <*> type_,
    BinOpExpr <$> binOp <*> expr <*> expr,
    EscapableBinOpExpr <$> bool <*> escapableBinOp <*> expr <*> expr <*> maybe expr,
    pure DefaultExpr,
    QualifiedNameExpr <$> qualifiedName,
    LiteralExpr <$> literal,
    InParensExpr <$> expr <*> maybe indirection,
    CaseExpr <$> maybe expr <*> nonEmpty (Range.exponential 1 20) whenClause <*> maybe expr,
    FuncExpr <$> funcApplication,
    SelectExpr <$> selectNoParens,
    ExistsSelectExpr <$> selectNoParens,
    ArraySelectExpr <$> selectNoParens,
    GroupingExpr <$> nonEmpty (Range.exponential 1 20) expr
  ]

binOp = element (toList HashSet.symbolicBinOp <> ["and", "or", "is distinct from", "is not distinct from"])

escapableBinOp = do
  a <- bool
  b <- element ["like", "ilike", "similar to"]
  return (if a then "not " <> b else b)

whenClause = WhenClause <$> expr <*> expr

funcApplication = FuncApplication <$> qualifiedName <*> maybe funcApplicationParams

funcApplicationParams = choice [
    NormalFuncApplicationParams <$> maybe allOrDistinct <*> nonEmpty (Range.exponential 1 20) funcArgExpr <*> maybe sortClause,
    VariadicFuncApplicationParams <$> maybe (nonEmpty (Range.exponential 1 20) funcArgExpr) <*> funcArgExpr <*> maybe sortClause,
    pure StarFuncApplicationParams
  ]

funcArgExpr = choice [
    ExprFuncArgExpr <$> expr,
    ColonEqualsFuncArgExpr <$> name <*> expr,
    EqualsGreaterFuncArgExpr <$> name <*> expr
  ]


-- * Literals
-------------------------

literal = choice [
    IntLiteral <$> intLiteral,
    FloatLiteral <$> floatLiteral,
    StringLiteral <$> stringLiteral,
    BitLiteral <$> text (Range.exponential 1 100) (element "01"),
    HexLiteral <$> text (Range.exponential 1 100) (element "0123456789abcdefABCDEF"),
    FuncLiteral <$> qualifiedName <*> maybe funcLiteralArgList <*> stringLiteral,
    ConstTypenameLiteral <$> constTypename <*> stringLiteral,
    StringIntervalLiteral <$> stringLiteral <*> maybe interval,
    IntIntervalLiteral <$> integral (Range.exponential 0 2309482309483029) <*> stringLiteral,
    BoolLiteral <$> bool,
    pure NullLiteral
  ]

funcLiteralArgList = FuncLiteralArgList <$> nonEmpty (Range.exponential 1 10) funcArgExpr <*> maybe sortClause

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
    FloatNumeric <$> maybe intLiteral,
    pure DoublePrecisionNumeric,
    DecimalNumeric <$> maybe (nonEmpty (Range.exponential 1 10) expr),
    DecNumeric <$> maybe (nonEmpty (Range.exponential 1 10) expr),
    NumericNumeric <$> maybe (nonEmpty (Range.exponential 1 10) expr),
    pure BooleanNumeric
  ]

constBit = ConstBit <$> bool <*> maybe (nonEmpty (Range.exponential 1 10) expr)

constCharacter = ConstCharacter <$> character <*> maybe intLiteral

character = choice [
    CharacterCharacter <$> bool,
    CharCharacter <$> bool,
    pure VarcharCharacter,
    NationalCharacterCharacter <$> bool,
    NationalCharCharacter <$> bool,
    NcharCharacter <$> bool
  ]

constDatetime = choice [
    TimestampConstDatetime <$> maybe intLiteral <*> maybe bool,
    TimeConstDatetime <$> maybe intLiteral <*> maybe bool
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

intervalSecond = maybe intLiteral

stringLiteral = text (Range.exponential 0 1000) unicode

intOrFloatLiteral = choice [Left <$> intLiteral <|> Right <$> floatLiteral]

intLiteral = integral (Range.exponentialFrom 0 (-97234095729345740293579345) 309457394857984375983475943)

floatLiteral = realFrac_ (Range.linearFracFrom 0 (-97234095729345740293579345) 309457394857984375983475943)


-- * Types
-------------------------

type_ = Type <$> typeName <*> nullable <*> arrayDimensionsAmount <*> nullable

nullable = pure False

arrayDimensionsAmount = int (Range.exponential 0 4)


-- * Names
-------------------------

typeName = identifier

identifier = do
  a <- element "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
  b <- text (Range.linear 1 29) (element "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$_")
  return (Text.cons a b)

name = choice [
    QuotedName <$> text (Range.linear 1 30) unicode,
    UnquotedName <$> identifier
  ]

qualifiedName = choice [
    SimpleQualifiedName <$> name,
    IndirectedQualifiedName <$> name <*> indirection
  ]

indirection = nonEmpty (Range.linear 1 3) indirectionEl

indirectionEl = choice [
    AttrNameIndirectionEl <$> name,
    pure AllIndirectionEl,
    ExprIndirectionEl <$> expr,
    SliceIndirectionEl <$> maybe expr <*> maybe expr
  ]
