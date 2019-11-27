module Hasql.TH.Syntax.Rendering where

import Hasql.TH.Prelude hiding (aExpr, try, option, many, sortBy, bit)
import Hasql.TH.Syntax.Ast
import Data.ByteString.FastBuilder
import qualified Hasql.TH.Extras.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Builder as BsBuilder
import qualified Data.ByteString.Lazy as LazyBs


-- * Execution
-------------------------

toByteString :: Builder -> ByteString
toByteString = toStrictByteString

toText :: Builder -> Text
toText = Text.decodeUtf8 . toByteString


-- * Helpers
-------------------------

text :: Text -> Builder
text = stringUtf8 . Text.unpack

commaNonEmpty :: (a -> Builder) -> NonEmpty a -> Builder
commaNonEmpty = NonEmpty.intersperseFoldMap ", "

spaceNonEmpty :: (a -> Builder) -> NonEmpty a -> Builder
spaceNonEmpty = NonEmpty.intersperseFoldMap " "

lexemes :: [Builder] -> Builder
lexemes = mconcat . intersperse " "

optLexemes :: [Maybe Builder] -> Builder
optLexemes = lexemes . catMaybes

inParens :: Builder -> Builder
inParens a = "(" <> a <> ")"

inBrackets :: Builder -> Builder
inBrackets a = "[" <> a <> "]"


-- * Select
-------------------------

preparableStmt = \ case
  SelectPreparableStmt a -> selectStmt a

selectStmt = \ case
  Left a -> selectNoParens a
  Right a -> selectWithParens a

selectNoParens (SelectNoParens a b c d e) =
  optLexemes
    [
      fmap withClause a,
      Just (selectClause b),
      fmap sortClause c,
      fmap selectLimit d,
      fmap forLockingClause e
    ]

selectWithParens = inParens . \ case
  NoParensSelectWithParens a -> selectNoParens a
  WithParensSelectWithParens a -> selectWithParens a

withClause (WithClause a b) =
  "WITH " <> bool "" "RECURSIVE " a <> commaNonEmpty commonTableExpr b

commonTableExpr (CommonTableExpr a b c d) =
  optLexemes
    [
      Just (ident a),
      fmap (inParens . commaNonEmpty ident) b,
      Just "AS",
      fmap materialization c,
      Just (inParens (preparableStmt d))
    ]

materialization = bool "NOT MATERIALIZED" "MATERIALIZED"

selectLimit = \ case
  LimitOffsetSelectLimit a b -> lexemes [limitClause a, offsetClause b]
  OffsetLimitSelectLimit a b -> lexemes [offsetClause a, limitClause b]
  LimitSelectLimit a -> limitClause a
  OffsetSelectLimit a -> offsetClause a

limitClause = \ case
  LimitLimitClause a b -> "LIMIT " <> selectLimitValue a <> foldMap (mappend ", " . aExpr) b
  FetchOnlyLimitClause a b c ->
    optLexemes
      [
        Just "FETCH",
        Just (firstOrNext a),
        fmap selectFetchFirstValue b,
        Just (rowOrRows c),
        Just "ONLY"
      ]

firstOrNext = bool "FIRST" "NEXT"

rowOrRows = bool "ROW" "ROWS"

selectFetchFirstValue = \ case
  ExprSelectFetchFirstValue a -> cExpr a
  NumSelectFetchFirstValue a b -> bool "+" "-" a <> intOrFloat b

intOrFloat = either int64Dec doubleDec

selectLimitValue = \ case
  ExprSelectLimitValue a -> aExpr a
  AllSelectLimitValue -> "ALL"

offsetClause = \ case
  ExprOffsetClause a -> "OFFSET " <> aExpr a
  FetchFirstOffsetClause a b -> "OFFSET " <> selectFetchFirstValue a <> " " <> rowOrRows b

forLockingClause = \ case
  ItemsForLockingClause a -> spaceNonEmpty forLockingItem a
  ReadOnlyForLockingClause -> "FOR READ ONLY"

forLockingItem (ForLockingItem a b c) =
  optLexemes
    [
      Just (forLockingStrength a),
      fmap lockedRelsList b,
      fmap nowaitOrSkip c
    ]

forLockingStrength = \ case
  UpdateForLockingStrength -> "FOR UPDATE"
  NoKeyUpdateForLockingStrength -> "FOR NO KEY UPDATE"
  ShareForLockingStrength -> "FOR SHARE"
  KeyForLockingStrength -> "FOR KEY SHARE"

lockedRelsList a = "OF " <> commaNonEmpty qualifiedName a

nowaitOrSkip = bool "NOWAIT" "SKIP LOCKED"

selectClause = either simpleSelect selectWithParens

simpleSelect = \ case
  NormalSimpleSelect a b c d e f g ->
    optLexemes
      [
        Just "SELECT",
        fmap targeting a,
        fmap intoClause b,
        fmap fromClause c,
        fmap whereClause d,
        fmap groupClause e,
        fmap havingClause f,
        fmap windowClause g
      ]
  ValuesSimpleSelect a -> valuesClause a
  BinSimpleSelect a b c d -> selectClause b <> " " <> selectBinOp a <> foldMap (mappend " ". allOrDistinct) c <> " " <> selectClause d

selectBinOp = \ case
  UnionSelectBinOp -> "UNION"
  IntersectSelectBinOp -> "INTERSECT"
  ExceptSelectBinOp -> "EXCEPT"

targeting = \ case
  NormalTargeting a -> commaNonEmpty target a
  AllTargeting a -> "ALL" <> foldMap (mappend " " . commaNonEmpty target) a
  DistinctTargeting a b -> "DISTINCT" <> foldMap (mappend " " . onExpressionsClause) a <> " " <> commaNonEmpty target b

onExpressionsClause a = "ON (" <> commaNonEmpty aExpr a <> ")"

target = \ case
  AliasedExprTarget a b -> aExpr a <> " AS " <> ident b
  ImplicitlyAliasedExprTarget a b -> aExpr a <> " " <> ident b
  ExprTarget a -> aExpr a
  AsteriskTarget -> "*"


-- * Select Into
-------------------------

intoClause a = "INTO " <> optTempTableName a

optTempTableName = \ case
  TemporaryOptTempTableName a b -> optLexemes [Just "TEMPORARY", bool Nothing (Just "TABLE") a, Just (qualifiedName b)]
  TempOptTempTableName a b -> optLexemes [Just "TEMP", bool Nothing (Just "TABLE") a, Just (qualifiedName b)]
  LocalTemporaryOptTempTableName a b -> optLexemes [Just "LOCAL TEMPORARY", bool Nothing (Just "TABLE") a, Just (qualifiedName b)]
  LocalTempOptTempTableName a b -> optLexemes [Just "LOCAL TEMP", bool Nothing (Just "TABLE") a, Just (qualifiedName b)]
  GlobalTemporaryOptTempTableName a b -> optLexemes [Just "GLOBAL TEMPORARY", bool Nothing (Just "TABLE") a, Just (qualifiedName b)]
  GlobalTempOptTempTableName a b -> optLexemes [Just "GLOBAL TEMP", bool Nothing (Just "TABLE") a, Just (qualifiedName b)]
  UnloggedOptTempTableName a b -> optLexemes [Just "UNLOGGED", bool Nothing (Just "TABLE") a, Just (qualifiedName b)]
  TableOptTempTableName a -> "TABLE " <> qualifiedName a
  QualifedOptTempTableName a -> qualifiedName a


-- * From
-------------------------

fromClause a = "FROM " <> commaNonEmpty tableRef a

tableRef = \ case
  RelationExprTableRef a b ->
    optLexemes
      [
        Just (relationExpr a),
        fmap aliasClause b
      ]
  SelectTableRef a b c ->
    optLexemes
      [
        if a then Just "LATERAL" else Nothing,
        Just (selectWithParens b),
        fmap aliasClause c
      ]
  JoinTableRef a b -> case b of
    Just c -> inParens (joinedTable a) <> " " <> aliasClause c
    Nothing -> joinedTable a

relationExpr = \ case
  SimpleRelationExpr a b -> qualifiedName a <> bool "" " *" b
  OnlyRelationExpr a b -> "ONLY " <> bool qualifiedName (inParens . qualifiedName) b a

aliasClause (AliasClause a b) =
  optLexemes
    [
      Just "AS",
      Just (ident a),
      fmap (inParens . commaNonEmpty ident) b
    ]

joinedTable = \ case
  InParensJoinedTable a -> inParens (joinedTable a)
  MethJoinedTable a b c -> case a of
    CrossJoinMeth -> tableRef b <> " CROSS JOIN " <> tableRef c
    QualJoinMeth d e -> tableRef b <> foldMap (mappend " " . joinType) d <> " JOIN " <> tableRef c <> " " <> joinQual e
    NaturalJoinMeth d -> tableRef b <> " NATURAL" <> foldMap (mappend " " . joinType) d <> " JOIN " <> tableRef c

joinType = \ case
  FullJoinType a -> "FULL" <> if a then " OUTER" else ""
  LeftJoinType a -> "LEFT" <> if a then " OUTER" else ""
  RightJoinType a -> "RIGHT" <> if a then " OUTER" else ""
  InnerJoinType -> "INNER"

joinQual = \ case
  UsingJoinQual a -> "USING (" <> commaNonEmpty ident a <> ")" 
  OnJoinQual a -> "ON " <> aExpr a


-- * Where
-------------------------

whereClause a = "WHERE " <> aExpr a


-- * Group By
-------------------------

groupClause a = "GROUP BY " <> commaNonEmpty groupByItem a

groupByItem = \ case
  ExprGroupByItem a -> aExpr a
  EmptyGroupingSetGroupByItem -> "()"
  RollupGroupByItem a -> "ROLLUP (" <> commaNonEmpty aExpr a <> ")"
  CubeGroupByItem a -> "CUBE (" <> commaNonEmpty aExpr a <> ")"
  GroupingSetsGroupByItem a -> "GROUPING SETS (" <> commaNonEmpty groupByItem a <> ")"


-- * Having
-------------------------

havingClause a = "HAVING " <> aExpr a


-- * Window
-------------------------

windowClause a = "WINDOW " <> commaNonEmpty windowDefinition a

windowDefinition (WindowDefinition a b) = ident a <> " AS " <> windowSpecification b

windowSpecification (WindowSpecification a b c d) =
  inParens $ optLexemes
    [
      fmap ident a,
      fmap partitionClause b,
      fmap sortClause c,
      fmap frameClause d
    ]

partitionClause a = "PARTITION BY " <> commaNonEmpty aExpr a

frameClause (FrameClause a b c) =
  optLexemes
    [
      Just (frameClauseMode a),
      Just (frameExtent b),
      fmap windowExclusionCause c
    ]

frameClauseMode = \ case
  RangeFrameClauseMode -> "RANGE"
  RowsFrameClauseMode -> "ROWS"
  GroupsFrameClauseMode -> "GROUPS"

frameExtent = \ case
  SingularFrameExtent a -> frameBound a
  BetweenFrameExtent a b -> "BETWEEN " <> frameBound a <> " AND " <> frameBound b

frameBound = \ case
  UnboundedPrecedingFrameBound -> "UNBOUNDED PRECEDING"
  UnboundedFollowingFrameBound -> "UNBOUNDED FOLLOWING"
  CurrentRowFrameBound -> "CURRENT ROW"
  PrecedingFrameBound a -> aExpr a <> " PRECEDING"
  FollowingFrameBound a -> aExpr a <> " FOLLOWING"

windowExclusionCause = \ case
  CurrentRowWindowExclusionClause -> "EXCLUDE CURRENT ROW"
  GroupWindowExclusionClause -> "EXCLUDE GROUP"
  TiesWindowExclusionClause -> "EXCLUDE TIES"
  NoOthersWindowExclusionClause -> "EXCLUDE NO OTHERS"


-- * Order By
-------------------------

sortClause a = "ORDER BY " <> commaNonEmpty sortBy a

sortBy (SortBy a b) = optLexemes [Just (aExpr a), fmap order b]

order = \ case
  AscOrder -> "ASC"
  DescOrder -> "DESC"


-- * Values
-------------------------

valuesClause a = "VALUES " <> commaNonEmpty (inParens . commaNonEmpty aExpr) a


-- * Exprs
-------------------------

exprList = commaNonEmpty aExpr

aExpr = \ case
  CExprAExpr a -> cExpr a
  TypecastAExpr a b -> aExpr a <> " :: " <> typecastTypename b
  CollateAExpr a b -> aExpr a <> " COLLATE " <> anyName b
  AtTimeZoneAExpr a b -> aExpr a <> " AT TIME ZONE " <> aExpr b
  PlusAExpr a -> "+" <> aExpr a
  MinusAExpr a -> "-" <> aExpr a
  SymbolicBinOpAExpr a b c -> aExpr a <> " " <> symbolicExprBinOp b <> " " <> aExpr c
  PrefixQualOpAExpr a b -> qualOp a <> aExpr b
  SuffixQualOpAExpr a b -> aExpr a <> qualOp b
  AndAExpr a b -> aExpr a <> " AND " <> aExpr b
  OrAExpr a b -> aExpr a <> " OR " <> aExpr b
  NotAExpr a -> "NOT " <> aExpr a
  VerbalExprBinOpAExpr a b c d e -> aExpr a <> " " <> verbalExprBinOp b c <> " " <> aExpr d <> foldMap (mappend " ESCAPE " . aExpr) e
  ReversableOpAExpr a b c -> aExpr a <> aExprReversableOp b c
  IsnullAExpr a -> "ISNULL " <> aExpr a
  NotnullAExpr a -> "NOTNULL " <> aExpr a
  OverlapsAExpr a b -> row a <> " OVERLAPS " <> row b
  SubqueryAExpr a b c d -> aExpr a <> " " <> subqueryOp b <> " " <> subType c <> " " <> either selectWithParens (inParens . aExpr) d
  UniqueAExpr a -> "UNIQUE " <> selectWithParens a
  DefaultAExpr -> "DEFAULT"

bExpr = \ case
  CExprBExpr a -> cExpr a
  TypecastBExpr a b -> bExpr a <> " :: " <> typecastTypename b
  PlusBExpr a -> "+" <> bExpr a
  MinusBExpr a -> "-" <> bExpr a
  SymbolicBinOpBExpr a b c -> bExpr a <> " " <> symbolicExprBinOp b <> " " <> bExpr c
  QualOpBExpr a b -> qualOp a <> bExpr b
  IsOpBExpr a b c -> bExpr a <> " " <> bExprIsOp b c

cExpr = \ case
  ColumnrefCExpr a -> columnref a
  AexprConstCExpr a -> aexprConst a
  ParamCExpr a b -> "$" <> intDec a <> foldMap indirection b
  InParensCExpr a b -> inParens (aExpr a) <> foldMap indirection b
  CaseCExpr a -> caseExpr a
  FuncCExpr a -> funcExpr a
  SelectWithParensCExpr a b -> selectWithParens a <> foldMap indirection b
  ExistsCExpr a -> "EXISTS " <> selectWithParens a
  ArrayCExpr a -> "ARRAY " <> either selectWithParens arrayExpr a
  ExplicitRowCExpr a -> explicitRow a
  ImplicitRowCExpr a -> implicitRow a
  GroupingCExpr a -> "GROUPING " <> inParens (exprList a)


-- * Ops
-------------------------

aExprReversableOp a = \ case
  NullAExprReversableOp -> bool "IS " "IS NOT " a <> "NULL"
  TrueAExprReversableOp -> bool "IS " "IS NOT " a <> "TRUE"
  FalseAExprReversableOp -> bool "IS " "IS NOT " a <> "FALSE"
  UnknownAExprReversableOp -> bool "IS " "IS NOT " a <> "UNKNOWN"
  DistinctFromAExprReversableOp b -> bool "IS " "IS NOT " a <> "DISTINCT FROM " <> aExpr b
  OfAExprReversableOp b -> bool "IS " "IS NOT " a <> "OF " <> inParens (typeList b)
  BetweenAExprReversableOp b c d -> bool "" "NOT " a <> bool "BETWEEN " "BETWEEN ASYMMETRIC " b <> bExpr c <> " AND " <> aExpr d
  BetweenSymmetricAExprReversableOp b c -> bool "" "NOT " a <> "BETWEEN SYMMETRIC " <> bExpr b <> " AND " <> aExpr c
  InAExprReversableOp b -> bool "" "NOT " a <> "IN " <> inExpr b
  DocumentAExprReversableOp -> bool "IS " "IS NOT " a <> "DOCUMENT"

verbalExprBinOp a = mappend (bool "" "NOT " a) . \ case
  LikeVerbalExprBinOp -> "LIKE"
  IlikeVerbalExprBinOp -> "ILIKE"
  SimilarToVerbalExprBinOp -> "SIMILAR TO"

subqueryOp = \ case
  AllSubqueryOp a -> allOp a
  AnySubqueryOp a -> "OPERATOR " <> inParens (anyOperator a)
  LikeSubqueryOp a -> bool "" "NOT " a <> "LIKE"
  IlikeSubqueryOp a -> bool "" "NOT " a <> "ILIKE"

bExprIsOp a = mappend (bool "IS" "IS NOT" a) . \ case
  DistinctFromBExprIsOp b -> "DISTINCT FROM " <> bExpr b
  OfBExprIsOp a -> "OF " <> inParens (typeList a)
  DocumentBExprIsOp -> "DOCUMENT"

symbolicExprBinOp = \ case
  MathSymbolicExprBinOp a -> mathOp a
  QualSymbolicExprBinOp a -> qualOp a

qualOp = \ case
  OpQualOp a -> op a
  OperatorQualOp a -> "OPERATOR (" <> anyOperator a <> ")"

op = text

anyOperator = \ case
  AllOpAnyOperator a -> allOp a
  QualifiedAnyOperator a b -> colId a <> "." <> anyOperator b

allOp = \ case
  OpAllOp a -> op a
  MathAllOp a -> mathOp a

mathOp = \ case
  PlusMathOp -> char7 '+'
  MinusMathOp -> char7 '-'
  AsteriskMathOp -> char7 '*'
  SlashMathOp -> char7 '/'
  PercentMathOp -> char7 '%'
  ArrowUpMathOp -> char7 '^'
  ArrowLeftMathOp -> char7 '<'
  ArrowRightMathOp -> char7 '>'
  EqualsMathOp -> char7 '='
  LessEqualsMathOp -> "<="
  GreaterEqualsMathOp -> ">="
  ArrowLeftArrowRightMathOp -> "<>"
  ExclamationEqualsMathOp -> "!="


-- *
-------------------------

inExpr = \ case
  SelectInExpr a -> selectWithParens a
  ExprListInExpr a -> inParens (exprList a)

caseExpr (CaseExpr a b c) = optLexemes [
    Just "CASE",
    fmap aExpr a,
    Just (spaceNonEmpty whenClause b),
    fmap caseDefault c,
    Just "END"
  ]

whenClause (WhenClause a b) = "WHEN " <> aExpr a <> " THEN " <> aExpr b

caseDefault a = "ELSE " <> aExpr a

arrayExpr = inBrackets . \ case
  ExprListArrayExpr a -> exprList a
  ArrayExprListArrayExpr a -> arrayExprList a
  EmptyArrayExpr -> mempty

arrayExprList = commaNonEmpty arrayExpr

row = \ case
  ExplicitRowRow a -> explicitRow a
  ImplicitRowRow a -> implicitRow a

explicitRow a = "ROW " <> inParens (foldMap exprList a)

implicitRow (ImplicitRow a b) = inParens (exprList a <> ", " <> aExpr b)

funcApplication (FuncApplication a b) =
  funcName a <> "(" <> foldMap funcApplicationParams b <> ")"

funcApplicationParams = \ case
  NormalFuncApplicationParams a b c ->
    optLexemes
      [
        fmap allOrDistinct a,
        Just (commaNonEmpty funcArgExpr b),
        fmap sortClause c
      ]
  VariadicFuncApplicationParams a b c ->
    optLexemes
      [
        fmap (flip mappend "," . commaNonEmpty funcArgExpr) a,
        Just "VARIADIC",
        Just (funcArgExpr b),
        fmap sortClause c
      ]
  StarFuncApplicationParams -> "*"

allOrDistinct = \ case
  False -> "ALL"
  True -> "DISTINCT"

funcArgExpr = \ case
  ExprFuncArgExpr a -> aExpr a
  ColonEqualsFuncArgExpr a b -> ident a <> " := " <> aExpr b
  EqualsGreaterFuncArgExpr a b -> ident a <> " => " <> aExpr b

-- ** Func Expr
-------------------------

funcExpr = \ case
  ApplicationFuncExpr a b c d -> optLexemes [
      Just (funcApplication a),
      fmap withinGroupClause b,
      fmap filterClause c,
      fmap overClause d
    ]
  SubexprFuncExpr a -> funcExprCommonSubExpr a

withinGroupClause a = "WITHIN GROUP (" <> sortClause a <> ")"

filterClause a = "FILTER (WHERE " <> aExpr a <> ")"

overClause = \ case
  WindowOverClause a -> "OVER " <> windowSpecification a
  ColIdOverClause a -> "OVER " <> colId a

funcExprCommonSubExpr = \ case
  CollationForFuncExprCommonSubExpr a -> "COLLATION FOR (" <> aExpr a <> ")"
  CurrentDateFuncExprCommonSubExpr -> "CURRENT_DATE"
  CurrentTimeFuncExprCommonSubExpr a -> "CURRENT_TIME" <> foldMap (mappend " " . inParens . iconst) a
  CurrentTimestampFuncExprCommonSubExpr a -> "CURRENT_TIMESTAMP" <> foldMap (mappend " " . inParens . iconst) a
  LocalTimeFuncExprCommonSubExpr a -> "LOCALTIME" <> foldMap (mappend " " . inParens . iconst) a
  LocalTimestampFuncExprCommonSubExpr a -> "LOCALTIMESTAMP" <> foldMap (mappend " " . inParens . iconst) a
  CurrentRoleFuncExprCommonSubExpr -> "CURRENT_ROLE"
  CurrentUserFuncExprCommonSubExpr -> "CURRENT_USER"
  SessionUserFuncExprCommonSubExpr -> "SESSION_USER"
  UserFuncExprCommonSubExpr -> "USER"
  CurrentCatalogFuncExprCommonSubExpr -> "CURRENT_CATALOG"
  CurrentSchemaFuncExprCommonSubExpr -> "CURRENT_SCHEMA"
  CastFuncExprCommonSubExpr a b -> "CAST (" <> aExpr a <> " AS " <> typename b <> ")"
  ExtractFuncExprCommonSubExpr a -> "EXTRACT (" <> foldMap extractList a <> ")"
  OverlayFuncExprCommonSubExpr a -> "OVERLAY (" <> overlayList a <> ")"
  PositionFuncExprCommonSubExpr a -> "POSITION (" <> foldMap positionList a <> ")"
  SubstringFuncExprCommonSubExpr a -> "SUBSTRING (" <> foldMap substrList a <> ")"
  TreatFuncExprCommonSubExpr a b -> "TREAT (" <> aExpr a <> " AS " <> typename b <> ")"
  TrimFuncExprCommonSubExpr a b -> "TRIM (" <> foldMap (flip mappend " " . trimModifier) a <> trimList b <> ")"
  NullIfFuncExprCommonSubExpr a b -> "NULLIF (" <> aExpr a <> ", " <> aExpr b <> ")"
  CoalesceFuncExprCommonSubExpr a -> "COALESCE (" <> exprList a <> ")"
  GreatestFuncExprCommonSubExpr a -> "GREATEST (" <> exprList a <> ")"
  LeastFuncExprCommonSubExpr a -> "LEAST (" <> exprList a <> ")"

extractList (ExtractList a b) = extractArg a <> " FROM " <> aExpr b

extractArg = \ case
  IdentExtractArg a -> ident a
  YearExtractArg -> "YEAR"
  MonthExtractArg -> "MONTH"
  DayExtractArg -> "DAY"
  HourExtractArg -> "HOUR"
  MinuteExtractArg -> "MINUTE"
  SecondExtractArg -> "SECOND"
  SconstExtractArg a -> sconst a

overlayList (OverlayList a b c d) = aExpr a <> " " <> overlayPlacing b <> " " <> substrFrom c <> foldMap (mappend " " . substrFor) d

overlayPlacing a = "PLACING " <> aExpr a

positionList (PositionList a b) = bExpr a <> " IN " <> bExpr b

substrList = \ case
  ExprSubstrList a b -> aExpr a <> " " <> substrListFromFor b
  ExprListSubstrList a -> exprList a

substrListFromFor = \ case
  FromForSubstrListFromFor a b -> substrFrom a <> " " <> substrFor b
  ForFromSubstrListFromFor a b -> substrFor a <> " " <> substrFrom b
  FromSubstrListFromFor a -> substrFrom a
  ForSubstrListFromFor a -> substrFor a

substrFrom a = "FROM " <> aExpr a

substrFor a = "FOR " <> aExpr a

trimModifier = \ case
  BothTrimModifier -> "BOTH"
  LeadingTrimModifier -> "LEADING"
  TrailingTrimModifier -> "TRAILING"

trimList = \ case
  ExprFromExprListTrimList a b -> aExpr a <> " FROM " <> exprList b
  FromExprListTrimList a -> "FROM " <> exprList a
  ExprListTrimList a -> exprList a


-- * AexprConsts
-------------------------

aexprConst = \ case
  IAexprConst a -> iconst a
  FAexprConst a -> fconst a
  SAexprConst a -> sconst a
  BAexprConst a -> "B'" <> text a <> "'"
  XAexprConst a -> "X'" <> text a <> "'"
  FuncAexprConst a b c -> funcName a <> foldMap (inParens . funcAexprConstArgList) b <> " " <> sconst c
  ConstTypenameAexprConst a b -> constTypename a <> " " <> sconst b
  StringIntervalAexprConst a b -> "INTERVAL " <> sconst a <> foldMap (mappend " " . interval) b
  IntIntervalAexprConst a b -> "INTERVAL " <> inParens (int64Dec a) <> " " <> sconst b
  BoolAexprConst a -> if a then "TRUE" else "FALSE"
  NullAexprConst -> "NULL"

iconst = int64Dec

fconst = doubleDec

sconst a = "'" <> text (Text.replace "'" "''" a) <> "'"

funcAexprConstArgList (FuncConstArgs a b) = commaNonEmpty funcArgExpr a <> foldMap (mappend " " . sortClause) b

constTypename = \ case
  NumericConstTypename a -> numeric a
  ConstBitConstTypename a -> constBit a
  ConstCharacterConstTypename a -> constCharacter a
  ConstDatetimeConstTypename a -> constDatetime a

numeric = \ case
  IntNumeric -> "INT"
  IntegerNumeric -> "INTEGER"
  SmallintNumeric -> "SMALLINT"
  BigintNumeric -> "BIGINT"
  RealNumeric -> "REAL"
  FloatNumeric a -> "FLOAT" <> foldMap (mappend " " . inParens . int64Dec) a
  DoublePrecisionNumeric -> "DOUBLE PRECISION"
  DecimalNumeric a -> "DECIMAL" <> foldMap (mappend " " . inParens . commaNonEmpty aExpr) a
  DecNumeric a -> "DEC" <> foldMap (mappend " " . inParens . commaNonEmpty aExpr) a
  NumericNumeric a -> "NUMERIC" <> foldMap (mappend " " . inParens . commaNonEmpty aExpr) a
  BooleanNumeric -> "BOOLEAN"

bit (Bit a b) = optLexemes [
    Just "BIT",
    bool Nothing (Just "VARYING") a,
    fmap (inParens . commaNonEmpty aExpr) b
  ]

constBit = bit

constCharacter (ConstCharacter a b) = character a <> foldMap (mappend " " . inParens . int64Dec) b

character = \ case
  CharacterCharacter a -> "CHARACTER" <> bool "" " VARYING" a
  CharCharacter a -> "CHAR" <> bool "" " VARYING" a
  VarcharCharacter -> "VARCHAR"
  NationalCharacterCharacter a -> "NATIONAL CHARACTER" <> bool "" " VARYING" a
  NationalCharCharacter a -> "NATIONAL CHAR" <> bool "" " VARYING" a
  NcharCharacter a -> "NCHAR" <> bool "" " VARYING" a

constDatetime = \ case
  TimestampConstDatetime a b -> optLexemes [
      Just "TIMESTAMP",
      fmap (inParens . int64Dec) a,
      fmap timezone b
    ]
  TimeConstDatetime a b -> optLexemes [
      Just "TIME",
      fmap (inParens . int64Dec) a,
      fmap timezone b
    ]

timezone = \ case
  False -> "WITH TIME ZONE"
  True -> "WITHOUT TIME ZONE"

interval = \ case
  YearInterval -> "YEAR"
  MonthInterval -> "MONTH"
  DayInterval -> "DAY"
  HourInterval -> "HOUR"
  MinuteInterval -> "MINUTE"
  SecondInterval a -> intervalSecond a
  YearToMonthInterval -> "YEAR TO MONTH"
  DayToHourInterval -> "DAY TO HOUR"
  DayToMinuteInterval -> "DAY TO MINUTE"
  DayToSecondInterval a -> "DAY TO " <> intervalSecond a
  HourToMinuteInterval -> "HOUR TO MINUTE"
  HourToSecondInterval a -> "HOUR TO " <> intervalSecond a
  MinuteToSecondInterval a -> "MINUTE TO " <> intervalSecond a

intervalSecond = \ case
  Nothing -> "SECOND" 
  Just a -> "SECOND " <> inParens (int64Dec a)


-- * Names and refs
-------------------------

columnref (Columnref a b) = colId a <> foldMap indirection b

ident = \ case
  QuotedIdent a -> char7 '"' <> text (Text.replace "\"" "\"\"" a) <> char7 '"'
  UnquotedIdent a -> text a

qualifiedName = \ case
  SimpleQualifiedName a -> ident a
  IndirectedQualifiedName a b -> ident a <> indirection b

indirection = foldMap indirectionEl

indirectionEl = \ case
  AttrNameIndirectionEl a -> "." <> ident a
  AllIndirectionEl -> ".*"
  ExprIndirectionEl a -> "[" <> aExpr a <> "]"
  SliceIndirectionEl a b -> "[" <> foldMap aExpr a <> ":" <> foldMap aExpr b <> "]"

colId = ident

colLabel = ident

attrName = colLabel

typeFunctionName = ident

funcName = \ case
  TypeFuncName a -> typeFunctionName a
  IndirectedFuncName a b -> colId a <> indirection b

anyName (AnyName a b) = colId a <> foldMap attrs b


-- * Types
-------------------------

typecastTypename (TypecastTypename a _ b _) =
  ident a <>
  fold (replicate b "[]")

typename = \ case
  ArrayBoundsTypename a b c ->
    bool "" "SETOF " a <> simpleTypename b <> foldMap (mappend " " . arrayBounds) c
  ArrayDimTypename a b c ->
    bool "" "SETOF " a <> simpleTypename b <> " ARRAY" <> foldMap (inBrackets . iconst) c

arrayBounds = spaceNonEmpty (inBrackets . foldMap iconst)

simpleTypename = \ case
  GenericTypeSimpleTypename a -> genericType a
  NumericSimpleTypename a -> numeric a
  BitSimpleTypename a -> bit a
  CharacterSimpleTypename a -> character a
  ConstDatetimeSimpleTypename a -> constDatetime a
  ConstIntervalSimpleTypename a -> "INTERVAL" <> either (foldMap (mappend " " . interval)) (mappend " " . inParens . iconst) a

genericType (GenericType a b c) = typeFunctionName a <> foldMap attrs b <> foldMap (mappend " " . typeModifiers) c

attrs = foldMap (mappend "." . attrName)

typeModifiers = inParens . exprList

typeList = commaNonEmpty typename

subType = \ case
  AnySubType -> "ANY"
  SomeSubType -> "SOME"
  AllSubType -> "ALL"
