{-|
Names for nodes mostly resemble the according definitions in the @gram.y@
original Postgres parser file, except for the cases where we can optimize on that.

For reasoning see the docs of the parsing module of this project.
-}
module Hasql.TH.Syntax.Ast where

import Hasql.TH.Prelude hiding (Order)


-- * Statement
-------------------------

{-
PreparableStmt:
  |  SelectStmt
  |  InsertStmt
  |  UpdateStmt
  |  DeleteStmt
-}
data PreparableStmt = 
  SelectPreparableStmt SelectStmt
  deriving (Show, Generic, Eq, Ord)


-- * Select
-------------------------

{-|
Covers the following cases:

@
SelectStmt:
  |  select_no_parens
  |  select_with_parens

select_with_parens:
  |  '(' select_no_parens ')'
  |  '(' select_with_parens ')'
@
-}
data SelectStmt =
  InParensSelectStmt SelectStmt |
  NoParensSelectStmt SelectNoParens
  deriving (Show, Generic, Eq, Ord)

{-|
Covers the following cases:

@
select_no_parens:
  |  simple_select
  |  select_clause sort_clause
  |  select_clause opt_sort_clause for_locking_clause opt_select_limit
  |  select_clause opt_sort_clause select_limit opt_for_locking_clause
  |  with_clause select_clause
  |  with_clause select_clause sort_clause
  |  with_clause select_clause opt_sort_clause for_locking_clause opt_select_limit
  |  with_clause select_clause opt_sort_clause select_limit opt_for_locking_clause
@
-}
data SelectNoParens =
  SelectNoParens (Maybe WithClause) SelectClause (Maybe SortClause) (Maybe SelectLimit) (Maybe ForLockingClause)
  deriving (Show, Generic, Eq, Ord)

{-|
@
select_clause:
  |  simple_select
  |  select_with_parens
@
-}
type SelectClause = Either SimpleSelect SelectNoParens

{-
simple_select:
  |  SELECT opt_all_clause opt_target_list
      into_clause from_clause where_clause
      group_clause having_clause window_clause
  |  SELECT distinct_clause target_list
      into_clause from_clause where_clause
      group_clause having_clause window_clause
  |  values_clause
  |  TABLE relation_expr
  |  select_clause UNION all_or_distinct select_clause
  |  select_clause INTERSECT all_or_distinct select_clause
  |  select_clause EXCEPT all_or_distinct select_clause

TODO: Cover TABLE clause.
-}
data SimpleSelect =
  NormalSimpleSelect (Maybe Targeting) (Maybe IntoClause) (Maybe FromClause) (Maybe WhereClause) (Maybe GroupClause) (Maybe HavingClause) (Maybe WindowClause) |
  ValuesSimpleSelect ValuesClause |
  BinSimpleSelect SelectBinOp SelectClause AllOrDistinct SelectClause
  deriving (Show, Generic, Eq, Ord)

{-|
Covers these parts of spec:

@
simple_select:
  |  SELECT opt_all_clause opt_target_list
      into_clause from_clause where_clause
      group_clause having_clause window_clause
  |  SELECT distinct_clause target_list
      into_clause from_clause where_clause
      group_clause having_clause window_clause

distinct_clause:
  |  DISTINCT
  |  DISTINCT ON '(' expr_list ')'
@
-}
data Targeting =
  NormalTargeting (NonEmpty Target) |
  AllTargeting (Maybe (NonEmpty Target)) |
  DistinctTargeting (Maybe (NonEmpty Expr)) (NonEmpty Target)
  deriving (Show, Generic, Eq, Ord)

{-|
@
target_el:
  |  a_expr AS ColLabel
  |  a_expr IDENT
  |  a_expr
  |  '*'
@
-}
data Target =
  AllTarget |
  ExprTarget Expr (Maybe Name)
  deriving (Show, Generic, Eq, Ord)

{-
  |  select_clause UNION all_or_distinct select_clause
  |  select_clause INTERSECT all_or_distinct select_clause
  |  select_clause EXCEPT all_or_distinct select_clause
-}
data SelectBinOp = UnionSelectBinOp | IntersectSelectBinOp | ExceptSelectBinOp
  deriving (Show, Generic, Eq, Ord)

{-
with_clause:
  |  WITH cte_list
  |  WITH_LA cte_list
  |  WITH RECURSIVE cte_list
-}
data WithClause = WithClause Bool (NonEmpty CommonTableExpr)
  deriving (Show, Generic, Eq, Ord)

{-
common_table_expr:
  |  name opt_name_list AS opt_materialized '(' PreparableStmt ')'
opt_materialized:
  | MATERIALIZED
  | NOT MATERIALIZED
  | EMPTY
-}
data CommonTableExpr = CommonTableExpr Name (Maybe (NonEmpty Name)) (Maybe Bool) PreparableStmt
  deriving (Show, Generic, Eq, Ord)

type IntoClause = OptTempTableName

{-
OptTempTableName:
  |  TEMPORARY opt_table qualified_name
  |  TEMP opt_table qualified_name
  |  LOCAL TEMPORARY opt_table qualified_name
  |  LOCAL TEMP opt_table qualified_name
  |  GLOBAL TEMPORARY opt_table qualified_name
  |  GLOBAL TEMP opt_table qualified_name
  |  UNLOGGED opt_table qualified_name
  |  TABLE qualified_name
  |  qualified_name
-}
data OptTempTableName =
  TemporaryOptTempTableName Bool QualifiedName |
  TempOptTempTableName Bool QualifiedName |
  LocalTemporaryOptTempTableName Bool QualifiedName |
  LocalTempOptTempTableName Bool QualifiedName |
  GlobalTemporaryOptTempTableName Bool QualifiedName |
  GlobalTempOptTempTableName Bool QualifiedName |
  UnloggedOptTempTableName Bool QualifiedName |
  TableOptTempTableName QualifiedName |
  QualifedOptTempTableName QualifiedName
  deriving (Show, Generic, Eq, Ord)

type FromClause = NonEmpty TableRef

type GroupClause = NonEmpty GroupByItem

{-
group_by_item:
  |  a_expr
  |  empty_grouping_set
  |  cube_clause
  |  rollup_clause
  |  grouping_sets_clause

empty_grouping_set:
  |  '(' ')'

rollup_clause:
  |  ROLLUP '(' expr_list ')'

cube_clause:
  |  CUBE '(' expr_list ')'

grouping_sets_clause:
  |  GROUPING SETS '(' group_by_list ')'
-}
data GroupByItem =
  ExprGroupByItem Expr |
  EmptyGroupingSetGroupByItem |
  RollupGroupByItem (NonEmpty Expr) |
  CubeGroupByItem (NonEmpty Expr) |
  GroupingSetsGroupByItem (NonEmpty GroupByItem)
  deriving (Show, Generic, Eq, Ord)

{-|
@
having_clause:
  |  HAVING a_expr
  |  EMPTY
@
-}
type HavingClause = Expr

{-|
@
window_clause:
  |  WINDOW window_definition_list
  |  EMPTY

window_definition_list:
  |  window_definition
  |  window_definition_list ',' window_definition
@
-}
type WindowClause = NonEmpty WindowDefinition

{-|
@
window_definition:
  |  ColId AS window_specification
@
-}
data WindowDefinition = WindowDefinition Name WindowSpecification
  deriving (Show, Generic, Eq, Ord)

{-|
@
window_specification:
  |  '(' opt_existing_window_name opt_partition_clause
            opt_sort_clause opt_frame_clause ')'

opt_existing_window_name:
  |  ColId
  |  EMPTY

opt_partition_clause:
  |  PARTITION BY expr_list
  |  EMPTY
@
-}
data WindowSpecification = WindowSpecification (Maybe Name) (Maybe (NonEmpty Expr)) (Maybe SortClause) (Maybe FrameClause)
  deriving (Show, Generic, Eq, Ord)

{-|
@
opt_frame_clause:
  |  RANGE frame_extent opt_window_exclusion_clause
  |  ROWS frame_extent opt_window_exclusion_clause
  |  GROUPS frame_extent opt_window_exclusion_clause
  |  EMPTY
@
-}
data FrameClause = FrameClause FrameClauseMode FrameExtent (Maybe WindowExclusionClause)
  deriving (Show, Generic, Eq, Ord)

{-|
@
opt_frame_clause:
  |  RANGE frame_extent opt_window_exclusion_clause
  |  ROWS frame_extent opt_window_exclusion_clause
  |  GROUPS frame_extent opt_window_exclusion_clause
  |  EMPTY
@
-}
data FrameClauseMode = RangeFrameClauseMode | RowsFrameClauseMode | GroupsFrameClauseMode
  deriving (Show, Generic, Eq, Ord)

{-|
@
frame_extent:
  |  frame_bound
  |  BETWEEN frame_bound AND frame_bound
@
-}
data FrameExtent = SingularFrameExtent FrameBound | BetweenFrameExtent FrameBound FrameBound
  deriving (Show, Generic, Eq, Ord)

{-|
@
frame_bound:
  |  UNBOUNDED PRECEDING
  |  UNBOUNDED FOLLOWING
  |  CURRENT_P ROW
  |  a_expr PRECEDING
  |  a_expr FOLLOWING
@
-}
data FrameBound =
  UnboundedPrecedingFrameBound |
  UnboundedFollowingFrameBound |
  CurrentRowFrameBound |
  PrecedingFrameBound Expr |
  FollowingFrameBound Expr
  deriving (Show, Generic, Eq, Ord)

{-|
@
opt_window_exclusion_clause:
  |  EXCLUDE CURRENT_P ROW
  |  EXCLUDE GROUP_P
  |  EXCLUDE TIES
  |  EXCLUDE NO OTHERS
  |  EMPTY
@
-}
data WindowExclusionClause =
  CurrentRowWindowExclusionClause |
  GroupWindowExclusionClause |
  TiesWindowExclusionClause |
  NoOthersWindowExclusionClause
  deriving (Show, Generic, Eq, Ord)

{-|
@
values_clause:
  |  VALUES '(' expr_list ')'
  |  values_clause ',' '(' expr_list ')'
@
-}
type ValuesClause = NonEmpty (NonEmpty Expr)

{-|

sort_clause:
  |  ORDER BY sortby_list

sortby_list:
  |  sortby
  |  sortby_list ',' sortby

-}
type SortClause = NonEmpty SortBy

data AllOrDistinct = AllAllOrDistinct | DistinctAllOrDistinct
  deriving (Show, Generic, Eq, Ord)

{-|
@
sortby:
  |  a_expr USING qual_all_Op opt_nulls_order
  |  a_expr opt_asc_desc opt_nulls_order
@

TODO: Add qual_all_Op support
TODO: Add opt_nulls_order support
-}
data SortBy = SortBy Expr (Maybe Order)
  deriving (Show, Generic, Eq, Ord)

data Order = AscOrder | DescOrder
  deriving (Show, Generic, Eq, Ord)

{-
select_limit:
  | limit_clause offset_clause
  | offset_clause limit_clause
  | limit_clause
  | offset_clause
-}
data SelectLimit =
  LimitOffsetSelectLimit LimitClause OffsetClause |
  OffsetLimitSelectLimit OffsetClause LimitClause |
  LimitSelectLimit LimitClause |
  OffsetSelectLimit OffsetClause
  deriving (Show, Generic, Eq, Ord)

{-
limit_clause:
  | LIMIT select_limit_value
  | LIMIT select_limit_value ',' select_offset_value
  | FETCH first_or_next select_fetch_first_value row_or_rows ONLY
  | FETCH first_or_next row_or_rows ONLY
select_offset_value:
  | a_expr
first_or_next:
  | FIRST_P
  | NEXT
row_or_rows:
  | ROW
  | ROWS
-}
data LimitClause =
  LimitLimitClause SelectLimitValue (Maybe Expr) |
  FetchOnlyLimitClause Bool (Maybe SelectFetchFirstValue) Bool
  deriving (Show, Generic, Eq, Ord)

{-
select_fetch_first_value:
  | c_expr
  | '+' I_or_F_const
  | '-' I_or_F_const
-}
data SelectFetchFirstValue =
  ExprSelectFetchFirstValue Expr |
  NumSelectFetchFirstValue Bool (Either Int64 Double)
  deriving (Show, Generic, Eq, Ord)

{-
select_limit_value:
  | a_expr
  | ALL
-}
data SelectLimitValue =
  ExprSelectLimitValue Expr |
  AllSelectLimitValue
  deriving (Show, Generic, Eq, Ord)

{-
offset_clause:
  | OFFSET select_offset_value
  | OFFSET select_fetch_first_value row_or_rows
select_offset_value:
  | a_expr
row_or_rows:
  | ROW
  | ROWS
-}
data OffsetClause =
  ExprOffsetClause Expr |
  FetchFirstOffsetClause SelectFetchFirstValue Bool
  deriving (Show, Generic, Eq, Ord)


-- * For Locking
-------------------------

{-
for_locking_clause:
  | for_locking_items
  | FOR READ ONLY
for_locking_items:
  | for_locking_item
  | for_locking_items for_locking_item
-}
data ForLockingClause =
  ItemsForLockingClause (NonEmpty ForLockingItem) |
  ReadOnlyForLockingClause
  deriving (Show, Generic, Eq, Ord)

{-
for_locking_item:
  | for_locking_strength locked_rels_list opt_nowait_or_skip
locked_rels_list:
  | OF qualified_name_list
  | EMPTY
opt_nowait_or_skip:
  | NOWAIT
  | SKIP LOCKED
  | EMPTY
-}
data ForLockingItem = ForLockingItem ForLockingStrength (Maybe (NonEmpty QualifiedName)) (Maybe Bool)
  deriving (Show, Generic, Eq, Ord)

{-
for_locking_strength:
  | FOR UPDATE
  | FOR NO KEY UPDATE
  | FOR SHARE
  | FOR KEY SHARE
-}
data ForLockingStrength =
  UpdateForLockingStrength |
  NoKeyUpdateForLockingStrength |
  ShareForLockingStrength |
  KeyForLockingStrength
  deriving (Show, Generic, Eq, Ord)


-- * Table references and joining
-------------------------

{-
| relation_expr opt_alias_clause
| relation_expr opt_alias_clause tablesample_clause
| func_table func_alias_clause
| LATERAL_P func_table func_alias_clause
| xmltable opt_alias_clause
| LATERAL_P xmltable opt_alias_clause
| select_with_parens opt_alias_clause
| LATERAL_P select_with_parens opt_alias_clause
| joined_table
| '(' joined_table ')' alias_clause

TODO: Add tablesample_clause
-}
data TableRef =
  {-
  | relation_expr opt_alias_clause
  | relation_expr opt_alias_clause tablesample_clause
  -}
  RelationExprTableRef RelationExpr (Maybe AliasClause) |
  {-
  | select_with_parens opt_alias_clause
  | LATERAL_P select_with_parens opt_alias_clause
  -}
  SelectTableRef Bool SelectNoParens (Maybe AliasClause) |
  {-
  | joined_table
  | '(' joined_table ')' alias_clause
  -}
  JoinTableRef JoinedTable (Maybe AliasClause)
  deriving (Show, Generic, Eq, Ord)

{-
| qualified_name
| qualified_name '*'
| ONLY qualified_name
| ONLY '(' qualified_name ')'
-}
data RelationExpr =
  SimpleRelationExpr QualifiedName Bool |
  OnlyRelationExpr QualifiedName Bool
  deriving (Show, Generic, Eq, Ord)

{-
alias_clause:
  |  AS ColId '(' name_list ')'
  |  AS ColId
  |  ColId '(' name_list ')'
  |  ColId
-}
data AliasClause = AliasClause Name (Maybe (NonEmpty Name))
  deriving (Show, Generic, Eq, Ord)

{-
| '(' joined_table ')'
| table_ref CROSS JOIN table_ref
| table_ref join_type JOIN table_ref join_qual
| table_ref JOIN table_ref join_qual
| table_ref NATURAL join_type JOIN table_ref
| table_ref NATURAL JOIN table_ref

The options are covered by the `JoinMeth` type.
-}
data JoinedTable =
  InParensJoinedTable JoinedTable |
  MethJoinedTable JoinMeth TableRef TableRef
  deriving (Show, Generic, Eq, Ord)

{-
| table_ref CROSS JOIN table_ref
| table_ref join_type JOIN table_ref join_qual
| table_ref JOIN table_ref join_qual
| table_ref NATURAL join_type JOIN table_ref
| table_ref NATURAL JOIN table_ref
-}
data JoinMeth =
  CrossJoinMeth |
  QualJoinMeth (Maybe JoinType) JoinQual |
  NaturalJoinMeth (Maybe JoinType)
  deriving (Show, Generic, Eq, Ord)

{-
| FULL join_outer
| LEFT join_outer
| RIGHT join_outer
| INNER_P
-}
data JoinType =
  FullJoinType Bool |
  LeftJoinType Bool |
  RightJoinType Bool |
  InnerJoinType
  deriving (Show, Generic, Eq, Ord)

{-
join_qual:
  |  USING '(' name_list ')'
  |  ON a_expr
-}
data JoinQual =
  UsingJoinQual (NonEmpty Name) |
  OnJoinQual Expr
  deriving (Show, Generic, Eq, Ord)


-- * Where
-------------------------

type WhereClause = Expr

{-
| WHERE a_expr
| WHERE CURRENT_P OF cursor_name
| /*EMPTY*/
-}
newtype WhereOrCurrentClause = WhereOrCurrentClause (Maybe (Either Expr Name))
  deriving (Show, Generic, Eq, Ord)


-- * Expression
-------------------------

data Expr =
  PlaceholderExpr Int |
  TypecastExpr Expr Type |
  BinOpExpr Text Expr Expr |
  EscapableBinOpExpr Bool Text Expr Expr (Maybe Expr) |
  DefaultExpr |
  QualifiedNameExpr QualifiedName |
  LiteralExpr Literal |
  InParensExpr Expr (Maybe Indirection) |
  {-
  case_expr:
    |  CASE case_arg when_clause_list case_default END_P
  case_default:
    |  ELSE a_expr
    |  EMPTY
  case_arg:
    |  a_expr
    |  EMPTY
  -}
  CaseExpr (Maybe Expr) (NonEmpty WhenClause) (Maybe Expr) |
  FuncExpr FuncApplication |
  SelectExpr SelectNoParens |
  ExistsSelectExpr SelectNoParens |
  ArraySelectExpr SelectNoParens |
  GroupingExpr (NonEmpty Expr)
  deriving (Show, Generic, Eq, Ord)

{-
when_clause:
  |  WHEN a_expr THEN a_expr
-}
data WhenClause = WhenClause Expr Expr
  deriving (Show, Generic, Eq, Ord)

{-
func_application:
  |  func_name '(' ')'
  |  func_name '(' func_arg_list opt_sort_clause ')'
  |  func_name '(' VARIADIC func_arg_expr opt_sort_clause ')'
  |  func_name '(' func_arg_list ',' VARIADIC func_arg_expr opt_sort_clause ')'
  |  func_name '(' ALL func_arg_list opt_sort_clause ')'
  |  func_name '(' DISTINCT func_arg_list opt_sort_clause ')'
  |  func_name '(' '*' ')'
-}
data FuncApplication = FuncApplication QualifiedName (Maybe FuncApplicationParams)
  deriving (Show, Generic, Eq, Ord)

{-
func_application:
  |  func_name '(' ')'
  |  func_name '(' func_arg_list opt_sort_clause ')'
  |  func_name '(' VARIADIC func_arg_expr opt_sort_clause ')'
  |  func_name '(' func_arg_list ',' VARIADIC func_arg_expr opt_sort_clause ')'
  |  func_name '(' ALL func_arg_list opt_sort_clause ')'
  |  func_name '(' DISTINCT func_arg_list opt_sort_clause ')'
  |  func_name '(' '*' ')'
-}
data FuncApplicationParams =
  NormalFuncApplicationParams (Maybe AllOrDistinct) (NonEmpty FuncArgExpr) (Maybe SortClause) |
  VariadicFuncApplicationParams (Maybe (NonEmpty FuncArgExpr)) FuncArgExpr (Maybe SortClause) |
  StarFuncApplicationParams
  deriving (Show, Generic, Eq, Ord)

data FuncArgExpr =
  ExprFuncArgExpr Expr |
  ColonEqualsFuncArgExpr Name Expr |
  EqualsGreaterFuncArgExpr Name Expr
  deriving (Show, Generic, Eq, Ord)

{-|
AexprConst:
  |  Iconst
  |  FCONST
  |  Sconst
  |  BCONST
  |  XCONST
  |  func_name Sconst
  |  func_name '(' func_arg_list opt_sort_clause ')' Sconst
  |  ConstTypename Sconst
  |  ConstInterval Sconst opt_interval
  |  ConstInterval '(' Iconst ')' Sconst
  |  TRUE_P
  |  FALSE_P
  |  NULL_P
-}
data Literal =
  IntLiteral Int64 |
  FloatLiteral Double |
  StringLiteral Text |
  BitLiteral Text |
  HexLiteral Text |
  FuncLiteral QualifiedName (Maybe FuncLiteralArgList) Text |
  ConstTypenameLiteral ConstTypename Text |
  StringIntervalLiteral Text (Maybe Interval) |
  IntIntervalLiteral Int64 Text |
  BoolLiteral Bool |
  NullLiteral
  deriving (Show, Generic, Eq, Ord)

data FuncLiteralArgList = FuncLiteralArgList (NonEmpty FuncArgExpr) (Maybe SortClause)
  deriving (Show, Generic, Eq, Ord)

{-
ConstTypename:
  | Numeric
  | ConstBit
  | ConstCharacter
  | ConstDatetime
-}
data ConstTypename =
  NumericConstTypename Numeric |
  ConstBitConstTypename ConstBit |
  ConstCharacterConstTypename ConstCharacter |
  ConstDatetimeConstTypename ConstDatetime
  deriving (Show, Generic, Eq, Ord)

{-
Numeric:
  | INT_P
  | INTEGER
  | SMALLINT
  | BIGINT
  | REAL
  | FLOAT_P opt_float
  | DOUBLE_P PRECISION
  | DECIMAL_P opt_type_modifiers
  | DEC opt_type_modifiers
  | NUMERIC opt_type_modifiers
  | BOOLEAN_P
opt_float:
  | '(' Iconst ')'
  | EMPTY
opt_type_modifiers:
  | '(' expr_list ')'
  | EMPTY
-}
data Numeric =
  IntNumeric |
  IntegerNumeric |
  SmallintNumeric |
  BigintNumeric |
  RealNumeric |
  FloatNumeric (Maybe Int64) |
  DoublePrecisionNumeric |
  DecimalNumeric (Maybe (NonEmpty Expr)) |
  DecNumeric (Maybe (NonEmpty Expr)) |
  NumericNumeric (Maybe (NonEmpty Expr)) |
  BooleanNumeric
  deriving (Show, Generic, Eq, Ord)

{-
Bit:
  | BitWithLength
  | BitWithoutLength
ConstBit:
  | BitWithLength
  | BitWithoutLength
BitWithLength:
  | BIT opt_varying '(' expr_list ')'
BitWithoutLength:
  | BIT opt_varying
-}
data ConstBit = ConstBit OptVarying (Maybe (NonEmpty Expr))
  deriving (Show, Generic, Eq, Ord)

{-
opt_varying:
  | VARYING
  | EMPTY
-}
type OptVarying = Bool

{-
Character:
  | CharacterWithLength
  | CharacterWithoutLength
ConstCharacter:
  | CharacterWithLength
  | CharacterWithoutLength
CharacterWithLength:
  | character '(' Iconst ')'
CharacterWithoutLength:
  | character
-}
data ConstCharacter = ConstCharacter Character (Maybe Int64)
  deriving (Show, Generic, Eq, Ord)

{-
character:
  | CHARACTER opt_varying
  | CHAR_P opt_varying
  | VARCHAR
  | NATIONAL CHARACTER opt_varying
  | NATIONAL CHAR_P opt_varying
  | NCHAR opt_varying
-}
data Character =
  CharacterCharacter OptVarying |
  CharCharacter OptVarying |
  VarcharCharacter |
  NationalCharacterCharacter OptVarying |
  NationalCharCharacter OptVarying |
  NcharCharacter OptVarying
  deriving (Show, Generic, Eq, Ord)

{-
ConstDatetime:
  | TIMESTAMP '(' Iconst ')' opt_timezone
  | TIMESTAMP opt_timezone
  | TIME '(' Iconst ')' opt_timezone
  | TIME opt_timezone
-}
data ConstDatetime =
  TimestampConstDatetime (Maybe Int64) (Maybe Timezone) |
  TimeConstDatetime (Maybe Int64) (Maybe Timezone)
  deriving (Show, Generic, Eq, Ord)

{-
opt_timezone:
  | WITH_LA TIME ZONE
  | WITHOUT TIME ZONE
  | EMPTY
-}
type Timezone = Bool

{-
opt_interval:
  | YEAR_P
  | MONTH_P
  | DAY_P
  | HOUR_P
  | MINUTE_P
  | interval_second
  | YEAR_P TO MONTH_P
  | DAY_P TO HOUR_P
  | DAY_P TO MINUTE_P
  | DAY_P TO interval_second
  | HOUR_P TO MINUTE_P
  | HOUR_P TO interval_second
  | MINUTE_P TO interval_second
  | EMPTY
-}
data Interval =
  YearInterval | MonthInterval | DayInterval | HourInterval | MinuteInterval |
  SecondInterval IntervalSecond |
  YearToMonthInterval |
  DayToHourInterval |
  DayToMinuteInterval |
  DayToSecondInterval IntervalSecond |
  HourToMinuteInterval |
  HourToSecondInterval IntervalSecond |
  MinuteToSecondInterval IntervalSecond
  deriving (Show, Generic, Eq, Ord)

{-
interval_second:
  | SECOND_P
  | SECOND_P '(' Iconst ')'
-}
type IntervalSecond = Maybe Int64


-- * Type
-------------------------

{-|
Consists of:

- Value/element type name
- Value/element nullability marker
- Array dimensions amount
- Array nullability marker
-}
data Type = Type Name Bool Int Bool
  deriving (Show, Generic, Eq, Ord)


-- * Names & References
-------------------------

data Name = QuotedName Text | UnquotedName Text
  deriving (Show, Generic, Eq, Ord)

{-
columnref:
  | ColId
  | ColId indirection
qualified_name:
  | ColId
  | ColId indirection
-}
data QualifiedName =
  SimpleQualifiedName Name |
  IndirectedQualifiedName Name Indirection
  deriving (Show, Generic, Eq, Ord)

{-
indirection:
  |  indirection_el
  |  indirection indirection_el
-}
type Indirection = NonEmpty IndirectionEl

{-
indirection_el:
  |  '.' attr_name
  |  '.' '*'
  |  '[' a_expr ']'
  |  '[' opt_slice_bound ':' opt_slice_bound ']'
opt_slice_bound:
  |  a_expr
  |  EMPTY
-}
data IndirectionEl =
  AttrNameIndirectionEl Name |
  AllIndirectionEl |
  ExprIndirectionEl Expr |
  SliceIndirectionEl (Maybe Expr) (Maybe Expr)
  deriving (Show, Generic, Eq, Ord)
