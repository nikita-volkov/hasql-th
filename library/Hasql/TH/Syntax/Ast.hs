{-|
Names for nodes mostly resemble the according definitions in the @gram.y@
original Postgres parser file, except for the cases where we can optimize on that.

For reasoning see the docs of the parsing module of this project.
-}
module Hasql.TH.Syntax.Ast where

import Hasql.TH.Prelude hiding (Order, Op)


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

{-
SelectStmt:
  |  select_no_parens
  |  select_with_parens
-}
type SelectStmt = Either SelectNoParens SelectWithParens

{-
select_with_parens:
  |  '(' select_no_parens ')'
  |  '(' select_with_parens ')'
-}
data SelectWithParens =
  NoParensSelectWithParens SelectNoParens |
  WithParensSelectWithParens SelectWithParens
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
type SelectClause = Either SimpleSelect SelectWithParens

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
  BinSimpleSelect SelectBinOp SelectClause (Maybe Bool) SelectClause
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

{-
target_el:
  |  a_expr AS ColLabel
  |  a_expr IDENT
  |  a_expr
  |  '*'
-}
data Target =
  AliasedExprTarget Expr Name |
  ImplicitlyAliasedExprTarget Expr Name |
  ExprTarget Expr |
  AsteriskTarget
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

{-
opt_frame_clause:
  |  RANGE frame_extent opt_window_exclusion_clause
  |  ROWS frame_extent opt_window_exclusion_clause
  |  GROUPS frame_extent opt_window_exclusion_clause
  |  EMPTY
-}
data FrameClause = FrameClause FrameClauseMode FrameExtent (Maybe WindowExclusionClause)
  deriving (Show, Generic, Eq, Ord)

{-
opt_frame_clause:
  |  RANGE frame_extent opt_window_exclusion_clause
  |  ROWS frame_extent opt_window_exclusion_clause
  |  GROUPS frame_extent opt_window_exclusion_clause
  |  EMPTY
-}
data FrameClauseMode = RangeFrameClauseMode | RowsFrameClauseMode | GroupsFrameClauseMode
  deriving (Show, Generic, Eq, Ord)

{-
frame_extent:
  |  frame_bound
  |  BETWEEN frame_bound AND frame_bound
-}
data FrameExtent = SingularFrameExtent FrameBound | BetweenFrameExtent FrameBound FrameBound
  deriving (Show, Generic, Eq, Ord)

{-
frame_bound:
  |  UNBOUNDED PRECEDING
  |  UNBOUNDED FOLLOWING
  |  CURRENT_P ROW
  |  a_expr PRECEDING
  |  a_expr FOLLOWING
-}
data FrameBound =
  UnboundedPrecedingFrameBound |
  UnboundedFollowingFrameBound |
  CurrentRowFrameBound |
  PrecedingFrameBound Expr |
  FollowingFrameBound Expr
  deriving (Show, Generic, Eq, Ord)

{-
opt_window_exclusion_clause:
  |  EXCLUDE CURRENT_P ROW
  |  EXCLUDE GROUP_P
  |  EXCLUDE TIES
  |  EXCLUDE NO OTHERS
  |  EMPTY
-}
data WindowExclusionClause =
  CurrentRowWindowExclusionClause |
  GroupWindowExclusionClause |
  TiesWindowExclusionClause |
  NoOthersWindowExclusionClause
  deriving (Show, Generic, Eq, Ord)

{-
values_clause:
  |  VALUES '(' expr_list ')'
  |  values_clause ',' '(' expr_list ')'
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
  SelectTableRef Bool SelectWithParens (Maybe AliasClause) |
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

type AExpr = Expr
type BExpr = Expr
type CExpr = Expr

type ExprList = NonEmpty Expr

{-
a_expr:
  | c_expr
  | a_expr TYPECAST Typename
  | a_expr COLLATE any_name
  | a_expr AT TIME ZONE a_expr
  | '+' a_expr
  | '-' a_expr
  | a_expr '+' a_expr
  | a_expr '-' a_expr
  | a_expr '*' a_expr
  | a_expr '/' a_expr
  | a_expr '%' a_expr
  | a_expr '^' a_expr
  | a_expr '<' a_expr
  | a_expr '>' a_expr
  | a_expr '=' a_expr
  | a_expr LESS_EQUALS a_expr
  | a_expr GREATER_EQUALS a_expr
  | a_expr NOT_EQUALS a_expr
  | a_expr qual_Op a_expr
  | qual_Op a_expr
  | a_expr qual_Op
  | a_expr AND a_expr
  | a_expr OR a_expr
  | NOT a_expr
  | NOT_LA a_expr
  | a_expr LIKE a_expr
  | a_expr LIKE a_expr ESCAPE a_expr
  | a_expr NOT_LA LIKE a_expr
  | a_expr NOT_LA LIKE a_expr ESCAPE a_expr
  | a_expr ILIKE a_expr
  | a_expr ILIKE a_expr ESCAPE a_expr
  | a_expr NOT_LA ILIKE a_expr
  | a_expr NOT_LA ILIKE a_expr ESCAPE a_expr
  | a_expr SIMILAR TO a_expr
  | a_expr SIMILAR TO a_expr ESCAPE a_expr
  | a_expr NOT_LA SIMILAR TO a_expr
  | a_expr NOT_LA SIMILAR TO a_expr ESCAPE a_expr
  | a_expr IS NULL_P
  | a_expr ISNULL
  | a_expr IS NOT NULL_P
  | a_expr NOTNULL
  | row OVERLAPS row
  | a_expr IS TRUE_P
  | a_expr IS NOT TRUE_P
  | a_expr IS FALSE_P
  | a_expr IS NOT FALSE_P
  | a_expr IS UNKNOWN
  | a_expr IS NOT UNKNOWN
  | a_expr IS DISTINCT FROM a_expr
  | a_expr IS NOT DISTINCT FROM a_expr
  | a_expr IS OF '(' type_list ')'
  | a_expr IS NOT OF '(' type_list ')'
  | a_expr BETWEEN opt_asymmetric b_expr AND a_expr
  | a_expr NOT_LA BETWEEN opt_asymmetric b_expr AND a_expr
  | a_expr BETWEEN SYMMETRIC b_expr AND a_expr
  | a_expr NOT_LA BETWEEN SYMMETRIC b_expr AND a_expr
  | a_expr IN_P in_expr
  | a_expr NOT_LA IN_P in_expr
  | a_expr subquery_Op sub_type select_with_parens
  | a_expr subquery_Op sub_type '(' a_expr ')'
  | UNIQUE select_with_parens
  | a_expr IS DOCUMENT_P
  | a_expr IS NOT DOCUMENT_P
  | DEFAULT
b_expr:
  | c_expr
  | b_expr TYPECAST Typename
  | '+' b_expr
  | '-' b_expr
  | b_expr '+' b_expr
  | b_expr '-' b_expr
  | b_expr '*' b_expr
  | b_expr '/' b_expr
  | b_expr '%' b_expr
  | b_expr '^' b_expr
  | b_expr '<' b_expr
  | b_expr '>' b_expr
  | b_expr '=' b_expr
  | b_expr LESS_EQUALS b_expr
  | b_expr GREATER_EQUALS b_expr
  | b_expr NOT_EQUALS b_expr
  | b_expr qual_Op b_expr
  | qual_Op b_expr
  | b_expr qual_Op
  | b_expr IS DISTINCT FROM b_expr
  | b_expr IS NOT DISTINCT FROM b_expr
  | b_expr IS OF '(' type_list ')'
  | b_expr IS NOT OF '(' type_list ')'
  | b_expr IS DOCUMENT_P
  | b_expr IS NOT DOCUMENT_P
c_expr:
  | columnref
  | AexprConst
  | PARAM opt_indirection
  | '(' a_expr ')' opt_indirection
  | case_expr
  | func_expr
  | select_with_parens
  | select_with_parens indirection
  | EXISTS select_with_parens
  | ARRAY select_with_parens
  | ARRAY array_expr
  | explicit_row
  | implicit_row
  | GROUPING '(' expr_list ')'
-}
data Expr =
  PlaceholderExpr Int |
  TypecastExpr Expr Type |
  BinOpExpr Text Expr Expr |
  EscapableBinOpExpr Bool Text Expr Expr (Maybe Expr) |
  DefaultExpr |
  QualifiedNameExpr QualifiedName |
  LiteralExpr Literal |
  {-
  | '(' a_expr ')' opt_indirection
  | select_with_parens
  | select_with_parens indirection
  -}
  InParensExpr (Either Expr SelectWithParens) (Maybe Indirection) |
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
  FuncExpr FuncExpr |
  ExistsSelectExpr SelectWithParens |
  ArraySelectExpr SelectWithParens |
  GroupingExpr (NonEmpty Expr) |
  PlusedExpr Expr |
  MinusedExpr Expr |
  QualOpExpr QualOp Expr
  deriving (Show, Generic, Eq, Ord)

{-
func_expr:
  | func_application within_group_clause filter_clause over_clause
  | func_expr_common_subexpr
-}
data FuncExpr =
  ApplicationFuncExpr FuncApplication (Maybe WithinGroupClause) (Maybe FilterClause) (Maybe OverClause) |
  SubexprFuncExpr FuncExprCommonSubExpr
  deriving (Show, Generic, Eq, Ord)

{-
within_group_clause:
  | WITHIN GROUP_P '(' sort_clause ')'
  | EMPTY
-}
type WithinGroupClause = SortClause

{-
filter_clause:
  | FILTER '(' WHERE a_expr ')'
  | EMPTY
-}
type FilterClause = Expr

{-
over_clause:
  | OVER window_specification
  | OVER ColId
  | EMPTY
-}
data OverClause =
  WindowOverClause WindowSpecification |
  ColIdOverClause ColId
  deriving (Show, Generic, Eq, Ord)

{-
func_expr_common_subexpr:
  | COLLATION FOR '(' a_expr ')'
  | CURRENT_DATE
  | CURRENT_TIME
  | CURRENT_TIME '(' Iconst ')'
  | CURRENT_TIMESTAMP
  | CURRENT_TIMESTAMP '(' Iconst ')'
  | LOCALTIME
  | LOCALTIME '(' Iconst ')'
  | LOCALTIMESTAMP
  | LOCALTIMESTAMP '(' Iconst ')'
  | CURRENT_ROLE
  | CURRENT_USER
  | SESSION_USER
  | USER
  | CURRENT_CATALOG
  | CURRENT_SCHEMA
  | CAST '(' a_expr AS Typename ')'
  | EXTRACT '(' extract_list ')'
  | OVERLAY '(' overlay_list ')'
  | POSITION '(' position_list ')'
  | SUBSTRING '(' substr_list ')'
  | TREAT '(' a_expr AS Typename ')'
  | TRIM '(' BOTH trim_list ')'
  | TRIM '(' LEADING trim_list ')'
  | TRIM '(' TRAILING trim_list ')'
  | TRIM '(' trim_list ')'
  | NULLIF '(' a_expr ',' a_expr ')'
  | COALESCE '(' expr_list ')'
  | GREATEST '(' expr_list ')'
  | LEAST '(' expr_list ')'
  | XMLCONCAT '(' expr_list ')'
  | XMLELEMENT '(' NAME_P ColLabel ')'
  | XMLELEMENT '(' NAME_P ColLabel ',' xml_attributes ')'
  | XMLELEMENT '(' NAME_P ColLabel ',' expr_list ')'
  | XMLELEMENT '(' NAME_P ColLabel ',' xml_attributes ',' expr_list ')'
  | XMLEXISTS '(' c_expr xmlexists_argument ')'
  | XMLFOREST '(' xml_attribute_list ')'
  | XMLPARSE '(' document_or_content a_expr xml_whitespace_option ')'
  | XMLPI '(' NAME_P ColLabel ')'
  | XMLPI '(' NAME_P ColLabel ',' a_expr ')'
  | XMLROOT '(' a_expr ',' xml_root_version opt_xml_root_standalone ')'
  | XMLSERIALIZE '(' document_or_content a_expr AS SimpleTypename ')'

TODO: Implement the XML cases
-}
data FuncExprCommonSubExpr =
  CollationForFuncExprCommonSubExpr Expr |
  CurrentDateFuncExprCommonSubExpr |
  CurrentTimeFuncExprCommonSubExpr (Maybe Int64) |
  CurrentTimestampFuncExprCommonSubExpr (Maybe Int64) |
  LocalTimeFuncExprCommonSubExpr (Maybe Int64) |
  LocalTimestampFuncExprCommonSubExpr (Maybe Int64) |
  CurrentRoleFuncExprCommonSubExpr |
  CurrentUserFuncExprCommonSubExpr |
  SessionUserFuncExprCommonSubExpr |
  UserFuncExprCommonSubExpr |
  CurrentCatalogFuncExprCommonSubExpr |
  CurrentSchemaFuncExprCommonSubExpr |
  CastFuncExprCommonSubExpr Expr Typename |
  ExtractFuncExprCommonSubExpr (Maybe ExtractList) |
  OverlayFuncExprCommonSubExpr OverlayList |
  PositionFuncExprCommonSubExpr (Maybe PositionList) |
  SubstringFuncExprCommonSubExpr (Maybe SubstrList) |
  TreatFuncExprCommonSubExpr Expr Typename |
  TrimFuncExprCommonSubExpr (Maybe TrimModifier) TrimList |
  NullIfFuncExprCommonSubExpr Expr Expr |
  CoalesceFuncExprCommonSubExpr ExprList |
  GreatestFuncExprCommonSubExpr ExprList |
  LeastFuncExprCommonSubExpr ExprList
  deriving (Show, Generic, Eq, Ord)

{-
extract_list:
  | extract_arg FROM a_expr
  | EMPTY
-}
data ExtractList = ExtractList ExtractArg Expr
  deriving (Show, Generic, Eq, Ord)

{-
extract_arg:
  | IDENT
  | YEAR_P
  | MONTH_P
  | DAY_P
  | HOUR_P
  | MINUTE_P
  | SECOND_P
  | Sconst
-}
data ExtractArg =
  IdentExtractArg Ident |
  YearExtractArg |
  MonthExtractArg |
  DayExtractArg |
  HourExtractArg |
  MinuteExtractArg |
  SecondExtractArg |
  SconstExtractArg Sconst
  deriving (Show, Generic, Eq, Ord)

{-
overlay_list:
  | a_expr overlay_placing substr_from substr_for
  | a_expr overlay_placing substr_from
-}
data OverlayList = OverlayList AExpr OverlayPlacing SubstrFrom (Maybe SubstrFor)
  deriving (Show, Generic, Eq, Ord)

{-
overlay_placing:
  | PLACING a_expr
-}
type OverlayPlacing = AExpr

{-
position_list:
  | b_expr IN_P b_expr
  | EMPTY
-}
data PositionList = PositionList BExpr BExpr
  deriving (Show, Generic, Eq, Ord)

{-
substr_list:
  | a_expr substr_from substr_for
  | a_expr substr_for substr_from
  | a_expr substr_from
  | a_expr substr_for
  | expr_list
  | EMPTY
-}
data SubstrList =
  ExprSubstrList AExpr SubstrListFromFor |
  ExprListSubstrList ExprList
  deriving (Show, Generic, Eq, Ord)

{-
  | a_expr substr_from substr_for
  | a_expr substr_for substr_from
  | a_expr substr_from
  | a_expr substr_for
-}
data SubstrListFromFor =
  FromForSubstrListFromFor SubstrFrom SubstrFor |
  ForFromSubstrListFromFor SubstrFor SubstrFrom |
  FromSubstrListFromFor SubstrFrom |
  ForSubstrListFromFor SubstrFor
  deriving (Show, Generic, Eq, Ord)

{-
substr_from:
  | FROM a_expr
-}
type SubstrFrom = AExpr

{-
substr_for:
  | FOR a_expr
-}
type SubstrFor = AExpr

{-
  | TRIM '(' BOTH trim_list ')'
  | TRIM '(' LEADING trim_list ')'
  | TRIM '(' TRAILING trim_list ')'
-}
data TrimModifier = BothTrimModifier | LeadingTrimModifier | TrailingTrimModifier
  deriving (Show, Generic, Eq, Ord, Enum, Bounded)

{-
trim_list:
  | a_expr FROM expr_list
  | FROM expr_list
  | expr_list
-}
data TrimList =
  ExprFromExprListTrimList AExpr ExprList |
  FromExprListTrimList ExprList |
  ExprListTrimList ExprList
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
  NormalFuncApplicationParams (Maybe Bool) (NonEmpty FuncArgExpr) (Maybe SortClause) |
  VariadicFuncApplicationParams (Maybe (NonEmpty FuncArgExpr)) FuncArgExpr (Maybe SortClause) |
  StarFuncApplicationParams
  deriving (Show, Generic, Eq, Ord)

data FuncArgExpr =
  ExprFuncArgExpr Expr |
  ColonEqualsFuncArgExpr Name Expr |
  EqualsGreaterFuncArgExpr Name Expr
  deriving (Show, Generic, Eq, Ord)


-- * Constants
-------------------------

type AexprConst = Literal
type Sconst = Text
type Iconst = Int64
type Fconst = Double

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
data Bit = Bit OptVarying (Maybe ExprList)
  deriving (Show, Generic, Eq, Ord)

type ConstBit = Bit

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
IDENT
-}
type Ident = Name

{-
ColId:
  | IDENT
  | unreserved_keyword
  | col_name_keyword
-}
type ColId = Name

{-
ColLabel:
  | IDENT
  | unreserved_keyword
  | col_name_keyword
  | type_func_name_keyword
  | reserved_keyword
-}
type ColLabel = Name

{-
type_function_name:
  | IDENT
  | unreserved_keyword
  | type_func_name_keyword
-}
type TypeFunctionName = Name

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

-- ** Typename
-------------------------

{-
Typename:
  | SimpleTypename opt_array_bounds
  | SETOF SimpleTypename opt_array_bounds
  | SimpleTypename ARRAY '[' Iconst ']'
  | SETOF SimpleTypename ARRAY '[' Iconst ']'
  | SimpleTypename ARRAY
  | SETOF SimpleTypename ARRAY
-}
data Typename =
  ArrayBoundsTypename Bool SimpleTypename (Maybe ArrayBounds) |
  ArrayDimTypename Bool SimpleTypename (Maybe Iconst)
  deriving (Show, Generic, Eq, Ord)

{-
opt_array_bounds:
  | opt_array_bounds '[' ']'
  | opt_array_bounds '[' Iconst ']'
  | EMPTY
-}
type ArrayBounds = NonEmpty (Maybe Iconst)

{-
SimpleTypename:
  | GenericType
  | Numeric
  | Bit
  | Character
  | ConstDatetime
  | ConstInterval opt_interval
  | ConstInterval '(' Iconst ')'
ConstInterval:
  | INTERVAL
-}
data SimpleTypename =
  GenericTypeSimpleTypename GenericType |
  NumericSimpleTypename Numeric |
  BitSimpleTypename Bit |
  CharacterSimpleTypename Character |
  ConstDatetimeSimpleTypename ConstDatetime |
  ConstIntervalSimpleTypename (Either (Maybe Interval) Iconst)
  deriving (Show, Generic, Eq, Ord)

{-
GenericType:
  | type_function_name opt_type_modifiers
  | type_function_name attrs opt_type_modifiers
-}
data GenericType = GenericType TypeFunctionName (Maybe Attrs) (Maybe TypeModifiers)
  deriving (Show, Generic, Eq, Ord)

{-
attrs:
  | '.' attr_name
  | attrs '.' attr_name
-}
type Attrs = NonEmpty AttrName

{-
attr_name:
  | ColLabel
-}
type AttrName = ColLabel

{-
opt_type_modifiers:
  | '(' expr_list ')'
  | EMPTY
-}
type TypeModifiers = ExprList


-- * Operators
-------------------------

{-
qual_Op:
  | Op
  | OPERATOR '(' any_operator ')'
-}
data QualOp =
  OpQualOp Op |
  OperatorQualOp AnyOperator
  deriving (Show, Generic, Eq, Ord)

{-
The operator name is a sequence of up to NAMEDATALEN-1 (63 by default) 
characters from the following list:

+ - * / < > = ~ ! @ # % ^ & | ` ?

There are a few restrictions on your choice of name:
-- and /* cannot appear anywhere in an operator name, 
since they will be taken as the start of a comment.

A multicharacter operator name cannot end in + or -, 
unless the name also contains at least one of these characters:

~ ! @ # % ^ & | ` ?

For example, @- is an allowed operator name, but *- is not. 
This restriction allows PostgreSQL to parse SQL-compliant 
commands without requiring spaces between tokens.
The use of => as an operator name is deprecated. 
It may be disallowed altogether in a future release.

The operator != is mapped to <> on input, 
so these two names are always equivalent.
-}
type Op = Text

{-
any_operator:
  | all_Op
  | ColId '.' any_operator
-}
data AnyOperator =
  AllOpAnyOperator AllOp |
  QualifiedAnyOperator ColId AnyOperator
  deriving (Show, Generic, Eq, Ord)

{-
all_Op:
  | Op
  | MathOp
-}
data AllOp =
  OpAllOp Op |
  MathAllOp MathOp
  deriving (Show, Generic, Eq, Ord)

{-
MathOp:
  | '+'
  | '-'
  | '*'
  | '/'
  | '%'
  | '^'
  | '<'
  | '>'
  | '='
  | LESS_EQUALS
  | GREATER_EQUALS
  | NOT_EQUALS
-}
data MathOp =
  PlusMathOp |
  MinusMathOp |
  AsteriskMathOp |
  SlashMathOp |
  PercentMathOp |
  ArrowUpMathOp |
  ArrowLeftMathOp |
  ArrowRightMathOp |
  EqualsMathOp |
  LessEqualsMathOp |
  GreaterEqualsMathOp |
  ArrowLeftArrowRightMathOp |
  ExclamationEqualsMathOp
  deriving (Show, Generic, Eq, Ord, Enum, Bounded)
