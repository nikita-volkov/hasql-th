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
  SelectPreparableStmt SelectStmt |
  InsertPreparableStmt InsertStmt |
  UpdatePreparableStmt UpdateStmt |
  DeletePreparableStmt DeleteStmt
  deriving (Show, Generic, Eq, Ord)


-- * Insert
-------------------------

{-
InsertStmt:
  | opt_with_clause INSERT INTO insert_target insert_rest
      opt_on_conflict returning_clause
-}
data InsertStmt = InsertStmt (Maybe WithClause) InsertTarget InsertRest (Maybe OnConflict) (Maybe ReturningClause)
  deriving (Show, Generic, Eq, Ord)

{-
insert_target:
  | qualified_name
  | qualified_name AS ColId
-}
data InsertTarget = InsertTarget QualifiedName (Maybe ColId)
  deriving (Show, Generic, Eq, Ord)

{-
insert_rest:
  | SelectStmt
  | OVERRIDING override_kind VALUE_P SelectStmt
  | '(' insert_column_list ')' SelectStmt
  | '(' insert_column_list ')' OVERRIDING override_kind VALUE_P SelectStmt
  | DEFAULT VALUES
-}
data InsertRest =
  SelectInsertRest (Maybe InsertColumnList) (Maybe OverrideKind) SelectStmt |
  DefaultValuesInsertRest
  deriving (Show, Generic, Eq, Ord)

{-
override_kind:
  | USER
  | SYSTEM_P
-}
data OverrideKind = UserOverrideKind | SystemOverrideKind
  deriving (Show, Generic, Eq, Ord, Enum, Bounded)

{-
insert_column_list:
  | insert_column_item
  | insert_column_list ',' insert_column_item
-}
type InsertColumnList = NonEmpty InsertColumnItem

{-
insert_column_item:
  | ColId opt_indirection
-}
data InsertColumnItem = InsertColumnItem ColId (Maybe Indirection)
  deriving (Show, Generic, Eq, Ord)

{-
opt_on_conflict:
  | ON CONFLICT opt_conf_expr DO UPDATE SET set_clause_list where_clause
  | ON CONFLICT opt_conf_expr DO NOTHING
  | EMPTY
-}
data OnConflict = OnConflict (Maybe ConfExpr) OnConflictDo
  deriving (Show, Generic, Eq, Ord)

{-
opt_on_conflict:
  | ON CONFLICT opt_conf_expr DO UPDATE SET set_clause_list where_clause
  | ON CONFLICT opt_conf_expr DO NOTHING
  | EMPTY
-}
data OnConflictDo =
  UpdateOnConflictDo SetClauseList (Maybe WhereClause) |
  NothingOnConflictDo
  deriving (Show, Generic, Eq, Ord)

{-
opt_conf_expr:
  | '(' index_params ')' where_clause
  | ON CONSTRAINT name
  | EMPTY
-}
data ConfExpr =
  WhereConfExpr IndexParams (Maybe WhereClause) |
  ConstraintConfExpr Name
  deriving (Show, Generic, Eq, Ord)

{-
returning_clause:
  | RETURNING target_list
  | EMPTY
-}
type ReturningClause = TargetList


-- * Update
-------------------------

{-
UpdateStmt:
  | opt_with_clause UPDATE relation_expr_opt_alias
      SET set_clause_list
      from_clause
      where_or_current_clause
      returning_clause
-}
data UpdateStmt = UpdateStmt (Maybe WithClause) RelationExprOptAlias SetClauseList (Maybe FromClause) (Maybe WhereOrCurrentClause) (Maybe ReturningClause)
  deriving (Show, Generic, Eq, Ord)

{-
set_clause_list:
  | set_clause
  | set_clause_list ',' set_clause
-}
type SetClauseList = NonEmpty SetClause

{-
set_clause:
  | set_target '=' a_expr
  | '(' set_target_list ')' '=' a_expr
-}
data SetClause =
  TargetSetClause SetTarget AExpr |
  TargetListSetClause SetTargetList AExpr
  deriving (Show, Generic, Eq, Ord)

{-
set_target:
  | ColId opt_indirection
-}
data SetTarget = SetTarget ColId (Maybe Indirection)
  deriving (Show, Generic, Eq, Ord)

{-
set_target_list:
  | set_target
  | set_target_list ',' set_target
-}
type SetTargetList = NonEmpty SetTarget


-- * Delete
-------------------------

{-
DeleteStmt:
  | opt_with_clause DELETE_P FROM relation_expr_opt_alias
      using_clause where_or_current_clause returning_clause
-}
data DeleteStmt = DeleteStmt (Maybe WithClause) RelationExprOptAlias (Maybe UsingClause) (Maybe WhereOrCurrentClause) (Maybe ReturningClause)
  deriving (Show, Generic, Eq, Ord)

{-
using_clause:
  | USING from_list
  | EMPTY
-}
type UsingClause = FromList


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
-}
data SimpleSelect =
  NormalSimpleSelect (Maybe Targeting) (Maybe IntoClause) (Maybe FromClause) (Maybe WhereClause) (Maybe GroupClause) (Maybe HavingClause) (Maybe WindowClause) |
  ValuesSimpleSelect ValuesClause |
  TableSimpleSelect RelationExpr |
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
  NormalTargeting TargetList |
  AllTargeting (Maybe TargetList) |
  DistinctTargeting (Maybe ExprList) TargetList
  deriving (Show, Generic, Eq, Ord)

{-
target_list:
  | target_el
  | target_list ',' target_el
-}
type TargetList = NonEmpty TargetEl

{-
target_el:
  |  a_expr AS ColLabel
  |  a_expr IDENT
  |  a_expr
  |  '*'
-}
data TargetEl =
  AliasedExprTargetEl AExpr Ident |
  ImplicitlyAliasedExprTargetEl AExpr Ident |
  ExprTargetEl AExpr |
  AsteriskTargetEl
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
data CommonTableExpr = CommonTableExpr Ident (Maybe (NonEmpty Ident)) (Maybe Bool) PreparableStmt
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
  ExprGroupByItem AExpr |
  EmptyGroupingSetGroupByItem |
  RollupGroupByItem ExprList |
  CubeGroupByItem ExprList |
  GroupingSetsGroupByItem (NonEmpty GroupByItem)
  deriving (Show, Generic, Eq, Ord)

{-|
@
having_clause:
  |  HAVING a_expr
  |  EMPTY
@
-}
type HavingClause = AExpr

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
data WindowDefinition = WindowDefinition Ident WindowSpecification
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
data WindowSpecification = WindowSpecification (Maybe ExistingWindowName) (Maybe PartitionClause) (Maybe SortClause) (Maybe FrameClause)
  deriving (Show, Generic, Eq, Ord)

type ExistingWindowName = ColId

type PartitionClause = ExprList

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
  PrecedingFrameBound AExpr |
  FollowingFrameBound AExpr
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
type ValuesClause = NonEmpty ExprList

{-|

sort_clause:
  |  ORDER BY sortby_list

sortby_list:
  |  sortby
  |  sortby_list ',' sortby

-}
type SortClause = NonEmpty SortBy

{-
sortby:
  |  a_expr USING qual_all_Op opt_nulls_order
  |  a_expr opt_asc_desc opt_nulls_order
-}
data SortBy =
  UsingSortBy AExpr QualAllOp (Maybe NullsOrder) |
  AscDescSortBy AExpr (Maybe AscDesc) (Maybe NullsOrder)
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
  LimitLimitClause SelectLimitValue (Maybe AExpr) |
  FetchOnlyLimitClause Bool (Maybe SelectFetchFirstValue) Bool
  deriving (Show, Generic, Eq, Ord)

{-
select_fetch_first_value:
  | c_expr
  | '+' I_or_F_const
  | '-' I_or_F_const
-}
data SelectFetchFirstValue =
  ExprSelectFetchFirstValue CExpr |
  NumSelectFetchFirstValue Bool (Either Int64 Double)
  deriving (Show, Generic, Eq, Ord)

{-
select_limit_value:
  | a_expr
  | ALL
-}
data SelectLimitValue =
  ExprSelectLimitValue AExpr |
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
  ExprOffsetClause AExpr |
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
from_list:
  | table_ref
  | from_list ',' table_ref
-}
type FromList = NonEmpty TableRef

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

TODO: Add xmltable
-}
data TableRef =
  {-
  | relation_expr opt_alias_clause
  | relation_expr opt_alias_clause tablesample_clause
  -}
  RelationExprTableRef RelationExpr (Maybe AliasClause) (Maybe TablesampleClause) |
  {-
  | func_table func_alias_clause
  | LATERAL_P func_table func_alias_clause
  -}
  FuncTableRef Bool FuncTable (Maybe FuncAliasClause) |
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
relation_expr_opt_alias:
  | relation_expr
  | relation_expr ColId
  | relation_expr AS ColId
-}
data RelationExprOptAlias = RelationExprOptAlias RelationExpr (Maybe (Bool, ColId))
  deriving (Show, Generic, Eq, Ord)

{-
tablesample_clause:
  | TABLESAMPLE func_name '(' expr_list ')' opt_repeatable_clause
-}
data TablesampleClause = TablesampleClause FuncName ExprList (Maybe RepeatableClause)
  deriving (Show, Generic, Eq, Ord)

{-
opt_repeatable_clause:
  | REPEATABLE '(' a_expr ')'
  | EMPTY
-}
type RepeatableClause = AExpr

{-
func_table:
  | func_expr_windowless opt_ordinality
  | ROWS FROM '(' rowsfrom_list ')' opt_ordinality
-}
data FuncTable =
  FuncExprFuncTable FuncExprWindowless OptOrdinality |
  RowsFromFuncTable RowsfromList OptOrdinality
  deriving (Show, Generic, Eq, Ord)

{-
rowsfrom_item:
  | func_expr_windowless opt_col_def_list
-}
data RowsfromItem = RowsfromItem FuncExprWindowless (Maybe ColDefList)
  deriving (Show, Generic, Eq, Ord)

{-
rowsfrom_list:
  | rowsfrom_item
  | rowsfrom_list ',' rowsfrom_item
-}
type RowsfromList = NonEmpty RowsfromItem

{-
opt_col_def_list:
  | AS '(' TableFuncElementList ')'
  | EMPTY
-}
type ColDefList = TableFuncElementList

{-
opt_ordinality:
  | WITH_LA ORDINALITY
  | EMPTY
-}
type OptOrdinality = Bool

{-
TableFuncElementList:
  | TableFuncElement
  | TableFuncElementList ',' TableFuncElement
-}
type TableFuncElementList = NonEmpty TableFuncElement

{-
TableFuncElement:
  | ColId Typename opt_collate_clause
-}
data TableFuncElement = TableFuncElement ColId Typename (Maybe CollateClause)
  deriving (Show, Generic, Eq, Ord)

{-
opt_collate_clause:
  | COLLATE any_name
  | EMPTY
-}
type CollateClause = AnyName

{-
alias_clause:
  |  AS ColId '(' name_list ')'
  |  AS ColId
  |  ColId '(' name_list ')'
  |  ColId
-}
data AliasClause = AliasClause Bool ColId (Maybe NameList)
  deriving (Show, Generic, Eq, Ord)

{-
func_alias_clause:
  | alias_clause
  | AS '(' TableFuncElementList ')'
  | AS ColId '(' TableFuncElementList ')'
  | ColId '(' TableFuncElementList ')'
  | EMPTY
-}
data FuncAliasClause =
  AliasFuncAliasClause AliasClause |
  AsFuncAliasClause TableFuncElementList |
  AsColIdFuncAliasClause ColId TableFuncElementList |
  ColIdFuncAliasClause ColId TableFuncElementList
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
  UsingJoinQual (NonEmpty Ident) |
  OnJoinQual AExpr
  deriving (Show, Generic, Eq, Ord)


-- * Where
-------------------------

type WhereClause = AExpr

{-
| WHERE a_expr
| WHERE CURRENT_P OF cursor_name
| /*EMPTY*/
-}
data WhereOrCurrentClause = 
  ExprWhereOrCurrentClause AExpr |
  CursorWhereOrCurrentClause CursorName
  deriving (Show, Generic, Eq, Ord)


-- * Expression
-------------------------

type ExprList = NonEmpty AExpr

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
-}
data AExpr =
  CExprAExpr CExpr |
  TypecastAExpr AExpr TypecastTypename |
  CollateAExpr AExpr AnyName |
  AtTimeZoneAExpr AExpr AExpr |
  PlusAExpr AExpr |
  MinusAExpr AExpr |
  SymbolicBinOpAExpr AExpr SymbolicExprBinOp AExpr |
  PrefixQualOpAExpr QualOp AExpr |
  SuffixQualOpAExpr AExpr QualOp |
  AndAExpr AExpr AExpr |
  OrAExpr AExpr AExpr |
  NotAExpr AExpr |
  VerbalExprBinOpAExpr AExpr Bool VerbalExprBinOp AExpr (Maybe AExpr) |
  ReversableOpAExpr AExpr Bool AExprReversableOp |
  IsnullAExpr AExpr |
  NotnullAExpr AExpr |
  OverlapsAExpr Row Row |
  SubqueryAExpr AExpr SubqueryOp SubType (Either SelectWithParens AExpr) |
  UniqueAExpr SelectWithParens |
  DefaultAExpr
  deriving (Show, Generic, Eq, Ord)

{-
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
-}
data BExpr =
  CExprBExpr CExpr |
  TypecastBExpr BExpr TypecastTypename |
  PlusBExpr BExpr |
  MinusBExpr BExpr |
  SymbolicBinOpBExpr BExpr SymbolicExprBinOp BExpr |
  QualOpBExpr QualOp BExpr |
  IsOpBExpr BExpr Bool BExprIsOp
  deriving (Show, Generic, Eq, Ord)

{-
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
data CExpr =
  ColumnrefCExpr Columnref |
  AexprConstCExpr AexprConst |
  ParamCExpr Int (Maybe Indirection) |
  InParensCExpr AExpr (Maybe Indirection) |
  CaseCExpr CaseExpr |
  FuncCExpr FuncExpr |
  SelectWithParensCExpr SelectWithParens (Maybe Indirection) |
  ExistsCExpr SelectWithParens |
  ArrayCExpr (Either SelectWithParens ArrayExpr) |
  ExplicitRowCExpr ExplicitRow |
  ImplicitRowCExpr ImplicitRow |
  GroupingCExpr ExprList
  deriving (Show, Generic, Eq, Ord)

-- **
-------------------------

{-
in_expr:
  | select_with_parens
  | '(' expr_list ')'
-}
data InExpr =
  SelectInExpr SelectWithParens |
  ExprListInExpr ExprList
  deriving (Show, Generic, Eq, Ord)

{-
sub_type:
  | ANY
  | SOME
  | ALL
-}
data SubType = AnySubType | SomeSubType | AllSubType
  deriving (Show, Generic, Eq, Ord, Enum, Bounded)

{-
array_expr:
  | '[' expr_list ']'
  | '[' array_expr_list ']'
  | '[' ']'
-}
data ArrayExpr =
  ExprListArrayExpr ExprList |
  ArrayExprListArrayExpr ArrayExprList |
  EmptyArrayExpr
  deriving (Show, Generic, Eq, Ord)

{-
array_expr_list:
  | array_expr
  | array_expr_list ',' array_expr
-}
type ArrayExprList = NonEmpty ArrayExpr

{-
row:
  | ROW '(' expr_list ')'
  | ROW '(' ')'
  | '(' expr_list ',' a_expr ')'
-}
data Row =
  ExplicitRowRow ExplicitRow |
  ImplicitRowRow ImplicitRow
  deriving (Show, Generic, Eq, Ord)

{-
explicit_row:
  | ROW '(' expr_list ')'
  | ROW '(' ')'
-}
type ExplicitRow = Maybe ExprList

{-
implicit_row:
  | '(' expr_list ',' a_expr ')'
-}
data ImplicitRow = ImplicitRow ExprList AExpr
  deriving (Show, Generic, Eq, Ord)

{-
func_expr:
  | func_application within_group_clause filter_clause over_clause
  | func_expr_common_subexpr
-}
data FuncExpr =
  ApplicationFuncExpr FuncApplication (Maybe WithinGroupClause) (Maybe FilterClause) (Maybe OverClause) |
  SubexprFuncExpr FuncExprCommonSubexpr
  deriving (Show, Generic, Eq, Ord)

{-
func_expr_windowless:
  | func_application
  | func_expr_common_subexpr
-}
data FuncExprWindowless =
  ApplicationFuncExprWindowless FuncApplication |
  CommonSubexprFuncExprWindowless FuncExprCommonSubexpr
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
type FilterClause = AExpr

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
data FuncExprCommonSubexpr =
  CollationForFuncExprCommonSubexpr AExpr |
  CurrentDateFuncExprCommonSubexpr |
  CurrentTimeFuncExprCommonSubexpr (Maybe Int64) |
  CurrentTimestampFuncExprCommonSubexpr (Maybe Int64) |
  LocalTimeFuncExprCommonSubexpr (Maybe Int64) |
  LocalTimestampFuncExprCommonSubexpr (Maybe Int64) |
  CurrentRoleFuncExprCommonSubexpr |
  CurrentUserFuncExprCommonSubexpr |
  SessionUserFuncExprCommonSubexpr |
  UserFuncExprCommonSubexpr |
  CurrentCatalogFuncExprCommonSubexpr |
  CurrentSchemaFuncExprCommonSubexpr |
  CastFuncExprCommonSubexpr AExpr Typename |
  ExtractFuncExprCommonSubexpr (Maybe ExtractList) |
  OverlayFuncExprCommonSubexpr OverlayList |
  PositionFuncExprCommonSubexpr (Maybe PositionList) |
  SubstringFuncExprCommonSubexpr (Maybe SubstrList) |
  TreatFuncExprCommonSubexpr AExpr Typename |
  TrimFuncExprCommonSubexpr (Maybe TrimModifier) TrimList |
  NullIfFuncExprCommonSubexpr AExpr AExpr |
  CoalesceFuncExprCommonSubexpr ExprList |
  GreatestFuncExprCommonSubexpr ExprList |
  LeastFuncExprCommonSubexpr ExprList
  deriving (Show, Generic, Eq, Ord)

{-
extract_list:
  | extract_arg FROM a_expr
  | EMPTY
-}
data ExtractList = ExtractList ExtractArg AExpr
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
case_expr:
  | CASE case_arg when_clause_list case_default END_P
-}
data CaseExpr = CaseExpr (Maybe CaseArg) WhenClauseList (Maybe CaseDefault)
  deriving (Show, Generic, Eq, Ord)

{-
case_arg:
  | a_expr
  | EMPTY
-}
type CaseArg = AExpr

{-
when_clause_list:
  | when_clause
  | when_clause_list when_clause
-}
type WhenClauseList = NonEmpty WhenClause

{-
case_default:
  | ELSE a_expr
  | EMPTY
-}
type CaseDefault = AExpr

{-
when_clause:
  |  WHEN a_expr THEN a_expr
-}
data WhenClause = WhenClause AExpr AExpr
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
data FuncApplication = FuncApplication FuncName (Maybe FuncApplicationParams)
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
  ExprFuncArgExpr AExpr |
  ColonEqualsFuncArgExpr Ident AExpr |
  EqualsGreaterFuncArgExpr Ident AExpr
  deriving (Show, Generic, Eq, Ord)


-- * Constants
-------------------------

type Sconst = Text
type Iconst = Int64
type Fconst = Double
type Bconst = Text
type Xconst = Text

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
data AexprConst =
  IAexprConst Iconst |
  FAexprConst Fconst |
  SAexprConst Sconst |
  BAexprConst Bconst |
  XAexprConst Xconst |
  FuncAexprConst FuncName (Maybe FuncConstArgs) Sconst |
  ConstTypenameAexprConst ConstTypename Sconst |
  StringIntervalAexprConst Sconst (Maybe Interval) |
  IntIntervalAexprConst Iconst Sconst |
  BoolAexprConst Bool |
  NullAexprConst
  deriving (Show, Generic, Eq, Ord)

{-
  |  func_name '(' func_arg_list opt_sort_clause ')' Sconst
-}
data FuncConstArgs = FuncConstArgs (NonEmpty FuncArgExpr) (Maybe SortClause)
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
  DecimalNumeric (Maybe TypeModifiers) |
  DecNumeric (Maybe TypeModifiers) |
  NumericNumeric (Maybe TypeModifiers) |
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


-- * Names & References
-------------------------

{-
IDENT
-}
data Ident = QuotedIdent Text | UnquotedIdent Text
  deriving (Show, Generic, Eq, Ord)

{-
ColId:
  | IDENT
  | unreserved_keyword
  | col_name_keyword
-}
type ColId = Ident

{-
ColLabel:
  | IDENT
  | unreserved_keyword
  | col_name_keyword
  | type_func_name_keyword
  | reserved_keyword
-}
type ColLabel = Ident

{-
name:
  | ColId
-}
type Name = ColId

{-
name_list:
  | name
  | name_list ',' name
-}
type NameList = NonEmpty Name

{-
cursor_name:
  | name
-}
type CursorName = Name

{-
columnref:
  | ColId
  | ColId indirection
-}
data Columnref = Columnref ColId (Maybe Indirection)
  deriving (Show, Generic, Eq, Ord)

{-
any_name:
  | ColId
  | ColId attrs
-}
data AnyName = AnyName ColId (Maybe Attrs)
  deriving (Show, Generic, Eq, Ord)

{-
func_name:
  | type_function_name
  | ColId indirection
-}
data FuncName =
  TypeFuncName TypeFunctionName |
  IndirectedFuncName ColId Indirection
  deriving (Show, Generic, Eq, Ord)

{-
type_function_name:
  | IDENT
  | unreserved_keyword
  | type_func_name_keyword
-}
type TypeFunctionName = Ident

{-
columnref:
  | ColId
  | ColId indirection
qualified_name:
  | ColId
  | ColId indirection
-}
data QualifiedName =
  SimpleQualifiedName Ident |
  IndirectedQualifiedName Ident Indirection
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
  AttrNameIndirectionEl Ident |
  AllIndirectionEl |
  ExprIndirectionEl AExpr |
  SliceIndirectionEl (Maybe AExpr) (Maybe AExpr)
  deriving (Show, Generic, Eq, Ord)


-- * Types
-------------------------

{-|
The only custom extension required for
support of nullability markers in typecasted types.

Consists of:

- Value/element type name
- Value/element nullability marker
- Array dimensions amount
- Array nullability marker
-}
data TypecastTypename = TypecastTypename Ident Bool Int Bool
  deriving (Show, Generic, Eq, Ord)

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

{-
type_list:
  | Typename
  | type_list ',' Typename
-}
type TypeList = NonEmpty Typename


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
qual_all_Op:
  | all_Op
  | OPERATOR '(' any_operator ')'
-}
data QualAllOp =
  AllQualAllOp AllOp |
  AnyQualAllOp AnyOperator
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

data SymbolicExprBinOp =
  MathSymbolicExprBinOp MathOp |
  QualSymbolicExprBinOp QualOp
  deriving (Show, Generic, Eq, Ord)

data VerbalExprBinOp =
  LikeVerbalExprBinOp |
  IlikeVerbalExprBinOp |
  SimilarToVerbalExprBinOp
  deriving (Show, Generic, Eq, Ord, Enum, Bounded)

{-
  | a_expr IS NULL_P
  | a_expr IS TRUE_P
  | a_expr IS FALSE_P
  | a_expr IS UNKNOWN
  | a_expr IS DISTINCT FROM a_expr
  | a_expr IS OF '(' type_list ')'
  | a_expr BETWEEN opt_asymmetric b_expr AND a_expr
  | a_expr BETWEEN SYMMETRIC b_expr AND a_expr
  | a_expr IN_P in_expr
  | a_expr IS DOCUMENT_P
-}
data AExprReversableOp =
  NullAExprReversableOp |
  TrueAExprReversableOp |
  FalseAExprReversableOp |
  UnknownAExprReversableOp |
  DistinctFromAExprReversableOp AExpr |
  OfAExprReversableOp TypeList |
  BetweenAExprReversableOp Bool BExpr AExpr |
  BetweenSymmetricAExprReversableOp BExpr AExpr |
  InAExprReversableOp InExpr |
  DocumentAExprReversableOp
  deriving (Show, Generic, Eq, Ord)

{-
  | b_expr IS DISTINCT FROM b_expr
  | b_expr IS NOT DISTINCT FROM b_expr
  | b_expr IS OF '(' type_list ')'
  | b_expr IS NOT OF '(' type_list ')'
  | b_expr IS DOCUMENT_P
  | b_expr IS NOT DOCUMENT_P
-}
data BExprIsOp =
  DistinctFromBExprIsOp BExpr |
  OfBExprIsOp TypeList |
  DocumentBExprIsOp
  deriving (Show, Generic, Eq, Ord)

{-
subquery_Op:
  | all_Op
  | OPERATOR '(' any_operator ')'
  | LIKE
  | NOT_LA LIKE
  | ILIKE
  | NOT_LA ILIKE
-}
data SubqueryOp =
  AllSubqueryOp AllOp |
  AnySubqueryOp AnyOperator |
  LikeSubqueryOp Bool |
  IlikeSubqueryOp Bool
  deriving (Show, Generic, Eq, Ord)


-- * Indexes
-------------------------

{-
index_params:
  | index_elem
  | index_params ',' index_elem
-}
type IndexParams = NonEmpty IndexElem

{-
index_elem:
  | ColId opt_collate opt_class opt_asc_desc opt_nulls_order
  | func_expr_windowless opt_collate opt_class opt_asc_desc opt_nulls_order
  | '(' a_expr ')' opt_collate opt_class opt_asc_desc opt_nulls_order
-}
data IndexElem = IndexElem IndexElemDef (Maybe Collate) (Maybe Class) (Maybe AscDesc) (Maybe NullsOrder)
  deriving (Show, Generic, Eq, Ord)

{-
  | ColId opt_collate opt_class opt_asc_desc opt_nulls_order
  | func_expr_windowless opt_collate opt_class opt_asc_desc opt_nulls_order
  | '(' a_expr ')' opt_collate opt_class opt_asc_desc opt_nulls_order
-}
data IndexElemDef =
  IdIndexElemDef ColId |
  FuncIndexElemDef FuncExprWindowless |
  ExprIndexElemDef AExpr
  deriving (Show, Generic, Eq, Ord)

{-
opt_collate:
  | COLLATE any_name
  | EMPTY
-}
type Collate = AnyName

{-
opt_class:
  | any_name
  | EMPTY
-}
type Class = AnyName

{-
opt_asc_desc:
  | ASC
  | DESC
  | EMPTY
-}
data AscDesc = AscAscDesc | DescAscDesc
  deriving (Show, Generic, Eq, Ord, Enum, Bounded)

{-
opt_nulls_order:
  | NULLS_LA FIRST_P
  | NULLS_LA LAST_P
  | EMPTY
-}
data NullsOrder = FirstNullsOrder | LastNullsOrder
  deriving (Show, Generic, Eq, Ord, Enum, Bounded)
