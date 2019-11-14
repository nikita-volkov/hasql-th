{-|
Names for nodes mostly resemble the according definitions in the @gram.y@
original Postgres parser file, except for the cases where we can optimize on that.

For reasoning see the docs of the parsing module of this project.
-}
module Hasql.TH.Syntax.Ast where

import Hasql.TH.Prelude hiding (Order)


-- * Select
-------------------------

data Select =
  Select
    (Maybe AllOrDistinctSelectClause)
    (Maybe (NonEmpty Selection))
    (Maybe (NonEmpty TableRef))
  deriving (Show, Eq, Ord)

data AllOrDistinctSelectClause =
  AllAllOrDistinctSelectClause |
  DistinctAllOrDistinctSelectClause (Maybe (NonEmpty Expr))
  deriving (Show, Eq, Ord)

data Selection =
  AllSelection |
  ExprSelection Expr (Maybe Name)
  deriving (Show, Eq, Ord)


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
  SelectTableRef Bool Select (Maybe AliasClause) |
  {-
  | joined_table
  | '(' joined_table ')' alias_clause
  -}
  JoinTableRef JoinedTable (Maybe AliasClause)
  deriving (Show, Eq, Ord)

{-
| qualified_name
| qualified_name '*'
| ONLY qualified_name
| ONLY '(' qualified_name ')'
-}
data RelationExpr =
  RelationExpr Bool Ref Bool
  deriving (Show, Eq, Ord)

data AliasClause = AliasClause Name (Maybe (NonEmpty Name))
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

data JoinQual =
  UsingJoinQual (NonEmpty Name) |
  OnJoinQual Expr
  deriving (Show, Eq, Ord)


-- * Where
-------------------------

newtype WhereClause = WhereClause (Maybe Expr)
  deriving (Show, Eq, Ord)

{-
| WHERE a_expr
| WHERE CURRENT_P OF cursor_name
| /*EMPTY*/
-}
newtype WhereOrCurrentClause = WhereOrCurrentClause (Maybe (Either Expr Name))
  deriving (Show, Eq, Ord)


-- * Expression
-------------------------

data Expr =
  PlaceholderExpr Int |
  TypecastExpr Expr Type |
  BinOpExpr Text Expr Expr |
  EscapableBinOpExpr Bool Text Expr Expr (Maybe Expr) |
  BetweenExpr Bool Expr Expr |
  DefaultExpr |
  ColumnRefExpr Ref |
  LiteralExpr Literal |
  InParensExpr Expr |
  CaseExpr (Maybe Expr) (NonEmpty WhenClause) (Maybe Expr) |
  FuncExpr FuncApplication |
  SelectExpr Select |
  ExistsSelectExpr Select |
  ArraySelectExpr Select |
  GroupingExpr (NonEmpty Expr)
  deriving (Show, Eq, Ord)

data WhenClause = WhenClause Expr Expr
  deriving (Show, Eq, Ord)

data FuncApplication = FuncApplication Name FuncApplicationParams
  deriving (Show, Eq, Ord)

data FuncApplicationParams =
  NoFuncApplicationParams |
  NormalFuncApplicationParams (Maybe AllOrDistinct) (NonEmpty FuncArg) (Maybe (NonEmpty OrderByItem)) |
  VariadicFuncApplicationParams (Maybe (NonEmpty FuncArg)) FuncArg (Maybe (NonEmpty OrderByItem)) |
  StarFuncApplicationParams
  deriving (Show, Eq, Ord)

data FuncArg =
  ExprFuncArg Expr
  deriving (Show, Eq, Ord)

data AllOrDistinct = AllAllOrDistinct | DistinctAllOrDistinct
  deriving (Show, Eq, Ord)

data OrderByItem = OrderByItem Expr (Maybe Order)
  deriving (Show, Eq, Ord)

data Order = AscOrder | DescOrder
  deriving (Show, Eq, Ord)

data Literal =
  IntLiteral Integer |
  FloatLiteral Scientific |
  StringLiteral Text |
  BitLiteral Text |
  NamedLiteral Text Text |
  IntervalLiteral Interval |
  BoolLiteral Bool |
  NullLiteral
  deriving (Show, Eq, Ord)

{-
| ConstInterval Sconst opt_interval
| ConstInterval '(' Iconst ')' Sconst
-}
data Interval =
  StringInterval Text (Maybe Text) |
  IntInterval Integer Text
  deriving (Show, Eq, Ord)


-- * Type
-------------------------

{-|
Consists of:

- Value/element type name
- Value/element nullability marker
- Array dimensions amount
- Array nullability marker
-}
data Type = Type Text Bool Int Bool
  deriving (Show, Eq, Ord)


-- * Names & References
-------------------------

data Ref = Ref (Maybe Name) Name
  deriving (Show, Eq, Ord)

data Name = QuotedName Text | UnquotedName Text
  deriving (Show, Eq, Ord)
