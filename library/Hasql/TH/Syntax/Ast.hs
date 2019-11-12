{-|
References:

- https://www.postgresql.org/docs/current/sql-select.html
-}
module Hasql.TH.Syntax.Ast where

import Hasql.TH.Prelude hiding (Order)


-- * Select
-------------------------

data Select =
  Select
    (Maybe AllOrDistinctSelectClause)
    (Maybe (NonEmpty Selection))
    (Maybe (NonEmpty FromItem))
  deriving (Show, Eq, Ord)

data AllOrDistinctSelectClause =
  AllAllOrDistinctSelectClause |
  DistinctAllOrDistinctSelectClause (Maybe (NonEmpty Expr))
  deriving (Show, Eq, Ord)

data Selection =
  AllSelection |
  ExprSelection Expr (Maybe Name)
  deriving (Show, Eq, Ord)

data FromItem =
  TableRefFromItem Bool Ref Bool (Maybe (Name, Maybe (NonEmpty Name)))
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
  InParenthesisExpr Expr |
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


-- * Type
-------------------------

{-|
Consists of:

- Nullability marker
- Base name
- Array levels amount
-}
data Type = Type Bool Text Int
  deriving (Show, Eq, Ord)


-- * Names & References
-------------------------

data Ref = Ref (Maybe Name) Name
  deriving (Show, Eq, Ord)

data Name = QuotedName Text | UnquotedName Text
  deriving (Show, Eq, Ord)
