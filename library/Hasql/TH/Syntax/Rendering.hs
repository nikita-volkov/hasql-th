module Hasql.TH.Syntax.Rendering where

import Hasql.TH.Prelude hiding (expr, try, option, many, sortBy)
import Hasql.TH.Syntax.Ast
import Data.ByteString.FastBuilder
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.ByteString.Builder.Scientific as Scientific
import qualified Data.ByteString.Builder as BsBuilder
import qualified Data.ByteString.Lazy as LazyBs


-- * Helpers
-------------------------

scientific :: Scientific -> Builder
scientific a = Scientific.scientificBuilder a & BsBuilder.toLazyByteString & LazyBs.toStrict & byteString

text :: Text -> Builder
text = stringUtf8 . Text.unpack

nonEmptyList :: (a -> Builder) -> NonEmpty a -> Builder
nonEmptyList = intersperseFoldMap1 ", "

lexemes :: [Builder] -> Builder
lexemes = mconcat . intersperse " "

optLexemes :: [Maybe Builder] -> Builder
optLexemes = lexemes . catMaybes

inParens :: Builder -> Builder
inParens a = "(" <> a <> ")"


-- * Select
-------------------------

preparableStmt :: PreparableStmt -> Builder
preparableStmt = \ case
  SelectPreparableStmt a -> selectStmt a

selectStmt :: SelectStmt -> Builder
selectStmt = \ case
  InParensSelectStmt a -> inParens (selectStmt a)
  NoParensSelectStmt a -> selectNoParens a

selectNoParens :: SelectNoParens -> Builder
selectNoParens = \ case
  SimpleSelectNoParens a -> simpleSelect a

selectClause :: SelectClause -> Builder
selectClause = either simpleSelect selectNoParens

simpleSelect :: SimpleSelect -> Builder
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
  BinSimpleSelect _ a _ b -> selectClause a <> selectClause b

targeting :: Targeting -> Builder
targeting = \ case
  NormalTargeting a -> nonEmptyList target a
  AllTargeting a -> "ALL" <> foldMap (mappend " " . nonEmptyList target) a
  DistinctTargeting a b -> "DISTINCT" <> foldMap (mappend " " . onExpressionsClause) a <> " " <> nonEmptyList target b

onExpressionsClause :: NonEmpty Expr -> Builder
onExpressionsClause a = "ON (" <> nonEmptyList expr a <> ")"

target :: Target -> Builder
target = \ case
  AllTarget -> "*"
  ExprTarget a b -> expr a <> foldMap (mappend " " . name) b


-- * Select Into
-------------------------

intoClause :: IntoClause -> Builder
intoClause a = "INTO " <> optTempTableName a

optTempTableName :: OptTempTableName -> Builder
optTempTableName (OptTempTableName a b c) =
  optLexemes
    [
      if a then Just "TEMP" else Nothing,
      if b then Just "UNLOGGED" else Nothing,
      Just (qualifiedName c)
    ]


-- * From
-------------------------

fromClause :: FromClause -> Builder
fromClause a = "FROM " <> nonEmptyList tableRef a

tableRef :: TableRef -> Builder
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
        Just (selectNoParens b),
        fmap aliasClause c
      ]
  JoinTableRef a b -> case b of
    Just c -> inParens (joinedTable a) <> " " <> aliasClause c
    Nothing -> joinedTable a

relationExpr :: RelationExpr -> Builder
relationExpr = \ case
  SimpleRelationExpr a b -> qualifiedName a <> bool "" " *" b
  OnlyRelationExpr a -> "ONLY " <> qualifiedName a

aliasClause :: AliasClause -> Builder
aliasClause (AliasClause a b) =
  optLexemes
    [
      Just "AS",
      Just (name a),
      fmap (inParens . nonEmptyList name) b
    ]

joinedTable :: JoinedTable -> Builder
joinedTable = \ case
  InParensJoinedTable a -> inParens (joinedTable a)
  MethJoinedTable a b c -> case a of
    CrossJoinMeth -> tableRef b <> " CROSS JOIN " <> tableRef c
    QualJoinMeth d e -> tableRef b <> foldMap (mappend " " . joinType) d <> " JOIN " <> tableRef c <> " " <> joinQual e
    NaturalJoinMeth d -> tableRef b <> " NATURAL" <> foldMap (mappend " " . joinType) d <> " " <> tableRef c

joinType :: JoinType -> Builder
joinType = \ case
  FullJoinType a -> "FULL" <> if a then " OUTER" else ""
  LeftJoinType a -> "LEFT" <> if a then " OUTER" else ""
  RightJoinType a -> "RIGHT" <> if a then " OUTER" else ""
  InnerJoinType -> "INNER"

joinQual :: JoinQual -> Builder
joinQual = \ case
  UsingJoinQual a -> "USING (" <> nonEmptyList name a <> ")" 
  OnJoinQual a -> "ON " <> expr a


-- * Where
-------------------------

whereClause :: Expr -> Builder
whereClause a = "WHERE " <> expr a


-- * Group By
-------------------------

groupClause :: GroupClause -> Builder
groupClause a = "GROUP BY " <> nonEmptyList groupByItem a

groupByItem :: GroupByItem -> Builder
groupByItem = \ case
  ExprGroupByItem a -> expr a
  EmptyGroupingSetGroupByItem -> "()"
  RollupGroupByItem a -> "ROLLUP (" <> nonEmptyList expr a <> ")"
  CubeGroupByItem a -> "CUBE (" <> nonEmptyList expr a <> ")"
  GroupingSetsGroupByItem a -> "GROUPING SETS (" <> nonEmptyList groupByItem a <> ")"


-- * Having
-------------------------

havingClause :: Expr -> Builder
havingClause a = "HAVING " <> expr a


-- * Window
-------------------------

windowClause :: NonEmpty WindowDefinition -> Builder
windowClause a = "WINDOW " <> nonEmptyList windowDefinition a

windowDefinition :: WindowDefinition -> Builder
windowDefinition (WindowDefinition a b) = name a <> " AS " <> windowSpecification b

windowSpecification :: WindowSpecification -> Builder
windowSpecification (WindowSpecification a b c d) =
  inParens $ optLexemes
    [
      fmap name a,
      fmap partitionClause b,
      fmap sortClause c,
      fmap frameClause d
    ]

partitionClause :: NonEmpty Expr -> Builder
partitionClause a = "PARTITION BY " <> nonEmptyList expr a

frameClause :: FrameClause -> Builder
frameClause (FrameClause a b c) =
  optLexemes
    [
      Just (frameClauseMode a),
      Just (frameExtent b),
      fmap windowExclusionCause c
    ]

frameClauseMode :: FrameClauseMode -> Builder
frameClauseMode = \ case
  RangeFrameClauseMode -> "RANGE"
  RowsFrameClauseMode -> "ROWS"
  GroupsFrameClauseMode -> "GROUPS"

frameExtent :: FrameExtent -> Builder
frameExtent = \ case
  SingularFrameExtent a -> frameBound a
  BetweenFrameExtent a b -> "BETWEEN " <> frameBound a <> " AND " <> frameBound b

frameBound :: FrameBound -> Builder
frameBound = \ case
  UnboundedPrecedingFrameBound -> "UNBOUNDED PRECEDING"
  UnboundedFollowingFrameBound -> "UNBOUNDED FOLLOWING"
  CurrentRowFrameBound -> "CURRENT ROW"
  PrecedingFrameBound a -> expr a <> " PRECEDING"
  FollowingFrameBound a -> expr a <> " FOLLOWING"

windowExclusionCause :: WindowExclusionClause -> Builder
windowExclusionCause = \ case
  CurrentRowWindowExclusionClause -> "EXCLUDE CURRENT ROW"
  GroupWindowExclusionClause -> "EXCLUDE GROUP"
  TiesWindowExclusionClause -> "EXCLUDE TIES"
  NoOthersWindowExclusionClause -> "EXCLUDE NO OTHERS"


-- * Order By
-------------------------

sortClause :: NonEmpty SortBy -> Builder
sortClause a = "ORDER BY " <> nonEmptyList sortBy a

sortBy :: SortBy -> Builder
sortBy (SortBy a b) = optLexemes [Just (expr a), fmap order b]

order :: Order -> Builder
order = \ case
  AscOrder -> "ASC"
  DescOrder -> "DESC"


-- * Values
-------------------------

valuesClause :: ValuesClause -> Builder
valuesClause a = "VALUES " <> nonEmptyList (inParens . nonEmptyList expr) a


-- * Expr
-------------------------

expr :: Expr -> Builder
expr = \ case
  PlaceholderExpr a -> "$" <> intDec a
  TypecastExpr a b -> expr a <> " :: " <> type_ b
  BinOpExpr a b c -> expr b <> " " <> text a <> " " <> expr c
  QualifiedNameExpr a -> qualifiedName a
  LiteralExpr a -> literal a
  InParensExpr a -> "(" <> expr a <> ")"
  CaseExpr a b c ->
    optLexemes [
      Just "CASE",
      fmap expr a,
      Just (nonEmptyList whenClause b),
      fmap caseDefault c,
      Just "END"
    ]
  FuncExpr a -> funcApplication a
  SelectExpr a -> inParens (selectNoParens a)
  ExistsSelectExpr a -> "EXISTS " <> inParens (selectNoParens a)
  ArraySelectExpr a -> "ARRAY " <> inParens (selectNoParens a)
  GroupingExpr a -> "GROUPING " <> inParens (nonEmptyList expr a)

type_ :: Type -> Builder
type_ (Type a _ b _) =
  text a <>
  fold (replicate b "[]")

whenClause :: WhenClause -> Builder
whenClause (WhenClause a b) = "WHEN " <> expr a <> " THEN " <> expr b

caseDefault :: Expr -> Builder
caseDefault a = "ELSE " <> expr a

funcApplication :: FuncApplication -> Builder
funcApplication (FuncApplication a b) =
  name a <> "(" <> foldMap funcApplicationParams b <> ")"

funcApplicationParams :: FuncApplicationParams -> Builder
funcApplicationParams = \ case
  NormalFuncApplicationParams a b c ->
    optLexemes
      [
        fmap allOrDistinct a,
        Just (nonEmptyList funcArgExpr b),
        fmap sortClause c
      ]
  VariadicFuncApplicationParams a b c ->
    optLexemes
      [
        fmap (nonEmptyList funcArgExpr) a,
        Just "VARIADIC",
        Just (funcArgExpr b),
        fmap sortClause c
      ]
  StarFuncApplicationParams -> "*"

allOrDistinct :: AllOrDistinct -> Builder
allOrDistinct = \ case
  AllAllOrDistinct -> "ALL"
  DistinctAllOrDistinct -> "DISTINCT"

funcArgExpr :: FuncArgExpr -> Builder
funcArgExpr = \ case
  ExprFuncArgExpr a -> expr a
  ColonEqualsFuncArgExpr a b -> name a <> " := " <> expr b
  EqualsGreaterFuncArgExpr a b -> name a <> " => " <> expr b


-- * Literals
-------------------------

literal :: Literal -> Builder
literal = \ case
  IntLiteral a -> integerDec a
  FloatLiteral a -> scientific a
  StringLiteral a -> "'" <> text (Text.replace "'" "''" a) <> "'"
  BoolLiteral a -> if a then "TRUE" else "FALSE"
  NullLiteral -> "NULL"


-- * Names and refs
-------------------------

name :: Name -> Builder
name = \ case
  QuotedName a -> char7 '"' <> text (Text.replace "\"" "\"\"" a) <> char7 '"'
  UnquotedName a -> text a

qualifiedName :: QualifiedName -> Builder
qualifiedName = \ case
  SimpleQualifiedName a -> name a
  IndirectedQualifiedName a b -> name a <> indirection b

indirection :: Indirection -> Builder
indirection = foldMap indirectionEl

indirectionEl :: IndirectionEl -> Builder
indirectionEl = \ case
  AttrNameIndirectionEl a -> "." <> name a
  AllIndirectionEl -> ".*"
  ExprIndirectionEl a -> "[" <> expr a <> "]"
  SliceIndirectionEl a b -> "[" <> foldMap expr a <> ":" <> foldMap expr b <> "]"
