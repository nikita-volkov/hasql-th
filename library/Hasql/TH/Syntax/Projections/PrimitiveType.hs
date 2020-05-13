module Hasql.TH.Syntax.Projections.PrimitiveType where

import Hasql.TH.Prelude hiding (sortBy, bit, fromList)
import PostgresqlSyntax.Ast


data PrimitiveType =
  BoolPrimitiveType |
  Int2PrimitiveType |
  Int4PrimitiveType |
  Int8PrimitiveType |
  Float4PrimitiveType |
  Float8PrimitiveType |
  NumericPrimitiveType |
  CharPrimitiveType |
  TextPrimitiveType |
  ByteaPrimitiveType |
  DatePrimitiveType |
  TimestampPrimitiveType |
  TimestamptzPrimitiveType |
  TimePrimitiveType |
  TimetzPrimitiveType |
  IntervalPrimitiveType |
  UuidPrimitiveType |
  InetPrimitiveType |
  JsonPrimitiveType |
  JsonbPrimitiveType

simpleTypename = \ case
  GenericTypeSimpleTypename a -> genericType a
  NumericSimpleTypename a -> numeric a
  BitSimpleTypename a -> bit a
  CharacterSimpleTypename a -> character a
  ConstDatetimeSimpleTypename a -> constDatetime a
  ConstIntervalSimpleTypename a -> Right IntervalPrimitiveType

genericType (GenericType a b c) = case b of
  Just _ -> Left "Type attributes are not supported"
  Nothing -> case c of
    Just _ -> Left "Type modifiers are not supported"
    Nothing -> ident a

numeric = \ case
  IntNumeric -> Right Int4PrimitiveType
  IntegerNumeric -> Right Int4PrimitiveType
  SmallintNumeric -> Right Int2PrimitiveType
  BigintNumeric -> Right Int8PrimitiveType
  RealNumeric -> Right Float4PrimitiveType
  FloatNumeric a -> case a of
    Just _ -> Left "Modifier on FLOAT is not supported"
    Nothing -> Right Float4PrimitiveType
  DoublePrecisionNumeric -> Right Float8PrimitiveType
  DecimalNumeric a -> case a of
    Just _ -> Left "Modifiers on DECIMAL are not supported"
    Nothing -> Right NumericPrimitiveType
  DecNumeric a -> case a of
    Just _ -> Left "Modifiers on DEC are not supported"
    Nothing -> Right NumericPrimitiveType
  NumericNumeric a -> case a of
    Just _ -> Left "Modifiers on NUMERIC are not supported"
    Nothing -> Right NumericPrimitiveType
  BooleanNumeric -> Right BoolPrimitiveType

bit _ = Left "Bit codec is not supported"

character _ = Right TextPrimitiveType

constDatetime = \ case
  TimestampConstDatetime _ a -> if tz a then Right TimestamptzPrimitiveType else Right TimestampPrimitiveType
  TimeConstDatetime _ a -> if tz a then Right TimetzPrimitiveType else Right TimePrimitiveType
  where
    tz = \ case
      Just a -> a
      Nothing -> False

ident = \ case
  QuotedIdent a -> name a
  UnquotedIdent a -> name a

name = \ case
  "bool" -> Right BoolPrimitiveType
  "int2" -> Right Int2PrimitiveType
  "int4" -> Right Int4PrimitiveType
  "int8" -> Right Int8PrimitiveType
  "float4" -> Right Float4PrimitiveType
  "float8" -> Right Float8PrimitiveType
  "numeric" -> Right NumericPrimitiveType
  "char" -> Right CharPrimitiveType
  "text" -> Right TextPrimitiveType
  "bytea" -> Right ByteaPrimitiveType
  "date" -> Right DatePrimitiveType
  "timestamp" -> Right TimestampPrimitiveType
  "timestamptz" -> Right TimestamptzPrimitiveType
  "time" -> Right TimePrimitiveType
  "timetz" -> Right TimetzPrimitiveType
  "interval" -> Right IntervalPrimitiveType
  "uuid" -> Right UuidPrimitiveType
  "inet" -> Right InetPrimitiveType
  "json" -> Right JsonPrimitiveType
  "jsonb" -> Right JsonbPrimitiveType
  name -> Left ("No codec exists for type: " <> name)
