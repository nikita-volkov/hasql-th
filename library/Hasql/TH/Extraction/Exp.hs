module Hasql.TH.Extraction.Exp where

import Hasql.TH.Prelude
import Language.Haskell.TH
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import qualified Hasql.TH.Extraction.InputTypeList as InputTypeList
import qualified Hasql.TH.Extraction.OutputTypeList as OutputTypeList
import qualified Hasql.TH.Extraction.PrimitiveType as PrimitiveType
import qualified Hasql.TH.Construction.Exp as Exp
import qualified PostgresqlSyntax.Ast as Ast
import qualified PostgresqlSyntax.Rendering as Rendering


undecodedStatement :: (Exp -> Exp) -> Ast.PreparableStmt -> Either Text Exp
undecodedStatement _decoderProj _ast = let
  _sql = (Exp.byteString . Rendering.toByteString . Rendering.preparableStmt) _ast
  in do
    _encoder <- paramsEncoder _ast
    _rowDecoder <- rowDecoder _ast
    return (Exp.statement _sql _encoder (_decoderProj _rowDecoder))

foldStatement :: Ast.PreparableStmt -> Either Text Exp
foldStatement _ast = let
  _sql = (Exp.byteString . Rendering.toByteString . Rendering.preparableStmt) _ast
  in do
    _encoder <- paramsEncoder _ast
    _rowDecoder <- rowDecoder _ast
    return (Exp.foldStatement _sql _encoder _rowDecoder)

paramsEncoder :: Ast.PreparableStmt -> Either Text Exp
paramsEncoder a = do
  b <- InputTypeList.preparableStmt a
  c <- traverse paramEncoder b
  return (Exp.contrazip c)

rowDecoder :: Ast.PreparableStmt -> Either Text Exp
rowDecoder a = do
  b <- OutputTypeList.preparableStmt a
  c <- traverse columnDecoder b
  return (Exp.cozip c)

paramEncoder :: Ast.Typename -> Either Text Exp
paramEncoder =
  byTypename
    (\ a b -> valueEncoder a & fmap (Exp.unidimensionalParamEncoder b))
    (\ a b c d -> valueEncoder a & fmap (Exp.multidimensionalParamEncoder b c d))

columnDecoder :: Ast.Typename -> Either Text Exp
columnDecoder =
  byTypename
    (\ a b -> valueDecoder a & fmap (Exp.unidimensionalColumnDecoder b))
    (\ a b c d -> valueDecoder a & fmap (Exp.multidimensionalColumnDecoder b c d))

byTypename :: (PrimitiveType.PrimitiveType -> Bool -> Either Text Exp) -> (PrimitiveType.PrimitiveType -> Bool -> Int -> Bool -> Either Text Exp) -> Ast.Typename -> Either Text Exp
byTypename unidimensional multidimensional (Ast.Typename a b c d) =
  if a
    then Left "SETOF is not supported"
    else do
      e <- PrimitiveType.simpleTypename b
      case d of
        Nothing -> unidimensional e c
        Just (f, g) -> case f of
          Ast.BoundsTypenameArrayDimensions h -> multidimensional e c (length h) g
          Ast.ExplicitTypenameArrayDimensions _ -> multidimensional e c 1 g

valueEncoder :: PrimitiveType.PrimitiveType -> Either Text Exp
valueEncoder = Right . VarE . \ case
  PrimitiveType.BoolPrimitiveType -> 'Encoders.bool
  PrimitiveType.Int2PrimitiveType -> 'Encoders.int2
  PrimitiveType.Int4PrimitiveType -> 'Encoders.int4
  PrimitiveType.Int8PrimitiveType -> 'Encoders.int8
  PrimitiveType.Float4PrimitiveType -> 'Encoders.float4
  PrimitiveType.Float8PrimitiveType -> 'Encoders.float8
  PrimitiveType.NumericPrimitiveType -> 'Encoders.numeric
  PrimitiveType.CharPrimitiveType -> 'Encoders.char
  PrimitiveType.TextPrimitiveType -> 'Encoders.text
  PrimitiveType.ByteaPrimitiveType -> 'Encoders.bytea
  PrimitiveType.DatePrimitiveType -> 'Encoders.date
  PrimitiveType.TimestampPrimitiveType -> 'Encoders.timestamp
  PrimitiveType.TimestamptzPrimitiveType -> 'Encoders.timestamptz
  PrimitiveType.TimePrimitiveType -> 'Encoders.time
  PrimitiveType.TimetzPrimitiveType -> 'Encoders.timetz
  PrimitiveType.IntervalPrimitiveType -> 'Encoders.interval
  PrimitiveType.UuidPrimitiveType -> 'Encoders.uuid
  PrimitiveType.InetPrimitiveType -> 'Encoders.inet
  PrimitiveType.JsonPrimitiveType -> 'Encoders.json
  PrimitiveType.JsonbPrimitiveType -> 'Encoders.jsonb

valueDecoder :: PrimitiveType.PrimitiveType -> Either Text Exp
valueDecoder = Right . VarE . \ case
  PrimitiveType.BoolPrimitiveType -> 'Decoders.bool
  PrimitiveType.Int2PrimitiveType -> 'Decoders.int2
  PrimitiveType.Int4PrimitiveType -> 'Decoders.int4
  PrimitiveType.Int8PrimitiveType -> 'Decoders.int8
  PrimitiveType.Float4PrimitiveType -> 'Decoders.float4
  PrimitiveType.Float8PrimitiveType -> 'Decoders.float8
  PrimitiveType.NumericPrimitiveType -> 'Decoders.numeric
  PrimitiveType.CharPrimitiveType -> 'Decoders.char
  PrimitiveType.TextPrimitiveType -> 'Decoders.text
  PrimitiveType.ByteaPrimitiveType -> 'Decoders.bytea
  PrimitiveType.DatePrimitiveType -> 'Decoders.date
  PrimitiveType.TimestampPrimitiveType -> 'Decoders.timestamp
  PrimitiveType.TimestamptzPrimitiveType -> 'Decoders.timestamptz
  PrimitiveType.TimePrimitiveType -> 'Decoders.time
  PrimitiveType.TimetzPrimitiveType -> 'Decoders.timetz
  PrimitiveType.IntervalPrimitiveType -> 'Decoders.interval
  PrimitiveType.UuidPrimitiveType -> 'Decoders.uuid
  PrimitiveType.InetPrimitiveType -> 'Decoders.inet
  PrimitiveType.JsonPrimitiveType -> 'Decoders.json
  PrimitiveType.JsonbPrimitiveType -> 'Decoders.jsonb
