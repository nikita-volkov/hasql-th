module MHasql.TH.Extraction.Exp where

import qualified Data.Text.Encoding as Text
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified MHasql.TH.Construction.Exp as Exp
import qualified MHasql.TH.Extraction.InputTypeList as InputTypeList
import qualified MHasql.TH.Extraction.OutputTypeList as OutputTypeList
import qualified MHasql.TH.Extraction.PrimitiveType as PrimitiveType
import MHasql.TH.Prelude
import Language.Haskell.TH
import qualified PostgresqlSyntax.Ast as Ast

undecodedStatement :: (Exp -> Exp) -> Text -> Ast.PreparableStmt -> Either Text Exp
undecodedStatement decoderProj sql ast = do
  encoder <- paramsEncoder ast
  rowDecoder' <- rowDecoder ast
  return (Exp.statement (Exp.byteString $ Text.encodeUtf8 sql) encoder (decoderProj rowDecoder'))

foldStatement :: Text -> Ast.PreparableStmt -> Either Text Exp
foldStatement sql ast = do
  encoder <- paramsEncoder ast
  rowDecoder' <- rowDecoder ast
  return (Exp.foldStatement (Exp.byteString $ Text.encodeUtf8 sql) encoder rowDecoder')

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
    (\a -> valueEncoder a & fmap Exp.unidimensionalParamEncoder)
    (\a b -> valueEncoder a & fmap (Exp.multidimensionalParamEncoder b))

columnDecoder :: Ast.Typename -> Either Text Exp
columnDecoder =
  byTypename
    (\a -> valueDecoder a & fmap Exp.unidimensionalColumnDecoder)
    (\a b -> valueDecoder a & fmap (Exp.multidimensionalColumnDecoder b))

byTypename
  :: (PrimitiveType.PrimitiveType -> Either Text Exp)
  -> (PrimitiveType.PrimitiveType -> Int -> Either Text Exp)
  -> Ast.Typename -> Either Text Exp
byTypename unidimensional multidimensional (Ast.Typename a b d) =
  if a
    then Left "SETOF is not supported"
    else do
      e <- PrimitiveType.simpleTypename b
      case d of
        Nothing -> unidimensional e
        Just f -> case f of
          Ast.BoundsTypenameArrayDimensions h -> multidimensional e (length h)
          Ast.ExplicitTypenameArrayDimensions _ -> multidimensional e 1

valueEncoder :: PrimitiveType.PrimitiveType -> Either Text Exp
valueEncoder =
  Right . VarE . \case
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
valueDecoder =
  Right . VarE . \case
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
