module Hasql.TH.Exp where

import Hasql.TH.Prelude hiding (sequence_, pure, string, list)
import Language.Haskell.TH
import qualified Hasql.TH.Prelude as Prelude
import qualified Hasql.TH.Syntax.Ast as Ast
import qualified Data.ByteString as ByteString
import qualified Hasql.Encoders as Encoders


byteString :: ByteString -> Exp
byteString x = AppE (VarE 'ByteString.pack) (list integral (ByteString.unpack x))

integral :: Integral a => a -> Exp
integral x = LitE (IntegerL (fromIntegral x))

list :: (a -> Exp) -> [a] -> Exp
list renderer x = ListE (map renderer x)

string :: String -> Exp
string x = LitE (StringL x)

char :: Char -> Exp
char x = LitE (CharL x)

sequence_ :: [Exp] -> Exp
sequence_ = foldl' andThen pure

pure :: Exp
pure = AppE (VarE 'Prelude.pure) (TupE [])

andThen :: Exp -> Exp -> Exp
andThen exp1 exp2 = AppE (AppE (VarE '(*>)) exp1) exp2

encoderByAstType :: Ast.Type -> Either Text Exp
encoderByAstType (Ast.Type nullable name arrayLevels) = let
  paramsEncoder =
    AppE (VarE 'Encoders.param) .
    AppE (VarE (if nullable then 'Encoders.nullable else 'Encoders.nonNullable))
  valueEncoder = case name of
    "bool" -> Right (VarE 'Encoders.bool)
    "int2" -> Right (VarE 'Encoders.int2)
    "int4" -> Right (VarE 'Encoders.int4)
    "int8" -> Right (VarE 'Encoders.int8)
    "float4" -> Right (VarE 'Encoders.float4)
    "float8" -> Right (VarE 'Encoders.float8)
    "numeric" -> Right (VarE 'Encoders.numeric)
    "char" -> Right (VarE 'Encoders.char)
    "text" -> Right (VarE 'Encoders.text)
    "bytea" -> Right (VarE 'Encoders.bytea)
    "date" -> Right (VarE 'Encoders.date)
    "timestamp" -> Right (VarE 'Encoders.timestamp)
    "timestamptz" -> Right (VarE 'Encoders.timestamptz)
    "time" -> Right (VarE 'Encoders.time)
    "timetz" -> Right (VarE 'Encoders.timetz)
    "interval" -> Right (VarE 'Encoders.interval)
    "uuid" -> Right (VarE 'Encoders.uuid)
    "inet" -> Right (VarE 'Encoders.inet)
    "json" -> Right (VarE 'Encoders.json)
    "jsonb" -> Right (VarE 'Encoders.jsonb)
    "enum" -> Right (VarE 'Encoders.enum)
    _ -> Left ("No value encoder exists for type: " <> name)
  in fmap paramsEncoder valueEncoder
