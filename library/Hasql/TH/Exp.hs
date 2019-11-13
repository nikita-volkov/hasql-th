module Hasql.TH.Exp where

import Hasql.TH.Prelude hiding (sequence_, pure, string, list)
import Language.Haskell.TH
import qualified Hasql.TH.Prelude as Prelude
import qualified Hasql.TH.Syntax.Ast as Ast
import qualified Data.ByteString as ByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Hasql.Encoders as Encoders
import qualified TupleTH


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

tuple :: Int -> Exp
tuple = VarE . tupleDataName

splitTupleAt :: Int -> Int -> Exp
splitTupleAt arity position = unsafePerformIO $ runQ $ TupleTH.splitTupleAt arity position

{-|
Given a list of contravariant functor expressions,
constructs an expression, which composes them together into
a single contravariant functor, parameterized by a tuple of the according arity.

>>> contrazip []
VarE ...conquer

>>> contrazip [LitE (IntegerL 1)]
LitE (IntegerL 1)

>>> contrazip [LitE (IntegerL 1), LitE (IntegerL 2)]
AppE (AppE (AppE (VarE ...divide) (...)) (LitE (IntegerL 1))) (LitE (IntegerL 2))

>>> contrazip [LitE (IntegerL 1), LitE (IntegerL 2), LitE (IntegerL 3)]
AppE (AppE (AppE (VarE ...divide) (...)) (LitE (IntegerL 1))) (AppE (AppE (AppE (VarE ...divide) (...)) (LitE (IntegerL 2))) (LitE (IntegerL 3)))
-}
contrazip :: [Exp] -> Exp
contrazip = \ case
  head : [] -> head
  head : tail -> foldl1 AppE [VarE 'divide, splitTupleAt (succ (length tail)) 1, head, contrazip tail]
  [] -> VarE 'conquer

paramsEncoderByAstType :: Ast.Type -> Either Text Exp
paramsEncoderByAstType = let
  applyParam = AppE (VarE 'Encoders.param)
  applyArray levels = AppE (VarE 'Encoders.array) . applyArrayDimensionality levels
  applyArrayDimensionality levels =
    if levels > 0
      then AppE (AppE (VarE 'Encoders.dimension) (VarE 'foldl')) . applyArrayDimensionality (pred levels)
      else AppE (VarE 'Encoders.element)
  applyNullability nullable = AppE (VarE (if nullable then 'Encoders.nullable else 'Encoders.nonNullable))
  valueEncoder = \ case
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
    name -> Left ("No value encoder exists for type: " <> name)
  in \ (Ast.Type name valueNull dimensionality arrayNull) ->
    if dimensionality > 0
      then valueEncoder name <&> applyNullability valueNull <&> applyArray dimensionality <&> applyNullability arrayNull <&> applyParam
      else valueEncoder name <&> applyNullability valueNull <&> applyParam
