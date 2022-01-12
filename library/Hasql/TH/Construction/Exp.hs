-- |
-- Expression construction.
module Hasql.TH.Construction.Exp where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector.Generic as Vector
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Statement
import Hasql.TH.Prelude hiding (list, sequence_, string)
import qualified Hasql.TH.Prelude as Prelude
import Language.Haskell.TH.Syntax
import qualified TemplateHaskell.Compat.V0208 as Compat

-- * Helpers

appList :: Exp -> [Exp] -> Exp
appList = foldl' AppE

byteString :: ByteString -> Exp
byteString x =
  appList
    (VarE 'unsafeDupablePerformIO)
    [ appList
        (VarE 'ByteString.unsafePackAddressLen)
        [ LitE (IntegerL (fromIntegral (ByteString.length x))),
          LitE (StringPrimL (ByteString.unpack x))
        ]
    ]

integral :: Integral a => a -> Exp
integral x = LitE (IntegerL (fromIntegral x))

list :: (a -> Exp) -> [a] -> Exp
list renderer x = ListE (map renderer x)

string :: String -> Exp
string x = LitE (StringL x)

char :: Char -> Exp
char x = LitE (CharL x)

sequence_ :: [Exp] -> Exp
sequence_ = foldl' andThen pureUnit

pureUnit :: Exp
pureUnit = AppE (VarE 'Prelude.pure) (TupE [])

andThen :: Exp -> Exp -> Exp
andThen exp1 exp2 = AppE (AppE (VarE '(*>)) exp1) exp2

tuple :: Int -> Exp
tuple = ConE . tupleDataName

splitTupleAt :: Int -> Int -> Exp
splitTupleAt arity position =
  let nameByIndex index = Name (OccName ('_' : show index)) NameS
      names = enumFromTo 0 (pred arity) & map nameByIndex
      pats = names & map VarP
      pat = TupP pats
      exps = names & map VarE
      body = splitAt position exps & \(a, b) -> Compat.tupE [Compat.tupE a, Compat.tupE b]
   in LamE [pat] body

-- |
-- Given a list of divisible functor expressions,
-- constructs an expression, which composes them together into
-- a single divisible functor, parameterized by a tuple of according arity.
contrazip :: [Exp] -> Exp
contrazip = \case
  _head : [] -> _head
  _head : _tail -> appList (VarE 'divide) [splitTupleAt (succ (length _tail)) 1, _head, contrazip _tail]
  [] ->
    SigE
      (VarE 'conquer)
      ( let _fName = mkName "f"
            _fVar = VarT _fName
         in ForallT
              [Compat.specifiedPlainTV _fName]
              [AppT (ConT ''Divisible) (VarT _fName)]
              (AppT (VarT _fName) (TupleT 0))
      )

-- |
-- Given a list of applicative functor expressions,
-- constructs an expression, which composes them together into
-- a single applicative functor, parameterized by a tuple of according arity.
--
-- >>> $(return (cozip [])) :: Maybe ()
-- Just ()
--
-- >>> $(return (cozip (fmap (AppE (ConE 'Just) . LitE . IntegerL) [1,2,3]))) :: Maybe (Int, Int, Int)
-- Just (1,2,3)
cozip :: [Exp] -> Exp
cozip = \case
  _head : [] -> _head
  _head : _tail ->
    let _length = length _tail + 1
     in foldl'
          (\a b -> AppE (AppE (VarE '(<*>)) a) b)
          (AppE (AppE (VarE 'fmap) (tuple _length)) _head)
          _tail
  [] -> AppE (VarE 'pure) (TupE [])

-- |
-- Lambda expression, which destructures 'Fold'.
foldLam :: (Exp -> Exp -> Exp -> Exp) -> Exp
foldLam _body =
  let _stepVarName = mkName "step"
      _initVarName = mkName "init"
      _extractVarName = mkName "extract"
   in LamE
        [ Compat.conp
            'Fold
            [ VarP _stepVarName,
              VarP _initVarName,
              VarP _extractVarName
            ]
        ]
        (_body (VarE _stepVarName) (VarE _initVarName) (VarE _extractVarName))

-- * Statement

statement :: Exp -> Exp -> Exp -> Exp
statement _sql _encoder _decoder =
  appList (ConE 'Statement.Statement) [_sql, _encoder, _decoder, ConE 'True]

noResultResultDecoder :: Exp
noResultResultDecoder = VarE 'Decoders.noResult

rowsAffectedResultDecoder :: Exp
rowsAffectedResultDecoder = VarE 'Decoders.rowsAffected

singleRowResultDecoder :: Exp -> Exp
singleRowResultDecoder = 'Decoders.singleRow & VarE & AppE

rowMaybeResultDecoder :: Exp -> Exp
rowMaybeResultDecoder = AppE (VarE 'Decoders.rowMaybe)

rowVectorResultDecoder :: Exp -> Exp
rowVectorResultDecoder = AppE (VarE 'Decoders.rowVector)

foldStatement :: Exp -> Exp -> Exp -> Exp
foldStatement _sql _encoder _rowDecoder =
  foldLam (\_step _init _extract -> statement _sql _encoder (foldResultDecoder _step _init _extract _rowDecoder))

foldResultDecoder :: Exp -> Exp -> Exp -> Exp -> Exp
foldResultDecoder _step _init _extract _rowDecoder =
  appList (VarE 'fmap) [_extract, appList (VarE 'Decoders.foldlRows) [_step, _init, _rowDecoder]]

unidimensionalParamEncoder :: Bool -> Exp -> Exp
unidimensionalParamEncoder nullable =
  applyParamToEncoder . applyNullabilityToEncoder nullable

multidimensionalParamEncoder :: Bool -> Int -> Bool -> Exp -> Exp
multidimensionalParamEncoder nullable dimensionality arrayNull =
  applyParamToEncoder . applyNullabilityToEncoder arrayNull . AppE (VarE 'Encoders.array)
    . applyArrayDimensionalityToEncoder dimensionality
    . applyNullabilityToEncoder nullable

applyParamToEncoder :: Exp -> Exp
applyParamToEncoder = AppE (VarE 'Encoders.param)

applyNullabilityToEncoder :: Bool -> Exp -> Exp
applyNullabilityToEncoder nullable = AppE (VarE (if nullable then 'Encoders.nullable else 'Encoders.nonNullable))

applyArrayDimensionalityToEncoder :: Int -> Exp -> Exp
applyArrayDimensionalityToEncoder levels =
  if levels > 0
    then AppE (AppE (VarE 'Encoders.dimension) (VarE 'Vector.foldl')) . applyArrayDimensionalityToEncoder (pred levels)
    else AppE (VarE 'Encoders.element)

rowDecoder :: [Exp] -> Exp
rowDecoder = cozip

unidimensionalColumnDecoder :: Bool -> Exp -> Exp
unidimensionalColumnDecoder nullable =
  applyColumnToDecoder . applyNullabilityToDecoder nullable

multidimensionalColumnDecoder :: Bool -> Int -> Bool -> Exp -> Exp
multidimensionalColumnDecoder nullable dimensionality arrayNull =
  applyColumnToDecoder . applyNullabilityToDecoder arrayNull . AppE (VarE 'Decoders.array)
    . applyArrayDimensionalityToDecoder dimensionality
    . applyNullabilityToDecoder nullable

applyColumnToDecoder :: Exp -> Exp
applyColumnToDecoder = AppE (VarE 'Decoders.column)

applyNullabilityToDecoder :: Bool -> Exp -> Exp
applyNullabilityToDecoder nullable = AppE (VarE (if nullable then 'Decoders.nullable else 'Decoders.nonNullable))

applyArrayDimensionalityToDecoder :: Int -> Exp -> Exp
applyArrayDimensionalityToDecoder levels =
  if levels > 0
    then AppE (AppE (VarE 'Decoders.dimension) (VarE 'Vector.replicateM)) . applyArrayDimensionalityToDecoder (pred levels)
    else AppE (VarE 'Decoders.element)
