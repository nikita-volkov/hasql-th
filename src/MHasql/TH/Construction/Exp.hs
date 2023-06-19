-- |
-- Expression construction.
module MHasql.TH.Construction.Exp where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Vector.Generic as Vector
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Statement
import MHasql.TH.Prelude
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
pureUnit = AppE (VarE 'pure) (TupE [])

andThen :: Exp -> Exp -> Exp
andThen exp = AppE (AppE (VarE '(*>)) exp)

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
  [head] -> head
  head : tail -> appList (VarE 'divide) [splitTupleAt (succ (length tail)) 1, head, contrazip tail]
  [] ->
    SigE
      (VarE 'conquer)
      ( let fName = mkName "f"
         in ForallT
              [Compat.specifiedPlainTV fName]
              [AppT (ConT ''Divisible) (VarT fName)]
              (AppT (VarT fName) (TupleT 0))
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
  [head] -> head
  head : tail ->
    let length' = length tail + 1
     in foldl'
          (AppE . AppE (VarE '(<*>)))
          (AppE (AppE (VarE 'fmap) (tuple length')) head)
          tail
  [] -> AppE (VarE 'pure) (TupE [])

-- |
-- Lambda expression, which destructures 'Fold'.
foldLam :: (Exp -> Exp -> Exp -> Exp) -> Exp
foldLam body =
  let stepVarName = mkName "progress"
      initVarName = mkName "start"
      extractVarName = mkName "finish"
   in LamE
        [ Compat.conP
            'Fold
            [ VarP stepVarName,
              VarP initVarName,
              VarP extractVarName
            ]
        ]
        (body (VarE stepVarName) (VarE initVarName) (VarE extractVarName))

-- * Statement

statement :: Exp -> Exp -> Exp -> Exp
statement sql encoder decoder =
  appList (ConE 'Statement.Statement) [sql, encoder, decoder, ConE 'True]

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
foldStatement sql encoder rowDecoder' =
  foldLam (\step init extract -> statement sql encoder (foldResultDecoder step init extract rowDecoder'))

foldResultDecoder :: Exp -> Exp -> Exp -> Exp -> Exp
foldResultDecoder step init extract rowDecoder' =
  appList (VarE 'fmap) [extract, appList (VarE 'Decoders.foldlRows) [step, init, rowDecoder']]

unidimensionalParamEncoder :: Exp -> Exp
unidimensionalParamEncoder =
  applyParamToEncoder . applyNullabilityToEncoder True

multidimensionalParamEncoder :: Int -> Exp -> Exp
multidimensionalParamEncoder dimensionality =
  applyParamToEncoder . applyNullabilityToEncoder True . AppE (VarE 'Encoders.array)
    . applyArrayDimensionalityToEncoder dimensionality
    . applyNullabilityToEncoder True

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

unidimensionalColumnDecoder :: Exp -> Exp
unidimensionalColumnDecoder =
  applyColumnToDecoder . applyNullabilityToDecoder True

multidimensionalColumnDecoder :: Int -> Exp -> Exp
multidimensionalColumnDecoder dimensionality =
  applyColumnToDecoder . applyNullabilityToDecoder True . AppE (VarE 'Decoders.array)
    . applyArrayDimensionalityToDecoder dimensionality
    . applyNullabilityToDecoder True

applyColumnToDecoder :: Exp -> Exp
applyColumnToDecoder = AppE (VarE 'Decoders.column)

applyNullabilityToDecoder :: Bool -> Exp -> Exp
applyNullabilityToDecoder nullable = AppE (VarE (if nullable then 'Decoders.nullable else 'Decoders.nonNullable))

applyArrayDimensionalityToDecoder :: Int -> Exp -> Exp
applyArrayDimensionalityToDecoder levels =
  if levels > 0
    then AppE (AppE (VarE 'Decoders.dimension) (VarE 'Vector.replicateM)) . applyArrayDimensionalityToDecoder (pred levels)
    else AppE (VarE 'Decoders.element)
