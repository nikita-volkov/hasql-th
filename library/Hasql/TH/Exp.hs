module Hasql.TH.Exp where

import Hasql.TH.Prelude hiding (sequence_, string, list)
import Language.Haskell.TH
import qualified Hasql.TH.Prelude as Prelude
import qualified Hasql.TH.Syntax.Extraction as Extraction
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Statement as Statement
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector.Generic as Vector
import qualified TupleTH


-- * Helpers
-------------------------

appList :: Exp -> [Exp] -> Exp
appList = foldl' AppE 

byteString :: ByteString -> Exp
byteString x =
  appList
    (VarE 'unsafeDupablePerformIO)
    [
      appList
        (VarE 'ByteString.unsafePackAddressLen)
        [
          LitE (IntegerL (fromIntegral (ByteString.length x))),
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
splitTupleAt arity position = unsafePerformIO $ runQ $ TupleTH.splitTupleAt arity position

{-|
Given a list of divisible functor expressions,
constructs an expression, which composes them together into
a single divisible functor, parameterized by a tuple of according arity.
-}
contrazip :: [Exp] -> Exp
contrazip = \ case
  head : [] -> head
  head : tail -> foldl1 AppE [VarE 'divide, splitTupleAt (succ (length tail)) 1, head, contrazip tail]
  [] -> SigE (VarE 'conquer)
    (let
      _fName = mkName "f"
      _fVar = VarT _fName
      in ForallT [PlainTV _fName] [AppT (ConT ''Divisible) (VarT _fName)]
          (AppT (VarT _fName) (TupleT 0)))

{-|
Given a list of applicative functor expressions,
constructs an expression, which composes them together into
a single applicative functor, parameterized by a tuple of according arity.

>>> $(return (cozip [])) :: Maybe ()
Just ()

>>> $(return (cozip (fmap (AppE (ConE 'Just) . LitE . IntegerL) [1,2,3]))) :: Maybe (Int, Int, Int)
Just (1,2,3)
-}
cozip :: [Exp] -> Exp
cozip = \ case
  _head : [] -> _head
  _head : _tail -> let
    _length = length _tail + 1
    in
      foldl' (\ a b -> AppE (AppE (VarE '(<*>)) a) b)
        (AppE (AppE (VarE 'fmap) (tuple _length)) _head)
        _tail
  [] -> AppE (VarE 'pure) (TupE [])


-- * Statement
-------------------------

statement :: ([Extraction.Decoder] -> Exp) -> Extraction.Statement -> Exp
statement _decodersExp (Extraction.Statement _sql _encoders _decoders) =
  foldl1 AppE
    [
      ConE 'Statement.Statement,
      byteString _sql,
      encoderList _encoders,
      _decodersExp _decoders,
      ConE 'True
    ]

resultlessStatement :: Extraction.Statement -> Exp
resultlessStatement = statement (const (VarE 'Decoders.noResult))

rowsAffectedStatement :: Extraction.Statement -> Exp
rowsAffectedStatement = statement (const (VarE 'Decoders.rowsAffected))

rowStatement :: Name -> Extraction.Statement -> Exp
rowStatement _rowDecoderName = statement (\ _decoders -> AppE (VarE _rowDecoderName) (decoderList _decoders))

{-|
>>> test = either (fail . show) (return . singletonStatement) . Extraction.statement

>>> :t $(test "select 1 :: int4")
$(test "select 1 :: int4") :: Statement.Statement () Int32

>>> :t $(test "select 1 :: int4, b :: text")
$(test "select 1 :: int4, b :: text")
  :: Statement.Statement () (Int32, Text)

>>> :t $(test "select $2 :: int4, $1 :: text")
$(test "select $2 :: int4, $1 :: text")
  :: Statement.Statement (Text, Int32) (Int32, Text)
-}
singletonStatement :: Extraction.Statement -> Exp
singletonStatement = rowStatement 'Decoders.singleRow

{-|
>>> test = either (fail . show) (return . maybeStatement) . Extraction.statement

>>> :t $(test "select 1 :: int4")
$(test "select 1 :: int4") :: Statement.Statement () (Maybe Int32)
-}
maybeStatement :: Extraction.Statement -> Exp
maybeStatement = rowStatement 'Decoders.rowMaybe

{-|
>>> test = either (fail . show) (return . vectorStatement) . Extraction.statement

>>> :t $(test "select 1 :: int4")
$(test "select 1 :: int4")
  :: Statement.Statement () (Data.Vector.Vector Int32)
-}
vectorStatement :: Extraction.Statement -> Exp
vectorStatement = rowStatement 'Decoders.rowVector

{-|
>>> test = either (fail . show) (return . foldStatement) . Extraction.statement

>>> :t $(test "select 1 :: int4")
$(test "select 1 :: int4")
  :: Fold Int32 b -> Statement.Statement () b
-}
foldStatement :: Extraction.Statement -> Exp
foldStatement _statement = let
  _stepVarName = mkName "step"
  _initVarName = mkName "init"
  _extractVarName = mkName "extract"
  in
    LamE
      [
        ConP 'Fold
          [
            VarP _stepVarName,
            VarP _initVarName,
            VarP _extractVarName
          ]
      ]
      (
        statement
          (\ _decoders ->
            AppE
              (AppE (VarE 'fmap) (VarE _extractVarName))
              (AppE
                (AppE
                  (AppE (VarE 'Decoders.foldlRows) (VarE _stepVarName))
                  (VarE _initVarName))
                (decoderList _decoders)))
          _statement
      )

{-|
Encoder of a product of parameters.
-}
encoderList :: [Extraction.Encoder] -> Exp
encoderList = contrazip . fmap encoder

encoder :: Extraction.Encoder -> Exp
encoder = let
  applyParam = AppE (VarE 'Encoders.param)
  applyArray levels = AppE (VarE 'Encoders.array) . applyArrayDimensionality levels
  applyArrayDimensionality levels =
    if levels > 0
      then AppE (AppE (VarE 'Encoders.dimension) (VarE 'foldl')) . applyArrayDimensionality (pred levels)
      else AppE (VarE 'Encoders.element)
  applyNullability nullable = AppE (VarE (if nullable then 'Encoders.nullable else 'Encoders.nonNullable))
  in \ (Extraction.Encoder valueEncoderName valueNull dimensionality arrayNull) ->
    if dimensionality > 0
      then VarE valueEncoderName & applyNullability valueNull & applyArray dimensionality & applyNullability arrayNull & applyParam
      else VarE valueEncoderName & applyNullability valueNull & applyParam

decoderList :: [Extraction.Decoder] -> Exp
decoderList = cozip . fmap decoder

decoder :: Extraction.Decoder -> Exp
decoder = let
  applyColumn = AppE (VarE 'Decoders.column)
  applyArray levels = AppE (VarE 'Decoders.array) . applyArrayDimensionality levels
  applyArrayDimensionality levels =
    if levels > 0
      then AppE (AppE (VarE 'Decoders.dimension) (VarE 'Vector.replicateM)) . applyArrayDimensionality (pred levels)
      else AppE (VarE 'Decoders.element)
  applyNullability nullable = AppE (VarE (if nullable then 'Decoders.nullable else 'Decoders.nonNullable))
  in \ (Extraction.Decoder valueDecoderName valueNull dimensionality arrayNull) ->
    if dimensionality > 0
      then VarE valueDecoderName & applyNullability valueNull & applyArray dimensionality & applyNullability arrayNull & applyColumn
      else VarE valueDecoderName & applyNullability valueNull & applyColumn
