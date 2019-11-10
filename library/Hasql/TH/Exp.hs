module Hasql.TH.Exp where

import Hasql.TH.Prelude
import Language.Haskell.TH
import qualified Data.ByteString as A


byteStringExp :: ByteString -> Exp
byteStringExp x =
  AppE (VarE 'A.pack) (listExp integralExp (A.unpack x))

integralExp :: Integral a => a -> Exp
integralExp x =
  LitE (IntegerL (fromIntegral x))

listExp :: (a -> Exp) -> [a] -> Exp
listExp renderer x =
  ListE (map renderer x)

stringExp :: String -> Exp
stringExp x =
  LitE (StringL x)

charExp :: Char -> Exp
charExp x =
  LitE (CharL x)

sequenceExp_ :: [Exp] -> Exp
sequenceExp_ =
  foldl' andThenExp pureExp_

pureExp_ :: Exp
pureExp_ =
  AppE (VarE 'pure) (TupE [])

andThenExp :: Exp -> Exp -> Exp
andThenExp exp1 exp2 =
  AppE (AppE (VarE '(*>)) exp1) exp2
