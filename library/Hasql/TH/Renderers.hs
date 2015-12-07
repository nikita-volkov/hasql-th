module Hasql.TH.Renderers where

import Hasql.TH.Prelude
import Language.Haskell.TH
import qualified Hasql.Transaction as Transaction
import qualified Hasql.TH.Transactions as Transactions
import qualified Data.ByteString as ByteString


statementsTransactionExp :: [ByteString] -> Exp
statementsTransactionExp x =
  sequenceExp_ (map statementTransactionExp x)

statementTransactionExp :: ByteString -> Exp
statementTransactionExp x =
  AppE (VarE 'Transactions.statement) (byteStringExp x)

byteStringExp :: ByteString -> Exp
byteStringExp x =
  AppE (VarE 'ByteString.pack) (listExp integralExp (ByteString.unpack x))

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
