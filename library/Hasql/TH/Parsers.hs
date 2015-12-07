module Hasql.TH.Parsers where

import Hasql.TH.Prelude
import Data.Attoparsec.Text
import qualified Data.Text.Encoding


run :: Parser a -> Text -> Either String a
run parser input =
  parseOnly parser input

statements :: Parser [ByteString]
statements =
  sepBy' statement separator
  where
    separator =
      char ';' *> skipSpace

statement :: Parser ByteString
statement =
  fmap Data.Text.Encoding.encodeUtf8 $
  takeWhile1 (/= ';')

