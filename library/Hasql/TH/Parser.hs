module Hasql.TH.Parser where

import Hasql.TH.Prelude
import Text.Megaparsec


type Parser = Parsec Err Text

data Err
