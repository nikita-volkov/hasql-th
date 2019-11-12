module Hasql.TH.Syntax.Parser where

import Hasql.TH.Prelude
import Text.Megaparsec


type Parser = Parsec Err Text

data Err
