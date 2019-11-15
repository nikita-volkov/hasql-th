module Hasql.TH.Syntax.Predicate where

import Hasql.TH.Prelude hiding (expression)
import qualified Data.HashSet as HashSet
import qualified Hasql.TH.Syntax.HashSet as HashSet


{-
SQL identifiers and key words must begin with a letter
(a-z, but also letters with diacritical marks and non-Latin letters) or an underscore (_).
Subsequent characters in an identifier or key word can be letters,
underscores, digits (0-9), or dollar signs ($). 
-}
firstIdentifierChar :: Char -> Bool
firstIdentifierChar x = isAlpha x || x == '_'

{-
SQL identifiers and key words must begin with a letter
(a-z, but also letters with diacritical marks and non-Latin letters) or an underscore (_).
Subsequent characters in an identifier or key word can be letters,
underscores, digits (0-9), or dollar signs ($). 
-}
notFirstIdentifierChar :: Char -> Bool
notFirstIdentifierChar x = isAlphaNum x || x == '_' || x == '$'

unreservedKeyword :: Text -> Bool
unreservedKeyword = inSet HashSet.unreservedKeyword

colNameKeyword :: Text -> Bool
colNameKeyword = inSet HashSet.colNameKeyword

typeFuncNameKeyword :: Text -> Bool
typeFuncNameKeyword = inSet HashSet.typeFuncNameKeyword

reservedKeyword :: Text -> Bool
reservedKeyword = inSet HashSet.reservedKeyword

inSet :: (Eq a, Hashable a) => HashSet a -> a -> Bool
inSet = flip HashSet.member

symbolicBinOpChar :: Char -> Bool
symbolicBinOpChar = inSet HashSet.symbolicBinOpChars

{-|
>>> test = oneOf [(==3), (==7), (==3), (==5)]
>>> test 1
False

>>> test 3
True

>>> test 5
True
-}
oneOf :: [a -> Bool] -> a -> Bool
oneOf = foldr (\ a b c -> a c || b c) (const False)
