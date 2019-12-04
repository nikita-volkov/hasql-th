module Hasql.TH.Syntax.Predicate where

import Hasql.TH.Prelude hiding (expression)
import qualified Data.HashSet as HashSet
import qualified Hasql.TH.Syntax.HashSet as HashSet


-- * Generic
-------------------------

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

inSet :: (Eq a, Hashable a) => HashSet a -> a -> Bool
inSet = flip HashSet.member


-- *
-------------------------

{-
ident_start   [A-Za-z\200-\377_]
-}
firstIdentifierChar :: Char -> Bool
firstIdentifierChar x = isAlpha x || x == '_' || x >= '\200' && x <= '\377'

{-
ident_cont    [A-Za-z\200-\377_0-9\$]
-}
notFirstIdentifierChar :: Char -> Bool
notFirstIdentifierChar x = isAlphaNum x || x == '_' || x == '$' || x >= '\200' && x <= '\377'

keyword :: Text -> Bool
keyword = inSet HashSet.keyword

unreservedKeyword :: Text -> Bool
unreservedKeyword = inSet HashSet.unreservedKeyword

colNameKeyword :: Text -> Bool
colNameKeyword = inSet HashSet.colNameKeyword

typeFuncNameKeyword :: Text -> Bool
typeFuncNameKeyword = inSet HashSet.typeFuncNameKeyword

reservedKeyword :: Text -> Bool
reservedKeyword = inSet HashSet.reservedKeyword

symbolicBinOpChar :: Char -> Bool
symbolicBinOpChar = inSet HashSet.symbolicBinOpChars

-- ** Op chars
-------------------------

opChar = inSet HashSet.opChars

prohibitedOpChar a = a == '+' || a == '-'

prohibitionLiftingOpChar = inSet HashSet.prohibitionLiftingOpChars
