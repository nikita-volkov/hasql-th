module Hasql.TH.Syntax.Validator where

import Hasql.TH.Prelude hiding (expression)
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Hasql.TH.Syntax.HashSet as HashSet
import qualified Hasql.TH.Syntax.Predicate as Predicate


{-
The operator name is a sequence of up to NAMEDATALEN-1 (63 by default) 
characters from the following list:

+ - * / < > = ~ ! @ # % ^ & | ` ?

There are a few restrictions on your choice of name:
-- and /* cannot appear anywhere in an operator name, 
since they will be taken as the start of a comment.

A multicharacter operator name cannot end in + or -, 
unless the name also contains at least one of these characters:

~ ! @ # % ^ & | ` ?

For example, @- is an allowed operator name, but *- is not. 
This restriction allows PostgreSQL to parse SQL-compliant 
commands without requiring spaces between tokens.
The use of => as an operator name is deprecated. 
It may be disallowed altogether in a future release.

The operator != is mapped to <> on input, 
so these two names are always equivalent.
-}
op :: Text -> Maybe Text
op a =
  if Text.null a
    then Just ("Operator is empty")
    else if Text.isInfixOf "--" a
      then Just ("Operator contains a prohibited \"--\" sequence: " <> a)
      else if Text.isInfixOf "/*" a
        then Just ("Operator contains a prohibited \"/*\" sequence: " <> a)
        else if Predicate.inSet HashSet.nonOp a
          then Just ("Operator is not generic: " <> a)
          else if Text.find Predicate.prohibitionLiftingOpChar a & isJust
            then Nothing
            else if Predicate.prohibitedOpChar (Text.last a)
              then Just ("Operator ends with a prohibited char: " <> a)
              else Nothing
