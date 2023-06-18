module MHasql.TH.Prelude
  ( module Exports
  , showAsText
  )
where

import Control.Applicative as Exports (Alternative, (<*>), (*>), (<|>), pure)
import Control.Category as Exports ((>>>), (.))
import Control.Foldl as Exports (Fold (..))
import Control.Monad as Exports (Monad, (<=<), join, foldM, return, zipWithM)
import Control.Monad.Fail as Exports (fail)
import Data.Bifunctor as Exports (first)
import Data.Bool as Exports (Bool(..))
import Data.ByteString as Exports (ByteString)
import Data.Either as Exports (Either(..), either)
import Data.Eq as Exports (Eq(..))
import Data.Foldable as Exports (Foldable, foldl', foldMap, toList)
import Data.Function as Exports ((&), ($), const)
import Data.Functor as Exports (fmap)
import Data.Functor.Contravariant.Divisible as Exports (Divisible, conquer, divide)
import Data.IntMap.Strict as Exports (IntMap)
import Data.List as Exports (map, length, splitAt)
import Data.List.NonEmpty as Exports (NonEmpty (..))
import Data.Map.Strict as Exports (Map)
import Data.Maybe as Exports (Maybe(..))
import Data.Ord as Exports (Ord(..))
import Data.Semigroup as Exports ((<>))
import Data.String as Exports (fromString)
import Data.Text as Exports (Text)
import Data.Traversable as Exports (sequence, traverse)
import Data.Tuple as Exports (snd)
import Data.UUID as Exports (UUID)
import GHC.Enum as Exports (Enum(..), enumFromTo)
import Prelude as Exports (Char, Int, Integral, String, Show, (+), show, fromIntegral)
import System.IO.Unsafe as Exports

showAsText :: Show a => a -> Text
showAsText = show >>> fromString
