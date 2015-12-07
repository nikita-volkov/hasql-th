module Hasql.TH.Queries where

import Hasql.TH.Prelude
import Hasql.Query
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders


statement :: ByteString -> Query () ()
statement sql =
  Query sql Encoders.unit Decoders.unit False



