module Hasql.TH.Transactions where

import Hasql.TH.Prelude
import Hasql.Transaction
import qualified Hasql.TH.Queries as Queries
import qualified Data.ByteString


statement :: ByteString -> Transaction ()
statement sql =
  query () (Queries.statement sql)

statements :: ByteString -> Transaction ()
statements =
  traverse_ statement .
  Data.ByteString.split (fromIntegral (ord ';'))
