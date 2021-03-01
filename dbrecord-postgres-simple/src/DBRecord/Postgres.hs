{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module DBRecord.Postgres
       ( module DBRecord.Postgres.Internal.Query
       , module Database.PostgreSQL.Simple
       , module Database.PostgreSQL.Simple.FromField 
       ) where

import Data.Aeson
import DBRecord.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField (..))
import qualified Database.PostgreSQL.Simple.FromField as FF
import Data.Typeable
import DBRecord.Postgres.Internal.Query

instance (FromJSON a, Typeable a) => FromField (Json a) where
  fromField f dat = Json <$> FF.fromJSONField f dat

deriving instance FromField Interval  



{-
instance (ToJSON a, Typeable a) => ToField (Json a) where
  toField = toJSONField . getJson
-}
