{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module DBRecord.Postgres where

import Data.Aeson
import Database.PostgreSQL.Simple.FromField
import Data.Typeable
import DBRecord.Internal.Types


instance (FromJSON a, Typeable a) => FromField (Json a) where
  fromField f dat = Json <$> fromJSONField f dat

deriving instance FromField Interval  



{-
instance (ToJSON a, Typeable a) => ToField (Json a) where
  toField = toJSONField . getJson
-}
