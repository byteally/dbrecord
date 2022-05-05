{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module DBRecord.Postgres
       ( module DBRecord.Postgres.Internal.Query
       , module Database.PostgreSQL.Simple
       , module Database.PostgreSQL.Simple.FromField 
       ) where

import qualified Data.Binary.Builder as Bin
import Data.Aeson
import DBRecord.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField
import qualified Database.PostgreSQL.Simple.FromField as FF
import Data.Typeable
import DBRecord.Postgres.Internal.Query

instance (FromJSON a, Typeable a) => FromField (Json a) where
  fromField f dat = do
    Json <$> (FF.fromJSONField f dat)

instance (ToJSON a) => ToField (Json a) where
  toField (Json a) = Plain . Bin.fromLazyByteString . encode $ a
