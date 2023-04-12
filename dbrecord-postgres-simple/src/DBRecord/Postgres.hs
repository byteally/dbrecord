{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, StandaloneDeriving, DataKinds, UndecidableInstances #-}
module DBRecord.Postgres
       ( module DBRecord.Postgres.Internal.Query
       , module Database.PostgreSQL.Simple
       , module Database.PostgreSQL.Simple.FromField 
       ) where

import qualified Data.Binary.Builder as Bin
import Data.Aeson hiding ((.=))
import DBRecord.Types
import DBRecord.Internal.Types (DbK (Postgres))
import DBRecord.Internal.DBTypes
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField
import qualified Database.PostgreSQL.Simple.FromField as FF
import Data.Typeable
import DBRecord.Postgres.Internal.Query
import Record
import GHC.OverloadedLabels
import GHC.TypeLits

instance (FromJSON a, Typeable a) => FromField (Json a) where
  fromField f dat = do
    Json <$> (FF.fromJSONField f dat)

instance (ToJSON a) => ToField (Json a) where
  toField (Json a) = Plain . Bin.fromLazyByteString . encode $ a

instance FromRow (Rec '[]) where
  fromRow = pure end

instance ( FromRow (Rec xs)
         , KnownSymbol fn
         , Typeable ft
         , FromRow (AnnEntity (ToDBType 'Postgres ft) (AutoCodec 'Postgres ft) ft)
         ) => FromRow (Rec ('(fn, ft) ': xs)) where
  fromRow = do
    hd <- getEntity <$> fromRow @(AnnEntity (ToDBType 'Postgres ft) (AutoCodec 'Postgres ft) ft) -- field @ft
    rst <- fromRow @(Rec xs)
    pure (fromLabel @fn .= hd .& rst)
