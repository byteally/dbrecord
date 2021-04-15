{-# LANGUAGE DeriveGeneric                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE DerivingStrategies           #-}
{-# LANGUAGE PolyKinds                    #-}

module DBRecord.Types where

import           Data.Aeson
import           Data.Kind
import qualified Data.Text as T
import           GHC.Generics

newtype JsonStr a = JsonStr { getJsonStr :: a }
newtype Json a = Json { getJson :: a }
               deriving (Show, Generic, FromJSON, ToJSON)

json :: (ToJSON a) => a -> Json a
json = Json

jsonStr :: (ToJSON a) => a -> JsonStr a
jsonStr = JsonStr

-- PG specific
newtype Interval = Interval T.Text
                 deriving (Show, Generic, FromJSON, ToJSON)

newtype LTree = LTree [T.Text]
              deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype Key (t :: k) (v :: Type) = Key {getKey :: v}
  deriving newtype (Show, Read, Eq, Ord, ToJSON, FromJSON)
  deriving stock Generic

-- | A Type representing a regclass. See <https://www.postgresql.org/docs/current/datatype-oid.html>
newtype RegClass = RegClass { getTypeName :: T.Text }
                 deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- citext rexport, uuid reexport
