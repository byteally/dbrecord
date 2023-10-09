{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE CPP                        #-}

module DBRecord.Types where

import           Data.Aeson
import           Data.Kind
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits
#ifndef ghcjs_HOST_OS
import           Codec.Serialise
#endif

newtype JsonStr a = JsonStr { getJsonStr :: a }
newtype Json a =
  Json { getJson :: a }
  deriving (Eq,Show, Generic)
  deriving newtype (FromJSON, ToJSON)
#ifndef ghcjs_HOST_OS
  deriving newtype (Serialise)
#endif

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
#ifndef ghcjs_HOST_OS
  deriving newtype (Serialise)
#endif
  deriving stock Generic

-- | A Type representing a regclass. See <https://www.postgresql.org/docs/current/datatype-oid.html>
-- newtype RegClass = RegClass { getTypeName :: T.Text }
--                  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype PGOID (t :: PGOIDType) = PGOID_ { getPGOID :: T.Text }
  deriving (Show, Eq, Generic, ToJSON)

mkUnsafePGOID :: T.Text -> PGOID oid
mkUnsafePGOID = PGOID_

data PGOIDType
  = PGOID
  | RegProc
  | RegProcedure
  | RegOper
  | RegOperator
  | RegClass
  | RegType
  | RegRole
  | RegNamespace
  | RegConfig
  | RegDictionary
    deriving (Show, Eq)

type RegClass = PGOID 'RegClass
type RegType = PGOID 'RegType
type RegRole = PGOID 'RegRole
type RegProc = PGOID 'RegProc
type RegProcedure = PGOID 'RegProcedure

instance FromJSON (PGOID 'RegType)

-- citext rexport, uuid reexport

newtype Sized (n :: Nat) t = Sized { getSized :: t }
  deriving newtype (Show, Eq)

class HasSized t where
  toSized :: t -> Sized n t
  fromSized :: Sized n t -> Maybe t
  
-- Postgres

-- Numeric Types
-- Monetary Types
-- Character Types
-- Binary Data Types
-- Date/Time Types
-- Boolean Type
-- Enumerated Types
-- Geometric Types
-- Network Address Types
data INET = INET
data CIDR = CIDR
data MacAddr = MacAddr
data MacAddr8 = MacAddr8
-- Bit String Types
data Bit = Bit
-- Text Search Types
data TSVector = TSVector
data TSQuery = TSQuery
-- UUID Type
-- XML Type
data XML = XML T.Text
-- JSON Types
-- Arrays
-- Composite Types
-- Range Types
data Int4Range = Int4Range -- {lower :: RangeBound Int, upper :: RangeBound Int}

data Int4MultiRange = Int4MultiRange

data NumRange = NumRange

data NumMultiRange = NumMultiRange

data TSRange = TSRange

data TSMultiRange = TSMultiRange

data TSTZRange = TSTZRange

data TSTZMultiRange = TSTZMultiRange

data DateRange = DateRange

data DateMultiRange = DateMultiRange

data RangeBound t
  = Inclusive t
  | Exclusive t
  deriving (Show, Eq)

-- Domain Types
-- Object Identifier Types
-- pg_lsn Type
-- Pseudo-Types
