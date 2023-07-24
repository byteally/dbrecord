{-# LANGUAGE OverloadedStrings          #-}
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
import qualified Data.List as L
import qualified Data.Text as T
import           Data.Tuple (swap)
import           GHC.Generics

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

escapeSequences :: [(T.Text, T.Text)]
escapeSequences =
  [ ("-", "_h")
  , ("_", "__")
  ]

escape :: [T.Text] -> [T.Text]
escape =
  fmap replacements

  where
    replacements t =
      L.foldr (\x t0 -> (uncurry T.replace) x t0) t escapeSequences      

unescape :: [T.Text] -> [T.Text]
unescape =
  fmap replacements

  where
    replacements t =
      L.foldr (\x t0 -> (uncurry T.replace) (swap x) t0) t escapeSequences

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
