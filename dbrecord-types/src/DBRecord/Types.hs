{-# LANGUAGE DeriveGeneric                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}

module DBRecord.Types where

import           Data.Aeson
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

-- citext rexport, uuid reexport




