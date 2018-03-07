{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, TypeOperators, GADTs, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, CPP, GeneralizedNewtypeDeriving, DeriveFunctor, OverloadedStrings #-}
>>>>>>> 4f3ac5e... custom fromfield instance interval
module DBRecord.Internal.Types where

import GHC.Generics
import GHC.TypeLits
import GHC.OverloadedLabels
import Data.Aeson
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Types as PSQL
import Data.Typeable
import qualified Data.Text as T
import qualified DBRecord.Internal.PrimQuery as PQ
import qualified Data.Text.Encoding as T

newtype (f :: Symbol) ::: t = Field t
  deriving (Show, Eq, Generic)

instance (fn ~ fn1, s ~ t) => IsLabel fn (s -> (fn1 ::: t)) where
#if __GLASGOW_HASKELL__ > 800
  fromLabel = Field
#else
  fromLabel _ = Field
#endif

valOf :: (s ::: t) -> t
valOf (Field v) = v

newtype JsonStr a = JsonStr { getJsonStr :: a }
newtype Json a = Json { getJson :: a }
               deriving (Show, Generic, FromJSON, ToJSON)

json :: (ToJSON a) => a -> Json a
json = Json

jsonStr :: (ToJSON a) => a -> JsonStr a
jsonStr = JsonStr

newtype CustomType a = CustomType a

data HList :: (k -> *) -> [k] -> * where
  Nil  :: HList f '[]
  (:&) :: f t -> HList f ts -> HList f (t ': ts)

infixr 7 :&

data DbK = Postgres
         | MySQL
         | SQLite
         | Cassandra
         | Presto

instance (FromJSON a, Typeable a) => FromField (Json a) where
  fromField f dat = Json <$> fromJSONField f dat

newtype Interval = Interval T.Text
                 deriving (Show, Generic, FromJSON, ToJSON)

-- parseOnly val
instance FromField Interval where                                                                                                                    
  fromField f Nothing   = returnError UnexpectedNull f ""
  fromField f (Just val) = do                                                 
    tName <- typename f
    if tName == "interval"
      then case T.decodeUtf8' val  of
        Left err -> returnError ConversionFailed f (show err)
        Right a -> return (Interval a)
      else returnError Incompatible f ("Wrong database type for Interval, saw: " ++ show tName)

{-
instance (ToJSON a, Typeable a) => ToField (Json a) where
  toField = toJSONField . getJson
-}

