{-# OPTIONS_GHC -Wno-redundant-constraints -Wall #-}
{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, TypeOperators, GADTs, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, CPP, GeneralizedNewtypeDeriving, DeriveFunctor #-}
module DBRecord.Internal.Types where

import GHC.Generics
import GHC.TypeLits
import GHC.OverloadedLabels
import Data.Aeson
-- import Database.PostgreSQL.Simple.FromField
import Data.Typeable
import qualified Data.Text as T
import qualified DBRecord.Internal.PrimQuery as PQ
import Data.Kind

data DBTag (db :: *) (tab :: *) (v :: k)

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


data DBTypeK
  = DBInt4
  | DBInt8
  | DBInt2
  | DBFloat4
  | DBFloat8
  | DBBool
  | DBNumeric Nat Nat
  | DBChar Nat
  | DBText
  | DBCiText
  | DBDate
  | DBTime
  | DBTimestamp
  | DBTimestamptz
  | DBUuid
  | DBByteArr
  | DBJson
  | DBJsonB
  | DBInterval
  | DBArray DBTypeK
  | DBNullable DBTypeK
  | DBCustomType Type DBTypeK Bool
  | DBTypeName Symbol
    
{-
instance (FromJSON a, Typeable a) => FromField (Json a) where
  fromField f dat = Json <$> fromJSONField f dat
-}

newtype Interval = Interval T.Text
                 deriving (Show, Generic, FromJSON, ToJSON) --, FromField)

newtype Expr (scopes :: [*]) (t :: *) = Expr { getExpr :: PQ.PrimExpr }
                                      deriving Show

unsafeCol :: [T.Text] -> Expr sc a
unsafeCol = Expr . PQ.AttrExpr . sym
  where sym = maybe (error "Panic: Empty col @col_") id . PQ.toSym


-- newtype Only a = Only { fromOnly :: a }
--                deriving (Eq, Ord, Read, Show, Typeable, Functor)
                        
{-
instance (ToJSON a, Typeable a) => ToField (Json a) where
  toField = toJSONField . getJson
-}
