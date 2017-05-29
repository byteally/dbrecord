{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, TypeOperators, GADTs, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
module DBRecord.Internal.Types where

import GHC.Generics
import GHC.TypeLits
import GHC.OverloadedLabels
import Data.Aeson

newtype (f :: Symbol) ::: t = Field t
  deriving (Show, Eq, Generic)

instance (fn ~ fn1, s ~ t) => IsLabel fn (s -> (fn1 ::: t)) where
  fromLabel _ = Field

valOf :: (s ::: t) -> t
valOf (Field v) = v

newtype JsonStr a = JsonStr a
newtype Json a = Json a

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
