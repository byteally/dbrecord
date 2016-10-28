{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, TypeOperators, GADTs, DeriveGeneric #-}
module Database.Internal.Types where

import GHC.Generics
import GHC.TypeLits
import Data.Aeson

newtype (f :: Symbol) ::: t = Field t
  deriving (Show, Eq, Generic)

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
