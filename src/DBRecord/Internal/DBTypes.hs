{-# LANGUAGE TypeApplications, DataKinds, KindSignatures, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, TypeFamilyDependencies, UndecidableInstances, FlexibleInstances, OverloadedStrings #-}
module DBRecord.Internal.DBTypes where

import Data.Aeson
import Data.UUID.Types
import Data.Time.LocalTime
import Data.ByteString (ByteString)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.CaseInsensitive  (CI)
import Data.Int
import Data.Vector (Vector)
import DBRecord.Internal.Types
import DBRecord.Internal.Common
import Data.Text
import GHC.TypeLits
import Data.Typeable
import Data.Kind
import qualified Data.Text as T
import GHC.Generics

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
  | DBArray DBTypeK
  | DBNullable DBTypeK
  | DBCustomType Type DBTypeK Bool
  | DBTypeName Symbol

class ShowDBType (db :: DbK) (dbTy :: DBTypeK) where
  showDBType :: Proxy db -> Proxy dbTy -> Text

instance ShowDBType 'Postgres 'DBInt2 where
  showDBType _ _ = "SMALLINT"

instance ShowDBType 'Postgres 'DBInt4 where
  showDBType _ _ = "INTEGER"

instance ShowDBType 'Postgres 'DBInt8 where
  showDBType _ _ = "BIGINT"

instance ShowDBType 'Postgres 'DBBool where
  showDBType _ _ = "BOOLEAN"

instance ShowDBType 'Postgres 'DBFloat8 where
  showDBType _ _ = "DOUBLE PRECISION"

instance KnownNat n => ShowDBType 'Postgres ('DBChar n) where
  showDBType _ _ = T.pack $ "CHARACTER (" ++ (show $ natVal $ Proxy @n) ++ ")"

instance ShowDBType 'Postgres 'DBText where
  showDBType _ _ = "TEXT"

instance ShowDBType 'Postgres 'DBByteArr where
  showDBType _ _ = "BYTEA"

instance ShowDBType 'Postgres 'DBTimestamptz where
  showDBType _ _ = "TIMESTAMPTZ"

instance ShowDBType 'Postgres 'DBTimestamp where
  showDBType _ _ = "TIMESTAMP"

instance ShowDBType 'Postgres 'DBDate where
  showDBType _ _ = "DATE"

instance ShowDBType 'Postgres 'DBTime where
  showDBType _ _ = "TIME"

instance ShowDBType 'Postgres 'DBUuid where
  showDBType _ _ = "UUID"

instance ShowDBType 'Postgres 'DBJsonB where
  showDBType _ _ = "JSONB"

instance ShowDBType 'Postgres dbTy => ShowDBType 'Postgres ('DBNullable dbTy) where
  showDBType db _ = showDBType db (Proxy :: Proxy dbTy)

instance ShowDBType 'Postgres dbTy => ShowDBType 'Postgres ('DBArray dbTy) where
  showDBType db _ = showDBType db (Proxy :: Proxy dbTy) `T.append` "[]"

instance KnownSymbol tab => ShowDBType 'Postgres ('DBTypeName tab) where
  showDBType _ _ = T.toUpper $ T.pack $ symbolVal (Proxy :: Proxy tab)

instance (ShowDBType 'Postgres (GetDBTypeRep 'Postgres (InnerTy ty))) => ShowDBType 'Postgres ('DBCustomType ty dbTy 'True) where
  showDBType db _ = showDBType db (Proxy :: Proxy (GetDBTypeRep 'Postgres (InnerTy ty)))

instance (ShowDBType 'Postgres dbTy, Typeable dbTy) => ShowDBType 'Postgres ('DBCustomType ty dbTy 'False) where
  showDBType db _ = if typeRepTyCon (typeRep (Proxy @dbTy)) == typeRepTyCon (typeRep (Proxy @ 'DBTypeName))
                    then doubleQuote $ showDBType db (Proxy :: Proxy dbTy)
                    else showDBType db (Proxy :: Proxy dbTy)

type family GetDBTypeRep db t where
  GetDBTypeRep 'Postgres t = GetPGTypeRep t

type family GetPGTypeRep (t :: *) = (r :: DBTypeK) | r -> t where
  GetPGTypeRep Int                = 'DBInt4
  GetPGTypeRep Int16              = 'DBInt2
  GetPGTypeRep Int64              = 'DBInt8
  GetPGTypeRep Double             = 'DBFloat8
  GetPGTypeRep Char               = 'DBChar 1
  GetPGTypeRep Text               = 'DBText
  GetPGTypeRep (CI Text)          = 'DBCiText
  GetPGTypeRep ByteString         = 'DBByteArr
  GetPGTypeRep Bool               = 'DBBool
  GetPGTypeRep Day                = 'DBDate
  GetPGTypeRep UTCTime            = 'DBTimestamptz
  GetPGTypeRep LocalTime          = 'DBTimestamp
  GetPGTypeRep TimeOfDay          = 'DBTime
  GetPGTypeRep Value              = 'DBJsonB
  GetPGTypeRep (Json a)           = 'DBCustomType (Json a) 'DBJsonB 'False
  GetPGTypeRep (JsonStr a)        = 'DBCustomType (JsonStr a) 'DBJson 'False
  GetPGTypeRep UUID               = 'DBUuid
  GetPGTypeRep (Maybe t)          = 'DBNullable (GetPGTypeRep t)
  GetPGTypeRep (Vector t)         = 'DBArray (GetPGTypeRep t)
  GetPGTypeRep (CustomType a)     = 'DBCustomType (CustomType a) (CustomDBTypeRep 'Postgres a) 'False
  GetPGTypeRep a                  = 'DBCustomType a ('DBTypeName (GetTypeName a)) (IsNewType (Rep a))

type family CustomDBTypeRep (db :: DbK) (ty :: *) :: DBTypeK

doubleQuote :: Text -> Text
doubleQuote = quoteBy '"' (Just '"')

quoteBy :: Char -> Maybe Char -> Text -> Text
quoteBy ch esc s = T.pack $ ch : go esc (T.unpack s) ++ (ch:[])
  where
    go Nothing s'           = s'
    go (Just _) ""          = ""
    go (Just esch) (ch':xs)
      | ch' == esch          = esch : ch': go esc xs
    go esc' (x:xs)          = x : go esc' xs
