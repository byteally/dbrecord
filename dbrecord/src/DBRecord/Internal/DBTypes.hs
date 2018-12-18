{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications, DataKinds, KindSignatures, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, TypeFamilyDependencies, UndecidableInstances, FlexibleInstances, OverloadedStrings #-}
module DBRecord.Internal.DBTypes where

import Data.Aeson
import Data.UUID (UUID)
import Data.Time.LocalTime
import Data.ByteString (ByteString)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.CaseInsensitive  (CI)
import Data.Int
-- import Data.Vector (Vector)
import DBRecord.Internal.Types (DbK (..), Interval, Json, JsonStr, CustomType (..))
import qualified DBRecord.Internal.Types as Type
import DBRecord.Internal.Common
import GHC.TypeLits
import Data.Typeable
import qualified Data.Text as T
import GHC.Generics

data DBType = DBInt4
            | DBInt8
            | DBInt2
            | DBFloat4
            | DBFloat8
            | DBBool
            | DBNumeric Word Word
            | DBChar Integer
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
            | DBArray DBType
            | DBNullable DBType
            | DBTypeName String
            | DBCustomType DBType Bool
            deriving (Show, Eq)

class SingDBType (db :: DbK) (dbTy :: Type.DBTypeK) where
  unliftDBType :: Proxy db -> Proxy dbTy -> DBType

instance SingDBType 'Postgres 'Type.DBInt2 where
  unliftDBType _ _ = DBInt2

instance SingDBType 'Postgres 'Type.DBInt4 where
  unliftDBType _ _ = DBInt4

instance SingDBType 'Postgres 'Type.DBInt8 where
  unliftDBType _ _ = DBInt8

instance SingDBType 'Postgres 'Type.DBBool where
  unliftDBType _ _ = DBBool

instance SingDBType 'Postgres 'Type.DBFloat8 where
  unliftDBType _ _ = DBFloat8

instance (KnownNat n) => SingDBType 'Postgres ('Type.DBChar n) where
  unliftDBType _ _ = DBChar (natVal (Proxy @n))

instance SingDBType 'Postgres 'Type.DBText where
  unliftDBType _ _ = DBText

instance SingDBType 'Postgres 'Type.DBByteArr where
  unliftDBType _ _ = DBByteArr

instance SingDBType 'Postgres 'Type.DBTimestamptz where
  unliftDBType _ _ = DBTimestamptz

instance SingDBType 'Postgres 'Type.DBInterval where
  unliftDBType _ _ = DBInterval

instance SingDBType 'Postgres 'Type.DBCiText where
  unliftDBType _ _ = DBCiText

instance SingDBType 'Postgres 'Type.DBTimestamp where
  unliftDBType _ _ = DBTimestamp

instance SingDBType 'Postgres 'Type.DBDate where
  unliftDBType _ _ = DBDate

instance SingDBType 'Postgres 'Type.DBTime where
  unliftDBType _ _ = DBTime

instance SingDBType 'Postgres 'Type.DBUuid where
  unliftDBType _ _ = DBUuid

instance SingDBType 'Postgres 'Type.DBJsonB where
  unliftDBType _ _ = DBJsonB

instance SingDBType 'Postgres 'Type.DBJson where
  unliftDBType _ _ = DBJson

instance KnownSymbol tab => SingDBType 'Postgres ('Type.DBTypeName tab) where
  unliftDBType _ _ = DBTypeName (T.unpack $ T.toUpper $ T.pack $ symbolVal (Proxy :: Proxy tab))

instance SingDBType 'Postgres dbTy => SingDBType 'Postgres ('Type.DBNullable dbTy) where
  unliftDBType dbk _ = DBNullable (unliftDBType dbk (Proxy :: Proxy dbTy))

instance SingDBType 'Postgres dbTy => SingDBType 'Postgres ('Type.DBArray dbTy) where
  unliftDBType dbk _ = DBArray (unliftDBType dbk (Proxy :: Proxy dbTy))

instance (SingDBType 'Postgres (GetDBTypeRep 'Postgres (InnerTy ty))) => SingDBType 'Postgres ('Type.DBCustomType ty dbTy 'True) where
  unliftDBType db _ = DBCustomType (unliftDBType db (Proxy :: Proxy (GetDBTypeRep 'Postgres (InnerTy ty)))) True

instance (SingDBType 'Postgres dbTy, Typeable dbTy) => SingDBType 'Postgres ('Type.DBCustomType ty dbTy 'False) where
  unliftDBType db _ = DBCustomType (unliftDBType db (Proxy :: Proxy dbTy)) False

class ShowDBType (db :: DbK) (dbTy :: Type.DBTypeK) where
  showDBType :: Proxy db -> Proxy dbTy -> T.Text

instance ShowDBType 'Postgres 'Type.DBInt2 where
  showDBType _ _ = "SMALLINT"

instance ShowDBType 'Postgres 'Type.DBInt4 where
  showDBType _ _ = "INTEGER"

instance ShowDBType 'Postgres 'Type.DBInt8 where
  showDBType _ _ = "BIGINT"

instance ShowDBType 'Postgres 'Type.DBBool where
  showDBType _ _ = "BOOLEAN"

instance ShowDBType 'Postgres 'Type.DBFloat8 where
  showDBType _ _ = "DOUBLE PRECISION"

instance KnownNat n => ShowDBType 'Postgres ('Type.DBChar n) where
  showDBType _ _ = T.pack $ "CHARACTER (" ++ (show $ natVal $ Proxy @n) ++ ")"

instance ShowDBType 'Postgres 'Type.DBText where
  showDBType _ _ = "TEXT"

instance ShowDBType 'Postgres 'Type.DBByteArr where
  showDBType _ _ = "BYTEA"

instance ShowDBType 'Postgres 'Type.DBTimestamptz where
  showDBType _ _ = "TIMESTAMPTZ"

instance ShowDBType 'Postgres 'Type.DBInterval where
  showDBType _ _ = "INTERVAL"

instance ShowDBType 'Postgres 'Type.DBCiText where
  showDBType _ _ = "CITEXT"

instance ShowDBType 'Postgres 'Type.DBTimestamp where
  showDBType _ _ = "TIMESTAMP"

instance ShowDBType 'Postgres 'Type.DBDate where
  showDBType _ _ = "DATE"

instance ShowDBType 'Postgres 'Type.DBTime where
  showDBType _ _ = "TIME"

instance ShowDBType 'Postgres 'Type.DBUuid where
  showDBType _ _ = "UUID"

instance ShowDBType 'Postgres 'Type.DBJsonB where
  showDBType _ _ = "JSONB"

instance ShowDBType 'Postgres 'Type.DBJson where
  showDBType _ _ = "JSON"

instance ShowDBType 'Postgres dbTy => ShowDBType 'Postgres ('Type.DBNullable dbTy) where
  showDBType db _ = showDBType db (Proxy :: Proxy dbTy)

instance ShowDBType 'Postgres dbTy => ShowDBType 'Postgres ('Type.DBArray dbTy) where
  showDBType db _ = showDBType db (Proxy :: Proxy dbTy) `T.append` "[]"

instance KnownSymbol tab => ShowDBType 'Postgres ('Type.DBTypeName tab) where
  showDBType _ _ = T.toUpper $ T.pack $ symbolVal (Proxy :: Proxy tab)

instance (ShowDBType 'Postgres (GetDBTypeRep 'Postgres (InnerTy ty))) => ShowDBType 'Postgres ('Type.DBCustomType ty dbTy 'True) where
  showDBType db _ = showDBType db (Proxy :: Proxy (GetDBTypeRep 'Postgres (InnerTy ty)))

instance (ShowDBType 'Postgres dbTy, Typeable dbTy) => ShowDBType 'Postgres ('Type.DBCustomType ty dbTy 'False) where
  showDBType db _ = if typeRepTyCon (typeRep (Proxy @dbTy)) == typeRepTyCon (typeRep (Proxy @ 'DBTypeName))
                    then doubleQuote $ showDBType db (Proxy :: Proxy dbTy)
                    else showDBType db (Proxy :: Proxy dbTy)

type family GetDBTypeRep db t where
  GetDBTypeRep 'Postgres t = GetPGTypeRep t

type family GetPGTypeRep (t :: *) = (r :: Type.DBTypeK) | r -> t where
  GetPGTypeRep Int                = 'Type.DBInt4
  GetPGTypeRep Int16              = 'Type.DBInt2
  GetPGTypeRep Int64              = 'Type.DBInt8
  GetPGTypeRep Double             = 'Type.DBFloat8
  GetPGTypeRep Char               = 'Type.DBChar 1
  GetPGTypeRep T.Text             = 'Type.DBText
  GetPGTypeRep (CI T.Text)        = 'Type.DBCiText
  GetPGTypeRep ByteString         = 'Type.DBByteArr
  GetPGTypeRep Bool               = 'Type.DBBool
  GetPGTypeRep Day                = 'Type.DBDate
  GetPGTypeRep UTCTime            = 'Type.DBTimestamptz
  GetPGTypeRep LocalTime          = 'Type.DBTimestamp
  GetPGTypeRep TimeOfDay          = 'Type.DBTime
  GetPGTypeRep Value              = 'Type.DBJsonB
  GetPGTypeRep Interval           = 'Type.DBInterval
  GetPGTypeRep (Json a)           = 'Type.DBCustomType (Json a) 'Type.DBJsonB 'False
  GetPGTypeRep (JsonStr a)        = 'Type.DBCustomType (JsonStr a) 'Type.DBJson 'False
  GetPGTypeRep UUID               = 'Type.DBUuid
  GetPGTypeRep (Maybe t)          = 'Type.DBNullable (GetPGTypeRep t)
  -- GetPGTypeRep (Vector t)         = 'DBArray (GetPGTypeRep t)
  GetPGTypeRep [t]                = 'Type.DBArray (GetPGTypeRep t)
  GetPGTypeRep (CustomType a)     = 'Type.DBCustomType (CustomType a) (CustomDBTypeRep 'Postgres a) 'False
  GetPGTypeRep a                  = 'Type.DBCustomType a ('Type.DBTypeName (GetTypeName a)) (IsNewType (Rep a))

type family CustomDBTypeRep (db :: DbK) (ty :: *) :: Type.DBTypeK

doubleQuote :: T.Text -> T.Text
doubleQuote = quoteBy '"' (Just '"')

quoteBy :: Char -> Maybe Char -> T.Text -> T.Text
quoteBy ch esc s = T.pack $ ch : go esc (T.unpack s) ++ (ch:[])
  where
    go Nothing s'           = s'
    go (Just _) ""          = ""
    go (Just esch) (ch':xs)
      | ch' == esch          = esch : ch': go esc xs
    go esc' (x:xs)          = x : go esc' xs

class ( Generic ty
      ) => UDType (db :: *) (ty :: *) where
  type TypeMappings db ty :: UDTypeMappings
  -- type TypeMappings db ty = 'Flat '[]

-- TODO: Support other type mappings as well
data UDTypeMappings = EnumType Symbol [Symbol]
{-
                    | Composite Symbol [(Symbol, DBTypeK)]
                    | Flat [(Symbol, DBTypeK)]
                    | EnumText [Symbol]
                    | Sum [(Symbol, [(Symbol, DBTypeK)])]
-}

toNullable :: DBType -> DBType
toNullable = DBNullable

removeNullable :: DBType -> DBType
removeNullable (DBNullable t) = t
removeNullable _ = error "Panic: Remove nullable failed"

isNullable :: DBType -> Bool
isNullable (DBNullable _) = True
isNullable _              = False

enumType :: String -> DBType
enumType v = DBCustomType (DBTypeName v) False
