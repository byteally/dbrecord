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
import DBRecord.Internal.Types
import DBRecord.Internal.Common
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
  | DBInterval
  | DBArray DBTypeK
  | DBNullable DBTypeK
  | DBCustomType Type DBTypeK Bool
  | DBTypeName Symbol

data PGType = PGInt4
            | PGInt8
            | PGInt2
            | PGFloat8
            | PGBool
            -- | PGNumeric Word Word
            | PGChar Integer
            | PGText
            | PGCiText
            | PGDate
            | PGTime
            | PGTimestamp
            | PGTimestamptz
            | PGUuid
            | PGByteArr
            | PGJson
            | PGJsonB
            | PGInterval
            | PGArray PGType
            | PGNullable PGType
            | PGTypeName String
            | PGCustomType PGType Bool
            deriving (Show, Eq)

-- newtype DBType a = DBType a
--                  deriving (Show, Eq)


type family UnliftDBType (db :: DbK) :: Type

type instance UnliftDBType 'Postgres = PGType 

class SingDBType (db :: DbK) (dbTy :: DBTypeK) where
  unliftDBType :: Proxy db -> Proxy dbTy -> UnliftDBType db

instance SingDBType 'Postgres 'DBInt2 where
  unliftDBType _ _ = PGInt2

instance SingDBType 'Postgres 'DBInt4 where
  unliftDBType _ _ = PGInt4

instance SingDBType 'Postgres 'DBInt8 where
  unliftDBType _ _ = PGInt8

instance SingDBType 'Postgres 'DBBool where
  unliftDBType _ _ = PGBool

instance SingDBType 'Postgres 'DBFloat8 where
  unliftDBType _ _ = PGFloat8

instance (KnownNat n) => SingDBType 'Postgres ('DBChar n) where
  unliftDBType _ _ = PGChar (natVal (Proxy @n))

instance SingDBType 'Postgres 'DBText where
  unliftDBType _ _ = PGText

instance SingDBType 'Postgres 'DBByteArr where
  unliftDBType _ _ = PGByteArr

instance SingDBType 'Postgres 'DBTimestamptz where
  unliftDBType _ _ = PGTimestamptz

instance SingDBType 'Postgres 'DBInterval where
  unliftDBType _ _ = PGInterval

instance SingDBType 'Postgres 'DBCiText where
  unliftDBType _ _ = PGCiText

instance SingDBType 'Postgres 'DBTimestamp where
  unliftDBType _ _ = PGTimestamp

instance SingDBType 'Postgres 'DBDate where
  unliftDBType _ _ = PGDate

instance SingDBType 'Postgres 'DBTime where
  unliftDBType _ _ = PGTime

instance SingDBType 'Postgres 'DBUuid where
  unliftDBType _ _ = PGUuid

instance SingDBType 'Postgres 'DBJsonB where
  unliftDBType _ _ = PGJsonB

instance SingDBType 'Postgres 'DBJson where
  unliftDBType _ _ = PGJson

instance KnownSymbol tab => SingDBType 'Postgres ('DBTypeName tab) where
  unliftDBType _ _ = PGTypeName (T.unpack $ T.toUpper $ T.pack $ symbolVal (Proxy :: Proxy tab))

instance SingDBType 'Postgres dbTy => SingDBType 'Postgres ('DBNullable dbTy) where
  unliftDBType dbk _ = PGNullable (unliftDBType dbk (Proxy :: Proxy dbTy))

instance SingDBType 'Postgres dbTy => SingDBType 'Postgres ('DBArray dbTy) where
  unliftDBType dbk _ = PGArray (unliftDBType dbk (Proxy :: Proxy dbTy))

instance (SingDBType 'Postgres (GetDBTypeRep 'Postgres (InnerTy ty))) => SingDBType 'Postgres ('DBCustomType ty dbTy 'True) where
  unliftDBType db _ = PGCustomType (unliftDBType db (Proxy :: Proxy (GetDBTypeRep 'Postgres (InnerTy ty)))) True

instance (SingDBType 'Postgres dbTy, Typeable dbTy) => SingDBType 'Postgres ('DBCustomType ty dbTy 'False) where
  unliftDBType db _ = PGCustomType (unliftDBType db (Proxy :: Proxy dbTy)) False

class ShowDBType (db :: DbK) (dbTy :: DBTypeK) where
  showDBType :: Proxy db -> Proxy dbTy -> T.Text

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

instance ShowDBType 'Postgres 'DBInterval where
  showDBType _ _ = "INTERVAL"

instance ShowDBType 'Postgres 'DBCiText where
  showDBType _ _ = "CITEXT"

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

instance ShowDBType 'Postgres 'DBJson where
  showDBType _ _ = "JSON"

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
  GetPGTypeRep T.Text             = 'DBText
  GetPGTypeRep (CI T.Text)        = 'DBCiText
  GetPGTypeRep ByteString         = 'DBByteArr
  GetPGTypeRep Bool               = 'DBBool
  GetPGTypeRep Day                = 'DBDate
  GetPGTypeRep UTCTime            = 'DBTimestamptz
  GetPGTypeRep LocalTime          = 'DBTimestamp
  GetPGTypeRep TimeOfDay          = 'DBTime
  GetPGTypeRep Value              = 'DBJsonB
  GetPGTypeRep Interval           = 'DBInterval
  GetPGTypeRep (Json a)           = 'DBCustomType (Json a) 'DBJsonB 'False
  GetPGTypeRep (JsonStr a)        = 'DBCustomType (JsonStr a) 'DBJson 'False
  GetPGTypeRep UUID               = 'DBUuid
  GetPGTypeRep (Maybe t)          = 'DBNullable (GetPGTypeRep t)
  -- GetPGTypeRep (Vector t)         = 'DBArray (GetPGTypeRep t)
  GetPGTypeRep [t]                = 'DBArray (GetPGTypeRep t)
  GetPGTypeRep (CustomType a)     = 'DBCustomType (CustomType a) (CustomDBTypeRep 'Postgres a) 'False
  GetPGTypeRep a                  = 'DBCustomType a ('DBTypeName (GetTypeName a)) (IsNewType (Rep a))

type family CustomDBTypeRep (db :: DbK) (ty :: *) :: DBTypeK

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
