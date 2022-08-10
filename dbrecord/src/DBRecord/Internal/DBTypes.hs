{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications, DataKinds, KindSignatures, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, TypeFamilyDependencies, UndecidableInstances, FlexibleInstances, OverloadedStrings, GADTs, TypeOperators, FlexibleContexts #-}
module DBRecord.Internal.DBTypes where

import Data.Aeson
import Data.UUID (UUID)
import Data.Time.LocalTime
import Data.ByteString (ByteString)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.CaseInsensitive  (CI)
import Data.Int
import Data.Word
import DBRecord.Types (Interval, Json {-, JsonStr,-} )

-- import Data.Vector (Vector)
import DBRecord.Internal.Types (DbK (..), CustomType (..))
import qualified DBRecord.Internal.Types as Type
import DBRecord.Internal.Types (Sing (..), SingE (..))
import DBRecord.Internal.Common
import qualified Data.Text as T
import GHC.Generics
import Data.Kind
import GHC.TypeLits
import qualified Path as Path

data DBType = DBInt4
            | DBInt8
            | DBInt2
            | DBFloat   Integer
            | DBNumeric Integer Integer
            | DBChar Integer
            | DBVarchar (Either Type.Max Integer)
            | DBBool 
            | DBDate
            | DBTime Integer
            | DBTimetz Integer
            | DBTimestamp Integer
            | DBTimestamptz Integer
            | DBInterval (Maybe ()) Integer
            | DBNullable DBType
            | DBXml
            | DBJson
            | DBBinary Integer
            | DBVarbinary (Either Type.Max Integer)
            | DBText
            | DBCiText              
            | DBUuid
            | DBBit    Integer
            | DBVarbit Integer
            | DBJsonB
            | DBArray DBType
            | DBLTree
            | OtherBuiltInType DBTypeName
            | DBCustomType
                T.Text -- Schema name
                DBTypeName
            deriving (Show, Eq, Ord, Read)

data DBTypeName = DBTypeName T.Text [TypeArg]
                deriving (Show, Eq, Ord, Read)

data TypeArg = TextArg    T.Text
             | IntegerArg Integer
             deriving (Show, Eq, Ord, Read)

type family DBTypeCtx (t :: Type.DBTypeK) :: Constraint where
  DBTypeCtx ('Type.DBFloat v)             = SingE v
  DBTypeCtx ('Type.DBNumeric v1 v2)       = (SingE v1, SingE v2)
  DBTypeCtx ('Type.DBChar v)              = SingE v
  DBTypeCtx ('Type.DBVarchar v)           = Type.EitherCtx SingE SingE v
  DBTypeCtx ('Type.DBTime v)              = SingE v
  DBTypeCtx ('Type.DBTimetz v)            = SingE v
  DBTypeCtx ('Type.DBTimestamp v)         = SingE v
  DBTypeCtx ('Type.DBTimestamptz v)       = SingE v
  DBTypeCtx ('Type.DBInterval _ v)        = SingE v
  DBTypeCtx ('Type.DBNullable v)          = SingE v
  DBTypeCtx ('Type.DBBinary v)            = SingE v
  DBTypeCtx ('Type.DBVarbinary v)         = Type.EitherCtx SingE SingE v
  DBTypeCtx ('Type.DBBit v)               = SingE v
  DBTypeCtx ('Type.DBVarbit v)            = SingE v
  DBTypeCtx ('Type.DBArray v)             = SingE v
  DBTypeCtx ('Type.DBCustomType _ _ dbt ) = SingE dbt
  DBTypeCtx _                             = ()
  
instance (DBTypeCtx t) => SingE (t :: Type.DBTypeK) where
  type Demote t = DBType
  
  fromSing SDBInt4                 = DBInt4
  fromSing SDBInt8                 = DBInt8
  fromSing SDBInt2                 = DBInt2
  fromSing (SDBFloat v)            = DBFloat (fromSing v)
  fromSing (SDBNumeric n1 n2)      = DBNumeric (fromSing n1) (fromSing n2)
  fromSing (SDBChar n)             = DBChar (fromSing n)
  fromSing (SDBVarchar n)          = DBVarchar (fromSing n)
  fromSing SDBBool                 = DBBool
  fromSing SDBDate                 = DBDate
  fromSing (SDBTime n)             = DBTime (fromSing n)
  fromSing (SDBTimetz n)           = DBTimetz (fromSing n)
  fromSing (SDBTimestamp n)        = DBTimestamp (fromSing n)
  fromSing (SDBTimestamptz n)      = DBTimestamptz (fromSing n)
  fromSing (SDBInterval _ n2)      = DBInterval Nothing (fromSing n2)
  fromSing (SDBNullable n)         = DBNullable (fromSing n)
  fromSing SDBXml                  = DBXml
  fromSing (SDBBinary n)           = DBBinary (fromSing n)
  fromSing (SDBVarbinary n)        = DBVarbinary (fromSing n)
  fromSing SDBText                 = DBText
  fromSing SDBCiText               = DBCiText
  fromSing SDBUuid                 = DBUuid
  fromSing (SDBBit n)              = DBBit (fromSing n)
  fromSing (SDBVarbit n)           = DBVarbit (fromSing n)
  fromSing SDBJson                 = DBJson  
  fromSing SDBJsonB                = DBJsonB
  fromSing (SDBArray a)            = DBArray (fromSing a)
  fromSing SDBLTree                = DBLTree
  fromSing (SDBCustomType sc _ t ) = DBCustomType (fromSing sc) (fromSing t)

type family DBTypeNameKCtx (typn :: Type.DBTypeNameK) where
  DBTypeNameKCtx ('Type.DBTypeName s args udm) = (SingE s, SingE args, Type.UDTCtx udm)

instance (DBTypeNameKCtx typn) => SingE (typn :: Type.DBTypeNameK) where
  type Demote typn = DBTypeName
  fromSing (SDBTypeName s args udm) = DBTypeName alias (fromSing args)
    where alias = case fromSing udm of
            Type.EnumTypeNM (Just a) _ -> a
            Type.CompositeNM (Just a) _ -> a
            _ -> fromSing s

type family TypeArgCtx (t :: Type.TypeArgK) where
  TypeArgCtx ('Type.SymArg sym) = SingE sym
  TypeArgCtx ('Type.NatArg n)   = SingE n

instance ( TypeArgCtx t
         ) => SingE (t :: Type.TypeArgK) where
  type Demote (t :: Type.TypeArgK) = TypeArg

  fromSing (SSymArg s) = TextArg (fromSing s)
  fromSing (SNatArg s) = IntegerArg (fromSing s)

{-
class SingDBType (db :: DbK) (dbTy :: Type.DBTypeK) where
  unliftDBType :: Proxy db -> Proxy dbTy -> DBType

instance SingDBType 'Postgres 'Type.DBInt2 where
  unliftDBType _ _ = DBInt2

instance SingDBType 'Postgres 'Type.DBInt4 where
  unliftDBType _ _ = DBInt4

instance SingDBType 'Postgres 'Type.DBInt8 where
  unliftDBType _ _ = DBInt8

instance (KnownNat n) => SingDBType 'Postgres ('Type.DBFloat n) where
  unliftDBType _ _ = DBFloat (fromInteger (natVal (Proxy :: Proxy n)))

instance SingDBType 'Postgres 'Type.DBBool where
  unliftDBType _ _ = DBBool

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
  unliftDBType db _ = DBNullable (unliftDBType db (Proxy :: Proxy dbTy))

instance SingDBType 'Postgres dbTy => SingDBType 'Postgres ('Type.DBArray dbTy) where
  unliftDBType db _ = DBArray (unliftDBType db (Proxy :: Proxy dbTy))

instance (SingDBType 'Postgres (GetDBTypeRep 'Postgres (InnerTy ty))) => SingDBType 'Postgres ('Type.DBCustomType ty dbTy 'True) where
  unliftDBType db _ = DBCustomType (unliftDBType db (Proxy :: Proxy (GetDBTypeRep Postgres (InnerTy ty)))) True

instance (SingDBType 'Postgres dbTy, Typeable dbTy) => SingDBType 'Postgres ('Type.DBCustomType ty dbTy 'False) where
  unliftDBType db _ = DBCustomType (unliftDBType db (Proxy :: Proxy dbTy)) False
-}


{-
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

instance ShowDBType 'Postgres 'Type.DBFloat  where
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
-}

type family GetDBTypeRep' sc dbk t where
  GetDBTypeRep' sc 'Postgres t = GetPGTypeRep sc t
  GetDBTypeRep' sc 'MSSQL    t = GetMSSQLTypeRep sc t

type family GetMSSQLTypeRep (sc :: Type) (t :: Type) = (r :: Type.DBTypeK) {-| r -> t-} where
  GetMSSQLTypeRep _ Int                = 'Type.DBInt8
  GetMSSQLTypeRep _ Int8               = 'Type.DBNumeric 3 0
  GetMSSQLTypeRep _ Int16              = 'Type.DBInt2
  GetMSSQLTypeRep _ Int32              = 'Type.DBInt4
  GetMSSQLTypeRep _ Int64              = 'Type.DBInt8
  GetMSSQLTypeRep _ Word               = 'Type.DBNumeric 20 0
  GetMSSQLTypeRep _ Word8              = 'Type.DBNumeric 3 0
  GetMSSQLTypeRep _ Word16             = 'Type.DBNumeric 5 0
  GetMSSQLTypeRep _ Word32             = 'Type.DBNumeric 10 0
  GetMSSQLTypeRep _ Word64             = 'Type.DBNumeric 20 0
  GetMSSQLTypeRep _ Float              = 'Type.DBFloat 24
  GetMSSQLTypeRep _ Double             = 'Type.DBFloat 53
  GetMSSQLTypeRep _ Char               = 'Type.DBChar 1
  GetMSSQLTypeRep _ T.Text             = 'Type.DBText
  GetMSSQLTypeRep _ ByteString         = 'Type.DBVarbinary ('Left 'Type.Max)
  GetMSSQLTypeRep _ Bool               = 'Type.DBBit 1
  GetMSSQLTypeRep _ Day                = 'Type.DBDate
  GetMSSQLTypeRep _ UTCTime            = 'Type.DBTimestamptz 7
  GetMSSQLTypeRep _ LocalTime          = 'Type.DBTimestamp 7
  GetMSSQLTypeRep _ TimeOfDay          = 'Type.DBTime 7
  GetMSSQLTypeRep sc (Maybe t)         = 'Type.DBNullable (GetMSSQLTypeRep sc t)
  GetMSSQLTypeRep sc [t]               = 'Type.DBArray (GetMSSQLTypeRep sc t)
  GetMSSQLTypeRep sc (CustomType a)    = CustomDBTypeRep sc a
  GetMSSQLTypeRep sc a                 =
    GetMSSQLTypeRepCustom sc a (NewtypeRep a)

type family GetMSSQLTypeRepCustom (sc :: Type) (ot :: Type) (t :: Maybe Type) where
  GetMSSQLTypeRepCustom sc a 'Nothing =
    'Type.DBCustomType (SchemaName sc) a ('Type.DBTypeName (GetTypeName a) '[] (TypeMappings sc a))
  GetMSSQLTypeRepCustom sc _ ('Just a) =
    GetMSSQLTypeRep sc a

type family GetPGTypeRep (sc :: Type) (t :: Type) = (r :: Type.DBTypeK) {-| r -> t-} where
  GetPGTypeRep _ Int                = 'Type.DBInt4
  GetPGTypeRep _ Int16              = 'Type.DBInt2
  GetPGTypeRep _ Int32              = 'Type.DBInt4
  GetPGTypeRep _ Int64              = 'Type.DBInt8
  GetPGTypeRep _ Float              = 'Type.DBFloat 24
  GetPGTypeRep _ Double             = 'Type.DBFloat 53
  GetPGTypeRep _ Char               = 'Type.DBChar 1
  GetPGTypeRep _ T.Text             = 'Type.DBText
  GetPGTypeRep _ (CI T.Text)        = 'Type.DBCiText
  GetPGTypeRep _ ByteString         = 'Type.DBVarbinary ('Left 'Type.Max)
  GetPGTypeRep _ Bool               = 'Type.DBBool
  GetPGTypeRep _ Day                = 'Type.DBDate
  GetPGTypeRep _ UTCTime            = 'Type.DBTimestamptz 6
  GetPGTypeRep _ LocalTime          = 'Type.DBTimestamp 6
  GetPGTypeRep _ TimeOfDay          = 'Type.DBTime 6
  GetPGTypeRep _ Value              = 'Type.DBJsonB
  GetPGTypeRep _ Interval           = 'Type.DBInterval 'Nothing 6
  GetPGTypeRep _ (Path.Path a ft)   = 'Type.DBText
  GetPGTypeRep _ (Json a)           = 'Type.DBJsonB
  -- GetPGTypeRep sc (Json a)          = 'Type.DBCustomType (Json a) 'Type.DBJsonB (TypeMappings sc (Json a))
  -- GetPGTypeRep sc (JsonStr a)       = 'Type.DBCustomType (JsonStr a) 'Type.DBJson (TypeMappings sc (JsonStr a))
  GetPGTypeRep _ UUID               = 'Type.DBUuid
  GetPGTypeRep sc (Maybe t)         = 'Type.DBNullable (GetPGTypeRep sc t)
  -- GetPGTypeRep (Vector t)         = 'DBArray (GetPGTypeRep t)
  GetPGTypeRep sc [t]               = 'Type.DBArray (GetPGTypeRep sc t)
  GetPGTypeRep sc (CustomType a)    = CustomDBTypeRep sc a
  GetPGTypeRep sc a                 =
    GetPGTypeRepCustom sc a (NewtypeRep a)

type family GetPGTypeRepCustom (sc :: Type) (ot :: Type) (t :: Maybe Type) :: Type.DBTypeK where
  GetPGTypeRepCustom sc a 'Nothing =
    'Type.DBCustomType (SchemaName sc) a ('Type.DBTypeName (GetTypeName a) '[] (TypeMappings sc a)) 
  GetPGTypeRepCustom sc _ ('Just a) =
    GetPGTypeRep sc a

type family CustomDBTypeRep (sc :: Type) (ty :: Type) :: Type.DBTypeK

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
      ) => UDType (sc :: Type) (ty :: Type) where
  type TypeMappings sc ty :: Type.UDTypeMappings
  type TypeMappings sc ty = GTypeMappings sc (Rep ty)

type family GTypeMappings sc rep where
  GTypeMappings sc (D1 _ g)    = GTypeMappings sc g
  GTypeMappings sc (g1 :+: g2) =
    GTypeMappingsSum sc (GTypeMappingsSum' (g1 :+: g2))
  GTypeMappings sc (C1 _ g)    = 'Type.Flat '[]

type family GTypeMappingsSum sc (isEnum :: Bool) where
  GTypeMappingsSum sc 'True  = 'Type.EnumType 'Nothing '[]
  GTypeMappingsSum sc 'False = 'Type.Sum 'Nothing '[]  

type family GTypeMappingsSum' rep where
  GTypeMappingsSum' (g1 :+: g2)  = GTypeMappingsSum' g1 && GTypeMappingsSum' g2
  GTypeMappingsSum' (C1 _ U1)    = 'True
  GTypeMappingsSum' (C1 _ _)     = 'False


class ( -- Break (NoGeneric db) (Rep db)
      -- TypeCxts db (Types db)
      ) => Database (db :: Type) where
  type DB db :: DbK
  type DB db = TypeError ('Text "DB type is not configured in the Database instance for type " ':<>: 'ShowType db ':$$:
                          'Text "Hint: add following to the Database instance for type "       ':<>: 'ShowType db ':$$:
                          'Text "type DB " ':<>: 'ShowType db ':<>: 'Text " = " ':<>: 'ShowType 'Postgres
                         )
  type DatabaseName db :: Symbol

class ( -- TypeCxts db (Types db)
        Database (SchemaDB sc)
      ) => Schema (sc :: Type) where
  type SchemaName sc :: Symbol
  type SchemaName sc = "public"
  
  type Tables sc :: [Type]
  
  type Types sc :: [Type]
  type Types sc = '[]

  type TabIgnore sc :: [Type]
  type TabIgnore sc = '[]
  
  type Baseline sc :: Nat
  type Baseline sc = 0
  
  type Version sc :: Nat
  type Version sc = 0

  type SchemaDB sc :: Type

toNullable :: DBType -> DBType
toNullable = DBNullable

removeNullable :: DBType -> DBType
removeNullable (DBNullable t) = t
removeNullable _ = error "Panic: Remove nullable failed"

isNullable :: DBType -> Bool
isNullable (DBNullable _) = True
isNullable _              = False

-- enumType :: T.Text -> DBType
-- enumType v = DBCustomType (DBTypeName v []) False

-- NOTE: newtype handling.

type TPair (a :: Symbol) (b :: Symbol) = '(a, b)

