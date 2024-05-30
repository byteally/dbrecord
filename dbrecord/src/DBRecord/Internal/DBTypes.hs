{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications, DataKinds, KindSignatures, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, TypeFamilyDependencies, UndecidableInstances, FlexibleInstances, OverloadedStrings, GADTs, TypeOperators, FlexibleContexts, DefaultSignatures, DerivingStrategies #-}
module DBRecord.Internal.DBTypes where

import Data.Aeson
import Data.UUID (UUID)
import Data.Time.LocalTime
import Data.ByteString (ByteString)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.CaseInsensitive  (CI)
import Data.Int
import Data.Functor.Identity
import Data.Functor.Const
-- import Data.Word
import Data.Scientific
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Proxy
import DBRecord.Types (PGOID(..), PGOIDType(..), LTree{-, Interval-}, Json {-, JsonStr,-})
import qualified DBRecord.Types as DBR

import Data.Vector (Vector)
import DBRecord.Internal.Types (DbK (..))
import qualified DBRecord.Internal.Types as Type
-- import DBRecord.Internal.Types (Sing (..), SingE (..))
-- import DBRecord.Internal.Common
import qualified Data.Text as T
import GHC.Generics
import Data.Kind
import Data.String
import GHC.Records
import GHC.TypeLits
-- import GHC.Exts
-- import Data.Type.Bool
--import Data.Type.Equality
--import Data.Typeable
-- import qualified Path as Path
import Record
import Record.Setter

-- TODO: Very similar to DBTypeK! Try to unify.
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
  
instance Type.SingE 'Type.DBInt4 where
  type Demote 'Type.DBInt4 = DBType
  fromSing Type.SDBInt4 = undefined
  
--   fromSing SDBInt4                 = DBInt4
--   fromSing SDBInt8                 = DBInt8
--   fromSing SDBInt2                 = DBInt2
--   fromSing (SDBFloat v)            = DBFloat (fromSing v)
--   fromSing (SDBNumeric n1 n2)      = DBNumeric (fromSing n1) (fromSing n2)
--   fromSing (SDBChar n)             = DBChar (fromSing n)
--   fromSing (SDBVarchar n)          = DBVarchar (fromSing n)
--   fromSing SDBBool                 = DBBool
--   fromSing SDBDate                 = DBDate
--   fromSing (SDBTime n)             = DBTime (fromSing n)
--   fromSing (SDBTimetz n)           = DBTimetz (fromSing n)
--   fromSing (SDBTimestamp n)        = DBTimestamp (fromSing n)
--   fromSing (SDBTimestamptz n)      = DBTimestamptz (fromSing n)
--   fromSing (SDBInterval _ n2)      = DBInterval Nothing (fromSing n2)
--   fromSing (SDBNullable n)         = DBNullable (fromSing n)
--   fromSing SDBXml                  = DBXml
--   fromSing (SDBBinary n)           = DBBinary (fromSing n)
--   fromSing (SDBVarbinary n)        = DBVarbinary (fromSing n)
--   fromSing SDBText                 = DBText
--   fromSing SDBCiText               = DBCiText
--   fromSing SDBUuid                 = DBUuid
--   fromSing (SDBBit n)              = DBBit (fromSing n)
--   fromSing (SDBVarbit n)           = DBVarbit (fromSing n)
--   fromSing SDBJson                 = DBJson  
--   fromSing SDBJsonB                = DBJsonB
--   fromSing (SDBArray a)            = DBArray (fromSing a)
--   fromSing SDBLTree                = DBLTree
--   fromSing (SDBCustomType sc _ t ) = DBCustomType (fromSing sc) (fromSing t)


-- type family GetDBTypeRep sc t where
--   GetDBTypeRep sc t = GetDBTypeRep' sc (DB (SchemaDB sc)) t

-- type family GetDBTypeRep' sc dbk t where
--   GetDBTypeRep' sc 'Postgres t = GetPGTypeRep sc t
--   GetDBTypeRep' sc 'MSSQL    t = GetMSSQLTypeRep sc t

-- type family GetMSSQLTypeRep (sc :: Type) (t :: Type) = (r :: Type.DBTypeK) {-| r -> t-} where
--   GetMSSQLTypeRep _ Int                = 'Type.DBInt8
--   GetMSSQLTypeRep _ Int8               = 'Type.DBNumeric 3 0
--   GetMSSQLTypeRep _ Int16              = 'Type.DBInt2
--   GetMSSQLTypeRep _ Int32              = 'Type.DBInt4
--   GetMSSQLTypeRep _ Int64              = 'Type.DBInt8
--   GetMSSQLTypeRep _ Word               = 'Type.DBNumeric 20 0
--   GetMSSQLTypeRep _ Word8              = 'Type.DBNumeric 3 0
--   GetMSSQLTypeRep _ Word16             = 'Type.DBNumeric 5 0
--   GetMSSQLTypeRep _ Word32             = 'Type.DBNumeric 10 0
--   GetMSSQLTypeRep _ Word64             = 'Type.DBNumeric 20 0
--   GetMSSQLTypeRep _ Float              = 'Type.DBFloat 24
--   GetMSSQLTypeRep _ Double             = 'Type.DBFloat 53
--   GetMSSQLTypeRep _ Char               = 'Type.DBChar 1
--   GetMSSQLTypeRep _ T.Text             = 'Type.DBText
--   GetMSSQLTypeRep _ ByteString         = 'Type.DBVarbinary ('Left 'Type.Max)
--   GetMSSQLTypeRep _ Bool               = 'Type.DBBit 1
--   GetMSSQLTypeRep _ Day                = 'Type.DBDate
--   GetMSSQLTypeRep _ UTCTime            = 'Type.DBTimestamptz 7
--   GetMSSQLTypeRep _ LocalTime          = 'Type.DBTimestamp 7
--   GetMSSQLTypeRep _ TimeOfDay          = 'Type.DBTime 7
--   GetMSSQLTypeRep sc (Maybe t)         = 'Type.DBNullable (GetMSSQLTypeRep sc t)
--   GetMSSQLTypeRep sc [t]               = 'Type.DBArray (GetMSSQLTypeRep sc t)
--   GetMSSQLTypeRep sc (CustomType a)    = CustomDBTypeRep sc a
--   GetMSSQLTypeRep sc a                 =
--     GetMSSQLTypeRepCustom sc a (NewtypeRep a)

-- type family GetMSSQLTypeRepCustom (sc :: Type) (ot :: Type) (t :: Maybe Type) where
--   GetMSSQLTypeRepCustom sc a 'Nothing =
--     'Type.DBCustomType (SchemaName sc) a ('Type.DBTypeName (GetTypeName a) '[] (TypeMappings sc a))
--   GetMSSQLTypeRepCustom sc _ ('Just a) =
--     GetMSSQLTypeRep sc a

-- type family GetPGTypeRep (sc :: Type) (t :: Type) = (r :: Type.DBTypeK) where
--   GetPGTypeRep _ Int                = 'Type.DBInt4
--   GetPGTypeRep _ Int16              = 'Type.DBInt2
--   GetPGTypeRep _ Int32              = 'Type.DBInt4
--   GetPGTypeRep _ Int64              = 'Type.DBInt8
--   GetPGTypeRep _ Float              = 'Type.DBFloat 24
--   GetPGTypeRep _ Double             = 'Type.DBFloat 53
--   GetPGTypeRep _ Rational           = 'Type.DBNumeric 1000 1000
--   GetPGTypeRep _ Scientific         = 'Type.DBNumeric 1000 1000
--   GetPGTypeRep _ Char               = 'Type.DBChar 1
--   GetPGTypeRep _ T.Text             = 'Type.DBText
--   GetPGTypeRep _ (CI T.Text)        = 'Type.DBCiText
--   GetPGTypeRep _ ByteString         = 'Type.DBVarbinary ('Left 'Type.Max)
--   GetPGTypeRep _ Bool               = 'Type.DBBool
--   GetPGTypeRep _ Day                = 'Type.DBDate
--   GetPGTypeRep _ UTCTime            = 'Type.DBTimestamptz 6
--   GetPGTypeRep _ LocalTime          = 'Type.DBTimestamp 6
--   GetPGTypeRep _ TimeOfDay          = 'Type.DBTime 6
--   GetPGTypeRep _ Value              = 'Type.DBJsonB
--   GetPGTypeRep _ Interval           = 'Type.DBInterval 'Nothing 6
--   GetPGTypeRep _ (Path.Path a ft)   = 'Type.DBText
--   GetPGTypeRep _ (Json a)           = 'Type.DBJsonB
--   -- GetPGTypeRep sc (Json a)          = 'Type.DBCustomType (Json a) 'Type.DBJsonB (TypeMappings sc (Json a))
--   -- GetPGTypeRep sc (JsonStr a)       = 'Type.DBCustomType (JsonStr a) 'Type.DBJson (TypeMappings sc (JsonStr a))
--   GetPGTypeRep _ UUID               = 'Type.DBUuid
--   GetPGTypeRep sc (Maybe t)         = 'Type.DBNullable (GetPGTypeRep sc t)
--   -- GetPGTypeRep (Vector t)         = 'DBArray (GetPGTypeRep t)
--   GetPGTypeRep sc [t]               = 'Type.DBArray (GetPGTypeRep sc t)
--   GetPGTypeRep sc (CustomType a)    = CustomDBTypeRep sc a
--   GetPGTypeRep sc a                 =
--     GetPGTypeRepCustom sc a (NewtypeRep a)

-- type family GetPGTypeRepCustom (sc :: Type) (ot :: Type) (t :: Maybe Type) :: Type.DBTypeK where
--   GetPGTypeRepCustom sc a 'Nothing =
--     'Type.DBCustomType (SchemaName sc) a ('Type.DBTypeName (GetTypeName a) '[] (TypeMappings sc a)) 
--   GetPGTypeRepCustom sc _ ('Just a) =
--     GetPGTypeRep sc a

-- type family CustomDBTypeRep (sc :: Type) (ty :: Type) :: Type.DBTypeK


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

newtype FieldAliases sc ty = FieldAliases (HM.HashMap Text Text)
  deriving newtype (Show, Semigroup, Monoid)

instance (HasField fn ty ft, KnownSymbol fn) => SetField (fn :: Symbol) (FieldAliases sc ty) Text where
  modifyField f (FieldAliases hmap) = FieldAliases $ HM.alter (Just . maybe (f fname) f) fname hmap
    where
      fname = T.pack $ symbolVal (Proxy :: Proxy fn)
  {-# INLINE modifyField #-}    

getAliasedFieldName :: forall fn ty sc ft. (HasField fn ty ft, KnownSymbol fn) => FieldAliases sc ty -> Const Text fn
getAliasedFieldName (FieldAliases hmap) = Const $ HM.findWithDefault fname fname hmap
  where
    fname = T.pack $ symbolVal (Proxy :: Proxy fn)
{-# INLINE getAliasedFieldName #-}

newtype ConAliases sc ty = ConAliases (HM.HashMap Text Text)
  deriving newtype (Show, Semigroup, Monoid)

instance (Generic ty, ValidateConName ty fn (Rep ty) (UnconsSymbol fn), KnownSymbol fn) => SetField (fn :: Symbol) (ConAliases sc ty) Text where
  modifyField f (ConAliases hmap) = ConAliases $ HM.alter (Just . maybe (f cname) f) cname hmap
    where
      -- Invariant: `ValidateConName` ensures that `fn` is not empty, making the use of `tail` safe
      cname = T.pack $ tail $ symbolVal (Proxy :: Proxy fn)
  {-# INLINE modifyField #-}

type family ValidateConName (ty :: Type) (k :: Symbol) (rep :: Type -> Type) (unconsedConName :: Maybe (Char, Symbol)) :: Constraint where
  ValidateConName _ _ _ 'Nothing = TypeError ('Text "Invalid Constructor Name: " ':<>: 'Text " for type " ':<>: 'Text "")
  ValidateConName ty k rep unconsedConName = ()

newtype UDTypeName sc ty = UDTypeName Text

instance IsString (UDTypeName sc ty) where
  fromString s = UDTypeName $ T.pack s

class UDType (sc :: Type) (ty :: Type) where
  type UDTypeRep sc ty :: Type.UDTypeK
  
  udTypeName :: UDTypeName sc ty
  default udTypeName :: (Generic ty) => UDTypeName sc ty
  udTypeName = ""

  fieldAliases :: FieldAliases sc ty
  fieldAliases = mempty

  conAliases :: ConAliases sc ty
  conAliases = mempty

type Pred = Type

data DBObjK
  = TableObj
  | NativeTypeObj Type.DBTypeK
  | UDTypeObj Type.UDTypeK
  | NewtypeObj Type
  | DomainType DBObjK
  | SimDomainType DBObjK
  | NullableObjOf DBObjK
  | ArrayObjOf DBObjK

class DBRepr (dbk :: DbK) (t :: Type) where
  type ToDBType dbk t :: DBObjK
  type ToDBType dbk t = 'TableObj
  type AutoCodec dbk t :: Bool
  type AutoCodec dbk t = 'True

instance DBRepr dbk Int where
  type ToDBType dbk Int = 'NativeTypeObj 'Type.DBInt8

instance DBRepr dbk Int64 where
  type ToDBType dbk Int64 = 'NativeTypeObj 'Type.DBInt8

instance DBRepr dbk Int32 where
  type ToDBType dbk Int32 = 'NativeTypeObj 'Type.DBInt4

instance DBRepr dbk Int16 where
  type ToDBType dbk Int16 = 'NativeTypeObj 'Type.DBInt2

instance DBRepr dbk Text where
  type ToDBType dbk Text = 'NativeTypeObj 'Type.DBText

deriving newtype instance DBRepr dbk t => DBRepr dbk (Identity t)

instance DBRepr dbk (CI t) where
  type ToDBType dbk (CI t) = 'NativeTypeObj ('Type.DBCiText)

instance DBRepr dbk TimeOfDay where
  type ToDBType dbk TimeOfDay = 'NativeTypeObj ('Type.DBTime 7)

instance DBRepr dbk Bool where
  type ToDBType dbk Bool = 'NativeTypeObj 'Type.DBBool

instance DBRepr dbk Double where
  type ToDBType dbk Double = 'NativeTypeObj ('Type.DBFloat 53)

instance DBRepr dbk Float where
  type ToDBType dbk Float = 'NativeTypeObj ('Type.DBFloat 24)

instance DBRepr dbk Rational where
  type ToDBType dbk Rational = 'NativeTypeObj ('Type.DBNumeric 1000 1000)
  
instance DBRepr dbk Scientific where
  type ToDBType dbk Scientific = 'NativeTypeObj ('Type.DBNumeric 1000 1000)

instance DBRepr dbk Day where
  type ToDBType dbk Day = 'NativeTypeObj 'Type.DBDate
  
instance DBRepr dbk LocalTime where
  type ToDBType dbk LocalTime = 'NativeTypeObj ('Type.DBTimestamp 6)

instance DBRepr dbk UTCTime where
  type ToDBType dbk UTCTime = 'NativeTypeObj ('Type.DBTimestamptz 6)

instance DBRepr dbk ByteString where
  type ToDBType dbk ByteString = 'NativeTypeObj ('Type.DBVarbinary ('Left 'Type.Max))

instance DBRepr dbk UUID where
  type ToDBType dbk UUID = 'NativeTypeObj 'Type.DBUuid 
  
instance DBRepr dbk a => DBRepr dbk (Maybe a) where
  type ToDBType dbk (Maybe a) = 'NullableObjOf (ToDBType dbk a)
  type AutoCodec dbk (Maybe a) = AutoCodec dbk a

instance DBRepr dbk a => DBRepr dbk [a] where
  type ToDBType dbk [a] = 'ArrayObjOf (ToDBType dbk a)
  type AutoCodec dbk [a] = AutoCodec dbk a

instance DBRepr dbk a => DBRepr dbk (Vector a) where
  type ToDBType dbk (Vector a) = 'ArrayObjOf (ToDBType dbk a)
  type AutoCodec dbk (Vector a) = AutoCodec dbk a

-- TODO: Json is not native is all the DB
instance DBRepr dbk (Json a) where
  type ToDBType dbk (Json a) = 'NativeTypeObj 'Type.DBJsonB

instance DBRepr dbk Value where
  type ToDBType dbk Value = 'NativeTypeObj 'Type.DBJsonB

instance DBRepr dbk (Rec xs) where
  type ToDBType dbk (Rec xs) = 'TableObj
  type AutoCodec dbk (Rec xs) = 'False

newtype Row xs = Row (Rec xs)

newtype Composite t = Composite_ t

getComposite :: Composite t -> t
getComposite (Composite_ c) = c

newtype TableVal t = TableVal t


instance DBRepr dbk (Row xs) where
  type ToDBType dbk (Row xs) = 'UDTypeObj ('Type.UDRec 'Type.FlatRec)

newtype AsUDType t = AsUDType t

instance DBRepr dbk (AsUDType t) where
  type ToDBType dbk (AsUDType t) = 'UDTypeObj (Type.GenUDTypeRep (Rep t))

instance DBRepr dbk (DBR.Key tab t) where
  type ToDBType dbk (DBR.Key tab t) = ToDBType dbk t
  type AutoCodec dbk (DBR.Key tab t) = AutoCodec dbk t

instance DBRepr dbk (PGOID 'RegType) where
  type ToDBType dbk (PGOID 'RegType) = 'NativeTypeObj 'Type.DBText -- TODO: Fix  

instance DBRepr dbk (a, b) where
  type ToDBType dbk (a, b) = 'TableObj  


instance DBRepr dbk LTree where
  type ToDBType dbk LTree = 'NativeTypeObj 'Type.DBText -- TODO: Fix
  

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

class DBCatalog (db :: Type) where
  type Schemas db :: [Type]
  type Roles db :: [Type]
  type Extensions db :: [Type]

class SchemaCatalog (sc :: Type) where
  type Tables' sc :: [Type]
  type Types' sc :: [Type]
  type Views sc :: [Type]
  type MaterializedViews sc :: [Type]
  type Functions sc :: [(Symbol, Type)]
  type AggFunctions sc :: [(Symbol, Type)]
--  type Sequences sc :: [Type]

-- toNullable :: DBType -> DBType
-- toNullable = DBNullable

-- removeNullable :: DBType -> DBType
-- removeNullable (DBNullable t) = t
-- removeNullable _ = error "Panic: Remove nullable failed"

-- isNullable :: DBType -> Bool
-- isNullable (DBNullable _) = True
-- isNullable _              = False

-- enumType :: T.Text -> DBType
-- enumType v = DBCustomType (DBTypeName v []) False

-- NOTE: newtype handling.

-- type TPair (a :: Symbol) (b :: Symbol) = '(a, b)

