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
import DBRecord.Internal.Common
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
            -- | DBCustomType
            --     T.Text -- Schema name
            --     DBTypeName
            deriving (Show, Eq, Ord, Read)

data DBTypeName = DBTypeName T.Text [TypeArg]
                deriving (Show, Eq, Ord, Read)

data TypeArg = TextArg    T.Text
             | IntegerArg Integer
             deriving (Show, Eq, Ord, Read)

instance Type.SingE 'Type.DBInt4 where
  type Demote 'Type.DBInt4 = DBType
  fromSing Type.SDBInt4 = undefined

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

newtype ConAliases sc ty = ConAliases (HM.HashMap Text (Either Text Int64))
  deriving newtype (Show, Semigroup, Monoid)

lookupConName :: Text -> Maybe Int64 -> ConAliases sc ty -> Either Text Int64
lookupConName cn pos (ConAliases hmap) = HM.findWithDefault (maybe (Left cn) Right pos) cn hmap

type family GetTagEnumK (dbObj :: DBObjK) = (res :: Type.UDEnumK) where
  GetTagEnumK ('UDTypeObj ('Type.UDEnum en)) = en
  GetTagEnumK _ = TypeError ('Text "Expecting only enum type")

instance (Generic ty
         , ValidateConName ty fn (Rep ty) (UnconsSymbol fn)
         , SetField '(fn, GetTagEnumK (ToDBType (DB (SchemaDB sc)) ty)) (ConAliases sc ty) fty
         , KnownSymbol fn
         ) => SetField (fn :: Symbol) (ConAliases sc ty) fty where
  modifyField f cas = modifyField @'(fn, GetTagEnumK (ToDBType (DB (SchemaDB sc)) ty)) f cas
  {-# INLINE modifyField #-}

instance (Generic ty
         , ValidateConName ty fn (Rep ty) (UnconsSymbol fn)
         , KnownSymbol fn
         ) => SetField '(fn :: Symbol, 'Type.EnumType) (ConAliases sc ty) Text where
  modifyField f (ConAliases hmap) = ConAliases $ HM.alter (Just . Left . maybe (f cname) (f . unsafeText)) cname hmap
    where
      -- Invariant: `ValidateConName` ensures that `fn` is not empty, making the use of `tail` safe
      cname = T.pack $ tail $ symbolVal (Proxy :: Proxy fn)
      unsafeText (Left n) = n
      unsafeText _ = error "Panic: Invariant: Expecting only Text"
  {-# INLINE modifyField #-}

instance (Generic ty
         , ValidateConName ty fn (Rep ty) (UnconsSymbol fn)
         , KnownSymbol fn
         ) => SetField '(fn :: Symbol, 'Type.EnumText) (ConAliases sc ty) Text where
  modifyField f (ConAliases hmap) = ConAliases $ HM.alter (Just . Left . maybe (f cname) (f . unsafeText)) cname hmap
    where
      -- Invariant: `ValidateConName` ensures that `fn` is not empty, making the use of `tail` safe
      cname = T.pack $ tail $ symbolVal (Proxy :: Proxy fn)
      unsafeText (Left n) = n
      unsafeText _ = error "Panic: Invariant: Expecting only Text"
  {-# INLINE modifyField #-}

instance (Generic ty
         , ValidateConName ty fn (Rep ty) (UnconsSymbol fn)
         , KnownSymbol fn
         ) => SetField '(fn :: Symbol, 'Type.EnumNum) (ConAliases sc ty) Int64 where
  modifyField f (ConAliases hmap) = ConAliases $ HM.alter (Just . Right . maybe (f minBound) (f . unsafeNum)) cname hmap -- TODO: remove `minBound` by making ValidateConName to return `Maybe (con's-Ix)`
    where
      -- Invariant: `ValidateConName` ensures that `fn` is not empty, making the use of `tail` safe
      cname = T.pack $ tail $ symbolVal (Proxy :: Proxy fn)
      unsafeNum (Right n) = n
      unsafeNum _ = error "Panic: Invariant: Expecting only Integer"
  {-# INLINE modifyField #-}



type family ValidateConName (ty :: Type) (k :: Symbol) (rep :: Type -> Type) (unconsedConName :: Maybe (Char, Symbol)) :: Constraint where
  ValidateConName _ _ _ 'Nothing = TypeError ('Text "Invalid Constructor Name: " ':<>: 'Text " for type " ':<>: 'Text "")
  ValidateConName ty k rep unconsedConName = ()

data DBObjK
  = TableObj
  | NativeTypeObj Type.DBTypeK
  | UDTypeObj Type.UDTypeK
  | NewtypeObj Type
  | DomainType DBObjK
  | SimDomainType DBObjK
  | NullableObjOf Type DBObjK -- ^ Invariant: Supports only Native column
  | ArrayObjOf Type DBObjK -- ^ Invariant: Supports only Native column

class DBRepr (dbk :: DbK) (t :: Type) where
  type ToDBType dbk t :: DBObjK
  type ToDBType dbk t = 'TableObj
  type AutoCodec dbk t :: Bool
  type AutoCodec dbk t = 'True

  -- Invariant: Empty for Sum Types. All the fields of rec types.
  -- Also used to fix field position independent of it's position in Haskell Declaration 
  type Fields t :: [(Symbol, Type)]
  type Fields t = GGetFields t (Rep t)


  type UnLifted dbk t :: Type
  type UnLifted dbk t = t

  univOfUnLifted :: Proxy '(dbk, t) -> [(Text, UnLifted dbk t)]
  default univOfUnLifted :: (UnivOfUnLifted (ToDBType dbk t) dbk t) => Proxy '(dbk, t) -> [(Text, UnLifted dbk t)]
  univOfUnLifted _ = univOfUnLifted' (Proxy @'(ToDBType dbk t, dbk, t))

  -- lift :: UnLifted dbk ty -> Expr sc ty
  -- unlift :: Expr sc ty -> UnLifted dbk ty

class UnivOfUnLifted (dbObjK :: DBObjK) (dbk :: DbK) (t :: Type) where
  univOfUnLifted' :: Proxy '(dbObjK, dbk, t) -> [(Text, UnLifted dbk t)]

instance UnivOfUnLifted ('UDTypeObj ('Type.UDEnum enk)) dbk t where
  univOfUnLifted' _ = []

instance UnivOfUnLifted ('UDTypeObj ('Type.TaggedSum enk lay)) dbk t where
  univOfUnLifted' _ = []

instance UnivOfUnLifted ('UDTypeObj ('Type.TaggedSumMono enk cty lay)) dbk t where
  univOfUnLifted' _ = []

instance UnivOfUnLifted ('UDTypeObj ('Type.SumOfCol lay)) dbk t where
  univOfUnLifted' _ = []

instance UnivOfUnLifted ('UDTypeObj ('Type.UDRec rt)) dbk t where
  univOfUnLifted' _ = []

instance UnivOfUnLifted ('UDTypeObj ('Type.SerializedBlob ct)) dbk t where
  univOfUnLifted' _ = []

instance UnivOfUnLifted ('NativeTypeObj dbt) dbk t where
  univOfUnLifted' _ = []

instance UnivOfUnLifted ('NullableObjOf el eldbt) dbk t where
  univOfUnLifted' _ = []

instance UnivOfUnLifted ('ArrayObjOf el eldbt) dbk t where
  univOfUnLifted' _ = []

instance UnivOfUnLifted ('TableObj) dbk t where
  univOfUnLifted' _ = []


-- data ConDeCons dbk t where
--   ConDeCons :: (expr pat -> UnLifted dbk t)
--             -> (expr t -> (expr Bool, expr pat))
--             -> expr t
--             -> ConDeCons dbk t

-- test :: [ConDeCons 'Postgres Bool]
-- test = [ConDeCons (const UTrue) (const (Proxy @())) (constExpr True)]

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
  type ToDBType dbk (Maybe a) = 'NullableObjOf a (ToDBType dbk a)
  type AutoCodec dbk (Maybe a) = AutoCodec dbk a

instance DBRepr dbk a => DBRepr dbk [a] where
  type ToDBType dbk [a] = 'ArrayObjOf a (ToDBType dbk a)
  type AutoCodec dbk [a] = AutoCodec dbk a

instance DBRepr dbk a => DBRepr dbk (Vector a) where
  type ToDBType dbk (Vector a) = 'ArrayObjOf a (ToDBType dbk a)
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
  univOfUnLifted _ = []

newtype AsEnum t = AsEnum t

instance DBRepr db (AsEnum t) where
  type ToDBType db (AsEnum t) = 'UDTypeObj ('Type.UDEnum (Type.GetDBEnumK db))

-- instance DBRepr 'Postgres (AsEnum t) where
--   type ToDBType 'Postgres (AsEnum t) = 'UDTypeObj ('Type.UDEnum 'Type.EnumType)

-- instance DBRepr 'SQLite (AsEnum t) where
--   type ToDBType 'SQLite (AsEnum t) = 'UDTypeObj ('Type.UDEnum 'Type.EnumText)

-- instance DBRepr 'MySQL (AsEnum t) where
--   type ToDBType 'MySQL (AsEnum t) = 'UDTypeObj ('Type.UDEnum 'Type.EnumType)

-- instance DBRepr 'MSSQL (AsEnum t) where
--   type ToDBType 'MSSQL (AsEnum t) = 'UDTypeObj ('Type.UDEnum 'Type.EnumText)

-- instance DBRepr 'Cassandra (AsEnum t) where
--   type ToDBType 'Cassandra (AsEnum t) = 'UDTypeObj ('Type.UDEnum 'Type.EnumText)

-- instance DBRepr 'Presto (AsEnum t) where
--   type ToDBType 'Presto (AsEnum t) = 'UDTypeObj ('Type.UDEnum 'Type.EnumText)

newtype AsEnumText t = AsEnumText t

instance DBRepr 'Postgres (AsEnumText t) where
  type ToDBType 'Postgres (AsEnumText t) = 'UDTypeObj ('Type.UDEnum 'Type.EnumText)

newtype AsEnumNum t = AsEnumNum t

instance DBRepr 'Postgres (AsEnumNum t) where
  type ToDBType 'Postgres (AsEnumNum t) = 'UDTypeObj ('Type.UDEnum 'Type.EnumNum)

newtype AsCompositeRec t = AsCompositeRec t

instance DBRepr 'Postgres (AsCompositeRec t) where
  type ToDBType 'Postgres (AsCompositeRec t) = 'UDTypeObj ('Type.UDRec 'Type.CompositeRec)

newtype AsFlatRec t = AsFlatRec t

instance DBRepr db (AsFlatRec t) where
  type ToDBType db (AsFlatRec t) = 'UDTypeObj ('Type.UDRec 'Type.FlatRec)

newtype AsJsonRec t = AsJsonRec t

instance DBRepr 'Postgres (AsJsonRec t) where
  type ToDBType 'Postgres (AsJsonRec t) = 'UDTypeObj ('Type.UDRec 'Type.JsonRec)

newtype AsJsonBlob t = AsJsonBlob t

instance DBRepr 'Postgres (AsJsonBlob t) where
  type ToDBType 'Postgres (AsJsonBlob t) = 'UDTypeObj ('Type.SerializedBlob ('Type.JsonContent 'Nothing))

newtype AsTaggedSumFlat t = AsTaggedSumFlat t

instance DBRepr db (AsTaggedSumFlat t) where
  type ToDBType db (AsTaggedSumFlat t) = 'UDTypeObj ('Type.TaggedSum (Type.GetDBEnumK db) 'Type.FlatRec)

newtype AsTaggedSumComposite t = AsTaggedSumComposite t

instance DBRepr 'Postgres (AsTaggedSumComposite t) where
  type ToDBType 'Postgres (AsTaggedSumComposite t) = 'UDTypeObj ('Type.TaggedSum 'Type.EnumType 'Type.CompositeRec)

newtype AsTaggedSumJson t = AsTaggedSumJson t

instance DBRepr db (AsTaggedSumJson t) where
  type ToDBType db (AsTaggedSumJson t) = 'UDTypeObj ('Type.TaggedSum (Type.GetDBEnumK db) 'Type.JsonRec)

newtype AsTaggedSumMonoFlat colTy t = AsTaggedSumMonoFlat t

instance DBRepr db cty => DBRepr db (AsTaggedSumMonoFlat cty t) where
  type ToDBType db (AsTaggedSumMonoFlat cty t) = 'UDTypeObj ('Type.TaggedSumMono (Type.GetDBEnumK db) cty 'Type.FlatRec)

newtype AsTaggedSumMonoComposite colTy t = AsTaggedSumMonoComposite t

instance DBRepr 'Postgres cty => DBRepr 'Postgres (AsTaggedSumMonoComposite cty t) where
  type ToDBType 'Postgres (AsTaggedSumMonoComposite cty t) = 'UDTypeObj ('Type.TaggedSumMono 'Type.EnumType cty 'Type.CompositeRec)

newtype AsTaggedSumMonoJson colTy t = AsTaggedSumMonoJson t

instance DBRepr db cty => DBRepr db (AsTaggedSumMonoJson cty t) where
  type ToDBType db (AsTaggedSumMonoJson cty t) = 'UDTypeObj ('Type.TaggedSumMono (Type.GetDBEnumK db) cty 'Type.JsonRec)

newtype AsSumOfColFlat t = AsSumOfColFlat t

instance DBRepr db (AsSumOfColFlat t) where
  type ToDBType db (AsSumOfColFlat t) = 'UDTypeObj ('Type.SumOfCol 'Type.FlatRec)

newtype AsSumOfColComposite t = AsSumOfColComposite t

instance DBRepr 'Postgres (AsSumOfColComposite t) where
  type ToDBType 'Postgres (AsSumOfColComposite t) = 'UDTypeObj ('Type.SumOfCol 'Type.CompositeRec)

newtype AsSumOfColJson t = AsSumOfColJson t

instance DBRepr 'Postgres (AsSumOfColJson t) where
  type ToDBType 'Postgres (AsSumOfColJson t) = 'UDTypeObj ('Type.SumOfCol 'Type.JsonRec)

instance DBRepr dbk (DBR.Key tab t) where
  type ToDBType dbk (DBR.Key tab t) = ToDBType dbk t
  type AutoCodec dbk (DBR.Key tab t) = AutoCodec dbk t
  univOfUnLifted _ = []

instance DBRepr dbk (PGOID 'RegType) where
  type ToDBType dbk (PGOID 'RegType) = 'NativeTypeObj 'Type.DBText -- TODO: Fix

instance DBRepr dbk (a, b) where
  type ToDBType dbk (a, b) = 'TableObj


instance DBRepr dbk LTree where
  type ToDBType dbk LTree = 'NativeTypeObj 'Type.DBText -- TODO: Fix

-- UD Type
newtype UDTypeName sc ty = UDTypeName Text

instance IsString (UDTypeName sc ty) where
  fromString s = UDTypeName $ T.pack s

class ( DBRepr (DB (SchemaDB sc)) ty
      ) => UDType (sc :: Type) (ty :: Type) where
  type TypeId sc ty = (oid :: Nat) | oid -> ty

  udTypeName :: UDTypeName sc ty
  default udTypeName :: (KnownSymbol (GenTyCon (Rep ty)), Break (NoGeneric ty) (Rep ty)) => UDTypeName sc ty
  udTypeName = UDTypeName $ defHSNameToDBName $ T.pack (symbolVal (Proxy @(GenTyCon (Rep ty))))

  fieldAliases :: FieldAliases sc ty
  fieldAliases = mempty

  conAliases :: ConAliases sc ty
  conAliases = mempty

  discriminatorTagName :: DiscriminatorTagName sc ty
  default discriminatorTagName :: (Type.SingI (Type.IsTaggedSum (GetUDTypeKind (DB (SchemaDB sc)) ty (ToDBType (DB (SchemaDB sc)) ty)))) => DiscriminatorTagName sc ty
  discriminatorTagName = case (Type.fromSing (Type.sing :: Type.Sing (Type.IsTaggedSum (GetUDTypeKind (DB (SchemaDB sc)) ty (ToDBType (DB (SchemaDB sc)) ty))))) of
    True -> let UDTypeName tn = udTypeName @sc @ty
            in DiscriminatorTagName (tn <> "_tag")
    False -> let UDTypeName tn = udTypeName @sc @ty
            in DiscriminatorTagName tn

  discriminatorTypeName :: UDTypeName sc ty
  default discriminatorTypeName :: (Type.SingI (Type.IsTaggedSum (GetUDTypeKind (DB (SchemaDB sc)) ty (ToDBType (DB (SchemaDB sc)) ty)))) => UDTypeName sc ty
  discriminatorTypeName = case (Type.fromSing (Type.sing :: Type.Sing (Type.IsTaggedSum (GetUDTypeKind (DB (SchemaDB sc)) ty (ToDBType (DB (SchemaDB sc)) ty))))) of
    True -> let UDTypeName tn = udTypeName @sc @ty
            in UDTypeName (tn <> "_tag")
    False -> udTypeName @sc @ty

newtype DiscriminatorTagName sc ty = DiscriminatorTagName Text
  deriving newtype (Show, IsString)

_getDiscriminatorTagName :: DiscriminatorTagName sc ty -> Text
_getDiscriminatorTagName (DiscriminatorTagName t) = t

type family GetUDTypeKind (dbk :: DbK) (ty :: Type) (dbt :: DBObjK) :: Type.UDTypeK where
  GetUDTypeKind _ _ ('UDTypeObj udt) = udt
  GetUDTypeKind dbk ty _ = TypeError ('ShowType ty ':<>: 'Text " is not a User Defined Type for database " ':<>: 'ShowType ty)

--
  


class ( -- Break (NoGeneric db) (Rep db)
      -- TypeCxts db (Types db)
      ) => Database (db :: Type) where
  type DB db :: DbK
  type DB db = TypeError ('Text "DB type is not configured in the Database instance for type " ':<>: 'ShowType db ':$$:
                          'Text "Hint: add following to the Database instance for type "       ':<>: 'ShowType db ':$$:
                          'Text "type DB " ':<>: 'ShowType db ':<>: 'Text " = " ':<>: 'ShowType 'Postgres
                         )

  databaseName :: DatabaseName db
  default databaseName :: (KnownSymbol (GenTyCon (Rep db)), Break (NoGeneric db) (Rep db)) => DatabaseName db
  databaseName = DatabaseName $ defHSNameToDBName $ T.pack (symbolVal (Proxy @(GenTyCon (Rep db))))

newtype DatabaseName db = DatabaseName Text
  deriving newtype (Show, Eq, IsString)

_getDatabaseName :: DatabaseName db -> Text
_getDatabaseName (DatabaseName db) = db

class ( Database (SchemaDB sc)
      ) => Schema (sc :: Type) where
  type Baseline sc :: Nat
  type Baseline sc = 0

  type Version sc :: Nat
  type Version sc = 0

  type SchemaDB sc :: Type

  schemaName :: SchemaName sc
  default schemaName :: (KnownSymbol (GenTyCon (Rep sc)), Break (NoGeneric sc) (Rep sc)) => SchemaName sc
  schemaName = SchemaName $ defHSNameToDBName $ T.pack (symbolVal (Proxy @(GenTyCon (Rep sc))))

newtype SchemaName sc = SchemaName Text
  deriving newtype (Show, Eq, IsString)

_getSchemaName :: SchemaName sc -> Text
_getSchemaName (SchemaName sc) = sc

class DBCatalog (db :: Type) where
  type Schemas db :: [Type]
  type Roles db :: [Type]
  type Extensions db :: [Type]

class SchemaCatalog (sc :: Type) where
  type Tables sc :: [Type]
  type Types sc :: [Type]
  type Views sc :: [Type]
  type MaterializedViews sc :: [Type]
  type Functions sc :: [(Symbol, Type)]
  type AggFunctions sc :: [(Symbol, Type)]
--  type Sequences sc :: [Type]
