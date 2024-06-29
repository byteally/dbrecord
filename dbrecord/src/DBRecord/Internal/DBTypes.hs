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
import DBRecord.Internal.Types
import DBRecord.Internal.Common
-- import qualified DBRecord.Internal.PrimQuery as PQ
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

newtype FieldAliases (dbk :: DbK) ty = FieldAliases (HM.HashMap Text Text)
  deriving newtype (Show, Semigroup, Monoid)

instance (HasField fn ty ft, KnownSymbol fn) => SetField (fn :: Symbol) (FieldAliases dbk ty) Text where
  modifyField f (FieldAliases hmap) = FieldAliases $ HM.alter (Just . maybe (f fname) f) fname hmap
    where
      fname = T.pack $ symbolVal (Proxy :: Proxy fn)
  {-# INLINE modifyField #-}

getAliasedFieldName :: forall fn ty dbk ft. (HasField fn ty ft, KnownSymbol fn) => FieldAliases dbk ty -> Const Text fn
getAliasedFieldName (FieldAliases hmap) = Const $ HM.findWithDefault fname fname hmap
  where
    fname = T.pack $ symbolVal (Proxy :: Proxy fn)
{-# INLINE getAliasedFieldName #-}

newtype ConAliases (dbk :: DbK) ty = ConAliases (HM.HashMap Text (Either Text Int64))
  deriving newtype (Show, Semigroup, Monoid)

lookupConName :: Text -> Maybe Int64 -> ConAliases dbk ty -> Either Text Int64
lookupConName cn pos (ConAliases hmap) = HM.findWithDefault (maybe (Left cn) Right pos) cn hmap

type family GetTagEnumK (dbObj :: DBObjK) = (res :: UDEnumK) where
  GetTagEnumK ('UDTypeObj ('UDEnum en)) = en
  GetTagEnumK _ = TypeError ('Text "Expecting only enum type")

instance (Generic ty
         , ValidateConName ty fn (Rep ty) (UnconsSymbol fn)
         , SetField '(fn, GetTagEnumK (ToDBType dbk ty)) (ConAliases dbk ty) fty
         , KnownSymbol fn
         ) => SetField (fn :: Symbol) (ConAliases dbk ty) fty where
  modifyField f cas = modifyField @'(fn, GetTagEnumK (ToDBType dbk ty)) f cas
  {-# INLINE modifyField #-}

instance (Generic ty
         , ValidateConName ty fn (Rep ty) (UnconsSymbol fn)
         , KnownSymbol fn
         ) => SetField '(fn :: Symbol, 'EnumType) (ConAliases dbk ty) Text where
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
         ) => SetField '(fn :: Symbol, 'EnumText) (ConAliases dbk ty) Text where
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
         ) => SetField '(fn :: Symbol, 'EnumNum) (ConAliases dbk ty) Int64 where
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
  | NativeTypeObj DBTypeK
  | UDTypeObj UDTypeK
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
  type Fields t = GGetFieldsOrEmpty t (Rep t)


  type UnLifted dbk t :: Type
  type UnLifted dbk t = t

  univOfUnLifted :: Proxy '(dbk, t) -> [(Text, UnLifted dbk t)]
  default univOfUnLifted :: (UnivOfUnLifted (ToDBType dbk t) dbk t) => Proxy '(dbk, t) -> [(Text, UnLifted dbk t)]
  univOfUnLifted _ = univOfUnLifted' (Proxy @'(ToDBType dbk t, dbk, t))

  typeName :: TypeName dbk t
  default typeName :: (Break (NoGeneric t) (Rep t), KnownSymbol (GenTyCon (Rep t))) => TypeName dbk t
  typeName = TypeName $ defHSNameToDBName $ T.pack (symbolVal (Proxy @(GenTyCon (Rep t))))

  fieldAliases :: FieldAliases dbk t
  fieldAliases = mempty

  conAliases :: ConAliases dbk t
  conAliases = mempty

  discriminatorTagName :: DiscriminatorTagName dbk t
  default discriminatorTagName :: (SingI (IsTaggedSum (ToDBType dbk t))) => DiscriminatorTagName dbk t
  discriminatorTagName = case (fromSing (sing :: Sing (IsTaggedSum (ToDBType dbk t)))) of
    True -> let TypeName tn = typeName @dbk @t
            in DiscriminatorTagName (tn <> "_tag")
    False -> let TypeName tn = typeName @dbk @t
            in DiscriminatorTagName tn

  discriminatorTypeName :: TypeName dbk t
  default discriminatorTypeName :: (SingI (IsTaggedSum (ToDBType dbk t))) => TypeName dbk t
  discriminatorTypeName = case (fromSing (sing :: Sing (IsTaggedSum (ToDBType dbk t)))) of
    True -> let TypeName tn = typeName @dbk @t
            in TypeName (tn <> "_tag")
    False -> typeName @dbk @t


  -- lift :: UnLifted dbk ty -> Expr sc ty
  -- unlift :: Expr sc ty -> UnLifted dbk ty

data SumRepr (dbk :: DbK) (t :: Type) = SumRepr
  { ctors :: [(Text, UnLifted dbk t)]
  , ctorTagOf :: t -> Text
  }

data Ctor (dbk :: DbK) (t :: Type) where
  Ctor :: Text -> (a -> t) -> (UnLifted dbk t -> f a) -> Ctor dbk t

data DeCtor (dbk :: DbK) (t :: Type) where
  DeCtor :: Text -> (t -> g a) -> (g a -> f a) -> (f a -> UnLifted dbk t) -> DeCtor dbk t  

class UnivOfUnLifted (dbObjK :: DBObjK) (dbk :: DbK) (t :: Type) where
  univOfUnLifted' :: Proxy '(dbObjK, dbk, t) -> [(Text, UnLifted dbk t)]

instance UnivOfUnLifted ('UDTypeObj ('UDEnum enk)) dbk t where
  univOfUnLifted' _ = []

instance UnivOfUnLifted ('UDTypeObj ('TaggedSum enk lay)) dbk t where
  univOfUnLifted' _ = []

instance UnivOfUnLifted ('UDTypeObj ('TaggedSumMono enk cty lay)) dbk t where
  univOfUnLifted' _ = []

instance UnivOfUnLifted ('UDTypeObj ('SumOfCol lay)) dbk t where
  univOfUnLifted' _ = []

instance UnivOfUnLifted ('UDTypeObj ('UDRec rt)) dbk t where
  univOfUnLifted' _ = []

instance UnivOfUnLifted ('UDTypeObj ('SerializedBlob ct)) dbk t where
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
  type ToDBType dbk Int = 'NativeTypeObj 'TDBInt8
  typeName = ""

instance DBRepr dbk Int64 where
  type ToDBType dbk Int64 = 'NativeTypeObj 'TDBInt8
  typeName = ""

instance DBRepr dbk Int32 where
  type ToDBType dbk Int32 = 'NativeTypeObj 'TDBInt4
  typeName = ""

instance DBRepr dbk Int16 where
  type ToDBType dbk Int16 = 'NativeTypeObj 'TDBInt2
  typeName = ""

instance DBRepr dbk Text where
  type ToDBType dbk Text = 'NativeTypeObj 'TDBText
  typeName = ""

deriving newtype instance DBRepr dbk t => DBRepr dbk (Identity t)

instance DBRepr dbk (CI t) where
  type ToDBType dbk (CI t) = 'NativeTypeObj ('TDBCiText)
  typeName = ""

instance DBRepr dbk TimeOfDay where
  type ToDBType dbk TimeOfDay = 'NativeTypeObj ('TDBTime 7)
  typeName = ""

instance DBRepr dbk Bool where
  type ToDBType dbk Bool = 'NativeTypeObj 'TDBBool
  typeName = ""

instance DBRepr dbk Double where
  type ToDBType dbk Double = 'NativeTypeObj ('TDBFloat 53)
  typeName = ""

instance DBRepr dbk Float where
  type ToDBType dbk Float = 'NativeTypeObj ('TDBFloat 24)
  typeName = ""

instance DBRepr dbk Rational where
  type ToDBType dbk Rational = 'NativeTypeObj ('TDBNumeric 1000 1000)
  typeName = ""

instance DBRepr dbk Scientific where
  type ToDBType dbk Scientific = 'NativeTypeObj ('TDBNumeric 1000 1000)
  typeName = ""

instance DBRepr dbk Day where
  type ToDBType dbk Day = 'NativeTypeObj 'TDBDate
  typeName = ""

instance DBRepr dbk LocalTime where
  type ToDBType dbk LocalTime = 'NativeTypeObj ('TDBTimestamp 6)
  typeName = ""

instance DBRepr dbk UTCTime where
  type ToDBType dbk UTCTime = 'NativeTypeObj ('TDBTimestamptz 6)
  typeName = ""

instance DBRepr dbk ByteString where
  type ToDBType dbk ByteString = 'NativeTypeObj ('TDBVarbinary ('Left 'Max))
  typeName = ""

instance DBRepr dbk UUID where
  type ToDBType dbk UUID = 'NativeTypeObj 'TDBUuid
  typeName = ""

instance DBRepr dbk a => DBRepr dbk (Maybe a) where
  type ToDBType dbk (Maybe a) = 'NullableObjOf a (ToDBType dbk a)
  type AutoCodec dbk (Maybe a) = AutoCodec dbk a
  typeName = ""

instance DBRepr dbk a => DBRepr dbk [a] where
  type ToDBType dbk [a] = 'ArrayObjOf a (ToDBType dbk a)
  type AutoCodec dbk [a] = AutoCodec dbk a
  typeName = ""

instance DBRepr dbk a => DBRepr dbk (Vector a) where
  type ToDBType dbk (Vector a) = 'ArrayObjOf a (ToDBType dbk a)
  type AutoCodec dbk (Vector a) = AutoCodec dbk a
  typeName = ""

-- TODO: Json is not native is all the DB
instance DBRepr dbk (Json a) where
  type ToDBType dbk (Json a) = 'NativeTypeObj 'TDBJsonB
  typeName = ""

instance DBRepr dbk Value where
  type ToDBType dbk Value = 'NativeTypeObj 'TDBJsonB
  typeName = ""

instance DBRepr dbk (Rec xs) where
  type ToDBType dbk (Rec xs) = 'TableObj
  type AutoCodec dbk (Rec xs) = 'False
  typeName = ""

newtype Row xs = Row (Rec xs)

newtype Composite t = Composite_ t

getComposite :: Composite t -> t
getComposite (Composite_ c) = c

newtype TableVal t = TableVal t


instance DBRepr dbk (Row xs) where
  type ToDBType dbk (Row xs) = 'UDTypeObj ('UDRec 'FlatRec)
  typeName = ""

newtype AsUDType t = AsUDType t

instance DBRepr dbk (AsUDType t) where
  type ToDBType dbk (AsUDType t) = 'UDTypeObj (GenUDTypeRep (Rep t))
  univOfUnLifted _ = []
  typeName = ""
  discriminatorTypeName = ""
  discriminatorTagName = ""

newtype AsEnum t = AsEnum t

instance DBRepr db (AsEnum t) where
  type ToDBType db (AsEnum t) = 'UDTypeObj ('UDEnum (GetDBEnumK db))
  typeName = ""

newtype AsEnumText t = AsEnumText t

instance DBRepr dbk (AsEnumText t) where
  type ToDBType dbk (AsEnumText t) = 'UDTypeObj ('UDEnum 'EnumText)
  typeName = ""

newtype AsEnumNum t = AsEnumNum t

instance DBRepr dbk (AsEnumNum t) where
  type ToDBType dbk (AsEnumNum t) = 'UDTypeObj ('UDEnum 'EnumNum)
  typeName = ""

newtype AsCompositeRec t = AsCompositeRec t

instance DBRepr 'Postgres (AsCompositeRec t) where
  type ToDBType 'Postgres (AsCompositeRec t) = 'UDTypeObj ('UDRec 'CompositeRec)
  typeName = ""

newtype AsFlatRec t = AsFlatRec t

instance DBRepr dbk (AsFlatRec t) where
  type ToDBType dbk (AsFlatRec t) = 'UDTypeObj ('UDRec 'FlatRec)
  typeName = ""

newtype AsJsonRec t = AsJsonRec t

instance DBRepr 'Postgres (AsJsonRec t) where
  type ToDBType 'Postgres (AsJsonRec t) = 'UDTypeObj ('UDRec 'JsonRec)
  typeName = ""

newtype AsJsonBlob t = AsJsonBlob t

instance DBRepr 'Postgres (AsJsonBlob t) where
  type ToDBType 'Postgres (AsJsonBlob t) = 'UDTypeObj ('SerializedBlob ('JsonContent 'Nothing))
  typeName = ""

newtype AsTaggedSumFlat t = AsTaggedSumFlat t

instance DBRepr db (AsTaggedSumFlat t) where
  type ToDBType db (AsTaggedSumFlat t) = 'UDTypeObj ('TaggedSum (GetDBEnumK db) 'FlatRec)
  typeName = ""

newtype AsTaggedSumComposite t = AsTaggedSumComposite t

instance DBRepr 'Postgres (AsTaggedSumComposite t) where
  type ToDBType 'Postgres (AsTaggedSumComposite t) = 'UDTypeObj ('TaggedSum 'EnumType 'CompositeRec)
  typeName = ""

newtype AsTaggedSumJson t = AsTaggedSumJson t

instance DBRepr db (AsTaggedSumJson t) where
  type ToDBType db (AsTaggedSumJson t) = 'UDTypeObj ('TaggedSum (GetDBEnumK db) 'JsonRec)
  typeName = ""

newtype AsTaggedSumMonoFlat colTy t = AsTaggedSumMonoFlat t

instance DBRepr db cty => DBRepr db (AsTaggedSumMonoFlat cty t) where
  type ToDBType db (AsTaggedSumMonoFlat cty t) = 'UDTypeObj ('TaggedSumMono (GetDBEnumK db) cty 'FlatRec)
  typeName = ""

newtype AsTaggedSumMonoComposite colTy t = AsTaggedSumMonoComposite t

instance DBRepr 'Postgres cty => DBRepr 'Postgres (AsTaggedSumMonoComposite cty t) where
  type ToDBType 'Postgres (AsTaggedSumMonoComposite cty t) = 'UDTypeObj ('TaggedSumMono 'EnumType cty 'CompositeRec)
  typeName = ""

newtype AsTaggedSumMonoJson colTy t = AsTaggedSumMonoJson t

instance DBRepr dbk cty => DBRepr dbk (AsTaggedSumMonoJson cty t) where
  type ToDBType dbk (AsTaggedSumMonoJson cty t) = 'UDTypeObj ('TaggedSumMono (GetDBEnumK dbk) cty 'JsonRec)
  typeName = ""

newtype AsSumOfColFlat t = AsSumOfColFlat t

instance DBRepr db (AsSumOfColFlat t) where
  type ToDBType db (AsSumOfColFlat t) = 'UDTypeObj ('SumOfCol 'FlatRec)
  typeName = ""

newtype AsSumOfColComposite t = AsSumOfColComposite t

instance DBRepr 'Postgres (AsSumOfColComposite t) where
  type ToDBType 'Postgres (AsSumOfColComposite t) = 'UDTypeObj ('SumOfCol 'CompositeRec)
  typeName = ""

newtype AsSumOfColJson t = AsSumOfColJson t

instance DBRepr 'Postgres (AsSumOfColJson t) where
  type ToDBType 'Postgres (AsSumOfColJson t) = 'UDTypeObj ('SumOfCol 'JsonRec)
  typeName = ""

instance DBRepr dbk (DBR.Key tab t) where
  type ToDBType dbk (DBR.Key tab t) = ToDBType dbk t
  type AutoCodec dbk (DBR.Key tab t) = AutoCodec dbk t
  univOfUnLifted _ = []
  typeName = ""
  discriminatorTypeName = ""
  discriminatorTagName = ""

instance DBRepr dbk (PGOID 'RegType) where
  type ToDBType dbk (PGOID 'RegType) = 'NativeTypeObj 'TDBText -- TODO: Fix
  typeName = ""

instance DBRepr dbk (a, b) where
  type ToDBType dbk (a, b) = 'TableObj
  typeName = ""


instance DBRepr dbk LTree where
  type ToDBType dbk LTree = 'NativeTypeObj 'TDBText -- TODO: Fix
  typeName = ""

-- UD Type
newtype TypeName (dbk :: DbK) ty = TypeName Text

_getTypeName :: TypeName dbk ty -> Text
_getTypeName (TypeName ty) = ty

instance IsString (TypeName dbk ty) where
  fromString s = TypeName $ T.pack s

newtype DiscriminatorTagName (dbk :: DbK) ty = DiscriminatorTagName Text
  deriving newtype (Show, IsString)

_getDiscriminatorTagName :: DiscriminatorTagName dbk ty -> Text
_getDiscriminatorTagName (DiscriminatorTagName t) = t

type family IsTaggedSum (dbt :: DBObjK) :: Bool where
  IsTaggedSum ('UDTypeObj ('TaggedSum _ _)) = 'True
  IsTaggedSum ('UDTypeObj ('TaggedSumMono _ _ _)) = 'True
  IsTaggedSum _ = 'False
  

--
  


