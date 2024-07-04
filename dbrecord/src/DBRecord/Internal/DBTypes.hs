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
import qualified DBRecord.Internal.PrimQuery as PQ
import qualified Data.Text as T
import GHC.Generics
import Data.Kind
import Data.String
import GHC.Records
import GHC.TypeLits
-- import GHC.Exts
-- import Data.Type.Bool
import Data.Type.Equality
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
         , ValidatePfxConName ty fn '_' (Rep ty) (UnconsSymbol fn)
         , SetField '(fn, GetTagEnumK (ToDBType dbk ty)) (ConAliases dbk ty) fty
         , KnownSymbol fn
         ) => SetField (fn :: Symbol) (ConAliases dbk ty) fty where
  modifyField f cas = modifyField @'(fn, GetTagEnumK (ToDBType dbk ty)) f cas
  {-# INLINE modifyField #-}

instance (Generic ty
         , ValidatePfxConName ty fn '_' (Rep ty) (UnconsSymbol fn)
         , KnownSymbol fn
         ) => SetField '(fn :: Symbol, 'EnumType) (ConAliases dbk ty) Text where
  modifyField f (ConAliases hmap) = ConAliases $ HM.alter (Just . Left . maybe (f cname) (f . unsafeText)) cname hmap
    where
      -- Invariant: `ValidatePfxConName` ensures that `fn` is not empty, making the use of `tail` safe
      cname = T.pack $ tail $ symbolVal (Proxy :: Proxy fn)
      unsafeText (Left n) = n
      unsafeText _ = error "Panic: Invariant: Expecting only Text"
  {-# INLINE modifyField #-}

instance (Generic ty
         , ValidatePfxConName ty fn '_' (Rep ty) (UnconsSymbol fn)
         , KnownSymbol fn
         ) => SetField '(fn :: Symbol, 'EnumText) (ConAliases dbk ty) Text where
  modifyField f (ConAliases hmap) = ConAliases $ HM.alter (Just . Left . maybe (f cname) (f . unsafeText)) cname hmap
    where
      -- Invariant: `ValidatePfxConName` ensures that `fn` is not empty, making the use of `tail` safe
      cname = T.pack $ tail $ symbolVal (Proxy :: Proxy fn)
      unsafeText (Left n) = n
      unsafeText _ = error "Panic: Invariant: Expecting only Text"
  {-# INLINE modifyField #-}

instance (Generic ty
         , ValidatePfxConName ty fn '_' (Rep ty) (UnconsSymbol fn)
         , KnownSymbol fn
         ) => SetField '(fn :: Symbol, 'EnumNum) (ConAliases dbk ty) Int64 where
  modifyField f (ConAliases hmap) = ConAliases $ HM.alter (Just . Right . maybe (f minBound) (f . unsafeNum)) cname hmap -- TODO: remove `minBound` by making ValidatePfxConName to return `Maybe (con's-Ix)`
    where
      -- Invariant: `ValidatePfxConName` ensures that `fn` is not empty, making the use of `tail` safe
      cname = T.pack $ tail $ symbolVal (Proxy :: Proxy fn)
      unsafeNum (Right n) = n
      unsafeNum _ = error "Panic: Invariant: Expecting only Integer"
  {-# INLINE modifyField #-}



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


  type Matcher dbk t :: MatcherK
  type Matcher dbk t = DefMatcher t (ToDBType dbk t)

  typeBaseExpr :: TypeBaseExpr dbk t
  typeBaseExpr = undefined

  sumRepr :: Proxy '(dbk, t) -> GetMatcherRep (Matcher dbk t)
  default sumRepr :: (HasSumRepr (ToDBType dbk t) dbk t (Matcher dbk t)) => Proxy '(dbk, t) -> GetMatcherRep (Matcher dbk t)
  sumRepr _ = sumRepr' (Proxy @'(ToDBType dbk t, dbk, t, Matcher dbk t))

  typeName :: TypeName dbk t
  default typeName :: (Break (NoGeneric t) (Rep t), KnownSymbol (GenTyCon (Rep t))) => TypeName dbk t
  typeName = TypeName $ genDBTypeName (Proxy @t)

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


  -- lift :: Matcher dbk t ()y -> Expr sc ty
  -- unlift :: Expr sc ty -> Matcher dbk ty

data MatcherK
  = EnumMatcher Type
  | PrimMatcher Type
  | SumMatcher (Maybe Char) Type (Type -> Type)
  | NoMatcher

type family DefMatcher (t :: Type) (dbObj :: DBObjK) :: MatcherK where
  DefMatcher t ('UDTypeObj ('UDEnum _)) = 'EnumMatcher t
  DefMatcher t ('UDTypeObj ('TaggedSum _ _)) = 'SumMatcher 'Nothing t Proxy
  DefMatcher t ('UDTypeObj ('TaggedSumMono _ _ _)) = 'SumMatcher 'Nothing t Proxy
  DefMatcher t _ = 'NoMatcher

data TypeBaseExpr (dbk :: DbK) (t :: Type)
  = RecBaseExpr [(Text, TypeBaseExpr dbk t)]
  | SumBaseExpr [(Text, TypeBaseExpr dbk t)]
  | PrimTypeExpr PQ.PrimExpr

data EnumMatchRep t = EnumMatchRep
  { ctors :: [(Text, t)]
  , enumMatcher :: (Text -> Int64 -> PQ.PrimExpr) -> t -> PQ.PrimExpr
  }

data NoMatcherRep = NoMatcherRep

data SumMatchRep (pfx :: Maybe Char) (t :: Type) (m :: Type -> Type) = SumMatchRep
  { ctors :: forall sc.[(Text, m sc)]
  , sumMatcher :: (Text -> Int64 -> PQ.PrimExpr -> PQ.PrimExpr) -> t -> PQ.PrimExpr
  }


data PrimMatchRep t = PrimMatchRep

type family GetMatcherRep (mat :: MatcherK) :: Type where
  GetMatcherRep ('EnumMatcher m) = EnumMatchRep m
  GetMatcherRep ('PrimMatcher m) = PrimMatchRep m
  GetMatcherRep ('SumMatcher pfx t m) = SumMatchRep pfx t m
  GetMatcherRep 'NoMatcher = NoMatcherRep


-- data Ctor (dbk :: DbK) (t :: Type) where
--   Ctor :: Text -> (a -> t) -> (Matcher dbk t () -> f a) -> Ctor dbk t

-- data DeCtor (dbk :: DbK) (t :: Type) where
--   DeCtor :: Text -> (t -> Maybe (TypeBaseExpr dbk a)) -> (TypeBaseExpr dbk a -> Matcher dbk t ()) -> DeCtor dbk t

class HasSumRepr (dbObjK :: DBObjK) (dbk :: DbK) (t :: Type) (mat :: MatcherK) where
  sumRepr' :: Proxy '(dbObjK, dbk, t, mat) -> GetMatcherRep mat

instance (Generic m, GenHasEnumRepr dbk m (Rep m)) => HasSumRepr ('UDTypeObj ('UDEnum enk)) dbk t ('EnumMatcher m) where
  sumRepr' _ = EnumMatchRep { ctors = (fmap . fmap) to $ gEnumCtorUniv (Proxy @'(dbk, m, Rep m))
                            , enumMatcher = \f t -> gEnumMatcher (Proxy @'(dbk, m)) f (from t)
                            }

-- TODO: if we try to unify t ~ t1, we will hit a error
instance HasSumRepr ('UDTypeObj ('TaggedSum enk lay)) dbk t ('SumMatcher pfx t1 mt) where
  sumRepr' _ = undefined

instance HasSumRepr ('UDTypeObj ('TaggedSumMono enk cty lay)) dbk t ('SumMatcher pfx t1 mt) where
  sumRepr' _ = undefined

instance HasSumRepr ('UDTypeObj ('SumOfCol lay)) dbk t ('SumMatcher pfx t1 mt) where
  sumRepr' _ = undefined

instance HasSumRepr dbobj dbk t 'NoMatcher where
  sumRepr' _ = NoMatcherRep

instance HasSumRepr dbobj dbk t ('PrimMatcher t) where
  sumRepr' _ = PrimMatchRep

class GenHasEnumRepr (dbk :: DbK) (t :: Type) (rep :: Type -> Type) where
  gEnumCtorUniv :: Proxy '(dbk, t, rep) -> [(Text, rep t)]
  gEnumMatcher :: Proxy '(dbk, t) -> (Text -> Int64 -> PQ.PrimExpr) -> rep t -> PQ.PrimExpr

instance GenHasEnumRepr dbk t f => GenHasEnumRepr dbk t (D1 d f) where
  gEnumCtorUniv _ = (fmap . fmap) M1 $ gEnumCtorUniv (Proxy @'(dbk, t, f))
  gEnumMatcher p mat (M1 f) = gEnumMatcher p mat f

instance (GenHasEnumRepr dbk t f, GenHasEnumRepr dbk t g) => GenHasEnumRepr dbk t (f :+: g) where
  gEnumCtorUniv _ = ((fmap . fmap) L1 $ gEnumCtorUniv (Proxy @'(dbk, t, f))) ++
                   ((fmap . fmap) R1 $ gEnumCtorUniv (Proxy @'(dbk, t, g)))
  gEnumMatcher p mat (L1 f) = gEnumMatcher p mat f
  gEnumMatcher p mat (R1 g) = gEnumMatcher p mat g

instance (KnownSymbol cn) => GenHasEnumRepr dbk t (C1 ('MetaCons cn p isr) U1) where
  gEnumCtorUniv _ = [(T.pack $ symbolVal (Proxy @cn), M1 U1)]
  gEnumMatcher _ mat (M1 U1) = mat (T.pack $ symbolVal (Proxy @cn)) 1

instance TypeError ('Text "[DBR-123] Expecting only Sum Type with all constructor being Nullary") => GenHasEnumRepr dbk t (C1 c (S1 s k)) where
  gEnumCtorUniv = error "Panic: [DBR-123]: Unreachable code"
  gEnumMatcher = error "Panic: [DBR-123]: Unreachable code"

instance TypeError ('Text "[DBR-123] Expecting only Sum Type with all constructor being Nullary") => GenHasEnumRepr dbk t (C1 c (f :*: g)) where
  gEnumCtorUniv _ = error "Panic: [DBR-123]: Unreachable code"
  gEnumMatcher = error "Panic: [DBR-123]: Unreachable code"

class GenHasSumRepr (dbk :: DbK) (t :: Type) (rep :: Type -> Type) where
  gHasSumRepr :: Proxy '(dbk, t, rep) -> [(Text, ())]

instance GenHasSumRepr dbk t f => GenHasSumRepr dbk t (D1 d f) where
  gHasSumRepr _ = gHasSumRepr (Proxy @'(dbk, t, f))

instance (GenHasSumRepr dbk t f, GenHasSumRepr dbk t g) => GenHasSumRepr dbk t (f :+: g) where
  gHasSumRepr _ = gHasSumRepr (Proxy @'(dbk, t, f)) ++ gHasSumRepr (Proxy @'(dbk, t, g))

instance GenHasSumRepr dbk t (C1 c (S1 s (K1 k t))) where
  gHasSumRepr _ = undefined

instance GenHasSumRepr dbk t (C1 c U1) where
  gHasSumRepr _ = undefined

instance (TypeError ('Text "[DBR-123] Multi-Arity Constructor is not supported! " ':<>: 'ShowType t)) => GenHasSumRepr dbk t (C1 c (f :*: g)) where
  gHasSumRepr _ = undefined

class GenInjUnlifted (cn :: Symbol) (carg :: Type) (dbk :: DbK) (t :: Type) (rep :: Type -> Type) where
  gInjUnlifted :: Proxy '(cn, carg, dbk, t) -> Maybe (rep t)

instance GenInjUnlifted cn carg dbk t f => GenInjUnlifted cn carg dbk t (D1 d f) where
  gInjUnlifted _ = M1 <$> gInjUnlifted (Proxy @'(cn, carg, dbk, t))

instance (GenInjUnlifted cn carg dbk t f, GenInjUnlifted cn carg dbk t x) => GenInjUnlifted cn carg dbk t ((f :+: g) :+: x) where
  gInjUnlifted _ = R1 <$> gInjUnlifted (Proxy @'(cn, carg, dbk, t))

instance GenInjUnlifted cn carg dbk t (C1 ('MetaCons cn1 f isr) (S1 s (K1 k t))) where
  gInjUnlifted = undefined

data ConMatch (mat :: Bool) (t :: Type) where
  ConMatched :: t -> ConMatch 'True t
  ConNotMatched :: ConMatch 'False t

deriving instance Functor (ConMatch mat)

type E a b = a == b

-- data ConDeCons dbk t where
--   ConDeCons :: (expr pat -> Matcher dbk t)
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

deriving newtype instance (DBRepr dbk t, Matcher dbk t ~ (Matcher dbk (Identity t))) => DBRepr dbk (Identity t)

instance DBRepr dbk (CI t) where
  type ToDBType dbk (CI t) = 'NativeTypeObj ('TDBCiText)
  typeName = ""

instance DBRepr dbk TimeOfDay where
  type ToDBType dbk TimeOfDay = 'NativeTypeObj ('TDBTime 7)
  typeName = ""

instance DBRepr dbk Bool where
  type ToDBType dbk Bool = 'NativeTypeObj 'TDBBool
  type Matcher dbk Bool = 'PrimMatcher Bool
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

newtype AsEnum t = AsEnum t

instance (Generic t, GenHasEnumRepr dbk t (Rep t), KnownSymbol (GenTyCon (Rep t))) => DBRepr dbk (AsEnum t) where
  type ToDBType dbk (AsEnum t) = 'UDTypeObj ('UDEnum (GetDBEnumK dbk))
  type Matcher dbk (AsEnum t) = 'EnumMatcher t
  typeName = TypeName $ genDBTypeName (Proxy @t)

newtype AsEnumText t = AsEnumText t

instance (Generic t, GenHasEnumRepr dbk t (Rep t), KnownSymbol (GenTyCon (Rep t))) => DBRepr dbk (AsEnumText t) where
  type ToDBType dbk (AsEnumText t) = 'UDTypeObj ('UDEnum 'EnumText)
  type Matcher dbk (AsEnumText t) = 'EnumMatcher t
  typeName = TypeName $ genDBTypeName (Proxy @t)

newtype AsEnumNum t = AsEnumNum t

instance (Generic t, GenHasEnumRepr dbk t (Rep t), KnownSymbol (GenTyCon (Rep t))) => DBRepr dbk (AsEnumNum t) where
  type ToDBType dbk (AsEnumNum t) = 'UDTypeObj ('UDEnum 'EnumNum)
  type Matcher dbk (AsEnumNum t) = 'EnumMatcher t
  typeName = TypeName $ genDBTypeName (Proxy @t)

newtype AsCompositeRec t = AsCompositeRec t

instance (Generic t, KnownSymbol (GenTyCon (Rep t))) => DBRepr 'Postgres (AsCompositeRec t) where
  type ToDBType 'Postgres (AsCompositeRec t) = 'UDTypeObj ('UDRec 'CompositeRec)
  typeName = TypeName $ genDBTypeName (Proxy @t)

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

instance DBRepr dbk (AsTaggedSumFlat t) where
  type ToDBType dbk (AsTaggedSumFlat t) = 'UDTypeObj ('TaggedSum (GetDBEnumK dbk) 'FlatRec)
  type Matcher dbk (AsTaggedSumFlat t) = 'SumMatcher 'Nothing t Proxy
  typeName = ""

newtype AsTaggedSumComposite t = AsTaggedSumComposite t

instance (Generic t, KnownSymbol (GenTyCon (Rep t))) => DBRepr 'Postgres (AsTaggedSumComposite t) where
  type ToDBType 'Postgres (AsTaggedSumComposite t) = 'UDTypeObj ('TaggedSum 'EnumType 'CompositeRec)
  type Matcher 'Postgres (AsTaggedSumComposite t) = 'SumMatcher 'Nothing t Proxy
  typeName = TypeName $ genDBTypeName (Proxy @t)

newtype AsTaggedSumJson t = AsTaggedSumJson t

instance DBRepr db (AsTaggedSumJson t) where
  type ToDBType db (AsTaggedSumJson t) = 'UDTypeObj ('TaggedSum (GetDBEnumK db) 'JsonRec)
  type Matcher db (AsTaggedSumJson t) = 'SumMatcher 'Nothing t Proxy
  typeName = ""

newtype AsTaggedSumMonoFlat colTy t = AsTaggedSumMonoFlat t

instance DBRepr db cty => DBRepr db (AsTaggedSumMonoFlat cty t) where
  type ToDBType db (AsTaggedSumMonoFlat cty t) = 'UDTypeObj ('TaggedSumMono (GetDBEnumK db) cty 'FlatRec)
  type Matcher db (AsTaggedSumMonoFlat cty t) = 'SumMatcher 'Nothing t Proxy
  typeName = ""

newtype AsTaggedSumMonoComposite colTy t = AsTaggedSumMonoComposite t

instance (DBRepr 'Postgres cty, Generic t, KnownSymbol (GenTyCon (Rep t))) => DBRepr 'Postgres (AsTaggedSumMonoComposite cty t) where
  type ToDBType 'Postgres (AsTaggedSumMonoComposite cty t) = 'UDTypeObj ('TaggedSumMono 'EnumType cty 'CompositeRec)
  type Matcher 'Postgres (AsTaggedSumMonoComposite cty t) = 'SumMatcher 'Nothing t Proxy
  typeName = TypeName $ genDBTypeName (Proxy @t)

newtype AsTaggedSumMonoJson colTy t = AsTaggedSumMonoJson t

instance DBRepr dbk cty => DBRepr dbk (AsTaggedSumMonoJson cty t) where
  type ToDBType dbk (AsTaggedSumMonoJson cty t) = 'UDTypeObj ('TaggedSumMono (GetDBEnumK dbk) cty 'JsonRec)
  type Matcher dbk (AsTaggedSumMonoJson cty t) = 'SumMatcher 'Nothing t Proxy
  typeName = ""

newtype AsSumOfColFlat t = AsSumOfColFlat t

instance DBRepr db (AsSumOfColFlat t) where
  type ToDBType db (AsSumOfColFlat t) = 'UDTypeObj ('SumOfCol 'FlatRec)
  typeName = ""

newtype AsSumOfColComposite t = AsSumOfColComposite t

instance (Generic t, KnownSymbol (GenTyCon (Rep t))) => DBRepr 'Postgres (AsSumOfColComposite t) where
  type ToDBType 'Postgres (AsSumOfColComposite t) = 'UDTypeObj ('SumOfCol 'CompositeRec)
  typeName = TypeName $ genDBTypeName (Proxy @t)

newtype AsSumOfColJson t = AsSumOfColJson t

instance DBRepr 'Postgres (AsSumOfColJson t) where
  type ToDBType 'Postgres (AsSumOfColJson t) = 'UDTypeObj ('SumOfCol 'JsonRec)
  typeName = ""

--newtype WithMatcher (m :: Type)  

instance DBRepr dbk (DBR.Key tab t) where
  type ToDBType dbk (DBR.Key tab t) = ToDBType dbk t
  type AutoCodec dbk (DBR.Key tab t) = AutoCodec dbk t
  type Matcher dbk (DBR.Key tab t) = 'NoMatcher
--  sumRepr _ = NonSumRepr
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
