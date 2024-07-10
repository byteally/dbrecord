-- {-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications, DataKinds, KindSignatures, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, TypeFamilyDependencies, UndecidableInstances, FlexibleInstances, OverloadedStrings, GADTs, TypeOperators, FlexibleContexts, DefaultSignatures, DerivingStrategies, LambdaCase #-}
module DBRecord.Internal.DBTypes where

import Data.Aeson as A
import Data.UUID (UUID)
import Data.Time.LocalTime
import Data.ByteString (ByteString)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.CaseInsensitive  (CI)
import Data.Int
import Data.Functor.Identity
import Data.Functor.Const
import Data.Foldable as F
-- import Data.Word
import Data.Scientific
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Text.Lazy as LT
import Data.Void
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Coerce
import Data.Proxy
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import DBRecord.Types (PGOID(..), PGOIDType(..), LTree{-, Interval-}, Json {-, JsonStr,-})
import qualified DBRecord.Types as DBR

import Data.Vector (Vector)
import DBRecord.Internal.Types
import DBRecord.Internal.Common
import DBRecord.Internal.Schema
import qualified DBRecord.Internal.PrimQuery as PQ
import qualified Data.Text as T
import GHC.Generics
import Data.Kind
import Data.String
import GHC.Records
import GHC.TypeLits
-- import GHC.Exts
-- import Data.Type.Bool
-- import Data.Type.Equality
import Data.Typeable
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
         , ValidatePfxConName ty fn 'U' (Rep ty) (UnconsSymbol fn)
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
  type Matcher dbk t = DefMatcher dbk t (ToDBType dbk t)

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

data MatcherK
  = EnumMatcher Type
  | PrimMatcher Type
  | SumMatcher DbK (Maybe Char) Type (Type -> Type)
  | NoMatcher

type family DefMatcher (dbk :: DbK) (t :: Type) (dbObj :: DBObjK) :: MatcherK where
  DefMatcher dbk t ('UDTypeObj ('UDEnum _)) = 'EnumMatcher t
  DefMatcher dbk t ('UDTypeObj ('TaggedSum _ _)) = TypeError ('Text "[DBRec-123] Pattern type not set for sum type: " ':<>: 'ShowType t ':<>: 'Text " for the database " ':<>: 'ShowType dbk)
  DefMatcher dbk t ('UDTypeObj ('TaggedSumMono _ _ _)) = TypeError ('Text "[DBRec-123] Pattern type not set for sum type: " ':<>: 'ShowType t ':<>: 'Text " for the database " ':<>: 'ShowType dbk)
  DefMatcher dbk t _ = 'NoMatcher

data EnumMatchRep t = EnumMatchRep
  { ctors :: [(Text, t)]
  , enumMatcher :: (Text -> Int64 -> PQ.PrimExpr) -> t -> PQ.PrimExpr
  }

data NoMatcherRep = NoMatcherRep

data SumMatchRep (dbk ::DbK) (pfx :: Maybe Char) (t :: Type) (m :: Type -> Type) = SumMatchRep
  { ctors :: forall sc. (Generic (m sc), GenHasSumRepr dbk t m sc (Rep (m sc))) => [(Text, Bool, Maybe PQ.PrimExpr -> m sc)]
  , sumMatcher :: forall (sc :: Type). (Generic t, GenSumMatcher dbk t sc (Rep t)) => Proxy sc -> (Text -> Int64 -> Maybe PQ.PrimExpr -> PQ.PrimExpr) -> t -> PQ.PrimExpr
  , sumConstructor :: forall (sc :: Type). (Generic (m sc), GenHasSumRepr dbk t m sc (Rep (m sc))) => m sc -> (Text -> Int64 -> Maybe PQ.PrimExpr -> Expr sc t) -> Expr sc t
  }


data PrimMatchRep t = PrimMatchRep

type family GetMatcherRep (mat :: MatcherK) :: Type where
  GetMatcherRep ('EnumMatcher m) = EnumMatchRep m
  GetMatcherRep ('PrimMatcher m) = PrimMatchRep m
  GetMatcherRep ('SumMatcher dbk pfx t m) = SumMatchRep dbk pfx t m
  GetMatcherRep 'NoMatcher = NoMatcherRep


class HasSumRepr (dbObjK :: DBObjK) (dbk :: DbK) (t :: Type) (mat :: MatcherK) where
  sumRepr' :: Proxy '(dbObjK, dbk, t, mat) -> GetMatcherRep mat

instance (Generic m, GenHasEnumRepr dbk m (Rep m)) => HasSumRepr ('UDTypeObj ('UDEnum enk)) dbk t ('EnumMatcher m) where
  sumRepr' _ = EnumMatchRep { ctors = (fmap . fmap) to $ gEnumCtorUniv (Proxy @'(dbk, m, Rep m))
                            , enumMatcher = \f t -> gEnumMatcher (Proxy @'(dbk, m)) f (from t)
                            }

instance (Coercible t t1) => HasSumRepr ('UDTypeObj ('TaggedSum enk lay)) dbk t ('SumMatcher dbk pfx t1 mt) where
  sumRepr' _ = SumMatchRep { ctors = getAllCons
                           , sumMatcher = \p f t -> genSumMatcher (withSumMat p) 0 f (from t)
                           , sumConstructor = \m f -> gSumConstructor (Proxy @'(dbk, mt)) (from m) f
                           }
    where
      getAllCons :: forall sc.(Generic (mt sc), GenHasSumRepr dbk t1 mt sc (Rep (mt sc))) => [(Text, Bool, Maybe PQ.PrimExpr -> mt sc)]
      getAllCons = (fmap . fmap . fmap) to $ gSumCtorUniv (Proxy @'(dbk, t1, mt, sc, Rep (mt sc)))
      withSumMat :: Proxy sc -> Proxy '(dbk, sc, t1)
      withSumMat _ = Proxy

instance (Coercible t t1) => HasSumRepr ('UDTypeObj ('TaggedSumMono enk cty lay)) dbk t ('SumMatcher dbk pfx t1 mt) where
  sumRepr' _ = SumMatchRep { ctors = getAllCons
                           , sumMatcher = \p f t -> genSumMatcher (withSumMat p) 0 f (from t)
                           , sumConstructor = \m f -> gSumConstructor (Proxy @'(dbk, mt)) (from m) f
                           }
    where
      getAllCons :: forall sc.(Generic (mt sc), GenHasSumRepr dbk t1 mt sc (Rep (mt sc))) => [(Text, Bool, Maybe PQ.PrimExpr -> mt sc)]
      getAllCons = (fmap . fmap . fmap) to $ gSumCtorUniv (Proxy @'(dbk, t1, mt, sc, Rep (mt sc)))
      withSumMat :: Proxy sc -> Proxy '(dbk, sc, t1)
      withSumMat _ = Proxy

instance (Coercible t t1) => HasSumRepr ('UDTypeObj ('SumOfCol lay)) dbk t ('SumMatcher dbk pfx t1 mt) where
  sumRepr' _ = SumMatchRep { ctors = getAllCons
                           , sumMatcher = \p f t -> genSumMatcher (withSumMat p) 0 f (from t)
                           , sumConstructor = \m f -> gSumConstructor (Proxy @'(dbk, mt)) (from m) f
                           }
    where
      getAllCons :: forall sc.(Generic (mt sc), GenHasSumRepr dbk t1 mt sc (Rep (mt sc))) => [(Text, Bool, Maybe PQ.PrimExpr -> mt sc)]
      getAllCons = (fmap . fmap . fmap) to $ gSumCtorUniv (Proxy @'(dbk, t1, mt, sc, Rep (mt sc)))
      withSumMat :: Proxy sc -> Proxy '(dbk, sc, t1)
      withSumMat _ = Proxy

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

class GenHasSumRepr (dbk :: DbK) (t :: Type) (m :: Type -> Type) (sc :: Type) (rep :: Type -> Type) where
  gSumCtorUniv :: Proxy '(dbk, t, m, sc, rep) -> [(Text, Bool, Maybe PQ.PrimExpr -> (rep (m sc)))]
  gSumConstructor :: Proxy '(dbk, m) -> rep (m sc) -> (Text -> Int64 -> Maybe PQ.PrimExpr -> Expr sc t) -> Expr sc t

instance GenHasSumRepr dbk t m sc f => GenHasSumRepr dbk t m sc (D1 d f) where
  gSumCtorUniv _ = (fmap . fmap . fmap) M1 $ gSumCtorUniv (Proxy @'(dbk, t, m, sc, f))
  gSumConstructor p (M1 f) = gSumConstructor p f

instance (GenHasSumRepr dbk t m sc f, GenHasSumRepr dbk t m sc g) => GenHasSumRepr dbk t m sc (f :+: g) where
  gSumCtorUniv _ = ((fmap . fmap . fmap) L1 $ gSumCtorUniv (Proxy @'(dbk, t, m, sc, f))) ++
                     ((fmap . fmap . fmap) R1 $ gSumCtorUniv (Proxy @'(dbk, t, m, sc, g)))
  gSumConstructor p (L1 f) = gSumConstructor p f
  gSumConstructor p (R1 f) = gSumConstructor p f

instance ( KnownSymbol cn
         , Generic t
         , Expr ~ e
         , Typeable t
         , Typeable a
         , ValidatePfxConName (m sc) cn 'U' (Rep t) (UnconsSymbol cn)
         ) => GenHasSumRepr dbk t m sc (C1 ('MetaCons cn p isr) (S1 s (K1 k (e (sc :: Type) (a :: Type))))) where
  gSumCtorUniv _ = [(T.pack $ drop 1 $ symbolVal (Proxy @cn), True, \case
                        Nothing -> error $ "Panic: [DBR-123]: Expected arg of type " ++ (show $ typeRep (Proxy @a)) ++ " for constructor " ++ (drop 1 $ symbolVal (Proxy @cn)) ++ " of type " ++ (show $ typeRep (Proxy @t))
                        Just pe -> M1 $ M1 $ K1 (Expr pe))]
  gSumConstructor _ (M1 (M1 (K1 (Expr e)))) f = f (T.pack $ drop 1 $ symbolVal (Proxy @cn)) 0 (Just e)

instance ( KnownSymbol cn
         , Generic t
         , ValidatePfxConName (m sc) cn 'U' (Rep t) (UnconsSymbol cn)
         ) => GenHasSumRepr dbk t m sc (C1 ('MetaCons cn p isr) U1) where
  gSumCtorUniv _ = [(T.pack $ drop 1 $ symbolVal (Proxy @cn), False, const $ M1 U1)]
  gSumConstructor _ (M1 U1) f = f (T.pack $ drop 1 $ symbolVal (Proxy @cn)) 0 Nothing

instance (TypeError ('Text "[DBR-123] Multi-Arity Constructor is not supported! " ':<>: 'ShowType m)) => GenHasSumRepr dbk t m sc (C1 c (f :*: g)) where
  gSumCtorUniv _ = error "Panic: [DBR-123]: Unreachable code"
  gSumConstructor _ _ = error "Panic: [DBR-123]: Unreachable code"

class GenSumMatcher (dbk :: DbK) (t :: Type) (sc :: Type) (rep :: Type -> Type) where
  genSumMatcher :: Proxy '(dbk, sc, t) -> Word -> (Text -> Int64 -> Maybe PQ.PrimExpr -> PQ.PrimExpr) -> rep t -> PQ.PrimExpr

instance (GenSumMatcher dbk t sc f) => GenSumMatcher dbk t sc (D1 d f) where
  genSumMatcher p pos f (M1 r) = genSumMatcher p pos f r

instance (GenSumMatcher dbk t sc f, GenSumMatcher dbk t sc g) => GenSumMatcher dbk t sc (f :+: g) where
  genSumMatcher p pos f (L1 r) = genSumMatcher p (pos+1) f r
  genSumMatcher p pos f (R1 r) = genSumMatcher p (pos+2) f r

instance ( DBRepr (DB (SchemaDB sc)) a
         , AutoConstExpr sc a (ToDBType (DB (SchemaDB sc)) a) (AutoCodec (DB (SchemaDB sc)) a)
         , KnownSymbol cn
         ) => GenSumMatcher dbk t sc (C1 ('MetaCons cn p isr) (S1 s (K1 k a))) where
  genSumMatcher _p pos f (M1 (M1 (K1 r))) = f (T.pack $ symbolVal (Proxy @cn)) (fromIntegral pos) (Just $ getExpr $ constExpr @a @sc r)

instance (KnownSymbol cn) => GenSumMatcher dbk t sc (C1 ('MetaCons cn p isr) U1) where
  genSumMatcher _p pos f (M1 U1) = f (T.pack $ symbolVal (Proxy @cn)) (fromIntegral pos) Nothing


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

newtype AsTaggedSumFlat (m :: Type -> Type) t = AsTaggedSumFlat t

instance (Generic t, KnownSymbol (GenTyCon (Rep t))) => DBRepr dbk (AsTaggedSumFlat m t) where
  type ToDBType dbk (AsTaggedSumFlat m t) = 'UDTypeObj ('TaggedSum (GetDBEnumK dbk) 'FlatRec)
  type Matcher dbk (AsTaggedSumFlat m t) = 'SumMatcher dbk 'Nothing t m
  typeName = TypeName $ genDBTypeName (Proxy @t)

newtype AsTaggedSumComposite (m :: Type -> Type) t = AsTaggedSumComposite t

instance (Generic t, KnownSymbol (GenTyCon (Rep t))) => DBRepr 'Postgres (AsTaggedSumComposite m t) where
  type ToDBType 'Postgres (AsTaggedSumComposite m t) = 'UDTypeObj ('TaggedSum 'EnumType 'CompositeRec)
  type Matcher 'Postgres (AsTaggedSumComposite m t) = 'SumMatcher 'Postgres 'Nothing t m
  typeName = TypeName $ genDBTypeName (Proxy @t)

newtype AsTaggedSumJson (m :: Type -> Type) t = AsTaggedSumJson t

instance (Generic t, KnownSymbol (GenTyCon (Rep t))) => DBRepr db (AsTaggedSumJson m t) where
  type ToDBType db (AsTaggedSumJson m t) = 'UDTypeObj ('TaggedSum (GetDBEnumK db) 'JsonRec)
  type Matcher db (AsTaggedSumJson m t) = 'SumMatcher db 'Nothing t m
  typeName = TypeName $ genDBTypeName (Proxy @t)

newtype AsTaggedSumMonoFlat colTy (m :: Type -> Type) t = AsTaggedSumMonoFlat t

instance (DBRepr db cty, Generic t, KnownSymbol (GenTyCon (Rep t))) => DBRepr db (AsTaggedSumMonoFlat cty m t) where
  type ToDBType db (AsTaggedSumMonoFlat cty m t) = 'UDTypeObj ('TaggedSumMono (GetDBEnumK db) cty 'FlatRec)
  type Matcher db (AsTaggedSumMonoFlat cty m t) = 'SumMatcher db 'Nothing t m
  typeName = TypeName $ genDBTypeName (Proxy @t)

newtype AsTaggedSumMonoComposite colTy (m :: Type -> Type) t = AsTaggedSumMonoComposite t

instance (DBRepr 'Postgres cty, Generic t, KnownSymbol (GenTyCon (Rep t))) => DBRepr 'Postgres (AsTaggedSumMonoComposite cty m t) where
  type ToDBType 'Postgres (AsTaggedSumMonoComposite cty m t) = 'UDTypeObj ('TaggedSumMono 'EnumType cty 'CompositeRec)
  type Matcher 'Postgres (AsTaggedSumMonoComposite cty m t) = 'SumMatcher 'Postgres 'Nothing t m
  typeName = TypeName $ genDBTypeName (Proxy @t)

newtype AsTaggedSumMonoJson colTy (m :: Type -> Type) t = AsTaggedSumMonoJson t

instance DBRepr dbk cty => DBRepr dbk (AsTaggedSumMonoJson cty m t) where
  type ToDBType dbk (AsTaggedSumMonoJson cty m t) = 'UDTypeObj ('TaggedSumMono (GetDBEnumK dbk) cty 'JsonRec)
  type Matcher dbk (AsTaggedSumMonoJson cty m t) = 'SumMatcher dbk 'Nothing t m
  typeName = ""

newtype AsSumOfColFlat (m :: Type -> Type) t = AsSumOfColFlat t

instance DBRepr db (AsSumOfColFlat m t) where
  type ToDBType db (AsSumOfColFlat m t) = 'UDTypeObj ('SumOfCol 'FlatRec)
  type Matcher db (AsSumOfColFlat m t) = 'SumMatcher db 'Nothing t m
  typeName = ""

newtype AsSumOfColComposite (m :: Type -> Type) t = AsSumOfColComposite t

instance (Generic t, KnownSymbol (GenTyCon (Rep t))) => DBRepr 'Postgres (AsSumOfColComposite m t) where
  type ToDBType 'Postgres (AsSumOfColComposite m t) = 'UDTypeObj ('SumOfCol 'CompositeRec)
  type Matcher 'Postgres (AsSumOfColComposite m t) = 'SumMatcher 'Postgres 'Nothing t m
  typeName = TypeName $ genDBTypeName (Proxy @t)

newtype AsSumOfColJson (m :: Type -> Type) t = AsSumOfColJson t

instance DBRepr 'Postgres (AsSumOfColJson m t) where
  type ToDBType 'Postgres (AsSumOfColJson m t) = 'UDTypeObj ('SumOfCol 'JsonRec)
  type Matcher 'Postgres (AsSumOfColJson m t) = 'SumMatcher 'Postgres 'Nothing t m
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
class HasDiscriminator (enumk :: UDEnumK) (sc :: Type) (ty :: Type) where
  getDiscriminator :: Proxy '(enumk, sc, ty) -> Text -> Int64 -> PQ.PrimExpr

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, Database (SchemaDB sc), Schema sc) => HasDiscriminator 'EnumType sc ty where
  getDiscriminator _ cn _ =
    let
      cname = case lookupConName cn Nothing (conAliases @(DB (SchemaDB sc)) @ty) of
                Left cn' -> cn'
                Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @ty))
      qual = DBQualified
             (_getDatabaseName $ databaseName @(SchemaDB sc))
             (_getSchemaName $ schemaName @sc)
      discTyN = DBTypeName qual (_getTypeName $ discriminatorTypeName @(DB (SchemaDB sc)) @ty) []
    in PQ.CastExpr (OtherType discTyN) (PQ.ConstExpr (PQ.String cname))

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty) => HasDiscriminator 'EnumText sc ty where
  getDiscriminator _ cn _ =
    let
      cname = case lookupConName cn Nothing (conAliases @(DB (SchemaDB sc)) @ty) of
                Left cn' -> cn'
                Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @ty))
    in PQ.ConstExpr (PQ.String cname)

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty) => HasDiscriminator 'EnumNum sc ty where
  getDiscriminator _ cn cpos =
    let
      ctag = case lookupConName cn (Just cpos) (conAliases @(DB (SchemaDB sc)) @ty) of
               Left _ -> error $ "Panic: Expecting only Int64, not Text as tag for: " ++ (show $ typeRep (Proxy @ty))
               Right ct -> ct
    in PQ.ConstExpr (PQ.Integer $ toInteger ctag)

--
typeConstExpr :: forall a sc.(DBRepr (DB (SchemaDB sc)) a, TypeConstExpr sc a (Fields a)) => a -> (NonEmpty (Text, PQ.PrimExpr) -> Expr sc a) -> Expr sc a
typeConstExpr = typeConstExpr_ (Proxy @(Fields a)) []

class TypeConstExpr sc a (flds :: [(Symbol, Type)]) where
  typeConstExpr_ :: Proxy flds -> [(Text, PQ.PrimExpr)] -> a -> (NonEmpty (Text, PQ.PrimExpr) -> Expr sc a) -> Expr sc a

instance TypeError ('Text "[DBR-123] Expecting record type with named fields! " ':<>: 'ShowType a ':<>: 'Text " does not have fields") => TypeConstExpr sc a '[] where
  typeConstExpr_ = error "Panic: Unreachable code: [DBR-123]"

instance ( HasField f1 a ft
         , DBRepr (DB (SchemaDB sc)) f1t
         , DBRepr (DB (SchemaDB sc)) a
         , KnownSymbol f1
         , TypeConstExpr sc a (x2 : xs)
         , AutoConstExpr sc f1t (ToDBType (DB (SchemaDB sc)) f1t) (AutoCodec (DB (SchemaDB sc)) f1t)
         , ft ~ f1t
         ) => TypeConstExpr sc a ('(f1, f1t) ': (x2 ': xs)) where
  typeConstExpr_ _ acc a f = typeConstExpr_ (Proxy @(x2 ': xs)) ((fname, getExpr (constExpr @f1t @sc (getField @f1 a))) : acc) a f
    where
      fname = getConst $ getAliasedFieldName @f1 @a @(DB (SchemaDB sc)) @ft fieldAliases

instance ( HasField f1 a ft
         , DBRepr (DB (SchemaDB sc)) f1t
         , DBRepr (DB (SchemaDB sc)) a
         , KnownSymbol f1
         , AutoConstExpr sc f1t (ToDBType (DB (SchemaDB sc)) f1t) (AutoCodec (DB (SchemaDB sc)) f1t)
         , ft ~ f1t
         ) => TypeConstExpr sc a ('(f1, f1t) ': '[]) where
  typeConstExpr_ _ acc a f =
    case reverse ((fname, getExpr (constExpr @f1t @sc (getField @f1 a))) : acc) of
      [] -> error "Panic: Invariant [DBR-123] violated: Fields cannot be empty"
      (e : es) -> f (e :| es)
    where
      fname = getConst $ getAliasedFieldName @f1 @a @(DB (SchemaDB sc)) @ft fieldAliases

--
class ConstPrimExpr (dbk :: DbK) (t :: Type) where
  constPrimExpr :: Proxy dbk -> t -> PQ.PrimExpr
--
class ConstExpr sc t where
  toConstExpr :: t -> Expr sc t

class AutoConstExpr (sc :: Type) t (dbObj :: DBObjK) (isAuto :: Bool) where
  autoConstExpr :: Proxy '(dbObj, isAuto) -> t -> Expr sc t

-- TODO: Add TypeError for `'TableObj`
instance ConstExpr sc t => AutoConstExpr sc t dbObj 'False where
  autoConstExpr _ = toConstExpr

instance TypeError ('Text "Unexpected Table in place of Type" ':<>: 'ShowType t) => AutoConstExpr sc t 'TableObj 'True where
  autoConstExpr = error "Panic: Unreachable code"

instance ConstExpr sc t => AutoConstExpr sc t ('NativeTypeObj nat) 'True where
  autoConstExpr _ = toConstExpr

instance (DBTypeOf sc t, TypeConstExpr sc t (Fields t)) => AutoConstExpr sc t ('UDTypeObj ('UDRec 'CompositeRec)) 'True where
  autoConstExpr _ t =
    typeConstExpr t (\fs -> annotateType @t (Expr $ PQ.RowExpr (fmap snd $ NE.toList fs)))

instance (DBRepr (DB (SchemaDB sc)) t, TypeConstExpr sc t (Fields t)) => AutoConstExpr sc t ('UDTypeObj ('UDRec 'FlatRec)) 'True where
  autoConstExpr _ t = typeConstExpr t (\fs -> Expr $ PQ.FlatComposite $ NE.toList fs)

instance (A.ToJSON t, DBRepr (DB (SchemaDB sc)) t) => AutoConstExpr sc t ('UDTypeObj ('UDRec 'JsonRec)) 'True where
  autoConstExpr _ t = unsafeCoerceExpr $ constExpr $ A.toJSON t

instance ( DBRepr (DB (SchemaDB sc)) t
         , HasDiscriminator enk sc t
         , Matcher (DB (SchemaDB sc)) t ~ 'EnumMatcher t
         ) => AutoConstExpr sc t ('UDTypeObj ('UDEnum enk)) 'True where
  autoConstExpr _ t =
    let
      eMatcher = enumMatcher $ sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      mat cn cpos = getDiscriminator (Proxy @'(enk, sc, t)) cn cpos
    in Expr $ eMatcher mat t

instance ( HasDiscriminator enk sc t
         , DBRepr (DB (SchemaDB sc)) t
         , DBTypeOf sc t
         , Generic t
         , GenSumMatcher (DB (SchemaDB sc)) t sc (Rep t)
         , Matcher (DB (SchemaDB sc)) t ~ 'SumMatcher (DB (SchemaDB sc)) pfx t m
         ) => AutoConstExpr sc t ('UDTypeObj ('TaggedSum enk 'FlatRec)) 'True where
  autoConstExpr _ t =
    let
      SumMatchRep { sumMatcher = sMatcher } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      mat cn cpos cargM =
        let
          discPE = getDiscriminator (Proxy @'(enk, sc, t)) cn cpos
          discTag = _getDiscriminatorTagName $ discriminatorTagName @(DB (SchemaDB sc)) @t
        in PQ.FlatComposite $ (discTag, discPE) : case cargM of
          Nothing -> []
          Just carg -> [(cn, carg)]
    in Expr $ sMatcher @sc Proxy mat t

instance ( HasDiscriminator enk sc t
         , DBRepr (DB (SchemaDB sc)) t
         , Generic t
         , GenSumMatcher (DB (SchemaDB sc)) t sc (Rep t)
         , Matcher (DB (SchemaDB sc)) t ~ 'SumMatcher (DB (SchemaDB sc)) pfx t m
         , Generic (m sc)
         , GenHasSumRepr (DB (SchemaDB sc)) t m sc (Rep (m sc))
         ) => AutoConstExpr sc t ('UDTypeObj ('TaggedSum enk 'CompositeRec)) 'True where
  autoConstExpr _ t =
    let
      SumMatchRep { sumMatcher = sMatcher } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      allCons = getPatArgs (Proxy @'(Matcher (DB (SchemaDB sc)) t, sc , t)) $ sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      mat cn cpos cargM =
        let
          discPE = getDiscriminator (Proxy @'(enk, sc, t)) cn cpos
        in PQ.RowExpr $ discPE : (catMaybes $ fmap (\(cpos', (_, hasArg, _)) -> case (cpos == cpos', hasArg) of
                                          (True, _) -> cargM
                                          (False, False) -> Nothing
                                          (False, True) -> Just $ PQ.ConstExpr PQ.Null
                                      ) (zip [1 .. ] allCons))
    in Expr $ sMatcher @sc Proxy mat t

instance (Generic t, TypeError ('Text "TODO: @AutoConstExpr TaggedSum")) => AutoConstExpr sc t ('UDTypeObj ('TaggedSum enk 'JsonRec)) 'True where
  autoConstExpr _ _t = error "Panic: TODO"

instance ( HasDiscriminator enk sc t
         , DBRepr (DB (SchemaDB sc)) t
         , DBTypeOf sc t
         , Generic t
         , GenSumMatcher (DB (SchemaDB sc)) t sc (Rep t)
         , Matcher (DB (SchemaDB sc)) t ~ 'SumMatcher (DB (SchemaDB sc)) pfx t m
         ) => AutoConstExpr sc t ('UDTypeObj ('TaggedSumMono enk colty 'FlatRec)) 'True where
  autoConstExpr _ t =
    let
      SumMatchRep { sumMatcher = sMatcher } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      mat cn cpos cargM =
        let
          discPE = getDiscriminator (Proxy @'(enk, sc, t)) cn cpos
          discTag = _getDiscriminatorTagName $ discriminatorTagName @(DB (SchemaDB sc)) @t
          tyN = _getTypeName $ typeName @(DB (SchemaDB sc)) @t
        in PQ.FlatComposite $ (discTag, discPE) : case cargM of
          Nothing -> []
          Just carg -> [(tyN, carg)]
    in Expr $ sMatcher @sc Proxy mat t

instance ( HasDiscriminator enk sc t
         , DBRepr (DB (SchemaDB sc)) t
         , Generic t
         , GenSumMatcher (DB (SchemaDB sc)) t sc (Rep t)
         , Matcher (DB (SchemaDB sc)) t ~ 'SumMatcher (DB (SchemaDB sc)) pfx t m
         ) => AutoConstExpr sc t ('UDTypeObj ('TaggedSumMono enk colty 'CompositeRec)) 'True where
  autoConstExpr _ t =
    let
      SumMatchRep { sumMatcher = sMatcher } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      mat cn cpos cargM =
        let
          discPE = getDiscriminator (Proxy @'(enk, sc, t)) cn cpos
        in PQ.RowExpr $ discPE : (maybe [] (:[]) cargM)
    in Expr $ sMatcher @sc Proxy mat t

instance (Generic t, TypeError ('Text "TODO: @AutoConstExpr TaggedSumMono")) => AutoConstExpr sc t ('UDTypeObj ('TaggedSumMono enk colty 'JsonRec)) 'True where
  autoConstExpr _ _t = error "Panic: TODO"

instance ( DBRepr (DB (SchemaDB sc)) t
         , Typeable t
         , Generic t
         , GenSumMatcher (DB (SchemaDB sc)) t sc (Rep t)
         , Matcher (DB (SchemaDB sc)) t ~ 'SumMatcher (DB (SchemaDB sc)) pfx t m
         ) => AutoConstExpr sc t ('UDTypeObj ('SumOfCol 'FlatRec)) 'True where
  autoConstExpr _ t =
    let
      SumMatchRep { sumMatcher = sMatcher } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      mat cn _ = \case
        Nothing -> error $ "Panic: [DBR-123]: Unexpected nullary constructor: " ++ (T.unpack cn) ++ " for sum type " ++ (show $ typeRep (Proxy @t)) ++ " while using SumOfCol synthesis"
        Just carg -> PQ.FlatComposite [(cn, carg)]
    in Expr $ sMatcher @sc Proxy mat t


instance ( DBRepr (DB (SchemaDB sc)) t
         , Typeable t
         , Generic t
         , GenSumMatcher (DB (SchemaDB sc)) t sc (Rep t)
         , Generic (m sc)
         , GenHasSumRepr (DB (SchemaDB sc)) t m sc (Rep (m sc))
         , Matcher (DB (SchemaDB sc)) t ~ 'SumMatcher (DB (SchemaDB sc)) pfx t m
         ) => AutoConstExpr sc t ('UDTypeObj ('SumOfCol 'CompositeRec)) 'True where
  autoConstExpr _ t =
    let
      SumMatchRep { sumMatcher = sMatcher } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      allCons = getPatArgs (Proxy @'(Matcher (DB (SchemaDB sc)) t, sc , t)) $ sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      mat cn cpos = \case
        Nothing -> error $ "Panic: [DBR-123]: Unexpected nullary constructor: " ++ (T.unpack cn) ++ " for sum type " ++ (show $ typeRep (Proxy @t)) ++ " while using SumOfCol synthesis"
        Just carg -> PQ.RowExpr $ fmap (\(cpos', _) ->
                                           if cpos == cpos'
                                           then carg
                                           else PQ.ConstExpr PQ.Null
                                       ) (zip [1..] allCons)
    in Expr $ sMatcher @sc Proxy mat t

instance (Generic t, TypeError ('Text "TODO: @AutoConstExpr SumOfCol")) => AutoConstExpr sc t ('UDTypeObj ('SumOfCol 'JsonRec)) 'True where
  autoConstExpr _ _t = error "Panic: TODO"

instance (A.ToJSON t, DBTypeOf sc t) => AutoConstExpr sc t ('UDTypeObj ('SerializedBlob ('JsonContent 'Nothing))) 'True where
  autoConstExpr _ t = jsonOf t

instance (Show t) => AutoConstExpr sc t ('UDTypeObj ('SerializedBlob ('TextContent 'Nothing))) 'True where
  autoConstExpr _ t = Expr . PQ.ConstExpr . PQ.String . T.pack . show $ t

instance (t ~ ety, AutoConstExpr sc ety edbk 'True) => AutoConstExpr sc (Maybe t) ('NullableObjOf ety edbk) 'True where
  autoConstExpr _ = \case
    Nothing -> nothing
    Just t -> toNullable $ autoConstExpr (Proxy @'(edbk, 'True)) t

instance (t ~ ety, AutoConstExpr sc ety edbk 'True, Foldable f, Functor f, DBTypeOf sc (f ety)) => AutoConstExpr sc (f t) ('ArrayObjOf ety edbk) 'True where
  autoConstExpr _ ts = arrayF (fmap (autoConstExpr (Proxy @'(edbk, 'True))) ts)
--
type DBTypeOf sc a = ( DBRepr (DB (SchemaDB sc)) a
                     , ReifyTypeName sc a (ToDBType (DB (SchemaDB sc)) a)
                     )

dbTypeOf :: forall a sc.DBTypeOf sc a => Proxy (sc, a) -> DBType
dbTypeOf _ = reifyTypeName (Proxy :: Proxy '(sc, a, ToDBType (DB (SchemaDB sc)) a))

class ReifyTypeName (sc :: Type) (a :: Type) (dbObj :: DBObjK) where
  reifyTypeName :: Proxy '(sc, a, dbObj) -> DBType

instance (TypeError ('Text "Table is used as Type")) => ReifyTypeName sc a 'TableObj where
  reifyTypeName = error "Panic: Unreachable code"

instance (DBRepr (DB (SchemaDB sc)) a, Database (SchemaDB sc), Schema sc) => ReifyTypeName sc a ('UDTypeObj udt) where
  reifyTypeName _ = OtherType $ DBTypeName qual (_getTypeName $ typeName @(DB (SchemaDB sc)) @a) []
    where
      qual = DBQualified
             (_getDatabaseName $ databaseName @(SchemaDB sc))
             (_getSchemaName $ schemaName @sc)

instance (SingI dbt, DBTypeCtx dbt) => ReifyTypeName sc a ('NativeTypeObj dbt) where
  reifyTypeName _ = fromSing (sing :: Sing dbt)

instance (DBTypeOf sc ty, DBRepr (DB (SchemaDB sc)) ty, ReifyTypeName sc ty (ToDBType (DB (SchemaDB sc)) ty) ) => ReifyTypeName sc a ('NewtypeObj ty) where
  reifyTypeName _ = dbTypeOf (Proxy @(sc, ty))

instance ReifyTypeName sc e dbObj => ReifyTypeName sc c ('ArrayObjOf e dbObj) where
  reifyTypeName _ = DBArray $ reifyTypeName (Proxy @'(sc, e, dbObj))

instance ReifyTypeName sc e dbObj => ReifyTypeName sc opt ('NullableObjOf e dbObj) where
  reifyTypeName _ = DBNullable $ reifyTypeName (Proxy @'(sc, e, dbObj))

--
newtype Expr (sc :: Type) (t :: Type) =
  Expr PQ.PrimExpr
  deriving Show

getExpr :: Expr (sc :: Type) (t :: Type) -> PQ.PrimExpr
getExpr (Expr e) = e

unsafeCast :: DBType -> Expr sc a -> Expr sc b
unsafeCast castTo (Expr expr) = Expr $ PQ.CastExpr castTo expr

annotateType :: forall a sc.
  ( DBTypeOf sc a
  ) => Expr sc a -> Expr sc a
annotateType (Expr e) = Expr $ PQ.CastExpr (dbTypeOf (Proxy @(sc, a))) e
{-# INLINE annotateType #-}

unsafeCoerceExpr :: Expr sc a -> Expr sc b
unsafeCoerceExpr (Expr e) = Expr e

unsafeCol :: [T.Text] -> Expr sc a
unsafeCol = Expr . PQ.unsafeAttrExpr



-- TODO: Without Region Parameter it is not safe to have these instance
instance (DBRepr (DB (SchemaDB sc)) t, HasField '(fn, ToDBType (DB (SchemaDB sc)) t) (Expr sc t) a) => HasField (fn :: Symbol) (Expr sc t) a where
  getField e = getField @'(fn, ToDBType (DB (SchemaDB sc)) t) e

instance (HasField fn t a, KnownSymbol fn) => HasField '(fn :: Symbol, 'TableObj) (Expr sc t) (Expr sc a) where
  getField (Expr (PQ.FlatComposite es)) =
    let
      cname = T.pack $ symbolVal (Proxy @fn)
    in case lookup cname es of
         Just t -> Expr t
         _      -> error $ "Panic: Impossible case! Field not found: " ++ show (cname, fmap fst es)
  getField (Expr _e) = error $ "Panic: Impossible case! Expected Flat Composite but got: " <> (show _e)


instance (HasField '(fn, dbrepr) (Expr sc t) (Expr sc a), HasField fn t a, KnownSymbol fn) => HasField '(fn :: Symbol, 'NullableObjOf t dbrepr) (Expr sc (Maybe t)) (Expr sc (Maybe a)) where
  getField e = toMaybe $ getField @'(fn, dbrepr) (unsafeUnMaybe e)
    where
      unsafeUnMaybe :: Expr sc (Maybe x) -> Expr sc x
      unsafeUnMaybe (Expr ex) = Expr ex
      toMaybe :: Expr sc x -> Expr sc (Maybe x)
      toMaybe (Expr ex) = Expr ex

instance (DBRepr (DB (SchemaDB sc)) t, HasField fn t a, KnownSymbol fn) => HasField '(fn :: Symbol, 'UDTypeObj ('UDRec 'CompositeRec)) (Expr sc t) (Expr sc a) where
  getField (Expr (PQ.FlatComposite _es)) = error "Panic: Unexpected Flat Composite"
  getField (Expr e) =
    let
      fldN = getConst $ getAliasedFieldName @fn @t @(DB (SchemaDB sc)) @a fieldAliases
    in Expr (PQ.CompositeExpr e fldN)

instance (DBRepr (DB (SchemaDB sc)) t, HasField fn t a, KnownSymbol fn) => HasField '(fn :: Symbol, 'UDTypeObj ('UDRec 'FlatRec)) (Expr sc t) (Expr sc a) where
  getField (Expr (PQ.FlatComposite es)) =
    let
      fldN = getConst $ getAliasedFieldName @fn @t @(DB (SchemaDB sc)) @a fieldAliases
    in case lookup fldN es of
         Just t -> Expr t
         _      -> error "Panic: Impossible case! Field not found"
  getField (Expr _e) = error $ "Panic: Impossible case! Expected Flat Composite but got: " <> (show _e)

instance (DBRepr (DB (SchemaDB sc)) t, HasField fn t a, TypeError ('Text "TODO @ HasField 'UDRec 'JsonRec")) => HasField '(fn :: Symbol, 'UDTypeObj ('UDRec 'JsonRec)) (Expr sc t) (Expr sc a) where
  getField = error "Panic: TODO"

instance (TypeError ('ShowType t ':<>: 'Text " does not have field " ':<>: 'ShowType fn)) => HasField '(fn :: Symbol, 'UDTypeObj ('UDEnum enk)) (Expr sc t) Void where
  getField = error "Panic: Unreachable code"

instance (TypeError ('ShowType t ':<>: 'Text " does not have field " ':<>: 'ShowType fn)) => HasField '(fn :: Symbol, 'UDTypeObj ('TaggedSum enk lay)) (Expr sc t) Void where
  getField = error "Panic: Unreachable code"

instance (TypeError ('ShowType t ':<>: 'Text " does not have field " ':<>: 'ShowType fn)) => HasField '(fn :: Symbol, 'UDTypeObj ('TaggedSumMono enk ct lay)) (Expr sc t) Void where
  getField = error "Panic: Unreachable code"

instance (TypeError ('ShowType t ':<>: 'Text " does not have field " ':<>: 'ShowType fn)) => HasField '(fn :: Symbol, 'UDTypeObj ('SumOfCol enk)) (Expr sc t) Void where
  getField = error "Panic: Unreachable code"

instance (TypeError ('ShowType t ':<>: 'Text " does not have field " ':<>: 'ShowType fn)) => HasField '(fn :: Symbol, 'UDTypeObj ('SerializedBlob ct)) (Expr sc t) Void where
  getField = error "Panic: Unreachable code"

newtype AggExpr (sc :: Type) (t :: Type) =
  AggExpr { getAggExpr :: Expr sc t }
  deriving Show

type family PatArg (sc :: Type) (t :: Type) (mat :: MatcherK) :: Type where
  PatArg sc t ('EnumMatcher mat) = mat
  PatArg sc t ('PrimMatcher mat) = mat
  PatArg sc _ ('SumMatcher _ _ _ mat) = mat sc
  PatArg sc t 'NoMatcher = Void

match :: forall r t sc.
  ( DBRepr (DB (SchemaDB sc)) t
  , Match (ToDBType (DB (SchemaDB sc)) t) sc t
  , GetPatArgs (Matcher (DB (SchemaDB sc)) t) sc t
  ) => Expr sc t -> (PatArg sc t (Matcher (DB (SchemaDB sc)) t) -> Expr sc r) -> Expr sc r
match scrut =
  let
    srepr = getPatArgs (Proxy @'(Matcher (DB (SchemaDB sc)) t, sc , t)) $ sumRepr (Proxy @'((DB (SchemaDB sc)), t))
  in match' (Proxy @(ToDBType (DB (SchemaDB sc)) t)) srepr scrut

matchTag :: Expr sc t -> (PatArg sc t (Matcher (DB (SchemaDB sc)) t) -> Bool) -> Expr sc Bool
matchTag = error "TODO:"

class GetPatArgs (matK :: MatcherK) (sc :: Type) (t :: Type) where
  getPatArgs :: Proxy '(matK, sc, t) -> GetMatcherRep matK -> [(Text, Bool, Maybe PQ.PrimExpr -> PatArg sc t matK)]

instance GetPatArgs ('EnumMatcher m) sc t where
  getPatArgs _ EnumMatchRep {ctors = ectors} = fmap (\(cn, c) -> (cn, False, const c)) ectors

instance (Generic (m sc), GenHasSumRepr dbk t m sc (Rep (m sc))) => GetPatArgs ('SumMatcher dbk pfx t m) sc t where
  getPatArgs _ SumMatchRep {ctors = sctors} = sctors @sc

instance GetPatArgs ('PrimMatcher m) sc t where
  getPatArgs _ _ = []

instance GetPatArgs ('NoMatcher) sc t where
  getPatArgs _ _ = []

class Match (dbrep :: DBObjK) (sc :: Type) (scrut :: Type) where
  match' :: Proxy dbrep -> [(Text, Bool, Maybe PQ.PrimExpr -> PatArg sc scrut (Matcher (DB (SchemaDB sc)) scrut))] -> Expr sc scrut -> (PatArg sc scrut (Matcher (DB (SchemaDB sc)) scrut) -> Expr sc r) -> Expr sc r

instance Match ('NativeTypeObj ty) sc Bool where
  match' _ _ scrut caseF = ifThenElse scrut (caseF True) (caseF False)

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, HasDiscriminator enumk sc ty) => Match ('UDTypeObj ('UDEnum enumk)) sc ty where
  match' _ allCons scrut caseF =
    case_ (fmap (\(cpos, (cn, _, c)) ->
                   let
                     discPE = getDiscriminator (Proxy @'(enumk, sc, ty)) cn cpos
                   in (Expr (PQ.BinExpr PQ.OpEq (getExpr scrut) discPE), caseF (c Nothing))
                ) (zip [1..] allCons)) (Expr $ PQ.ConstExpr PQ.Null)


instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, HasDiscriminator enk sc ty) => Match ('UDTypeObj ('TaggedSum enk 'FlatRec)) sc ty where
  match' _ allCons (Expr (PQ.FlatComposite es)) caseF =
    case_ (catMaybes $ fmap (\(cpos, (cn, hasArg, c)) ->
                   let
                     discFld = case es of
                       [] -> error $ "Panic: Impossible case! Discriminator not found: " ++ T.unpack cn
                       ((_, e) : _) -> e
                     discPE = getDiscriminator (Proxy @'(enk, sc, ty)) cn cpos
                     carg' = lookup cn es
                   in if hasArg
                      then fmap (\carg -> (Expr (PQ.BinExpr PQ.OpEq discFld discPE), caseF (c $ Just carg))) carg'
                      else Just (Expr (PQ.BinExpr PQ.OpEq discFld discPE), caseF (c Nothing))
                ) (zip [1..] allCons)) (Expr $ PQ.ConstExpr PQ.Null)
  match' _ _ (Expr _e) _ = error $ "Panic: Impossible case! Expected Flat Composite but got: " <> (show _e)

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, HasDiscriminator enk sc ty) => Match ('UDTypeObj ('TaggedSum enk 'CompositeRec)) sc ty where
  match' _ allCons scrut caseF =
    case_ (fmap (\(cpos, (cn, hasArg, c)) ->
                   let
                     discFld = PQ.CompositeExpr (getExpr scrut) (_getDiscriminatorTagName $ discriminatorTagName @(DB (SchemaDB sc)) @ty)
                     discPE = getDiscriminator (Proxy @'(enk, sc, ty)) cn cpos
                     carg = PQ.CompositeExpr (getExpr scrut) (defHSNameToDBName cn)
                   in if hasArg
                      then (Expr (PQ.BinExpr PQ.OpEq discFld discPE), caseF (c $ Just carg))
                      else (Expr (PQ.BinExpr PQ.OpEq discFld discPE), caseF (c Nothing))
                ) (zip [1..] allCons)) (Expr $ PQ.ConstExpr PQ.Null)

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, HasDiscriminator enk sc ty, TypeError ('Text "TODO: @Match ('UDTypeObj ('TaggedSum enk 'JsonRec))")) => Match ('UDTypeObj ('TaggedSum enk 'JsonRec)) sc ty where
  match' = error "Panic: TODO"

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, HasDiscriminator enk sc ty) => Match ('UDTypeObj ('TaggedSumMono enk at 'FlatRec)) sc ty where
  match' _ allCons (Expr (PQ.FlatComposite es)) caseF =
    case_ (fmap (\(cpos, (cn, _hasArg, c)) ->
                   let
                     (discFld, arg) = case es of
                       ((_, e) : (_, a) :[]) -> (e, a)
                       _ -> error $ "Panic: Impossible case! Expecting (tag, value) pair: " ++ T.unpack cn
                     discPE = getDiscriminator (Proxy @'(enk, sc, ty)) cn cpos
                   in (Expr (PQ.BinExpr PQ.OpEq discFld discPE), caseF (c $ Just arg))
                ) (zip [1..] allCons)) (Expr $ PQ.ConstExpr PQ.Null)
  match' _ _ (Expr _e) _ = error $ "Panic: Impossible case! Expected Flat Composite but got: " <> (show _e)

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, HasDiscriminator enk sc ty) => Match ('UDTypeObj ('TaggedSumMono enk at 'CompositeRec)) sc ty where
  match' _ allCons scrut caseF =
    case_ (fmap (\(cpos, (cn, hasArg, c)) ->
                   let
                     discFld = PQ.CompositeExpr (getExpr scrut) (_getDiscriminatorTagName $ discriminatorTagName @(DB (SchemaDB sc)) @ty)
                     discPE = getDiscriminator (Proxy @'(enk, sc, ty)) cn cpos
                     -- TODO: Revisit the field name convention
                     carg = PQ.CompositeExpr (getExpr scrut) (defHSNameToDBName "value")
                   in if hasArg
                      then (Expr (PQ.BinExpr PQ.OpEq discFld discPE), caseF (c $ Just carg))
                      else (Expr (PQ.BinExpr PQ.OpEq discFld discPE), caseF (c Nothing))
                ) (zip [1..] allCons)) (Expr $ PQ.ConstExpr PQ.Null)

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, HasDiscriminator enk sc ty, TypeError ('Text "TODO: @Match ('UDTypeObj ('TaggedSumMono enk 'JsonRec))")) => Match ('UDTypeObj ('TaggedSumMono enk at 'JsonRec)) sc ty where
  match' = error "Panic: TODO"

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty) => Match ('UDTypeObj ('SumOfCol 'FlatRec)) sc ty where
  match' _ allCons (Expr (PQ.FlatComposite es)) caseF =
    case_ (fmap (\(esMay, (cn, _hasArg, c)) ->
                   let
                     discFld = case esMay of
                       Just (_, e) -> e
                       Nothing -> error $ "Panic: Impossible case! Unable to find expr for tag: " ++ T.unpack cn
                     carg = lookup cn es
                   in (Expr (PQ.PostfixExpr PQ.OpIsNotNull discFld), caseF (c carg))
                ) (zip ((fmap Just es) ++ (repeat Nothing)) allCons)) (Expr $ PQ.ConstExpr PQ.Null)
  match' _ _ (Expr _e) _ = error $ "Panic: Impossible case! Expected Flat Composite but got: " <> (show _e)

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty) => Match ('UDTypeObj ('SumOfCol 'CompositeRec)) sc ty where
  match' _ allCons scrut caseF =
    case_ (fmap (\(cn, hasArg, c) -> if not hasArg
                  then error $ "Panic: [DBR-123]: Unexpected nullary constructor: " ++ (T.unpack cn) ++ " for sum type " ++ (show $ typeRep (Proxy @ty)) ++ " while using SumOfCol synthesis"
                  else
                    let
                      cname = case lookupConName cn Nothing (conAliases @(DB (SchemaDB sc)) @ty) of
                        Left cn' -> cn'
                        Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @ty))
                      discFld = PQ.CompositeExpr (getExpr scrut) (defHSNameToDBName cname)
                    in (Expr (PQ.PostfixExpr PQ.OpIsNotNull discFld), caseF (c $ Just discFld))
                ) allCons) (Expr $ PQ.ConstExpr PQ.Null)

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, TypeError ('Text "TODO: @Match ('UDTypeObj ('SumOfCol 'JsonRec))")) => Match ('UDTypeObj ('SumOfCol 'JsonRec)) sc ty where
  match' = error "Panic: TODO"

instance TypeError ('Text "Pattern match not supported for type which are serialzied as blob") => Match ('UDTypeObj ('SerializedBlob ct)) sc ty where
  match' = error "Panic: Unreachable code"

instance TypeError ('Text "Pattern match not supported for record type") => Match ('UDTypeObj ('UDRec rt)) sc ty where
  match' = error "Panic: Unreachable code"


constExpr :: forall t sc.(DBRepr (DB (SchemaDB sc)) t, AutoConstExpr sc t (ToDBType (DB (SchemaDB sc)) t) (AutoCodec (DB (SchemaDB sc)) t)) => t -> Expr sc t
constExpr = autoConstExpr (Proxy @'(ToDBType (DB (SchemaDB sc)) t, AutoCodec (DB (SchemaDB sc)) t))

class HasConstructExpr (sc :: Type) (t :: Type) (m :: Type -> Type) (dbObj :: DBObjK) where
  constructExpr_ :: Proxy '(dbObj, m) -> PatArg sc t (Matcher (DB (SchemaDB sc)) t) -> Expr sc t

instance
  ( Generic (m sc)
  , GenHasSumRepr (DB (SchemaDB sc)) t m sc (Rep (m sc))
  , DBRepr (DB (SchemaDB sc)) t
  , HasDiscriminator enk sc t
  , Matcher (DB (SchemaDB sc)) t ~ 'SumMatcher (DB (SchemaDB sc)) pfx t m
  ) => HasConstructExpr sc t m ('UDTypeObj ('TaggedSum enk 'FlatRec)) where
  constructExpr_ _ pat =
    let
      SumMatchRep { sumConstructor = sCtor } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
    in sCtor @sc pat
       (\cn cpos cargM ->
           let
             discPE = getDiscriminator (Proxy @'(enk, sc, t)) cn cpos
             discTag = _getDiscriminatorTagName $ discriminatorTagName @(DB (SchemaDB sc)) @t
           in Expr $ PQ.FlatComposite $ case cargM of
             Nothing -> [(discTag, discPE)]
             Just carg -> [(discTag, discPE), (cn, carg)]
       )
--


-- TODO: Try remove pfx
constructExpr :: forall (t :: Type) (sc :: Type) m pfx.
  ( DBRepr (DB (SchemaDB sc)) t
  , Matcher (DB (SchemaDB sc)) t ~ 'SumMatcher (DB (SchemaDB sc)) pfx t m
  , HasConstructExpr sc t m (ToDBType (DB (SchemaDB sc)) t)
  ) => PatArg sc t (Matcher (DB (SchemaDB sc)) t) -> Expr sc t
constructExpr pat = constructExpr_ (Proxy @'(ToDBType (DB (SchemaDB sc)) t, m)) pat

case_ :: [(Expr sc Bool, Expr sc r)] -> Expr sc r -> Expr sc r
case_ alts (Expr def) = Expr $ PQ.CaseExpr (fmap (\(Expr f,Expr s) -> (f,s)) alts) def

ifThenElse :: Expr sc Bool -> Expr sc a -> Expr sc a -> Expr sc a
ifThenElse cond t f = case_ [(cond, t)] f

toNullable :: Expr sc a -> Expr sc (Maybe a)
toNullable = unsafeCoerceExpr

nothing :: Expr sc (Maybe a)
nothing = Expr $ PQ.ConstExpr PQ.Null

jsonOf ::
  forall sc a.
  ( A.ToJSON a
  , DBTypeOf sc a
  ) => a -> Expr sc a
jsonOf = annotateType . Expr . PQ.ConstExpr . PQ.String . jsonify
  where jsonify = T.pack . lazyDecodeUtf8 . A.encode

jsonValue ::
  forall sc a.
  ( A.ToJSON a
  ) => a -> Expr sc A.Value
jsonValue = annotateType . Expr . PQ.ConstExpr . PQ.String . jsonify
  where jsonify = T.pack . lazyDecodeUtf8 . A.encode


arrayF :: ( DBTypeOf sc (f a)
          , Foldable f
          ) => f (Expr sc a) -> Expr sc (f a)
arrayF = annotateType . Expr . PQ.ArrayExpr . coerce . F.toList

lazyDecodeUtf8 :: LB.ByteString -> String
lazyDecodeUtf8 = LT.unpack . LTE.decodeUtf8

instance ( ) => ConstExpr sc A.Value where
  toConstExpr = jsonValue
