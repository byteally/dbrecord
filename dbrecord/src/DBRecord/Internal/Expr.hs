{-# OPTIONS_GHC -fno-warn-redundant-constraints -Wno-orphans #-}

{-# LANGUAGE KindSignatures, DataKinds, ViewPatterns, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses, TypeApplications, TypeOperators, PatternSynonyms, CPP, PolyKinds, TypeFamilies, DefaultSignatures, DerivingStrategies, LambdaCase #-}
module DBRecord.Internal.Expr
       ( module DBRecord.Internal.Expr
       ) where

import qualified DBRecord.Internal.PrimQuery as PQ
import           DBRecord.Types
import qualified Data.Foldable as F
import           Data.Functor.Identity (Identity)
import qualified Data.Functor.Identity as I
import           Data.Functor.Const
-- import qualified Data.HashMap.Strict as HM
import           Data.String
import qualified Data.Text as T
import           Data.Typeable
-- import GHC.TypeLits
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Aeson as A
import qualified Data.Text.Encoding as STE
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import qualified Data.Text.Lazy as LT
import           Data.Time
import           Data.Text (Text)
import           Data.Scientific
import           Data.Void
import           DBRecord.Internal.Types hiding (DBTypeK (..), DBTypeNameK(..)) 
import           DBRecord.Internal.DBTypes
import           DBRecord.Internal.Schema
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Data.CaseInsensitive (CI, foldedCase, mk)
import           Data.Coerce
import           Data.Kind
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           GHC.Generics
import           GHC.Records
import           GHC.TypeLits


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
annotateType te@(Expr e) = Expr $ PQ.CastExpr (dbTypeOf te) e
{-# INLINE annotateType #-}

unsafeCoerceExpr :: Expr sc a -> Expr sc b
unsafeCoerceExpr (Expr e) = Expr e

type DBTypeOf sc a = ( DBRepr (DB (SchemaDB sc)) a
                     , ReifyTypeName sc a (ToDBType (DB (SchemaDB sc)) a)
                     )

dbTypeOf :: forall a sc.DBTypeOf sc a => Expr sc a -> DBType
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
  reifyTypeName _ = dbTypeOf (undefined :: Expr sc ty)

instance ReifyTypeName sc e dbObj => ReifyTypeName sc c ('ArrayObjOf e dbObj) where
  reifyTypeName _ = DBArray $ reifyTypeName (Proxy @'(sc, e, dbObj))

instance ReifyTypeName sc e dbObj => ReifyTypeName sc opt ('NullableObjOf e dbObj) where
  reifyTypeName _ = DBNullable $ reifyTypeName (Proxy @'(sc, e, dbObj))  


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

unsafeCol :: [T.Text] -> Expr sc a
unsafeCol = Expr . PQ.unsafeAttrExpr


match :: forall r t sc.
  ( DBRepr (DB (SchemaDB sc)) t
  , Match (ToDBType (DB (SchemaDB sc)) t) sc t
  ) => Expr sc t -> (UnLifted (DB (SchemaDB sc)) t -> Expr sc r) -> Expr sc r
match scrut =
  let
    univs = univOfUnLifted (Proxy @'((DB (SchemaDB sc)), t))
  in match' (Proxy @(ToDBType (DB (SchemaDB sc)) t)) univs scrut

matchTag :: Expr sc t -> UnLifted (DB (SchemaDB sc)) t -> Expr sc Bool
matchTag = undefined

class Match (dbrep :: DBObjK) (sc :: Type) (scrut :: Type) where
  match' :: Proxy dbrep -> [(Text, UnLifted (DB (SchemaDB sc)) scrut)] -> Expr sc scrut -> (UnLifted (DB (SchemaDB sc)) scrut -> Expr sc r) -> Expr sc r

instance Match ('NativeTypeObj ty) sc Bool where
  match' _ _ scrut caseF = ifThenElse scrut (caseF True) (caseF False)

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, HasDiscriminator enumk sc ty) => Match ('UDTypeObj ('UDEnum enumk)) sc ty where
  match' _ allCons scrut caseF =
    case_ (fmap (\(cpos, (cn, c)) ->
                   let
                     discPE = getDiscriminator (Proxy @'(enumk, sc, ty)) cn cpos
                   in (Expr (PQ.BinExpr PQ.OpEq (getExpr scrut) discPE), caseF c)
                ) (zip [1..] allCons)) (Expr $ PQ.ConstExpr PQ.Null)


instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, HasDiscriminator enk sc ty) => Match ('UDTypeObj ('TaggedSum enk 'FlatRec)) sc ty where
  match' _ allCons (Expr (PQ.FlatComposite es)) caseF =
    case_ (fmap (\(cpos, (cn, c)) ->
                   let
                     discFld = case es of
                       [] -> error $ "Panic: Impossible case! Discriminator not found: " ++ T.unpack cn
                       ((_, e) : _) -> e
                     discPE = getDiscriminator (Proxy @'(enk, sc, ty)) cn cpos
                   in (Expr (PQ.BinExpr PQ.OpEq discFld discPE), caseF c)
                ) (zip [1..] allCons)) (Expr $ PQ.ConstExpr PQ.Null)
  match' _ _ (Expr _e) _ = error $ "Panic: Impossible case! Expected Flat Composite but got: " <> (show _e)

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, HasDiscriminator enk sc ty) => Match ('UDTypeObj ('TaggedSum enk 'CompositeRec)) sc ty where
  match' _ allCons scrut caseF =
    case_ (fmap (\(cpos, (cn, c)) ->
                   let
                     discFld = PQ.CompositeExpr (getExpr scrut) (_getDiscriminatorTagName $ discriminatorTagName @(DB (SchemaDB sc)) @ty)
                     discPE = getDiscriminator (Proxy @'(enk, sc, ty)) cn cpos
                   in (Expr (PQ.BinExpr PQ.OpEq discFld discPE), caseF c)
                ) (zip [1..] allCons)) (Expr $ PQ.ConstExpr PQ.Null)

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, HasDiscriminator enk sc ty, TypeError ('Text "TODO: @Match ('UDTypeObj ('TaggedSum enk 'JsonRec))")) => Match ('UDTypeObj ('TaggedSum enk 'JsonRec)) sc ty where
  match' = error "Panic: TODO"

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, HasDiscriminator enk sc ty) => Match ('UDTypeObj ('TaggedSumMono enk at 'FlatRec)) sc ty where
  match' _ allCons (Expr (PQ.FlatComposite es)) caseF =
    case_ (fmap (\(cpos, (cn, c)) ->
                   let
                     (discFld, _arg) = case es of
                       ((_, e) : (_, a) :[]) -> (e, a)
                       _ -> error $ "Panic: Impossible case! Expecting (tag, value) pair: " ++ T.unpack cn
                     discPE = getDiscriminator (Proxy @'(enk, sc, ty)) cn cpos
                   in (Expr (PQ.BinExpr PQ.OpEq discFld discPE), caseF c)
                ) (zip [1..] allCons)) (Expr $ PQ.ConstExpr PQ.Null)
  match' _ _ (Expr _e) _ = error $ "Panic: Impossible case! Expected Flat Composite but got: " <> (show _e)

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, HasDiscriminator enk sc ty) => Match ('UDTypeObj ('TaggedSumMono enk at 'CompositeRec)) sc ty where
  match' _ allCons scrut caseF =
    case_ (fmap (\(cpos, (cn, c)) ->
                   let
                     discFld = PQ.CompositeExpr (getExpr scrut) (_getDiscriminatorTagName $ discriminatorTagName @(DB (SchemaDB sc)) @ty)
                     discPE = getDiscriminator (Proxy @'(enk, sc, ty)) cn cpos
                   in (Expr (PQ.BinExpr PQ.OpEq discFld discPE), caseF c)
                ) (zip [1..] allCons)) (Expr $ PQ.ConstExpr PQ.Null)

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, HasDiscriminator enk sc ty, TypeError ('Text "TODO: @Match ('UDTypeObj ('TaggedSumMono enk 'JsonRec))")) => Match ('UDTypeObj ('TaggedSumMono enk at 'JsonRec)) sc ty where
  match' = error "Panic: TODO"

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty) => Match ('UDTypeObj ('SumOfCol 'FlatRec)) sc ty where
  match' _ allCons (Expr (PQ.FlatComposite es)) caseF =
    case_ (fmap (\(esMay, (cn, c)) ->
                   let
                     discFld = case esMay of
                       Just (_, e) -> e
                       Nothing -> error $ "Panic: Impossible case! Unable to find expr for tag: " ++ T.unpack cn
                   in (Expr (PQ.PostfixExpr PQ.OpIsNotNull discFld), caseF c)
                ) (zip ((fmap Just es) ++ (repeat Nothing)) allCons)) (Expr $ PQ.ConstExpr PQ.Null)
  match' _ _ (Expr _e) _ = error $ "Panic: Impossible case! Expected Flat Composite but got: " <> (show _e)

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty) => Match ('UDTypeObj ('SumOfCol 'CompositeRec)) sc ty where
  match' _ allCons scrut caseF =
    case_ (fmap (\(cn, c) ->
                   let
                     cname = case lookupConName cn Nothing (conAliases @(DB (SchemaDB sc)) @ty) of
                       Left cn' -> cn'
                       Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @ty))
                     discFld = PQ.CompositeExpr (getExpr scrut) cname
                   in (Expr (PQ.PostfixExpr PQ.OpIsNotNull discFld), caseF c)
                ) allCons) (Expr $ PQ.ConstExpr PQ.Null)

instance (DBRepr (DB (SchemaDB sc)) ty, Typeable ty, TypeError ('Text "TODO: @Match ('UDTypeObj ('SumOfCol 'JsonRec))")) => Match ('UDTypeObj ('SumOfCol 'JsonRec)) sc ty where
  match' = error "Panic: TODO"

instance TypeError ('Text "Pattern match not supported for type which are serialzied as blob") => Match ('UDTypeObj ('SerializedBlob ct)) sc ty where
  match' = error "Panic: Unreachable code"

instance TypeError ('Text "Pattern match not supported for record type") => Match ('UDTypeObj ('UDRec rt)) sc ty where
  match' = error "Panic: Unreachable code"

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

constExpr :: forall t sc.(DBRepr (DB (SchemaDB sc)) t, AutoConstExpr sc t (ToDBType (DB (SchemaDB sc)) t) (AutoCodec (DB (SchemaDB sc)) t)) => t -> Expr sc t
constExpr = autoConstExpr (Proxy @'(ToDBType (DB (SchemaDB sc)) t, AutoCodec (DB (SchemaDB sc)) t))

class ConstExpr sc t where
  toConstExpr :: t -> Expr sc t

class AutoConstExpr sc t (dbObj :: DBObjK) (isAuto :: Bool) where
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

instance (Generic t, DBRepr (DB (SchemaDB sc)) t) => AutoConstExpr sc t ('UDTypeObj ('UDEnum enum)) 'True where
  autoConstExpr _ _t = undefined -- genEnumExpr t

instance (HasDiscriminator enk sc t) => AutoConstExpr sc t ('UDTypeObj ('TaggedSum enk 'FlatRec)) 'True where
  autoConstExpr _ _t =
    let
      (cn, cpos) = undefined
      discPE = getDiscriminator (Proxy @'(enk, sc, t)) cn cpos
    in Expr (PQ.FlatComposite [(cn, discPE), undefined])

instance (HasDiscriminator enk sc t) => AutoConstExpr sc t ('UDTypeObj ('TaggedSum enk 'CompositeRec)) 'True where
  autoConstExpr _ _t =
    let
      (cn, cpos) = undefined
      _discPE = getDiscriminator (Proxy @'(enk, sc, t)) cn cpos
    in undefined

instance (Generic t, TypeError ('Text "TODO: @AutoConstExpr TaggedSum")) => AutoConstExpr sc t ('UDTypeObj ('TaggedSum enk 'JsonRec)) 'True where
  autoConstExpr _ _t = error "Panic: TODO"

instance (HasDiscriminator enk sc t) => AutoConstExpr sc t ('UDTypeObj ('TaggedSumMono enk colty 'FlatRec)) 'True where
  autoConstExpr _ _t =
    let
      (cn, cpos) = undefined
      discPE = getDiscriminator (Proxy @'(enk, sc, t)) cn cpos
    in Expr (PQ.FlatComposite [(cn, discPE), undefined])

instance (HasDiscriminator enk sc t) => AutoConstExpr sc t ('UDTypeObj ('TaggedSumMono enk colty 'CompositeRec)) 'True where
  autoConstExpr _ _t =
    let
      (cn, cpos) = undefined
      _discPE = getDiscriminator (Proxy @'(enk, sc, t)) cn cpos
    in undefined

instance (Generic t, TypeError ('Text "TODO: @AutoConstExpr TaggedSumMono")) => AutoConstExpr sc t ('UDTypeObj ('TaggedSumMono enk colty 'JsonRec)) 'True where
  autoConstExpr _ _t = error "Panic: TODO"

instance (Generic t) => AutoConstExpr sc t ('UDTypeObj ('SumOfCol 'FlatRec)) 'True where
  autoConstExpr _ _t = Expr (PQ.FlatComposite [(undefined, undefined)])

instance (Generic t) => AutoConstExpr sc t ('UDTypeObj ('SumOfCol 'CompositeRec)) 'True where
  autoConstExpr _ _t = undefined

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

{-
genEnumExpr :: forall sc t.
  ( Generic t, UDType sc t
  , GenEnumExpr sc t (Rep t) (GetTagEnumK (ToDBType (DB (SchemaDB sc)) t))
  ) => t -> Expr sc t
genEnumExpr = genEnumExpr' (Proxy @(GetTagEnumK (ToDBType (DB (SchemaDB sc)) t))) (conAliases @sc @t) . from

class GenEnumExpr (sc :: Type) (t :: Type) (rep :: Type -> Type) (enumK :: UDEnumK) where
  genEnumExpr' :: Proxy enumK -> ConAliases sc t -> rep a -> Expr sc t

instance GenEnumExpr sc t f enk => GenEnumExpr sc t (D1 d f) enk where
  genEnumExpr' pe conAs (M1 f) = genEnumExpr' pe conAs f

instance (GenEnumExpr sc t f enk, GenEnumExpr sc t g enk) => GenEnumExpr sc t (f :+: g) enk where
  genEnumExpr' pe conAs (L1 l) = genEnumExpr' pe conAs l
  genEnumExpr' pe  conAs (R1 r) = genEnumExpr' pe conAs r

instance (Constructor c) => GenEnumExpr sc t (C1 c U1) 'EnumType where
  genEnumExpr' _ conAs c@(M1 _) = case lookupConName (T.pack $ conName c) Nothing conAs of
    Left t -> Expr (PQ.ConstExpr (PQ.String t))
    Right _ -> error "Panic: Expecting Only Text for 'EnumType lookup"

instance (Constructor c) => GenEnumExpr sc t (C1 c U1) 'EnumText where
  genEnumExpr' _ conAs c@(M1 _) = case lookupConName (T.pack $ conName c) Nothing conAs of
    Left t -> Expr (PQ.ConstExpr (PQ.String t))
    Right _ -> error "Panic: Expecting Only Text for 'EnumText lookup"

instance (Constructor c) => GenEnumExpr sc t (C1 c U1) 'EnumNum where
  -- TODO: Use Con Ix instead of `minBound`
  genEnumExpr' _ conAs c@(M1 _) = case lookupConName (T.pack $ conName c) (Just minBound) conAs of
    Right t -> Expr (PQ.ConstExpr (PQ.Integer $ toInteger t))
    Left _ -> error "Panic: Expecting Only Number for 'EnumNum lookup"

instance (TypeError ('Text "Expected Only Unary Constructor " ':<>: 'ShowType t)) => GenEnumExpr sc t (C1 c (f :*: g)) enk where
  genEnumExpr' = error "Panic: Unreachable code"
-}


instance ConstExpr sc Text where
  toConstExpr = fromString . T.unpack

instance ConstExpr sc Int where
  toConstExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Int8 where
  toConstExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Int16 where
  toConstExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Int32 where
  toConstExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Int64 where
  toConstExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Word where
  toConstExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Word8 where
  toConstExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Word16 where
  toConstExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Word32 where
  toConstExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Word64 where
  toConstExpr = exprFromInteger . fromIntegral

instance ConstExpr sc SB.ByteString where
  toConstExpr = bytes

instance ConstExpr sc Double where
  toConstExpr = literalExpr . PQ.Double

instance ConstExpr sc Float where
  toConstExpr = literalExpr . PQ.Double . fromRational . toRational

instance ConstExpr sc Rational where
  toConstExpr = literalExpr . PQ.Double . fromRational

instance ConstExpr sc Scientific where
  toConstExpr = literalExpr . PQ.Double . toRealFloat -- (flip toRationalRepetend 2)

instance (
         ) => ConstExpr sc (CI T.Text) where
  toConstExpr = citext

instance (
         ) => ConstExpr sc Day where
  toConstExpr = date

instance () => ConstExpr sc UTCTime where
  toConstExpr = utcTime

instance () => ConstExpr sc LocalTime where
  toConstExpr = localTime

instance () => ConstExpr sc TimeOfDay where
  toConstExpr = timeOfDay

instance (ConstExpr sc a) => ConstExpr sc (Identity a) where
  toConstExpr = toIdentity . toConstExpr . I.runIdentity

instance ( ConstExpr sc a
         , DBTypeOf sc a
         ) => ConstExpr sc [a] where
  toConstExpr = array . map toConstExpr

instance ConstExpr sc Bool where
  toConstExpr = literalExpr . PQ.Bool

instance ( ) => ConstExpr sc A.Value where
  toConstExpr = jsonValue

instance ( ) => ConstExpr sc UUID where
  toConstExpr = uuid

instance (ConstExpr sc a) => ConstExpr sc (Maybe a) where
  toConstExpr =
    maybe (literalExpr PQ.Null) (toNullable . toConstExpr)

instance ConstExpr sc LTree where
  toConstExpr = ltree

ltree :: LTree -> Expr sc LTree
ltree (LTree vs) = go vs
    where
      go = literalExpr . PQ.String . dotSep
      dotSep = T.intercalate "."

literalExpr :: PQ.Lit -> Expr sc a
literalExpr = Expr . PQ.ConstExpr


instance (OrdExpr db v) => OrdExpr db (Key t v) where
  a .<= b = (coerceExprTo a .<= coerceExprTo b)
   where coerceExprTo :: Expr sc (Key t v) -> Expr sc v
         coerceExprTo = coerceExpr

deriving newtype instance (EqExpr db v) => EqExpr db (Key t v)

-- instance (NumExpr v) => NumExpr (Key t v) where
--   exprFromInteger = coerceExprTo . exprFromInteger . coerce
--    where coerceExprTo :: Expr sc v -> Expr sc (Key t v)
--          coerceExprTo = coerceExpr

instance (ConstExpr db v) => ConstExpr db (Key t v) where
  toConstExpr (Key a) = coerceExpr . toConstExpr $ a

-- instance (ToJSON a, Typeable a) => ConstExpr sc (Json a) where
--   toConstExpr =
--     toJson . getJson


binOp :: PQ.BinOp -> Expr sc a -> Expr sc b -> Expr sc c
binOp op (Expr lhs) (Expr rhs) = Expr (PQ.BinExpr op lhs rhs)

prefixOp :: PQ.UnOp -> Expr sc a -> Expr sc b
prefixOp op (Expr expr) = Expr (PQ.PrefixExpr op expr)

postfixOp :: PQ.UnOp -> Expr sc a -> Expr sc b
postfixOp op (Expr expr) = Expr (PQ.PostfixExpr op expr)

funOp :: String -> Expr sc a -> Expr sc b
funOp op (Expr expr) = Expr (PQ.PrefixExpr (PQ.OpOtherFun op) expr)

strictDecodeUtf8 :: SB.ByteString -> String
strictDecodeUtf8 = T.unpack . STE.decodeUtf8

lazyDecodeUtf8 :: LB.ByteString -> String
lazyDecodeUtf8 = LT.unpack . LTE.decodeUtf8

class (Num a) => NumExpr a where
  exprFromInteger :: Integer -> Expr sc a

deriving instance (NumExpr a) => NumExpr (Identity a)

instance ( NumExpr a
         , OrdExpr sc a
         ) => Num (Expr sc a) where
  fromInteger = exprFromInteger
  (*)      = binOp PQ.OpMul
  (+)      = binOp PQ.OpPlus
  (-)      = binOp PQ.OpMinus
  abs      = prefixOp PQ.OpAbs
  negate   = prefixOp PQ.OpNegate
  signum a = case_ [ (a .== 0, 0)
                   , (a .<  0, (-1))
                   , (a .>  0, 1)
                   ] a

class IntegralExpr a where
  quot_ :: Expr sc a ->  Expr sc a -> Expr sc a
  rem_  :: Expr sc a ->  Expr sc a -> Expr sc a

  quot_ = binOp PQ.OpDiv
  rem_  = binOp PQ.OpMod

instance IntegralExpr Int
instance IntegralExpr Word
instance IntegralExpr Integer

class NumExpr a => FractionalExpr a where
  exprFromRational :: Rational -> Expr sc a

instance (FractionalExpr a, OrdExpr sc a) => Fractional (Expr sc a) where
  fromRational = exprFromRational
  (/)    = binOp PQ.OpDiv

instance NumExpr Word where
  exprFromInteger = literalExpr . PQ.Integer . fromIntegral

instance NumExpr Word8 where
  exprFromInteger = literalExpr . PQ.Integer . fromIntegral

instance NumExpr Word16 where
  exprFromInteger = literalExpr . PQ.Integer . fromIntegral

instance NumExpr Word32 where
  exprFromInteger = literalExpr . PQ.Integer . fromIntegral

instance NumExpr Word64 where
  exprFromInteger = literalExpr . PQ.Integer . fromIntegral

instance NumExpr Int where
  exprFromInteger = literalExpr . PQ.Integer . fromIntegral

instance NumExpr Int8 where
  exprFromInteger = literalExpr . PQ.Integer . fromIntegral

instance NumExpr Int16 where
  exprFromInteger = literalExpr . PQ.Integer . fromIntegral

instance NumExpr Int32 where
  exprFromInteger = literalExpr . PQ.Integer . fromIntegral

instance NumExpr Int64 where
  exprFromInteger = literalExpr . PQ.Integer . fromIntegral

instance NumExpr Integer where
  exprFromInteger = literalExpr . PQ.Integer

instance NumExpr Float where
  exprFromInteger = literalExpr . PQ.Double . fromIntegral

instance NumExpr Double where
  exprFromInteger = literalExpr . PQ.Double . fromIntegral

instance NumExpr Rational where
  exprFromInteger = literalExpr . PQ.Double . fromIntegral

instance NumExpr Scientific where
  exprFromInteger = literalExpr . PQ.Double . fromIntegral

instance FractionalExpr Float where
  exprFromRational = literalExpr . PQ.Double . fromRational

instance FractionalExpr Double where
  exprFromRational = literalExpr . PQ.Double . fromRational

fromIntegralExpr :: (Integral a, NumExpr b) => Expr sc a -> Expr sc b
fromIntegralExpr e = unsafeCoerceExpr e

instance () => IsString (Expr sc T.Text) where
  fromString = text . T.pack

instance () => IsString (Expr sc (CI T.Text)) where
  fromString = citext . mk . T.pack

instance ( IsString (Expr sc a)
         ) => IsString (Expr sc (Identity a)) where
  fromString = (coerce :: Expr sc a -> Expr sc (Identity a)) . fromString

class EqExpr sc a where
  (.==) :: Expr sc a -> Expr sc a -> Expr sc Bool

  -- default (.==) :: (Generic a, GEqExpr sc (TypeMappings sc a) (Rep a) a) => Expr sc a -> Expr sc a -> Expr sc Bool
  -- (.==) = geqExpr (Proxy :: Proxy '(Rep a, TypeMappings sc a))

(./=) :: EqExpr sc a => Expr sc a -> Expr sc a -> Expr sc Bool
(./=) a b = case (a .== b) of
  Expr (PQ.BinExpr PQ.OpEq x y) -> Expr (PQ.BinExpr PQ.OpNotEq x y)
  e -> not_ e

infix 4 .==
infix 4 ./=

pattern TRUE :: Expr sc Bool
pattern TRUE = Expr (PQ.ConstExpr (PQ.Bool True))

pattern FALSE :: Expr sc Bool
pattern FALSE = Expr (PQ.ConstExpr (PQ.Bool False))


-- class GEqExpr sc (ud :: UDTypeMappings) rep a where
--   geqExpr :: Proxy '(rep, ud) -> Expr sc a -> Expr sc a -> Expr sc Bool

-- instance ( EqExpr sc (FromJust (NewtypeRep a))
--          , Coercible a (FromJust (NewtypeRep a))
--          ) => GEqExpr sc map (D1 ('MetaData n f s 'True) c) a where
--   geqExpr _ e1 e2 = (coerceExpr @(FromJust (NewtypeRep a)) e1) .== coerceExpr e2

-- instance GEqExpr sc ('EnumType nal als) (D1 ('MetaData n f s 'False) c) a where
--   geqExpr _ = binOp PQ.OpEq

-- instance GEqExpr sc ('Composite nal als) (D1 ('MetaData n f s 'False) c) a where
--   geqExpr _ = binOp PQ.OpEq

-- instance GEqExpr sc ('EnumText als) (D1 ('MetaData n f s 'False) c) a where
--   geqExpr _ = binOp PQ.OpEq

-- instance ( GEqExprFlat sc a als (D1 ('MetaData n f s 'False) c)
--          ) => GEqExpr sc ('Flat als) (D1 ('MetaData n f s 'False) c) a where
--   geqExpr _ = geqExprFlat (Proxy @'((D1 ('MetaData n f s 'False) c), als))

-- class GEqExprFlat sc a (als :: [(Symbol, Symbol)]) rep where
--   geqExprFlat :: Proxy '(rep, als) -> Expr sc a -> Expr sc a -> Expr sc Bool

-- instance ( GEqExprFlat sc a als c
--          ) => GEqExprFlat sc a als (D1 m c) where
--   geqExprFlat _ e1 e2 =
--     geqExprFlat (Proxy @'(c, als)) e1 e2

-- instance ( GEqExprFlat sc a als c
--          ) => GEqExprFlat sc a als (C1 m c) where
--   geqExprFlat _ e1 e2 =
--     geqExprFlat (Proxy @'(c, als)) e1 e2

-- instance ( GEqExprFlat sc a als p
--          , GEqExprFlat sc a als q
--          ) => GEqExprFlat sc a als (p :*: q) where
--   geqExprFlat _ e1 e2 =
--     geqExprFlat (Proxy @'(p, als)) e1 e2 .&&
--     geqExprFlat (Proxy @'(q, als)) e1 e2

-- instance ( EqExpr sc t
--          , UDTargetType ('Flat als) fld t a
--          , t ~ GTarget fld (Rep a)
--          ) => GEqExprFlat sc a als (S1 ('MetaSel ('Just fld) m1 m2 m3) (K1 m t)) where
--   geqExprFlat _ e1 e2 =
--     -- snd (hasField @fld e1) .== snd (hasField @fld e2)
--     snd (udTargetType (Proxy @'(fld, 'Flat als)) e1) .==
--     snd (udTargetType (Proxy @'(fld, 'Flat als)) e2)

instance EqExpr sc () where
  _ .== _ = true

instance EqExpr sc UTCTime where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc UUID where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc Integer where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc Float where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc Double where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc Day where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc A.Value where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc LocalTime where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc Int16 where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc Scientific where
  a .== b = binOp PQ.OpEq a b

instance OrdExpr sc Day where
  a .<= b = binOp PQ.OpLtEq a b

snoc :: Expr sc [a] -> Expr sc a -> Expr sc [a]
snoc arr v =
  let fun = PQ.FunExpr "array_append" [getExpr arr, getExpr v]
  in  Expr fun

append :: Expr sc [a] -> Expr sc [a] -> Expr sc [a]
append arrl arrr =
  let fun = PQ.FunExpr "array_cat" [getExpr arrl, getExpr arrr]
  in  Expr fun

nil :: (DBTypeOf sc a) => Expr sc [a]
nil = array []

class (EqExpr sc a) => OrdExpr sc a where
  (.>) :: Expr sc a -> Expr sc a -> Expr sc Bool
  (.<)  :: Expr sc a -> Expr sc a -> Expr sc Bool
  (.>=) :: Expr sc a -> Expr sc a -> Expr sc Bool
  (.<=) :: Expr sc a -> Expr sc a -> Expr sc Bool

  (.>) a b  = not_ (a .<= b)
  (.<) a b  = (a .<= b) .&& not_ (a .== b)
  (.>=) a b = not_ (a .<= b) .|| (a .== b)

  {-# MINIMAL (.<=) #-}

infix 4 .>
infix 4 .<
infix 4 .>=
infix 4 .<=

instance OrdExpr sc Int where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr sc Int32 where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr sc Int64 where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr sc Word where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr sc T.Text where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr sc (CI T.Text) where
  a .<= b = binOp PQ.OpLtEq a b

instance (OrdExpr sc a) => OrdExpr sc (Maybe a) where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr sc UTCTime where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr sc Integer where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr sc Float where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr sc Double where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr sc LocalTime where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr sc Scientific where
  a .<= b = binOp PQ.OpLtEq a b

infixr 3 .&&
(.&&) :: Expr sc Bool -> Expr sc Bool -> Expr sc Bool
(.&&) a b = binOp PQ.OpAnd a b

infixr 3 .||
(.||) :: Expr sc Bool -> Expr sc Bool -> Expr sc Bool
(.||) a b = binOp PQ.OpOr a b

not_ :: Expr sc Bool -> Expr sc Bool
not_ = \case
  TRUE -> FALSE
  FALSE -> TRUE
  e -> prefixOp PQ.OpNot e

isNull :: Expr sc (Maybe a) -> Expr sc Bool
isNull = postfixOp PQ.OpIsNull

isNotNull :: Expr sc (Maybe a) -> Expr sc Bool
isNotNull = postfixOp PQ.OpIsNotNull

nothing :: Expr sc (Maybe a)
nothing = Expr $ PQ.ConstExpr PQ.Null

toEnum :: forall a sc. (Enum a, Show a) => a -> Expr sc a
toEnum = Expr . PQ.ConstExpr . PQ.Other . quoteEnum
  where quoteEnum :: a -> T.Text
        quoteEnum s = let str = T.pack . show $ s
                      in "\'" <> str <> "\'"

toNullable :: Expr sc a -> Expr sc (Maybe a)
toNullable = unsafeCoerceExpr

matchNullable :: Expr sc b -> (Expr sc a -> Expr sc b) -> Expr sc (Maybe a) -> Expr sc b
matchNullable def f val = ifThenElse (isNull val) def (f $ unsafeCoerceExpr val)

fromNullable :: Expr sc a -> Expr sc (Maybe a) -> Expr sc a
fromNullable = flip matchNullable id

maybeToNullable :: Maybe (Expr sc a) -> Expr sc (Maybe a)
maybeToNullable = maybe nothing toNullable

case_ :: [(Expr sc Bool, Expr sc r)] -> Expr sc r -> Expr sc r
case_ alts (Expr def) = Expr $ PQ.CaseExpr (fmap (\(Expr f,Expr s) -> (f,s)) alts) def

ifThenElse :: Expr sc Bool -> Expr sc a -> Expr sc a -> Expr sc a
ifThenElse cond t f = case_ [(cond, t)] f

(.++) :: Expr sc T.Text -> Expr sc T.Text -> Expr sc T.Text
(.++) a b = binOp PQ.OpCat a b

like :: Expr sc T.Text -> Expr sc T.Text -> Expr sc Bool
like = binOp PQ.OpLike

between :: OrdExpr sc a => Expr sc a -> (Expr sc a, Expr sc a) -> Expr sc Bool
between v (Expr lb, Expr ub) = binOp PQ.OpBetween v (Expr $ PQ.ArrayExpr [lb, ub])

lower :: Expr sc T.Text -> Expr sc T.Text
lower = prefixOp PQ.OpLower

upper :: Expr sc T.Text -> Expr sc T.Text
upper = prefixOp PQ.OpUpper

ors :: Foldable f => f (Expr sc Bool) -> Expr sc Bool
ors = F.foldl' (.||) false

in_ :: (Functor f, Foldable f, EqExpr sc a) => Expr sc a -> f (Expr sc a) -> Expr sc Bool
in_ e exprs = ors . fmap (e .==) $ exprs

true :: Expr sc Bool
true = Expr $ PQ.ConstExpr $ PQ.Bool True

false :: Expr sc Bool
false = Expr $ PQ.ConstExpr $ PQ.Bool False

array :: ( DBTypeOf sc a
         ) => [Expr sc a] -> Expr sc [a]
array = annotateType . Expr . PQ.ArrayExpr . coerce

arrayF :: ( DBTypeOf sc (f a)
          , Foldable f
          ) => f (Expr sc a) -> Expr sc (f a)
arrayF = annotateType . Expr . PQ.ArrayExpr . coerce . F.toList

iscontainedBy :: Expr sc [a] -> Expr sc [a] -> Expr sc Bool
iscontainedBy a b = binOp (PQ.OpOther "<@") a b

-- any :: Expr sc [a] -> Expr sc a
-- any (Expr e) = Expr (PQ.UnExpr (PQ.UnOpOtherFun "ANY") e)

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

text :: T.Text -> Expr sc T.Text
text = Expr . PQ.ConstExpr . PQ.String

citext :: ( ) => CI T.Text -> Expr sc (CI T.Text)
citext = annotateType . Expr . PQ.ConstExpr . PQ.String . foldedCase

date :: ( ) => Day -> Expr sc Day
date = annotateType . Expr . PQ.ConstExpr . PQ.Other . T.pack . format
  where format = formatTime defaultTimeLocale "'%F'"

utcTime :: ( ) => UTCTime -> Expr sc UTCTime
utcTime = annotateType . Expr . PQ.ConstExpr . PQ.Other . T.pack . format
  where format = formatTime defaultTimeLocale "'%FT%TZ'"

localTime :: ( ) => LocalTime -> Expr sc LocalTime
localTime = annotateType . Expr . PQ.ConstExpr . PQ.Other . T.pack . format
  where format = formatTime defaultTimeLocale "'%FT%T%Q'"

timeOfDay :: ( ) => TimeOfDay -> Expr sc TimeOfDay
timeOfDay = annotateType . Expr . PQ.ConstExpr . PQ.Other . T.pack . format
  where format = formatTime defaultTimeLocale "'%T%Q'"

utcTimeNow :: Expr sc UTCTime
utcTimeNow =
  let now = PQ.FunExpr "now" []
      utcT = PQ.BinExpr PQ.OpAtTimeZone now utcText
      utcText = PQ.ConstExpr (PQ.String "utc")
  in  Expr utcT

ist :: Expr sc TimeZone
ist = Expr (PQ.ConstExpr (PQ.String "ist"))

atTimeZone :: Expr sc TimeZone -> Expr sc UTCTime -> Expr sc LocalTime
atTimeZone (Expr tz) (Expr utct) = Expr (PQ.FunExpr "timezone" [tz, utct])

dayTruncTZ :: Expr sc LocalTime -> Expr sc LocalTime
dayTruncTZ (Expr utct) = Expr (PQ.FunExpr "date_trunc" [PQ.ConstExpr (PQ.String "day"), utct])

pgOID :: PGOID t -> Expr sc (PGOID t)
pgOID oid = go (getPGOID oid)
    where
      go = literalExpr . PQ.String

-- TODO: Reimplement this
-- interval :: () => Interval -> Expr sc Interval
-- interval (Interval e) = annotateType (literalExpr (PQ.Other e))

hours :: ( ) => Int -> Expr sc Interval
hours i = prefixOp (PQ.OpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " hours\'"

months :: () => Int -> Expr sc Interval
months i = prefixOp (PQ.OpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " months\'"

days :: ( ) => Int -> Expr sc Interval
days i = prefixOp (PQ.OpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " days\'"

minutes :: ( ) => Int -> Expr sc Interval
minutes i = prefixOp (PQ.OpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " minutes\'"

seconds :: ( ) => Int -> Expr sc Interval
seconds i = prefixOp (PQ.OpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " seconds\'"

bytes :: SB.ByteString -> Expr sc SB.ByteString
bytes = Expr . PQ.ConstExpr . PQ.Byte

addInterval :: Expr sc Interval -> Expr sc Interval -> Expr sc Interval
addInterval e1 e2 = binOp PQ.OpPlus e1 e2

uuid :: ( ) => UUID -> Expr sc UUID
uuid = annotateType . Expr . PQ.ConstExpr . PQ.Other . quoteVal . T.pack . UUID.toString
  where
    quoteVal str = "\'" <> str <> "\'"

addToDate :: Expr sc UTCTime -> Expr sc Interval -> Expr sc UTCTime
addToDate e1 e2 = binOp PQ.OpPlus e1 e2

dbDefault :: Expr sc a
dbDefault = Expr $ PQ.DefaultInsertExpr

dbDefault' :: PQ.PrimExpr
dbDefault' = PQ.DefaultInsertExpr

utcToLocalTime :: Expr sc T.Text
               -> Expr sc UTCTime
               -> Expr sc LocalTime
utcToLocalTime tz ut = binOp PQ.OpAtTimeZone ut tz

localTimeToUTC :: Expr sc T.Text
               -> Expr sc LocalTime
               -> Expr sc UTCTime
localTimeToUTC tz lt = binOp PQ.OpAtTimeZone lt tz

(%) :: Expr sc T.Text -> Expr sc T.Text -> Expr sc Bool
l % r = binOp (PQ.OpOther "%") l r

(%?) :: Expr sc (Maybe T.Text) -> Expr sc (Maybe T.Text) -> Expr sc Bool
l %? r = binOp (PQ.OpOther "%") l r

coalesce :: Expr sc a -> Expr sc (Maybe a) -> Expr sc a
coalesce (Expr d) (Expr opt) =
  Expr (PQ.FunExpr "COALESCE" [opt, d])

sum :: (NumExpr a) => Expr sc a -> Expr sc a
sum = Expr . PQ.FunExpr "sum" . singleton . getExpr
  where singleton x = [x]

avg :: (FractionalExpr a) => Expr sc a -> Expr sc a
avg = Expr . PQ.FunExpr "avg" . singleton . getExpr
  where singleton x = [x]

jsonbSet ::
  forall sc b.
  ( A.ToJSON b
  ) => Expr sc A.Value -> [ Text ] -> b -> Expr sc A.Value
jsonbSet col vs val =
  Expr (PQ.FunExpr "jsonb_set" args)

  where
    args =
      [ getExpr col
      , getExpr (toConstExpr @sc vs)
      , json0
      ]
    json0 = coerce $ jsonValue @sc val

instance EqExpr sc Bool where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc T.Text where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc (CI T.Text) where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc Int where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc Int32 where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc Int64 where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc Word where
  a .== b = binOp PQ.OpEq a b

instance (EqExpr sc a) => EqExpr sc (Maybe a) where
  a .== b = binOp PQ.OpEq a b

deriving instance (EqExpr sc a)  => EqExpr sc (Identity a)
deriving instance (OrdExpr sc a) => OrdExpr sc (Identity a)

instance EqExpr sc LTree where
  a .== b = binOp PQ.OpEq a b


formatCol :: T.Text -> Maybe [T.Text]
formatCol col'
  | isCol col'   = Just (splitCol col')
  | otherwise    = Nothing

  where isCol t = case T.null t of
          True  -> False
          False -> T.head t == '"' && T.last t == '"'
        splitCol = T.split (== '.') . T.dropEnd 1 . T.drop 1


runIdentity :: Expr sc (Identity a) -> Expr sc a
runIdentity = unsafeCoerceExpr

toIdentity :: Expr sc a -> Expr sc (Identity a)
toIdentity = unsafeCoerceExpr

coerceExpr :: forall b a sc. (Coercible a b) => Expr sc a -> Expr sc b
coerceExpr = unsafeCoerceExpr

unsafeCoerceAggExpr :: AggExpr sc a -> AggExpr sc b
unsafeCoerceAggExpr = coerce

coerceAggExpr :: forall b a sc. (Coercible a b) => AggExpr sc a -> AggExpr sc b
coerceAggExpr = unsafeCoerceAggExpr

rawExpr :: T.Text -> Expr sc a
rawExpr = (Expr . PQ.RawExpr)

count :: Expr sc a -> AggExpr sc Int64
count = coerce . funOp "count"

sumOf :: NumExpr n => Expr sc n -> AggExpr sc n
sumOf = coerce . funOp "sum"
