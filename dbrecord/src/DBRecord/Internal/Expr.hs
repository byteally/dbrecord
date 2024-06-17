{-# OPTIONS_GHC -fno-warn-redundant-constraints -Wno-orphans #-}

{-# LANGUAGE KindSignatures, DataKinds, ViewPatterns, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses, TypeApplications, TypeOperators, PatternSynonyms, CPP, PolyKinds, TypeFamilies, DefaultSignatures, DerivingStrategies, LambdaCase #-}
module DBRecord.Internal.Expr
       ( module DBRecord.Internal.Expr
       , module DBRecord.Internal.PrimQuery
       ) where

import           DBRecord.Internal.PrimQuery ( Expr (..), AggExpr (..), getExpr
                                             , unsafeCast, annotateType, unsafeCoerceExpr
                                             )
import qualified DBRecord.Internal.PrimQuery as PQ
import           DBRecord.Types
import qualified Data.Foldable as F
import           Data.Functor.Identity (Identity)
import qualified Data.Functor.Identity as I
import qualified Data.HashMap.Strict as HM
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
import           DBRecord.Internal.Types
import           DBRecord.Internal.DBTypes
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Data.CaseInsensitive (CI, foldedCase, mk)
import           Data.Coerce
import           Data.Kind
--import           DBRecord.Internal.Schema (UDTargetType (..), GTarget)
-- import           DBRecord.Internal.Common (FindAlias, NewtypeRep, FromJust)
import           GHC.Generics
import           GHC.TypeLits
-- import           GHC.OverloadedLabels


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

instance (UDType sc ty, Typeable ty, HasDiscriminator enumk sc ty) => Match ('UDTypeObj ('UDEnum enumk)) sc ty where
  match' _ allCons scrut caseF =
    case_ (fmap (\(cpos, (cn, c)) ->
                   let
                     discPE = getDiscriminator (Proxy @'(enumk, sc, ty)) undefined cn cpos
                   in (PQ.Expr (PQ.BinExpr PQ.OpEq (PQ.getExpr scrut) discPE), caseF c)
                ) (zip [1..] allCons)) (Expr $ PQ.ConstExpr PQ.Null)


instance (UDType sc ty, Typeable ty, HasDiscriminator enk sc ty) => Match ('UDTypeObj ('TaggedSum enk 'FlatRec)) sc ty where
  match' _ allCons (PQ.Expr (PQ.FlatComposite es)) caseF =
    case_ (fmap (\(cpos, (cn, c)) ->
                   let
                     discFld = case es of
                       [] -> error $ "Panic: Impossible case! Discriminator not found: " ++ T.unpack cn
                       ((_, e) : _) -> e
                     discPE = getDiscriminator (Proxy @'(enk, sc, ty)) undefined cn cpos
                   in (PQ.Expr (PQ.BinExpr PQ.OpEq discFld discPE), caseF c)
                ) (zip [1..] allCons)) (Expr $ PQ.ConstExpr PQ.Null)
  match' _ _ (PQ.Expr _e) _ = error $ "Panic: Impossible case! Expected Flat Composite but got: " <> (show _e)

instance (UDType sc ty, Typeable ty, HasDiscriminator enk sc ty) => Match ('UDTypeObj ('TaggedSum enk 'CompositeRec)) sc ty where
  match' _ allCons scrut caseF =
    case_ (fmap (\(cpos, (cn, c)) ->
                   let
                     discFld = PQ.CompositeExpr (PQ.getExpr scrut) (_getDiscriminatorTagName $ discriminatorTagName @sc @ty)
                     discPE = getDiscriminator (Proxy @'(enk, sc, ty)) undefined cn cpos
                   in (PQ.Expr (PQ.BinExpr PQ.OpEq discFld discPE), caseF c)
                ) (zip [1..] allCons)) (Expr $ PQ.ConstExpr PQ.Null)

instance (UDType sc ty, Typeable ty, HasDiscriminator enk sc ty, TypeError ('Text "TODO: @Match ('UDTypeObj ('TaggedSum enk 'JsonRec))")) => Match ('UDTypeObj ('TaggedSum enk 'JsonRec)) sc ty where
  match' = error "Panic: TODO"

instance (UDType sc ty, Typeable ty, HasDiscriminator enk sc ty) => Match ('UDTypeObj ('TaggedSumMono enk at 'FlatRec)) sc ty where
  match' _ allCons (PQ.Expr (PQ.FlatComposite es)) caseF =
    case_ (fmap (\(cpos, (cn, c)) ->
                   let
                     (discFld, _arg) = case es of
                       ((_, e) : (_, a) :[]) -> (e, a)
                       _ -> error $ "Panic: Impossible case! Expecting (tag, value) pair: " ++ T.unpack cn
                     discPE = getDiscriminator (Proxy @'(enk, sc, ty)) undefined cn cpos
                   in (PQ.Expr (PQ.BinExpr PQ.OpEq discFld discPE), caseF c)
                ) (zip [1..] allCons)) (Expr $ PQ.ConstExpr PQ.Null)
  match' _ _ (PQ.Expr _e) _ = error $ "Panic: Impossible case! Expected Flat Composite but got: " <> (show _e)

instance (UDType sc ty, Typeable ty, HasDiscriminator enk sc ty) => Match ('UDTypeObj ('TaggedSumMono enk at 'CompositeRec)) sc ty where
  match' _ allCons scrut caseF =
    case_ (fmap (\(cpos, (cn, c)) ->
                   let
                     discFld = PQ.CompositeExpr (PQ.getExpr scrut) (_getDiscriminatorTagName $ discriminatorTagName @sc @ty)
                     discPE = getDiscriminator (Proxy @'(enk, sc, ty)) undefined cn cpos
                   in (PQ.Expr (PQ.BinExpr PQ.OpEq discFld discPE), caseF c)
                ) (zip [1..] allCons)) (Expr $ PQ.ConstExpr PQ.Null)

instance (UDType sc ty, Typeable ty, HasDiscriminator enk sc ty, TypeError ('Text "TODO: @Match ('UDTypeObj ('TaggedSumMono enk 'JsonRec))")) => Match ('UDTypeObj ('TaggedSumMono enk at 'JsonRec)) sc ty where
  match' = error "Panic: TODO"

instance (UDType sc ty, Typeable ty) => Match ('UDTypeObj ('SumOfCol 'FlatRec)) sc ty where
  match' _ allCons (PQ.Expr (PQ.FlatComposite es)) caseF =
    case_ (fmap (\(esMay, (cn, c)) ->
                   let
                     discFld = case esMay of
                       Just (_, e) -> e
                       Nothing -> error $ "Panic: Impossible case! Unable to find expr for tag: " ++ T.unpack cn
                   in (PQ.Expr (PQ.PostfixExpr PQ.OpIsNotNull discFld), caseF c)
                ) (zip ((fmap Just es) ++ (repeat Nothing)) allCons)) (Expr $ PQ.ConstExpr PQ.Null)
  match' _ _ (PQ.Expr _e) _ = error $ "Panic: Impossible case! Expected Flat Composite but got: " <> (show _e)

instance (UDType sc ty, Typeable ty) => Match ('UDTypeObj ('SumOfCol 'CompositeRec)) sc ty where
  match' _ allCons scrut caseF =
    case_ (fmap (\(cn, c) ->
                   let
                     cname = case lookupConName cn Nothing (conAliases @sc @ty) of
                       Left cn' -> cn'
                       Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @ty))
                     discFld = PQ.CompositeExpr (PQ.getExpr scrut) cname
                   in (PQ.Expr (PQ.PostfixExpr PQ.OpIsNotNull discFld), caseF c)
                ) allCons) (Expr $ PQ.ConstExpr PQ.Null)

instance (UDType sc ty, Typeable ty, TypeError ('Text "TODO: @Match ('UDTypeObj ('SumOfCol 'JsonRec))")) => Match ('UDTypeObj ('SumOfCol 'JsonRec)) sc ty where
  match' = error "Panic: TODO"

instance TypeError ('Text "Pattern match not supported for type which are serialzied as blob") => Match ('UDTypeObj ('SerializedBlob ct)) sc ty where
  match' = error "Panic: Unreachable code"

instance TypeError ('Text "Pattern match not supported for record type") => Match ('UDTypeObj ('UDRec rt)) sc ty where
  match' = error "Panic: Unreachable code"

class HasDiscriminator (enumk :: UDEnumK) (sc :: Type) (ty :: Type) where
  getDiscriminator :: Proxy '(enumk, sc, ty) -> UDTypeName sc ty -> Text -> Int64 -> PQ.PrimExpr

instance (UDType sc ty, Typeable ty) => HasDiscriminator 'EnumType sc ty where
  getDiscriminator _ _disN cn _ =
    let
      cname = case lookupConName cn Nothing (conAliases @sc @ty) of
                Left cn' -> cn'
                Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @ty))
    in PQ.CastExpr undefined (PQ.ConstExpr (PQ.String cname))

instance (UDType sc ty, Typeable ty) => HasDiscriminator 'EnumText sc ty where
  getDiscriminator _ _ cn _ =
    let
      cname = case lookupConName cn Nothing (conAliases @sc @ty) of
                Left cn' -> cn'
                Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @ty))
    in PQ.ConstExpr (PQ.String cname)

instance (UDType sc ty, Typeable ty) => HasDiscriminator 'EnumNum sc ty where
  getDiscriminator _ _ cn cpos =
    let
      ctag = case lookupConName cn (Just cpos) (conAliases @sc @ty) of
               Left _ -> error $ "Panic: Expecting only Int64, not Text as tag for: " ++ (show $ typeRep (Proxy @ty))
               Right ct -> ct
    in PQ.ConstExpr (PQ.Integer $ toInteger ctag)

class ConstExpr sc t where
  constExpr :: t -> PQ.Expr sc t

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


instance ConstExpr sc Text where
  constExpr = fromString . T.unpack

instance ConstExpr sc Int where
  constExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Int8 where
  constExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Int16 where
  constExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Int32 where
  constExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Int64 where
  constExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Word where
  constExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Word8 where
  constExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Word16 where
  constExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Word32 where
  constExpr = exprFromInteger . fromIntegral

instance ConstExpr sc Word64 where
  constExpr = exprFromInteger . fromIntegral

instance ConstExpr sc SB.ByteString where
  constExpr = bytes

instance ConstExpr sc t => ConstExpr sc (fn ::: t) where
  constExpr (DBRecord.Internal.Types.Field v) = unwrap $ constExpr v
    where unwrap :: PQ.Expr sc t -> PQ.Expr sc (fn ::: t)
          unwrap = PQ.unsafeCoerceExpr

instance ConstExpr sc Double where
  constExpr = literalExpr . PQ.Double

instance ConstExpr sc Float where
  constExpr = literalExpr . PQ.Double . fromRational . toRational

instance ConstExpr sc Rational where
  constExpr = literalExpr . PQ.Double . fromRational

instance ConstExpr sc Scientific where
  constExpr = literalExpr . PQ.Double . toRealFloat -- (flip toRationalRepetend 2)

instance (
         ) => ConstExpr sc (CI T.Text) where
  constExpr = citext

instance (
         ) => ConstExpr sc Day where
  constExpr = date

instance () => ConstExpr sc UTCTime where
  constExpr = utcTime

instance () => ConstExpr sc LocalTime where
  constExpr = localTime

instance () => ConstExpr sc TimeOfDay where
  constExpr = timeOfDay

instance (ConstExpr sc a) => ConstExpr sc (Identity a) where
  constExpr = toIdentity . constExpr . I.runIdentity

instance ( ConstExpr sc a
         , PQ.DBTypeOf sc a
         ) => ConstExpr sc [a] where
  constExpr = array . map constExpr

instance ConstExpr sc Bool where
  constExpr = literalExpr . PQ.Bool

instance ( ) => ConstExpr sc A.Value where
  constExpr = jsonb

instance ( ) => ConstExpr sc UUID where
  constExpr = uuid

instance (ConstExpr sc a) => ConstExpr sc (Maybe a) where
  constExpr =
    maybe (literalExpr PQ.Null) (toNullable . constExpr)

instance ConstExpr sc LTree where
  constExpr = ltree

ltree :: LTree -> PQ.Expr sc LTree
ltree (LTree vs) = go vs
    where
      go = literalExpr . PQ.String . dotSep
      dotSep = T.intercalate "."

literalExpr :: PQ.Lit -> PQ.Expr sc a
literalExpr = PQ.Expr . PQ.ConstExpr


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
  constExpr (Key a) = coerceExpr . constExpr $ a

-- instance (ToJSON a, Typeable a) => ConstExpr sc (Json a) where
--   constExpr =
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
  PQ.Expr (PQ.BinExpr PQ.OpEq x y) -> PQ.Expr (PQ.BinExpr PQ.OpNotEq x y)
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

instance (EqExpr sc t) => EqExpr sc (fld ::: t) where
  a .== b = coerceExprTo a .== coerceExprTo b

    where coerceExprTo :: Expr sc (fld ::: a) -> Expr sc a
          coerceExprTo = coerceExpr

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

nil :: (PQ.DBTypeOf sc a) => Expr sc [a]
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
between _v (_lb, _ub) = undefined

lower :: Expr sc T.Text -> Expr sc T.Text
lower = prefixOp PQ.OpLower

upper :: Expr sc T.Text -> Expr sc T.Text
upper = prefixOp PQ.OpUpper

ors :: Foldable f => f (Expr sc Bool) -> Expr sc Bool
ors = F.foldl' (.||) false

in_ :: (Functor f, Foldable f, EqExpr sc a) => f (Expr sc a) -> Expr sc a -> Expr sc Bool
in_ exprs e = ors . fmap (e .==) $ exprs

true :: Expr sc Bool
true = Expr $ PQ.ConstExpr $ PQ.Bool True

false :: Expr sc Bool
false = Expr $ PQ.ConstExpr $ PQ.Bool False

array :: ( PQ.DBTypeOf sc a
         ) => [Expr sc a] -> Expr sc [a]
array = annotateType . Expr . PQ.ArrayExpr . coerce

iscontainedBy :: Expr sc [a] -> Expr sc [a] -> Expr sc Bool
iscontainedBy a b = binOp (PQ.OpOther "<@") a b

-- any :: Expr sc [a] -> Expr sc a
-- any (Expr e) = Expr (PQ.UnExpr (PQ.UnOpOtherFun "ANY") e)

jsonb ::
  forall sc a.
  ( A.ToJSON a
  ) => a -> Expr sc A.Value
jsonb = annotateType . Expr . PQ.ConstExpr . PQ.String . jsonify
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

{-
-- TODO: Provide a mapping to DiffTime
-- https://github.com/lpsmith/postgresql-simple/pull/115#issuecomment-48754627

-- From: https://github.com/lpsmith/postgresql-simple/blob/c1a3238b3bce67592fbf62c0ea0cd73708b947b3/src/Database/PostgreSQL/Simple/Time/Implementation.hs
parseTimeInterval :: A.Parser Interval
parseTimeInterval = do
  h <- A.decimal
  _ <- A.char ':'
  m <- A.decimal
  _ <- A.char ':'
  s <- A.decimal
  subsec <- A.option 0 (A.char '.' *> (A.decimal))
  return $ undefined -- secondsToDiffTime (h*3600 + m*60 + s) + picosecondsToDiffTime (subsec * 100000000000)

parseDayInterval :: A.Parser Interval
parseDayInterval = do
  n <- A.signed A.decimal
  factor <- A.choice [ A.string " year" *> pure (12*30*86400)
                     , A.string " mon"  *> pure (   30*86400)
                     , A.string " day"  *> pure (      86400)
                     ]
  _ <- A.string "s " <|> A.string " "
  return undefined -- (secondsToDiffTime (n*factor))

parseInterval :: A.Parser Interval
parseInterval = do
  ds <- many parseDayInterval
  timesign <- A.option 1 (A.char '+' *> pure 1 <|> A.char '-' *> pure (-1))
  time <- parseTimeInterval
  return undefined -- (sum ds + time*timesign)
-}


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
      , getExpr (constExpr @sc vs)
      , json0
      ]
    json0 = coerce $ jsonb @sc val

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

data ScopeRep = FieldRepNode  TypeRep
              | ScopeRepNode  ScopeRepMap
              deriving Show

newtype ScopeRepMap = ScopeRepMap { getScopeRepMap :: HM.HashMap T.Text ScopeRep }
                    deriving Show

insertScopeRepMap :: T.Text -> ScopeRep -> ScopeRepMap -> ScopeRepMap
insertScopeRepMap k v = ScopeRepMap . HM.insert k v . getScopeRepMap

emptyScopeRepMap :: ScopeRepMap
emptyScopeRepMap = ScopeRepMap HM.empty

lookupScopeRepMap :: T.Text -> ScopeRepMap -> Maybe ScopeRep
lookupScopeRepMap k = HM.lookup k . getScopeRepMap

class ToScopeRep (sc :: [Type]) acc where
  toScopeRep :: Proxy sc -> acc -> ScopeRepMap

instance ToScopeRep '[] acc where
  toScopeRep _ _ = emptyScopeRepMap

getScopeRep :: ( ToScopeRep sc (Proxy ('[] :: [Type -> Type]))
                ) => Proxy (sc :: [Type]) -> ScopeRepMap
getScopeRep = flip toScopeRep (Proxy :: Proxy ('[] :: [Type -> Type]))

lookupField :: [T.Text] -> ScopeRepMap -> Maybe TypeRep
lookupField (fld : flds) scrMap = case lookupScopeRepMap fld scrMap of
  Just (FieldRepNode tRep) -> case flds of
    [] -> Just tRep
    _  -> Nothing
  Just (ScopeRepNode scs) -> lookupField flds scs
  Nothing -> Nothing
lookupField _ _ = Nothing

checkFieldType :: (Typeable t) => [T.Text] -> Proxy t -> ScopeRepMap -> Bool
checkFieldType colPieces t scrMap =
  maybe False (== tyRep) (lookupField colPieces scrMap)
  where tyRep = typeRep t

type Validation = Either [ExprError]
data ExprError  = TypeMismatch TypeRep TypeRep -- Expected, Got
                | ParseErr
                deriving Show
                -- ...

choice :: Validation a -> Validation a -> Validation a
choice (Right a) _        = Right a
choice (Left _) (Right b) = Right b
choice (Left _) (Left b)  = Left b

infixr 3 `choice`

exprParseErr :: Validation a
exprParseErr = Left [ParseErr]

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
