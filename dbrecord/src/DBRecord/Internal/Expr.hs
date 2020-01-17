{-# OPTIONS_GHC -fno-warn-redundant-constraints -Wno-orphans #-}
{-# LANGUAGE KindSignatures, DataKinds, ViewPatterns, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses, TypeApplications, TypeOperators, PatternSynonyms, CPP, PolyKinds, TypeFamilies, DefaultSignatures #-}
module DBRecord.Internal.Expr
       ( module DBRecord.Internal.Expr
       , Expr (..)
       ) where

import DBRecord.Internal.PrimQuery (Expr (..), AggExpr (..))
import qualified DBRecord.Internal.PrimQuery as PQ
import qualified Data.HashMap.Strict as HM
import qualified Data.Foldable as F
import Data.String
import qualified Data.Text as T
import Data.Functor.Identity (Identity)
import qualified Data.Functor.Identity as I
import Data.Typeable
-- import GHC.TypeLits
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Aeson as A
import qualified Data.Text.Encoding as STE
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import qualified Data.Text.Lazy as LT
import Data.Monoid ((<>))
import Data.Time
import Data.Text (Text)
import DBRecord.Internal.Types
import Data.Coerce (coerce)
import DBRecord.Internal.DBTypes hiding (toNullable)
import Data.Time.Calendar (Day)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.CaseInsensitive (CI, foldedCase, mk)
import Data.Coerce
-- import Data.Aeson
import DBRecord.Internal.Schema (GetDBTypeRep, UDTargetType (..), GTarget)
import DBRecord.Internal.Common (FindAlias, NewtypeRep, FromJust)
import GHC.Generics
import GHC.TypeLits
import GHC.OverloadedLabels

class ConstExpr sc t where
  constExpr :: t -> Expr sc scope t

  default constExpr :: (Generic t, GConstExpr (TypeMappings sc t) (Rep t) sc t) => t -> Expr sc scope t
  constExpr = gconstExpr (Proxy @(TypeMappings sc t)) . from

class GConstExpr (udType :: UDTypeMappings) (rep :: * -> *) sc a where
  gconstExpr :: Proxy udType -> rep x -> Expr sc scopes a

instance (GConstExpr ('EnumType al als) g sc a) => GConstExpr ('EnumType al als) (D1 m g) sc a where
  gconstExpr p (M1 rep) =
    gconstExpr p rep

instance ( al ~ FindAlias als n
         , DBTypeCtx (GetDBTypeRep sc a)
         , SingI (GetDBTypeRep sc a)
         , KnownSymbol n
         , MaybeCtx SingE al
         , SingI al
         , SingE al
         ) => GConstExpr ('EnumType nal als) (C1 ('MetaCons n f s) U1) sc a where
  gconstExpr _ (M1 _) =
    annotateType (PQ.Expr . PQ.ConstExpr . PQ.String $ al)

    where al = maybe (T.pack (symbolVal (Proxy @n)))
               id
               (fromSing (sing :: Sing al))

instance ( GConstExpr ('EnumType nal als) g2 sc a
         , GConstExpr ('EnumType nal als) g1 sc a
         ) => GConstExpr ('EnumType nal als) (g1 :+: g2) sc a where
  gconstExpr p (L1 rep) =
    gconstExpr p rep
  gconstExpr p (R1 rep) =
    gconstExpr p rep

{-
instance GConstExpr ('Composite al als) rep sc a where
  gconstExpr _ _ev = undefined
-}

instance (GConstExpr ('EnumText als) g sc a) => GConstExpr ('EnumText als) (D1 m g) sc a where
  gconstExpr p (M1 rep) =
    gconstExpr p rep

instance ( al ~ FindAlias als n
         , DBTypeCtx (GetDBTypeRep sc a)
         , SingI (GetDBTypeRep sc a)
         , KnownSymbol n
         , MaybeCtx SingE al
         , SingI al
         , SingE al
         ) => GConstExpr ('EnumText als) (C1 ('MetaCons n f s) U1) sc a where
  gconstExpr _ (M1 _) =
    PQ.Expr . PQ.ConstExpr . PQ.String $ al

    where al = maybe (T.pack (symbolVal (Proxy @n)))
               id
               (fromSing (sing :: Sing al))

instance ( GConstExpr ('EnumText als) g1 sc a
         , GConstExpr ('EnumText als) g2 sc a
         ) => GConstExpr ('EnumText als) (g1 :+: g2) sc a where
  gconstExpr p (L1 rep) =
    gconstExpr p rep
  gconstExpr p (R1 rep) =
    gconstExpr p rep

instance ( GConstExprFlat als rep sc a
         ) => GConstExpr ('Flat als) rep sc a where
  gconstExpr _ = gconstExprFlat (Proxy @als) 

class GConstExprFlat als rep sc a where
  gconstExprFlat :: Proxy als -> rep x -> Expr sc scopes a

instance (GConstExprFlat als g sc a) => GConstExprFlat als (D1 m g) sc a where
  gconstExprFlat pals (M1 rep) =
    gconstExprFlat pals rep

instance (GConstExprFlat als g sc a) => GConstExprFlat als (C1 m g) sc a where
  gconstExprFlat pals (M1 rep) =
    gconstExprFlat pals rep

instance ( GConstExprFlat als g1 sc a
         , GConstExprFlat als g2 sc a
         ) => GConstExprFlat als (g1 :*: g2) sc a where
  gconstExprFlat pals (g1 :*: g2) =
    unsafeCoerceExpr (gconstExprFlat pals g1 `appendFlatComposite` gconstExprFlat pals g2)

    where appendFlatComposite :: Expr sc scopes a -> Expr sc scopes a -> Expr sc scopes a
          appendFlatComposite (Expr (PQ.FlatComposite xs)) (Expr (PQ.FlatComposite ys)) = Expr (PQ.FlatComposite (xs ++ ys))
          appendFlatComposite a b = error $ "Panic: expecting only flatcomposite @appendFlatComposite" ++ show (a, b)

instance ( al ~ FindAlias als n
         , MaybeCtx SingE al
         , SingI al
         , KnownSymbol n
         , ConstExpr sc t
         ) => GConstExprFlat als (S1 ('MetaSel ('Just n) su ss ds) (K1 i t)) sc a where
  gconstExprFlat _ (M1 (K1 v)) =
    unsafeCoerceExpr (flatComposite (constExpr v))

    where flatComposite :: forall scopes. Expr sc scopes t -> Expr sc scopes t
          flatComposite (Expr v0) = Expr (PQ.FlatComposite (pure (al, v0)))

          al = maybe (T.pack (symbolVal (Proxy @n)))
               id
               (fromSing (sing :: Sing al))

instance ( DBTypeCtx (GetDBTypeRep sc Text)
         , SingI (GetDBTypeRep sc Text)
         ) => ConstExpr sc Text where
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
  constExpr (Field v) = unwrap $ constExpr v
    where unwrap :: Expr sc scopes t -> Expr sc scopes (fn ::: t)
          unwrap = unsafeCoerceExpr

instance ConstExpr sc Double where
  constExpr = literalExpr . PQ.Double

instance ConstExpr sc Float where
  constExpr = literalExpr . PQ.Double . fromRational . toRational

instance ( DBTypeCtx (GetDBTypeRep sc (CI T.Text))
         , SingI (GetDBTypeRep sc (CI T.Text))
         ) => ConstExpr sc (CI T.Text) where
  constExpr = citext

instance ( DBTypeCtx (GetDBTypeRep sc Day)
         , SingI (GetDBTypeRep sc Day)
         ) => ConstExpr sc Day where
  constExpr = date

instance ( DBTypeCtx (GetDBTypeRep sc UTCTime)
         , SingI (GetDBTypeRep sc UTCTime)
         ) => ConstExpr sc UTCTime where
  constExpr = utcTime

instance ( DBTypeCtx (GetDBTypeRep sc LocalTime)
         , SingI (GetDBTypeRep sc LocalTime)
         ) => ConstExpr sc LocalTime where
  constExpr = localTime

instance ( DBTypeCtx (GetDBTypeRep sc TimeOfDay)
         , SingI (GetDBTypeRep sc TimeOfDay)
         ) => ConstExpr sc TimeOfDay where
  constExpr = timeOfDay

instance (ConstExpr sc a) => ConstExpr sc (Identity a) where
  constExpr = toIdentity . constExpr . I.runIdentity

instance ( DBTypeCtx (GetDBTypeRep sc [a])
         , SingI (GetDBTypeRep sc [a])
         , ConstExpr sc a
         ) => ConstExpr sc [a] where
  constExpr = array . map constExpr

instance ConstExpr sc Bool where
  constExpr = literalExpr . PQ.Bool

instance ConstExpr sc A.Value where
  constExpr = literalExpr . PQ.String . jsonify
    where jsonify = T.pack . lazyDecodeUtf8 . A.encode

instance ( DBTypeCtx (GetDBTypeRep sc UUID)
         , SingI (GetDBTypeRep sc UUID)
         ) => ConstExpr sc UUID where
  constExpr = uuid

instance (ConstExpr sc a) => ConstExpr sc (Maybe a) where
  constExpr =
    maybe (literalExpr PQ.Null) (toNullable . constExpr)

-- instance (ToJSON a, Typeable a) => ConstExpr sc (Json a) where
--   constExpr =
--     toJson . getJson

{-
class HasInsertValues sc t where
  insertValues :: Proxy sc -> t -> PQ.Assoc

instance (KnownSymbol fn, ConstExpr sc t) => HasInsertValues sc (Identity (fn ::: t)) where
  insertValues _ (I.Identity (Field v)) = [(T.pack $ symbolVal (Proxy @fn), getExpr $ constExpr v)]

instance ( KnownSymbol fn1
         , KnownSymbol fn2
         , ConstExpr sc t1
         , ConstExpr sc t2
         ) => HasInsertValues sc (fn1 ::: t1, fn2 ::: t2) where
  insertValues _ (Field v1, Field v2)
    = [ (T.pack $ symbolVal (Proxy @fn1), getExpr $ constExpr v1)
      , (T.pack $ symbolVal (Proxy @fn2), getExpr $ constExpr v2)
      ]

instance ( KnownSymbol fn1
         , KnownSymbol fn2
         , KnownSymbol fn3
         , ConstExpr sc t1
         , ConstExpr sc t2
         , ConstExpr sc t3
         ) => HasInsertValues sc (fn1 ::: t1, fn2 ::: t2, fn3 ::: t3) where
  insertValues _ (Field v1, Field v2, Field v3)
    = [ (T.pack $ symbolVal (Proxy @fn1), getExpr $ constExpr v1)
      , (T.pack $ symbolVal (Proxy @fn2), getExpr $ constExpr v2)
      , (T.pack $ symbolVal (Proxy @fn3), getExpr $ constExpr v3)
      ]
-}

binOp :: PQ.BinOp -> Expr sc scope a -> Expr sc scope b -> Expr sc scope c
binOp op (Expr lhs) (Expr rhs) = Expr (PQ.BinExpr op lhs rhs)

prefixOp :: PQ.UnOp -> Expr sc scope a -> Expr sc scope b
prefixOp op (Expr expr) = Expr (PQ.PrefixExpr op expr)

postfixOp :: PQ.UnOp -> Expr sc scope a -> Expr sc scope b
postfixOp op (Expr expr) = Expr (PQ.PostfixExpr op expr)

funOp :: String -> Expr sc scope a -> Expr sc scope b
funOp op (Expr expr) = Expr (PQ.PrefixExpr (PQ.OpOtherFun op) expr)

unsafeCast :: DBType -> Expr sc scope a -> Expr sc scope b
unsafeCast castTo (Expr expr) = Expr $ PQ.CastExpr castTo expr

annotateType :: forall sc scope a.
                 ( DBTypeCtx (GetDBTypeRep sc a)
                 , SingI (GetDBTypeRep sc a)
                 ) => Expr sc scope a -> Expr sc scope a
annotateType = unsafeCast tyRep
  where tyRep = fromSing (sing :: Sing (GetDBTypeRep sc a))

unsafeCoerceExpr :: Expr sc scope a -> Expr sc scope b
unsafeCoerceExpr (Expr e) = Expr e

strictDecodeUtf8 :: SB.ByteString -> String
strictDecodeUtf8 = T.unpack . STE.decodeUtf8

lazyDecodeUtf8 :: LB.ByteString -> String
lazyDecodeUtf8 = LT.unpack . LTE.decodeUtf8

class (Num a) => NumExpr a where
  exprFromInteger :: Integer -> Expr sc scope a

literalExpr :: PQ.Lit -> Expr sc scope a
literalExpr = Expr . PQ.ConstExpr

deriving instance (NumExpr a) => NumExpr (Identity a)

instance ( NumExpr a
         , OrdExpr sc a
         ) => Num (Expr sc scope a) where
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
  quot_ :: Expr sc scope a ->  Expr sc scope a -> Expr sc scope a
  rem_  :: Expr sc scope a ->  Expr sc scope a -> Expr sc scope a

  quot_ = binOp PQ.OpDiv
  rem_  = binOp PQ.OpMod

instance IntegralExpr Int
instance IntegralExpr Word
instance IntegralExpr Integer

class NumExpr a => FractionalExpr a where
  exprFromRational :: Rational -> Expr sc scope a

instance (FractionalExpr a, OrdExpr sc a) => Fractional (Expr sc scope a) where
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

instance FractionalExpr Float where
  exprFromRational = literalExpr . PQ.Double . fromRational

instance FractionalExpr Double where
  exprFromRational = literalExpr . PQ.Double . fromRational

instance ( DBTypeCtx (GetDBTypeRep sc T.Text)
         , SingI (GetDBTypeRep sc T.Text)
         ) => IsString (Expr sc scope T.Text) where
  fromString = text . T.pack

instance ( DBTypeCtx (GetDBTypeRep sc (CI T.Text))
         , SingI (GetDBTypeRep sc (CI T.Text))
         ) => IsString (Expr sc scope (CI T.Text)) where
  fromString = citext . mk . T.pack

instance ( IsString (Expr sc scope a)
         ) => IsString (Expr sc scope (Identity a)) where
  fromString = (coerce :: Expr sc scope a -> Expr sc scope (Identity a)) . fromString

class EqExpr sc a where
  (.==) :: Expr sc scope a -> Expr sc scope a -> Expr sc scope Bool

  (./=) :: Expr sc scope a -> Expr sc scope a -> Expr sc scope Bool
  (./=) a b = not_ (a .== b)

  default (.==) :: forall scope. (Generic a, GEqExpr sc (TypeMappings sc a) (Rep a) a) => Expr sc scope a -> Expr sc scope a -> Expr sc scope Bool
  (.==) = geqExpr (Proxy :: Proxy '(Rep a, TypeMappings sc a))

infix 4 .==
infix 4 ./=

class GEqExpr sc (ud :: UDTypeMappings) rep a where
  geqExpr :: Proxy '(rep, ud) -> Expr sc scope a -> Expr sc scope a -> Expr sc scope Bool

instance ( EqExpr sc (FromJust (NewtypeRep a))
         , Coercible a (FromJust (NewtypeRep a))
         ) => GEqExpr sc map (D1 ('MetaData n f s 'True) c) a where
  geqExpr _ e1 e2 = (coerceExpr @(FromJust (NewtypeRep a)) e1) .== coerceExpr e2

instance GEqExpr sc ('EnumType nal als) (D1 ('MetaData n f s 'False) c) a where
  geqExpr _ = binOp PQ.OpEq

instance GEqExpr sc ('Composite nal als) (D1 ('MetaData n f s 'False) c) a where
  geqExpr _ = binOp PQ.OpEq

instance GEqExpr sc ('EnumText als) (D1 ('MetaData n f s 'False) c) a where
  geqExpr _ = binOp PQ.OpEq

instance ( GEqExprFlat sc a als (D1 ('MetaData n f s 'False) c)
         ) => GEqExpr sc ('Flat als) (D1 ('MetaData n f s 'False) c) a where
  geqExpr _ = geqExprFlat (Proxy @'((D1 ('MetaData n f s 'False) c), als))

class GEqExprFlat sc a (als :: [(Symbol, Symbol)]) rep where
  geqExprFlat :: Proxy '(rep, als) -> Expr sc scope a -> Expr sc scope a -> Expr sc scope Bool

instance ( GEqExprFlat sc a als c
         ) => GEqExprFlat sc a als (D1 m c) where
  geqExprFlat _ e1 e2 =
    geqExprFlat (Proxy @'(c, als)) e1 e2

instance ( GEqExprFlat sc a als c
         ) => GEqExprFlat sc a als (C1 m c) where
  geqExprFlat _ e1 e2 =
    geqExprFlat (Proxy @'(c, als)) e1 e2

instance ( GEqExprFlat sc a als p
         , GEqExprFlat sc a als q
         ) => GEqExprFlat sc a als (p :*: q) where
  geqExprFlat _ e1 e2 =
    geqExprFlat (Proxy @'(p, als)) e1 e2 .&&
    geqExprFlat (Proxy @'(q, als)) e1 e2 

instance ( EqExpr sc t
         , UDTargetType ('Flat als) fld t a
         , t ~ GTarget fld (Rep a)
         ) => GEqExprFlat sc a als (S1 ('MetaSel ('Just fld) m1 m2 m3) (K1 m t)) where
  geqExprFlat _ e1 e2 =
    -- snd (hasField @fld e1) .== snd (hasField @fld e2)
    snd (udTargetType (Proxy @'(fld, 'Flat als)) e1) .==
    snd (udTargetType (Proxy @'(fld, 'Flat als)) e2)

instance (EqExpr sc t) => EqExpr sc (fld ::: t) where
  a .== b = coerceExprTo a .== coerceExprTo b

    where coerceExprTo :: Expr sc scope (fld ::: a) -> Expr sc scope a
          coerceExprTo = coerceExpr

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

instance OrdExpr sc Day where
  a .<= b = binOp PQ.OpLtEq a b

snoc :: Expr sc scope [a] -> Expr sc scope a -> Expr sc scope [a]
snoc arr v =
  let fun = PQ.FunExpr "array_append" [getExpr arr, getExpr v]
  in  Expr fun

append :: Expr sc scope [a] -> Expr sc scope [a] -> Expr sc scope [a]
append arrl arrr =
  let fun = PQ.FunExpr "array_cat" [getExpr arrl, getExpr arrr]
  in  Expr fun

nil :: ( SingI (GetDBTypeRep sc [a])
      , DBTypeCtx (GetDBTypeRep sc [a])
      ) => Expr sc scope [a]
nil = array []

class (EqExpr sc a) => OrdExpr sc a where
  (.>) :: Expr sc scope a -> Expr sc scope a -> Expr sc scope Bool
  (.<)  :: Expr sc scope a -> Expr sc scope a -> Expr sc scope Bool
  (.>=) :: Expr sc scope a -> Expr sc scope a -> Expr sc scope Bool
  (.<=) :: Expr sc scope a -> Expr sc scope a -> Expr sc scope Bool

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

infixr 3 .&&
(.&&) :: Expr sc scope Bool -> Expr sc scope Bool -> Expr sc scope Bool
(.&&) a b = binOp PQ.OpAnd a b

infixr 3 .||
(.||) :: Expr sc scope Bool -> Expr sc scope Bool -> Expr sc scope Bool
(.||) a b = binOp PQ.OpOr a b

not_ :: Expr sc scope Bool -> Expr sc scope Bool
not_ = prefixOp PQ.OpNot

isNull :: Expr sc scope (Maybe a) -> Expr sc scope Bool
isNull = postfixOp PQ.OpIsNull

isNotNull :: Expr sc scope (Maybe a) -> Expr sc scope Bool
isNotNull = postfixOp PQ.OpIsNotNull

nothing :: Expr sc scope (Maybe a)
nothing = Expr $ PQ.ConstExpr PQ.Null

toEnum :: forall a sc scope. (Enum a, Show a) => a -> Expr sc scope a
toEnum = Expr . PQ.ConstExpr . PQ.Other . quoteEnum
  where quoteEnum :: a -> T.Text
        quoteEnum s = let str = T.pack . show $ s
                      in "\'" <> str <> "\'"

toNullable :: Expr sc scope a -> Expr sc scope (Maybe a)
toNullable = unsafeCoerceExpr

matchNullable :: Expr sc scope b -> (Expr sc scope a -> Expr sc scope b) -> Expr sc scope (Maybe a) -> Expr sc scope b
matchNullable def f val = ifThenElse (isNull val) def (f $ unsafeCoerceExpr val)

fromNullable :: Expr sc scope a -> Expr sc scope (Maybe a) -> Expr sc scope a
fromNullable = flip matchNullable id

maybeToNullable :: Maybe (Expr sc scope a) -> Expr sc scope (Maybe a)
maybeToNullable = maybe nothing toNullable

case_ :: [(Expr sc scope Bool, Expr sc scope r)] -> Expr sc scope r -> Expr sc scope r
case_ alts (Expr def) = Expr $ PQ.CaseExpr (fmap (\(Expr f,Expr s) -> (f,s)) alts) def

ifThenElse :: Expr sc scope Bool -> Expr sc scope a -> Expr sc scope a -> Expr sc scope a
ifThenElse cond t f = case_ [(cond, t)] f

(.++) :: Expr sc scope T.Text -> Expr sc scope T.Text -> Expr sc scope T.Text
(.++) a b = binOp PQ.OpCat a b

like :: Expr sc scope T.Text -> Expr sc scope T.Text -> Expr sc scope Bool
like = binOp PQ.OpLike

lower :: Expr sc scope T.Text -> Expr sc scope T.Text
lower = prefixOp PQ.OpLower

upper :: Expr sc scope T.Text -> Expr sc scope T.Text
upper = prefixOp PQ.OpUpper

ors :: Foldable f => f (Expr sc scope Bool) -> Expr sc scope Bool
ors = F.foldl' (.||) false

in_ :: (Functor f, Foldable f, EqExpr sc a) => f (Expr sc scope a) -> Expr sc scope a -> Expr sc scope Bool
in_ exprs e = ors . fmap (e .==) $ exprs

true :: Expr sc scope Bool
true = Expr $ PQ.ConstExpr $ PQ.Bool True

false :: Expr sc scope Bool
false = Expr $ PQ.ConstExpr $ PQ.Bool False

array :: ( DBTypeCtx (GetDBTypeRep sc [a])
        , SingI (GetDBTypeRep sc [a])
        ) => [Expr sc scope a] -> Expr sc scope [a]
array = annotateType . Expr . PQ.ArrayExpr . coerce

iscontainedBy :: Expr sc scope [a] -> Expr sc scope [a] -> Expr sc scope Bool
iscontainedBy a b = binOp (PQ.OpOther "<@") a b

-- any :: Expr sc scope [a] -> Expr sc scope a
-- any (Expr e) = Expr (PQ.UnExpr (PQ.UnOpOtherFun "ANY") e)

pattern TRUE :: Expr sc scope Bool
pattern TRUE = Expr (PQ.ConstExpr (PQ.Bool True))

pattern FALSE :: Expr sc scope Bool
pattern FALSE = Expr (PQ.ConstExpr (PQ.Bool False))

text :: T.Text -> Expr sc scope T.Text
text = Expr . PQ.ConstExpr . PQ.String

citext :: ( DBTypeCtx (GetDBTypeRep sc (CI T.Text))
         , SingI (GetDBTypeRep sc (CI T.Text))
         ) => CI T.Text -> Expr sc scope (CI T.Text)
citext = annotateType . Expr . PQ.ConstExpr . PQ.String . foldedCase

date :: ( DBTypeCtx (GetDBTypeRep sc Day)
       , SingI (GetDBTypeRep sc Day)
       ) => Day -> Expr sc scope Day
date = annotateType . Expr . PQ.ConstExpr . PQ.Other . T.pack . format
  where format = formatTime defaultTimeLocale "'%F'"

utcTime :: ( DBTypeCtx (GetDBTypeRep sc UTCTime)
          , SingI (GetDBTypeRep sc UTCTime)
          ) => UTCTime -> Expr sc scope UTCTime
utcTime = annotateType . Expr . PQ.ConstExpr . PQ.Other . T.pack . format
  where format = formatTime defaultTimeLocale "'%FT%TZ'"

localTime :: ( DBTypeCtx (GetDBTypeRep sc LocalTime)
            , SingI (GetDBTypeRep sc LocalTime)
            ) => LocalTime -> Expr sc scope LocalTime
localTime = annotateType . Expr . PQ.ConstExpr . PQ.Other . T.pack . format
  where format = formatTime defaultTimeLocale "'%FT%T%Q'"

timeOfDay :: ( DBTypeCtx (GetDBTypeRep sc TimeOfDay)
            , SingI (GetDBTypeRep sc TimeOfDay)
            ) => TimeOfDay -> Expr sc scope TimeOfDay
timeOfDay = annotateType . Expr . PQ.ConstExpr . PQ.Other . T.pack . format
  where format = formatTime defaultTimeLocale "'%T%Q'"

utcTimeNow :: Expr sc scope UTCTime
utcTimeNow =
  let now = PQ.FunExpr "now" []
      utcT = PQ.BinExpr PQ.OpAtTimeZone now utcText
      utcText = PQ.ConstExpr (PQ.String "utc")
  in  Expr utcT

ist :: Expr sc scope TimeZone
ist = Expr (PQ.ConstExpr (PQ.String "ist"))

atTimeZone :: Expr sc scope TimeZone -> Expr sc scope UTCTime -> Expr sc scope LocalTime
atTimeZone (Expr tz) (Expr utct) = Expr (PQ.FunExpr "timezone" [tz, utct])

dayTruncTZ :: Expr sc scope LocalTime -> Expr sc scope LocalTime
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

interval :: ( DBTypeCtx (GetDBTypeRep sc Interval)
           , SingI (GetDBTypeRep sc Interval)
           ) => Interval -> Expr sc scope Interval
interval (Interval e) = annotateType (literalExpr (PQ.Other e))

hours :: ( DBTypeCtx (GetDBTypeRep sc Interval)
           , SingI (GetDBTypeRep sc Interval)
           ) => Int -> Expr sc scope Interval
hours i = prefixOp (PQ.OpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " hours\'"

months :: ( DBTypeCtx (GetDBTypeRep sc Interval)
           , SingI (GetDBTypeRep sc Interval)
           ) => Int -> Expr sc scope Interval
months i = prefixOp (PQ.OpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " months\'"

days :: ( DBTypeCtx (GetDBTypeRep sc Interval)
           , SingI (GetDBTypeRep sc Interval)
           ) => Int -> Expr sc scope Interval
days i = prefixOp (PQ.OpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " days\'"

minutes :: ( DBTypeCtx (GetDBTypeRep sc Interval)
           , SingI (GetDBTypeRep sc Interval)
           ) => Int -> Expr sc scope Interval
minutes i = prefixOp (PQ.OpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " minutes\'"

seconds :: ( DBTypeCtx (GetDBTypeRep sc Interval)
           , SingI (GetDBTypeRep sc Interval)
           ) => Int -> Expr sc scope Interval
seconds i = prefixOp (PQ.OpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " seconds\'"

{-
-- TODO: decide on json
strToJson :: (Typeable a) => String -> Expr sc scope (Json a)
strToJson = annotateType . Expr . PQ.ConstExpr . PQ.String . T.pack

strToJsonStr :: (Typeable a) => String -> Expr sc scope (JsonStr a)
strToJsonStr = annotateType . Expr . PQ.ConstExpr . PQ.String . T.pack

lazyJson :: (Typeable a) => LB.ByteString -> Expr sc scope (Json a)
lazyJson = strToJson . lazyDecodeUtf8

toJson :: (ToJSON a, Typeable a) => a -> Expr sc scope (Json a)
toJson = lazyJson . A.encode

toJsonStr :: (ToJSON a, Typeable a) => a -> Expr sc scope (JsonStr a)
toJsonStr = strToJsonStr . lazyDecodeUtf8 . A.encode
-}

bytes :: SB.ByteString -> Expr sc scope SB.ByteString
bytes = Expr . PQ.ConstExpr . PQ.Byte

addInterval :: Expr sc scope Interval -> Expr sc scope Interval -> Expr sc scope Interval
addInterval e1 e2 = binOp PQ.OpPlus e1 e2

uuid :: ( DBTypeCtx (GetDBTypeRep sc UUID)
       , SingI (GetDBTypeRep sc UUID)
       ) => UUID -> Expr sc scope UUID
uuid = annotateType . Expr . PQ.ConstExpr . PQ.Other . quoteVal . T.pack . UUID.toString
  where
    quoteVal str = "\'" <> str <> "\'"

addToDate :: Expr sc scope UTCTime -> Expr sc scope Interval -> Expr sc scope UTCTime
addToDate e1 e2 = binOp PQ.OpPlus e1 e2

dbDefault :: Expr sc scope a
dbDefault = Expr $ PQ.DefaultInsertExpr

dbDefault' :: PQ.PrimExpr
dbDefault' = PQ.DefaultInsertExpr

utcToLocalTime :: Expr sc scope T.Text
               -> Expr sc scope UTCTime
               -> Expr sc scope LocalTime
utcToLocalTime tz ut = binOp PQ.OpAtTimeZone ut tz

localTimeToUTC :: Expr sc scope T.Text
               -> Expr sc scope LocalTime
               -> Expr sc scope UTCTime
localTimeToUTC tz lt = binOp PQ.OpAtTimeZone lt tz

(%) :: Expr sc scope T.Text -> Expr sc scope T.Text -> Expr sc scope Bool
l % r = binOp (PQ.OpOther "%") l r

(%?) :: Expr sc scope (Maybe T.Text) -> Expr sc scope (Maybe T.Text) -> Expr sc scope Bool
l %? r = binOp (PQ.OpOther "%") l r

coalesce :: Expr sc scope a -> Expr sc scope (Maybe a) -> Expr sc scope a
coalesce (Expr d) (Expr opt) =
  Expr (PQ.FunExpr "COALESCE" [opt, d])

sum :: (NumExpr a) => Expr sc scope a -> Expr sc scope a
sum = Expr . PQ.FunExpr "sum" . singleton . getExpr
  where singleton x = [x]

avg :: (FractionalExpr a) => Expr sc scope a -> Expr sc scope a
avg = Expr . PQ.FunExpr "avg" . singleton . getExpr
  where singleton x = [x]

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

class ToScopeRep (sc :: [*]) acc where
  toScopeRep :: Proxy sc -> acc -> ScopeRepMap

instance ToScopeRep '[] acc where
  toScopeRep _ _ = emptyScopeRepMap

getScopeRep :: ( ToScopeRep sc (Proxy ('[] :: [* -> *]))
                ) => Proxy (sc :: [*]) -> ScopeRepMap
getScopeRep = flip toScopeRep (Proxy :: Proxy ('[] :: [* -> *]))

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

{-
parseExpr :: (Typeable t, ToScopeRep sc (Proxy ('[] :: [* -> *]))) => T.Text -> Validation (Expr sc t)
parseExpr t =
  parseColumnName  t `choice`
  parseLiteral     t `choice`
  exprParseErr

parseColumnName :: forall sc t. (Typeable t, ToScopeRep sc (Proxy ('[] :: [* -> *]))) => T.Text -> Validation (Expr sc t)
parseColumnName t = case formatCol t of
  Just colPieces -> case checkFieldType colPieces (Proxy @t) (getScopeRep (Proxy @sc)) of
    True  -> pure (Expr (PQ.AttrExpr (PQ.unsafeToSym colPieces)))
    False -> exprParseErr
  Nothing -> exprParseErr

parseLiteral :: forall sc t. (Typeable t) => T.Text -> Validation (Expr sc t)
parseLiteral = (Expr . PQ.ConstExpr <$>) . go
  where go lit = parseLitNull tRep lit    *> pure PQ.Null     `choice`
                 -- parseDefault tRep lit *> pure PQ.Default `choice`
                 PQ.Bool     <$> parseLitBool tRep lit        `choice`
                 PQ.String   <$> parseLitString tRep lit      `choice`
                 PQ.Integer  <$> parseLitInteger tRep lit     `choice`
                 PQ.Double   <$> parseLitDouble tRep lit      `choice`
                 -- PQ.Other    <$> parseLitOther tRep lit       `choice`
                 exprParseErr
        tRep = typeRep (Proxy @t)

parseLitNull :: TypeRep -> T.Text -> Validation ()
parseLitNull txtRep t
  | t == "null" = hasMaybeWrapper txtRep
  | otherwise   = exprParseErr
  where hasMaybeWrapper tRep = case splitTyConApp tRep of
          (tc, tReps) -> case (typeRepTyCon maybeTRep == tc) of
                              True -> pure ()
                              False -> foldr choice exprParseErr (map hasMaybeWrapper tReps)
        maybeTRep = typeRep (Proxy :: Proxy Maybe)

parseLitBool :: TypeRep -> T.Text -> Validation Bool
parseLitBool tRep t
  | tRep == typeOf True = case t of
    "true"  -> pure True
    "false" -> pure False
    _       -> exprParseErr
  | otherwise = exprParseErr

parseLitString :: TypeRep -> T.Text -> Validation T.Text
parseLitString tRep txt
  | tRep == typeRep (Proxy :: Proxy T.Text) && isLitStr txt = pure (dropQuotes txt)
  | otherwise                                               = exprParseErr

  where isLitStr t = case T.null t of
          True  -> False
          False -> T.head t == '\'' && T.last t == '\''
        dropQuotes = T.dropEnd 1 . T.drop 1


parseLitInteger :: TypeRep -> T.Text -> Validation Integer
parseLitInteger tRep t = case t of
  _ | typeRep (Proxy :: Proxy Int)     == tRep -> parseIntLike (Proxy @Int) t
    | typeRep (Proxy :: Proxy Int8)    == tRep -> parseIntLike (Proxy @Int8) t
    | typeRep (Proxy :: Proxy Int16)   == tRep -> parseIntLike (Proxy @Int16) t
    | typeRep (Proxy :: Proxy Int32)   == tRep -> parseIntLike (Proxy @Int32) t
    | typeRep (Proxy :: Proxy Int64)   == tRep -> parseIntLike (Proxy @Int64) t
    | typeRep (Proxy :: Proxy Integer) == tRep -> parseIntLike (Proxy @Integer) t
    | typeRep (Proxy :: Proxy Word)    == tRep -> parseWordLike (Proxy @Word) t
    | typeRep (Proxy :: Proxy Word8)   == tRep -> parseWordLike (Proxy @Word8) t
    | typeRep (Proxy :: Proxy Word16)  == tRep -> parseWordLike (Proxy @Word16) t
    | typeRep (Proxy :: Proxy Word32)  == tRep -> parseWordLike (Proxy @Word32) t
    | typeRep (Proxy :: Proxy Word64)  == tRep -> parseWordLike (Proxy @Word64) t
    | otherwise                                -> exprParseErr

  where parseIntLike :: (Integral i) => Proxy i -> T.Text -> Validation Integer
        parseIntLike p = (toInteger <$>) . getParsedVal p . R.signed R.decimal

        parseWordLike :: (Integral i) => Proxy i -> T.Text -> Validation Integer
        parseWordLike p = (toInteger <$>) . getParsedVal p . R.decimal

parseLitDouble :: TypeRep -> T.Text -> Validation Double
parseLitDouble tRep t = case t of
  _ | typeRep (Proxy :: Proxy Float)   == tRep -> parseFloat  t
    | typeRep (Proxy :: Proxy Double)  == tRep -> parseDouble t
    | otherwise                                -> exprParseErr

  where parseDouble = getParsedVal (Proxy @Double) . R.double
        -- TODO: Fix float
        parseFloat  = getParsedVal (Proxy @Double) . R.double

parseLitOther :: TypeRep -> T.Text -> Validation T.Text
parseLitOther _ _ = exprParseErr

getParsedVal :: Proxy i -> Either String (i, T.Text) -> Validation i
getParsedVal _ (Right (v, "")) = Right v
getParsedVal _ _               = exprParseErr

instance ToJSON (Expr sc a) where
  toEncoding e = A.pairs ("trusted" A..= getExpr e)
  toJSON     e = A.object ["trusted" A..= getExpr e]

instance (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => FromJSON (Expr sc Int) where
  parseJSON = parseJSONExpr

instance (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => FromJSON (Expr sc Int8) where
  parseJSON = parseJSONExpr

instance (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => FromJSON (Expr sc Int16) where
  parseJSON = parseJSONExpr

instance (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => FromJSON (Expr sc Int32) where
  parseJSON = parseJSONExpr

instance (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => FromJSON (Expr sc Int64) where
  parseJSON = parseJSONExpr

instance (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => FromJSON (Expr sc Bool) where
  parseJSON = parseJSONExpr

instance (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => FromJSON (Expr sc Char) where
  parseJSON = parseJSONExpr

instance (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => FromJSON (Expr sc Float) where
  parseJSON = parseJSONExpr

instance (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => FromJSON (Expr sc Double) where
  parseJSON = parseJSONExpr

instance (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => FromJSON (Expr sc Word) where
  parseJSON = parseJSONExpr

instance (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => FromJSON (Expr sc Word8) where
  parseJSON = parseJSONExpr

instance (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => FromJSON (Expr sc Word16) where
  parseJSON = parseJSONExpr

instance (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => FromJSON (Expr sc Word32) where
  parseJSON = parseJSONExpr

instance (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => FromJSON (Expr sc Word64) where
  parseJSON = parseJSONExpr

parseJSONExpr :: (Typeable a, ToScopeRep sc (Proxy ('[] :: [* -> *]))) => A.Value -> Parser (Expr sc a)
parseJSONExpr (A.Object eobj) = do
  tst  <- eobj A..:? "trusted"
  case tst of
    Just e -> pure (Expr e)
    Nothing -> case A.withText "Expr" go <$> (HM.lookup "untrusted" eobj) of
      Just p  -> p
      Nothing -> fail "key not found"
  where go texpr = case parseExpr texpr of
          Left  errs -> fail (T.unpack (renderErrs errs))
          Right v    -> pure v
parseJSONExpr e = typeMismatch "Expr" e

renderErrs :: [ExprError] -> T.Text
renderErrs errs =
  "Following errors occured while parsing the Expr\n" <>
  T.concat (map renderErr errs)

  where renderErr (TypeMismatch expr got) =
          "Couldn't match expected type '" <> (T.pack (show expr)) <> "' with actual type '" <> (T.pack (show got)) <> "\n"
        renderErr (ParseErr)             =
          "Parse error"
-}
{-
-- We trust the binary input
instance Binary (Expr sc a) where
  put = put . getExpr
  get = Expr <$> get
-}

runIdentity :: Expr sc scope (Identity a) -> Expr sc scope a
runIdentity = unsafeCoerceExpr

toIdentity :: Expr sc scope a -> Expr sc scope (Identity a)
toIdentity = unsafeCoerceExpr

coerceExpr :: forall b a sc scope. (Coercible a b) => Expr sc scope a -> Expr sc scope b
coerceExpr = unsafeCoerceExpr

unsafeCoerceAggExpr :: AggExpr sc scope a -> AggExpr sc scope b
unsafeCoerceAggExpr = coerce

coerceAggExpr :: forall b a sc scope. (Coercible a b) => AggExpr sc scope a -> AggExpr sc scope b
coerceAggExpr = unsafeCoerceAggExpr

rawExpr :: T.Text -> Expr sc scope a
rawExpr = (Expr . PQ.RawExpr)

count :: Expr sc scopes a -> AggExpr sc scopes Int64
count = coerce . funOp "count"

class Alias f where
  as :: f a -> Proxy fld -> f (fld ::: a)

instance Alias (Expr sc scope) where
  as e _ = coerceExpr e

instance Alias (AggExpr sc scope) where
  as e _ = coerceAggExpr e

instance ( lab ~ sym
         ) => IsLabel lab (Proxy sym) where
  fromLabel = Proxy
