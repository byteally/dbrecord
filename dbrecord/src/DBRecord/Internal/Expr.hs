{-# OPTIONS_GHC -fno-warn-redundant-constraints -Wno-orphans #-}
{-# LANGUAGE KindSignatures, DataKinds, ViewPatterns, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses, TypeApplications, TypeOperators, PatternSynonyms, CPP, PolyKinds, TypeFamilies, DefaultSignatures #-}
module DBRecord.Internal.Expr
       ( module DBRecord.Internal.Expr
       , Expr (..)
       ) where

import DBRecord.Internal.PrimQuery (Expr (..), AggExpr (..))
import DBRecord.Types
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
import Data.Time
import Data.Text (Text)
import DBRecord.Internal.Types
import DBRecord.Internal.DBTypes hiding (toNullable)
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
  constExpr :: t -> Expr sc t

  default constExpr :: (Generic t, GConstExpr (TypeMappings sc t) (Rep t) sc t) => t -> Expr sc t
  constExpr = gconstExpr (Proxy @(TypeMappings sc t)) . from

class GConstExpr (udType :: UDTypeMappings) (rep :: * -> *) sc a where
  gconstExpr :: Proxy udType -> rep x -> Expr sc a

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
  gconstExprFlat :: Proxy als -> rep x -> Expr sc a

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

    where appendFlatComposite :: Expr sc a -> Expr sc a -> Expr sc a
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

    where flatComposite :: Expr sc t -> Expr sc t
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
    where unwrap :: Expr sc t -> Expr sc (fn ::: t)
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

instance ( DBTypeCtx (GetDBTypeRep sc A.Value)
         , SingI (GetDBTypeRep sc A.Value)
         ) => ConstExpr sc A.Value where
  constExpr = jsonb

instance ( DBTypeCtx (GetDBTypeRep sc UUID)
         , SingI (GetDBTypeRep sc UUID)
         ) => ConstExpr sc UUID where
  constExpr = uuid

instance (ConstExpr sc a) => ConstExpr sc (Maybe a) where
  constExpr =
    maybe (literalExpr PQ.Null) (toNullable . constExpr)

instance ConstExpr sc LTree where
  constExpr = ltree

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

binOp :: PQ.BinOp -> Expr sc a -> Expr sc b -> Expr sc c
binOp op (Expr lhs) (Expr rhs) = Expr (PQ.BinExpr op lhs rhs)

prefixOp :: PQ.UnOp -> Expr sc a -> Expr sc b
prefixOp op (Expr expr) = Expr (PQ.PrefixExpr op expr)

postfixOp :: PQ.UnOp -> Expr sc a -> Expr sc b
postfixOp op (Expr expr) = Expr (PQ.PostfixExpr op expr)

funOp :: String -> Expr sc a -> Expr sc b
funOp op (Expr expr) = Expr (PQ.PrefixExpr (PQ.OpOtherFun op) expr)

unsafeCast :: DBType -> Expr sc a -> Expr sc b
unsafeCast castTo (Expr expr) = Expr $ PQ.CastExpr castTo expr

annotateType :: forall sc a.
                 ( DBTypeCtx (GetDBTypeRep sc a)
                 , SingI (GetDBTypeRep sc a)
                 ) => Expr sc a -> Expr sc a
annotateType = unsafeCast tyRep
  where tyRep = fromSing (sing :: Sing (GetDBTypeRep sc a))

unsafeCoerceExpr :: Expr sc a -> Expr sc b
unsafeCoerceExpr (Expr e) = Expr e

strictDecodeUtf8 :: SB.ByteString -> String
strictDecodeUtf8 = T.unpack . STE.decodeUtf8

lazyDecodeUtf8 :: LB.ByteString -> String
lazyDecodeUtf8 = LT.unpack . LTE.decodeUtf8

class (Num a) => NumExpr a where
  exprFromInteger :: Integer -> Expr sc a

literalExpr :: PQ.Lit -> Expr sc a
literalExpr = Expr . PQ.ConstExpr

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

instance FractionalExpr Float where
  exprFromRational = literalExpr . PQ.Double . fromRational

instance FractionalExpr Double where
  exprFromRational = literalExpr . PQ.Double . fromRational

instance ( DBTypeCtx (GetDBTypeRep sc T.Text)
         , SingI (GetDBTypeRep sc T.Text)
         ) => IsString (Expr sc T.Text) where
  fromString = text . T.pack

instance ( DBTypeCtx (GetDBTypeRep sc (CI T.Text))
         , SingI (GetDBTypeRep sc (CI T.Text))
         ) => IsString (Expr sc (CI T.Text)) where
  fromString = citext . mk . T.pack

instance ( IsString (Expr sc a)
         ) => IsString (Expr sc (Identity a)) where
  fromString = (coerce :: Expr sc a -> Expr sc (Identity a)) . fromString

class EqExpr sc a where
  (.==) :: Expr sc a -> Expr sc a -> Expr sc Bool

  (./=) :: Expr sc a -> Expr sc a -> Expr sc Bool
  (./=) a b = not_ (a .== b)

  default (.==) :: (Generic a, GEqExpr sc (TypeMappings sc a) (Rep a) a) => Expr sc a -> Expr sc a -> Expr sc Bool
  (.==) = geqExpr (Proxy :: Proxy '(Rep a, TypeMappings sc a))

infix 4 .==
infix 4 ./=

class GEqExpr sc (ud :: UDTypeMappings) rep a where
  geqExpr :: Proxy '(rep, ud) -> Expr sc a -> Expr sc a -> Expr sc Bool

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
  geqExprFlat :: Proxy '(rep, als) -> Expr sc a -> Expr sc a -> Expr sc Bool

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

    where coerceExprTo :: Expr sc (fld ::: a) -> Expr sc a
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

snoc :: Expr sc [a] -> Expr sc a -> Expr sc [a]
snoc arr v =
  let fun = PQ.FunExpr "array_append" [getExpr arr, getExpr v]
  in  Expr fun

append :: Expr sc [a] -> Expr sc [a] -> Expr sc [a]
append arrl arrr =
  let fun = PQ.FunExpr "array_cat" [getExpr arrl, getExpr arrr]
  in  Expr fun

nil :: ( SingI (GetDBTypeRep sc [a])
      , DBTypeCtx (GetDBTypeRep sc [a])
      ) => Expr sc [a]
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

infixr 3 .&&
(.&&) :: Expr sc Bool -> Expr sc Bool -> Expr sc Bool
(.&&) a b = binOp PQ.OpAnd a b

infixr 3 .||
(.||) :: Expr sc Bool -> Expr sc Bool -> Expr sc Bool
(.||) a b = binOp PQ.OpOr a b

not_ :: Expr sc Bool -> Expr sc Bool
not_ = prefixOp PQ.OpNot

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

array :: ( DBTypeCtx (GetDBTypeRep sc [a])
        , SingI (GetDBTypeRep sc [a])
        ) => [Expr sc a] -> Expr sc [a]
array = annotateType . Expr . PQ.ArrayExpr . coerce

iscontainedBy :: Expr sc [a] -> Expr sc [a] -> Expr sc Bool
iscontainedBy a b = binOp (PQ.OpOther "<@") a b

-- any :: Expr sc [a] -> Expr sc a
-- any (Expr e) = Expr (PQ.UnExpr (PQ.UnOpOtherFun "ANY") e)

pattern TRUE :: Expr sc Bool
pattern TRUE = Expr (PQ.ConstExpr (PQ.Bool True))

pattern FALSE :: Expr sc Bool
pattern FALSE = Expr (PQ.ConstExpr (PQ.Bool False))

jsonb ::
  forall sc a.
  ( A.ToJSON a
  , DBTypeCtx (GetDBTypeRep sc A.Value)
  , SingI (GetDBTypeRep sc A.Value)
  ) => a -> Expr sc A.Value
jsonb = annotateType . Expr . PQ.ConstExpr . PQ.String . jsonify
  where jsonify = T.pack . lazyDecodeUtf8 . A.encode

text :: T.Text -> Expr sc T.Text
text = Expr . PQ.ConstExpr . PQ.String

citext :: ( DBTypeCtx (GetDBTypeRep sc (CI T.Text))
         , SingI (GetDBTypeRep sc (CI T.Text))
         ) => CI T.Text -> Expr sc (CI T.Text)
citext = annotateType . Expr . PQ.ConstExpr . PQ.String . foldedCase

date :: ( DBTypeCtx (GetDBTypeRep sc Day)
       , SingI (GetDBTypeRep sc Day)
       ) => Day -> Expr sc Day
date = annotateType . Expr . PQ.ConstExpr . PQ.Other . T.pack . format
  where format = formatTime defaultTimeLocale "'%F'"

utcTime :: ( DBTypeCtx (GetDBTypeRep sc UTCTime)
          , SingI (GetDBTypeRep sc UTCTime)
          ) => UTCTime -> Expr sc UTCTime
utcTime = annotateType . Expr . PQ.ConstExpr . PQ.Other . T.pack . format
  where format = formatTime defaultTimeLocale "'%FT%TZ'"

localTime :: ( DBTypeCtx (GetDBTypeRep sc LocalTime)
            , SingI (GetDBTypeRep sc LocalTime)
            ) => LocalTime -> Expr sc LocalTime
localTime = annotateType . Expr . PQ.ConstExpr . PQ.Other . T.pack . format
  where format = formatTime defaultTimeLocale "'%FT%T%Q'"

timeOfDay :: ( DBTypeCtx (GetDBTypeRep sc TimeOfDay)
            , SingI (GetDBTypeRep sc TimeOfDay)
            ) => TimeOfDay -> Expr sc TimeOfDay
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

ltree :: LTree -> Expr sc LTree
ltree (LTree vs) = go vs
    where
      go = literalExpr . PQ.String . dotSep
      dotSep = T.intercalate "."

pgOID :: PGOID t -> Expr sc (PGOID t)
pgOID oid = go (getPGOID oid)
    where
      go = literalExpr . PQ.String

interval :: ( DBTypeCtx (GetDBTypeRep sc Interval)
           , SingI (GetDBTypeRep sc Interval)
           ) => Interval -> Expr sc Interval
interval (Interval e) = annotateType (literalExpr (PQ.Other e))

hours :: ( DBTypeCtx (GetDBTypeRep sc Interval)
           , SingI (GetDBTypeRep sc Interval)
           ) => Int -> Expr sc Interval
hours i = prefixOp (PQ.OpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " hours\'"

months :: ( DBTypeCtx (GetDBTypeRep sc Interval)
           , SingI (GetDBTypeRep sc Interval)
           ) => Int -> Expr sc Interval
months i = prefixOp (PQ.OpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " months\'"

days :: ( DBTypeCtx (GetDBTypeRep sc Interval)
        , SingI (GetDBTypeRep sc Interval)
        ) => Int -> Expr sc Interval
days i = prefixOp (PQ.OpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " days\'"

minutes :: ( DBTypeCtx (GetDBTypeRep sc Interval)
           , SingI (GetDBTypeRep sc Interval)
           ) => Int -> Expr sc Interval
minutes i = prefixOp (PQ.OpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " minutes\'"

seconds :: ( DBTypeCtx (GetDBTypeRep sc Interval)
           , SingI (GetDBTypeRep sc Interval)
           ) => Int -> Expr sc Interval
seconds i = prefixOp (PQ.OpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " seconds\'"

{-
-- TODO: decide on json
strToJson :: (Typeable a) => String -> Expr sc (Json a)
strToJson = annotateType . Expr . PQ.ConstExpr . PQ.String . T.pack

strToJsonStr :: (Typeable a) => String -> Expr sc (JsonStr a)
strToJsonStr = annotateType . Expr . PQ.ConstExpr . PQ.String . T.pack

lazyJson :: (Typeable a) => LB.ByteString -> Expr sc (Json a)
lazyJson = strToJson . lazyDecodeUtf8

toJson :: (ToJSON a, Typeable a) => a -> Expr sc (Json a)
toJson = lazyJson . A.encode

toJsonStr :: (ToJSON a, Typeable a) => a -> Expr sc (JsonStr a)
toJsonStr = strToJsonStr . lazyDecodeUtf8 . A.encode
-}

bytes :: SB.ByteString -> Expr sc SB.ByteString
bytes = Expr . PQ.ConstExpr . PQ.Byte

addInterval :: Expr sc Interval -> Expr sc Interval -> Expr sc Interval
addInterval e1 e2 = binOp PQ.OpPlus e1 e2

uuid :: ( DBTypeCtx (GetDBTypeRep sc UUID)
       , SingI (GetDBTypeRep sc UUID)
       ) => UUID -> Expr sc UUID
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
  , DBTypeCtx (GetDBTypeRep sc T.Text)
  , SingI (GetDBTypeRep sc T.Text)
  , DBTypeCtx (GetDBTypeRep sc [T.Text])
  , SingI (GetDBTypeRep sc [T.Text])
  , DBTypeCtx (GetDBTypeRep sc A.Value)
  , SingI (GetDBTypeRep sc A.Value)
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

count :: Expr scs a -> AggExpr scs Int64
count = coerce . funOp "count"

class Alias f where
  as :: f a -> Proxy fld -> f (fld ::: a)

instance Alias (Expr sc) where
  as e _ = coerceExpr e

instance Alias (AggExpr sc) where
  as e _ = coerceAggExpr e

instance ( lab ~ sym
         ) => IsLabel lab (Proxy sym) where
  fromLabel = Proxy
