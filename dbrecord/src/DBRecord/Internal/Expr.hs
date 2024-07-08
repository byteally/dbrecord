{-# OPTIONS_GHC -fno-warn-redundant-constraints -Wno-orphans #-}

{-# LANGUAGE KindSignatures, DataKinds, ViewPatterns, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses, TypeApplications, TypeOperators, PatternSynonyms, CPP, PolyKinds, TypeFamilies, DefaultSignatures, DerivingStrategies, LambdaCase #-}
module DBRecord.Internal.Expr
       ( module DBRecord.Internal.Expr
       , Expr (..), getExpr, toNullable, AggExpr (..), constExpr, ConstExpr (..), unsafeCast, unsafeCoerceExpr, match
       ) where

import qualified DBRecord.Internal.PrimQuery as PQ
import           DBRecord.Types
import qualified Data.Foldable as F
import           Data.Functor.Identity (Identity)
import qualified Data.Functor.Identity as I
-- import           Data.Functor.Const
-- import qualified Data.HashMap.Strict as HM
import           Data.String
import qualified Data.Text as T
-- import           Data.Typeable
-- import GHC.TypeLits
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Aeson as A
import qualified Data.Text.Encoding as STE
-- import qualified Data.Text.Lazy.Encoding as LTE
-- import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
-- import qualified Data.Text.Lazy as LT
import           Data.Time
import           Data.Text (Text)
import           Data.Scientific
-- import           Data.Void
-- import           DBRecord.Internal.Types hiding (DBTypeK (..), DBTypeNameK(..))
import           DBRecord.Internal.DBTypes
-- import           DBRecord.Internal.Schema
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Data.CaseInsensitive (CI, foldedCase, mk)
import           Data.Coerce
-- import           Data.Kind
-- import           Data.List.NonEmpty (NonEmpty (..))
-- import qualified Data.List.NonEmpty as NE
-- import           GHC.Generics
-- import           GHC.Records
-- import           GHC.TypeLits





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

(./=) :: EqExpr sc a => Expr sc a -> Expr sc a -> Expr sc Bool
(./=) a b = case (a .== b) of
  Expr (PQ.BinExpr PQ.OpEq x y) -> Expr (PQ.BinExpr PQ.OpNotEq x y)
  e -> not_ e

infix 4 .==
infix 4 ./=

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

pattern TRUE :: Expr sc Bool
pattern TRUE = Expr (PQ.ConstExpr (PQ.Bool True))

pattern FALSE :: Expr sc Bool
pattern FALSE = Expr (PQ.ConstExpr (PQ.Bool False))

not_ :: Expr sc Bool -> Expr sc Bool
not_ = \case
  TRUE -> FALSE
  FALSE -> TRUE
  e -> prefixOp PQ.OpNot e

isNull :: Expr sc (Maybe a) -> Expr sc Bool
isNull = postfixOp PQ.OpIsNull

isNotNull :: Expr sc (Maybe a) -> Expr sc Bool
isNotNull = postfixOp PQ.OpIsNotNull


matchNullable :: Expr sc b -> (Expr sc a -> Expr sc b) -> Expr sc (Maybe a) -> Expr sc b
matchNullable def f val = ifThenElse (isNull val) def (f $ unsafeCoerceExpr val)

fromNullable :: Expr sc a -> Expr sc (Maybe a) -> Expr sc a
fromNullable = flip matchNullable id

maybeToNullable :: Maybe (Expr sc a) -> Expr sc (Maybe a)
maybeToNullable = maybe nothing toNullable


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


iscontainedBy :: Expr sc [a] -> Expr sc [a] -> Expr sc Bool
iscontainedBy a b = binOp (PQ.OpOther "<@") a b

-- any :: Expr sc [a] -> Expr sc a
-- any (Expr e) = Expr (PQ.UnExpr (PQ.UnOpOtherFun "ANY") e)



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
