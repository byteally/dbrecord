{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE KindSignatures, DataKinds, ViewPatterns, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses, TypeApplications, TypeOperators, PatternSynonyms, CPP, PolyKinds #-}
module DBRecord.Internal.Expr
       ( module DBRecord.Internal.Expr
       , Expr (..)
       ) where

import DBRecord.Internal.PrimQuery (Expr (..))
import qualified DBRecord.Internal.PrimQuery as PQ
import qualified Data.HashMap.Strict as HM
import qualified Data.Foldable as F
import Data.String
import qualified Data.Text as T
import Data.Functor.Identity
import Data.Typeable
import GHC.TypeLits
import GHC.OverloadedLabels
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Text.Read as R
import qualified Data.Aeson as A
import Data.Aeson (ToJSON (..), FromJSON (..))
import qualified Data.Text.Encoding as STE
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import qualified Data.Text.Lazy as LT
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Monoid ((<>))
import Data.Binary
import Data.Time
import Data.Text (Text)
import DBRecord.Internal.Common
import DBRecord.Internal.Types
import Data.Coerce (coerce)
import DBRecord.Internal.DBTypes hiding (toNullable)
import Data.Time.Calendar (Day)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.CaseInsensitive (CI, foldedCase, mk)
import Control.Applicative
import Data.Coerce
import DBRecord.Internal.Schema hiding (toNullable, DBType (..))
import DBRecord.Internal.Common
import GHC.Generics

class ConstExpr t where
  constExpr :: t -> Expr sc t

instance ConstExpr Text where
  constExpr = fromString . T.unpack

instance ConstExpr Int where
  constExpr = exprFromInteger . fromIntegral

instance ConstExpr SB.ByteString where
  constExpr = bytes

instance ConstExpr t => ConstExpr (fn ::: t) where
  constExpr (Field v) = coerce $ constExpr v

class HasInsertValues t where
  insertValues :: t -> PQ.Assoc

instance (KnownSymbol fn, ConstExpr t) => HasInsertValues (Identity (fn ::: t)) where
  insertValues (Identity (Field v)) = [(T.pack $ symbolVal (Proxy @fn), getExpr $ constExpr v)]

instance ( KnownSymbol fn1
         , KnownSymbol fn2
         , ConstExpr t1
         , ConstExpr t2
         ) => HasInsertValues (fn1 ::: t1, fn2 ::: t2) where
  insertValues (Field v1, Field v2)
    = [ (T.pack $ symbolVal (Proxy @fn1), getExpr $ constExpr v1)
      , (T.pack $ symbolVal (Proxy @fn2), getExpr $ constExpr v2)
      ]

instance ( KnownSymbol fn1
         , KnownSymbol fn2
         , KnownSymbol fn3
         , ConstExpr t1
         , ConstExpr t2
         , ConstExpr t3
         ) => HasInsertValues (fn1 ::: t1, fn2 ::: t2, fn3 ::: t3) where
  insertValues (Field v1, Field v2, Field v3)
    = [ (T.pack $ symbolVal (Proxy @fn1), getExpr $ constExpr v1)
      , (T.pack $ symbolVal (Proxy @fn2), getExpr $ constExpr v2)
      , (T.pack $ symbolVal (Proxy @fn3), getExpr $ constExpr v3)
      ]


binOp :: PQ.BinOp -> Expr sc a -> Expr sc b -> Expr sc c
binOp op (Expr lhs) (Expr rhs) = Expr (PQ.BinExpr op lhs rhs)

unOp :: PQ.UnOp -> Expr sc a -> Expr sc b
unOp op (Expr expr) = Expr (PQ.UnExpr op expr)

unsafeCast :: DBType -> Expr sc a -> Expr sc b
unsafeCast castTo (Expr expr) = Expr $ PQ.CastExpr castTo expr

annotateType' :: forall dbK sc a.
                 ( DBTypeCtx (GetDBTypeRep dbK a)
                 , SingI (GetDBTypeRep dbK a)
                 ) => Proxy (dbK :: DbK) -> Expr sc a -> Expr sc a
annotateType' _ = unsafeCast tyRep
  where tyRep = fromSing (sing :: Sing (GetDBTypeRep dbK a))


annotateType :: forall a sc.
               ( SingI (GetPGTypeRep a)
               , DBTypeCtx (GetPGTypeRep a)
               ) => Expr sc a -> Expr sc a
annotateType = unsafeCast tyRep
  where tyRep = fromSing (sing :: Sing (GetDBTypeRep 'Postgres a))


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
         , OrdExpr a
         ) => Num (Expr sc a) where
  fromInteger = exprFromInteger
  (*)      = binOp PQ.OpMul
  (+)      = binOp PQ.OpPlus
  (-)      = binOp PQ.OpMinus
  abs      = unOp PQ.OpAbs
  negate   = unOp PQ.OpNegate
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

instance (FractionalExpr a, OrdExpr a) => Fractional (Expr sc a) where
  fromRational = exprFromRational
  (/)    = binOp PQ.OpDiv

instance NumExpr Word where
  exprFromInteger = literalExpr . PQ.Integer . fromIntegral

instance NumExpr Int where
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

instance IsString (Expr sc T.Text) where
  fromString = text . T.pack

instance IsString (Expr sc (CI T.Text)) where
  fromString = citext . mk . T.pack

instance (IsString (Expr sc a)
         ) => IsString (Expr sc (Identity a)) where
  fromString = (coerce :: Expr sc a -> Expr sc (Identity a)) . fromString

class EqExpr a where
  (.==) :: Expr sc a -> Expr sc a -> Expr sc Bool
  (.==) a b = not_ (a ./= b)

  (./=) :: Expr sc a -> Expr sc a -> Expr sc Bool
  (./=) a b = not_ (a .== b)
  {-# MINIMAL (.==) | (./=) #-}

infix 4 .==
infix 4 ./=

instance (EqExpr t) => EqExpr (fld ::: t) where
  a .== b = coerceExprTo a .== coerceExprTo b

    where coerceExprTo :: Expr sc (fld ::: a) -> Expr sc a
          coerceExprTo = coerceExpr

instance EqExpr UTCTime where
  a .== b = binOp PQ.OpEq a b

instance EqExpr Integer where
  a .== b = binOp PQ.OpEq a b

instance EqExpr Float where
  a .== b = binOp PQ.OpEq a b

instance EqExpr Double where
  a .== b = binOp PQ.OpEq a b

class (EqExpr a) => OrdExpr a where
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

instance OrdExpr Int where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr Word where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr T.Text where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr (CI T.Text) where
  a .<= b = binOp PQ.OpLtEq a b

instance (OrdExpr a) => OrdExpr (Maybe a) where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr UTCTime where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr Integer where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr Float where
  a .<= b = binOp PQ.OpLtEq a b

instance OrdExpr Double where
  a .<= b = binOp PQ.OpLtEq a b

infixr 3 .&&
(.&&) :: Expr sc Bool -> Expr sc Bool -> Expr sc Bool
(.&&) a b = binOp PQ.OpAnd a b

infixr 3 .||
(.||) :: Expr sc Bool -> Expr sc Bool -> Expr sc Bool
(.||) a b = binOp PQ.OpOr a b

not_ :: Expr sc Bool -> Expr sc Bool
not_ = unOp PQ.OpNot

isNull :: Expr sc (Maybe a) -> Expr sc Bool
isNull = unOp PQ.OpIsNull

isNotNull :: Expr sc (Maybe a) -> Expr sc Bool
isNotNull = unOp PQ.OpIsNotNull

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
lower = unOp PQ.OpLower

upper :: Expr sc T.Text -> Expr sc T.Text
upper = unOp PQ.OpUpper

ors :: Foldable f => f (Expr sc Bool) -> Expr sc Bool
ors = F.foldl' (.||) false

in_ :: (Functor f, Foldable f, EqExpr a) => f (Expr sc a) -> Expr sc a -> Expr sc Bool
in_ exprs e = ors . fmap (e .==) $ exprs

true :: Expr sc Bool
true = Expr $ PQ.ConstExpr $ PQ.Bool True

false :: Expr sc Bool
false = Expr $ PQ.ConstExpr $ PQ.Bool False

array :: ( SingI (GetPGTypeRep a)
        , DBTypeCtx (GetPGTypeRep a)
        ) => [Expr sc a] -> Expr sc [a]
array = annotateType . Expr . PQ.ArrayExpr . coerce

isContainedBy :: Expr sc [a] -> Expr sc [a] -> Expr sc Bool
isContainedBy a b = binOp (PQ.OpOther "<@") a b

-- any :: Expr sc [a] -> Expr sc a
-- any (Expr e) = Expr (PQ.UnExpr (PQ.UnOpOtherFun "ANY") e)

pattern TRUE :: Expr sc Bool
pattern TRUE = Expr (PQ.ConstExpr (PQ.Bool True))

pattern FALSE :: Expr sc Bool
pattern FALSE = Expr (PQ.ConstExpr (PQ.Bool False))

text :: T.Text -> Expr sc T.Text
text = annotateType . Expr . PQ.ConstExpr . PQ.String

citext :: CI T.Text -> Expr sc (CI T.Text)
citext = annotateType . Expr . PQ.ConstExpr . PQ.String . foldedCase

date :: Day -> Expr sc Day
date = annotateType . Expr . PQ.ConstExpr . PQ.Other . T.pack . format
  where format = formatTime defaultTimeLocale "'%F'"

utcTime :: UTCTime -> Expr sc UTCTime
utcTime = annotateType . Expr . PQ.ConstExpr . PQ.Other . T.pack . format
  where format = formatTime defaultTimeLocale "'%FT%TZ'"

localTime :: LocalTime -> Expr sc LocalTime
localTime = annotateType . Expr . PQ.ConstExpr . PQ.Other . T.pack . format
  where format = formatTime defaultTimeLocale "'%FT%T%Q'"

timeOfDay :: TimeOfDay -> Expr sc TimeOfDay
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
atTimeZone (Expr tz) (Expr utc) = Expr (PQ.FunExpr "timezone" [tz, utc])

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

interval :: Interval -> Expr sc Interval
interval (Interval e) = annotateType (literalExpr (PQ.Other e))

hours :: Int -> Expr sc Interval
hours i = unOp (PQ.UnOpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " hours\'"

months :: Int -> Expr sc Interval
months i = unOp (PQ.UnOpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " months\'"

days :: Int -> Expr sc Interval
days i = unOp (PQ.UnOpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " days\'"

minutes :: Int -> Expr sc Interval
minutes i = unOp (PQ.UnOpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " minutes\'"

seconds :: Int -> Expr sc Interval
seconds i = unOp (PQ.UnOpOtherPrefix "interval") (literalExpr (PQ.Other txt))
  where txt = T.pack $ "\'" ++ show i ++ " seconds\'"

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

bytes :: SB.ByteString -> Expr sc SB.ByteString
bytes = Expr . PQ.ConstExpr . PQ.Byte

addInterval :: Expr sc Interval -> Expr sc Interval -> Expr sc Interval
addInterval e1 e2 = binOp PQ.OpPlus e1 e2

uuid :: UUID -> Expr sc UUID
uuid = annotateType . Expr . PQ.ConstExpr . PQ.Other . T.pack . UUID.toString

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

instance EqExpr Bool where
  a .== b = binOp PQ.OpEq a b

instance EqExpr T.Text where
  a .== b = binOp PQ.OpEq a b

instance EqExpr (CI T.Text) where
  a .== b = binOp PQ.OpEq a b

instance EqExpr Int where
  a .== b = binOp PQ.OpEq a b

instance EqExpr Word where
  a .== b = binOp PQ.OpEq a b

instance (EqExpr a) => EqExpr (Maybe a) where
  a .== b = binOp PQ.OpEq a b

deriving instance (EqExpr a)  => EqExpr (Identity a)
deriving instance (OrdExpr a) => OrdExpr (Identity a)


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

{-
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

runIdentity :: Expr sc (Identity a) -> Expr sc a
runIdentity = unsafeCoerceExpr

toIdentity :: Expr sc a -> Expr sc (Identity a)
toIdentity = unsafeCoerceExpr

coerceExpr :: (Coercible a b) => Expr sc a -> Expr sc b
coerceExpr = unsafeCoerceExpr