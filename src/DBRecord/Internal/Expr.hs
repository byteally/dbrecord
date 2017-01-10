{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE KindSignatures, DataKinds, ViewPatterns, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses, TypeApplications #-}
module DBRecord.Internal.Expr where

import qualified DBRecord.Internal.PrimQuery as PQ
import qualified Data.HashMap.Strict as HM
import qualified Data.Foldable as F
import Data.String
import qualified Data.Text as T
import Data.Functor.Identity
import Data.Typeable
import GHC.Exts
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Text.Read as R
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Monoid ((<>))
import Data.Binary
import Data.Time

newtype Expr (scopes :: [*]) (t :: *) = Expr { getExpr :: PQ.PrimExpr }
                                      deriving Show

binOp :: PQ.BinOp -> Expr sc a -> Expr sc b -> Expr sc c
binOp op (Expr lhs) (Expr rhs) = Expr (PQ.BinExpr op lhs rhs)

unOp :: PQ.UnOp -> Expr sc a -> Expr sc b
unOp op (Expr expr) = Expr (PQ.UnExpr op expr)

unsafeCast :: T.Text -> Expr sc a -> Expr sc b
unsafeCast castTo (Expr expr) = Expr $ PQ.CastExpr castTo expr

unsafeCoerceExpr :: Expr sc a -> Expr sc b
unsafeCoerceExpr (Expr e) = Expr e

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
  negate   = unOp PQ.OpAbs
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
  fromString = literalExpr . PQ.String . T.pack

instance (IsString (Expr sc a)
         ) => IsString (Expr sc (Identity a)) where
  fromString = (coerce :: Expr sc a -> Expr sc (Identity a)) . fromString

class EqExpr a where
  (.==) :: Expr sc a -> Expr sc a -> Expr sc Bool
  
  (./=) :: Expr sc a -> Expr sc a -> Expr sc Bool
  (./=) a b = not_ (a .== b)

infix 4 .==
infix 4 ./=

class (EqExpr a) => OrdExpr a where
  (.>) :: Expr sc a -> Expr sc a -> Expr sc Bool
  (.<)  :: Expr sc a -> Expr sc a -> Expr sc Bool
  (.>=) :: Expr sc a -> Expr sc a -> Expr sc Bool
  (.<=) :: Expr sc a -> Expr sc a -> Expr sc Bool
  
  (.>) a b = binOp PQ.OpGt a b
  (.<) a b = binOp PQ.OpLt a b
  (.>=) a b = binOp PQ.OpGtEq a b
  (.<=) a b = binOp PQ.OpLtEq a b

infix 4 .>
infix 4 .<
infix 4 .>=
infix 4 .<=

instance OrdExpr Int
instance OrdExpr Word

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

toNullable :: Expr sc a -> Expr sc (Maybe a)
toNullable = unsafeCoerceExpr

matchNullable :: Expr sc b -> (Expr sc a -> Expr sc b) -> Expr sc (Maybe a) -> Expr sc b
matchNullable def f val = ifThenElse (isNull val) (f $ unsafeCoerceExpr val) def

fromNullable :: Expr sc a -> Expr sc (Maybe a) -> Expr sc a
fromNullable = flip matchNullable id

maybeToNullable :: Maybe (Expr sc a) -> Expr sc (Maybe a)
maybeToNullable = maybe nothing toNullable

case_ :: [(Expr sc Bool, Expr sc r)] -> Expr sc a -> Expr sc r
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

text :: T.Text -> Expr sc T.Text
text = fromString . T.unpack

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


instance EqExpr T.Text where
  a .== b = binOp PQ.OpEq a b

instance EqExpr Int where
  a .== b = binOp PQ.OpEq a b

instance EqExpr Word where
  a .== b = binOp PQ.OpEq a b

instance (EqExpr a) => EqExpr (Maybe a) where
  a .== b = binOp PQ.OpEq a b

deriving instance (EqExpr a) => EqExpr (Identity a)


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
formatCol col
  | isCol col   = Just (splitCol col)
  | otherwise   = Nothing

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

instance ToJSON (Expr sc a) where
  toEncoding e = pairs ("trusted" .= getExpr e)
  toJSON     e = object ["trusted" .= getExpr e]

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

parseJSONExpr :: (Typeable a, ToScopeRep sc (Proxy ('[] :: [* -> *]))) => Value -> Parser (Expr sc a)
parseJSONExpr (Object eobj) = do
  tst  <- eobj .:? "trusted"
  case tst of
    Just e -> pure (Expr e)
    Nothing -> case withText "Expr" go <$> (HM.lookup "untrusted" eobj) of
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

-- We trust the binary input
instance Binary (Expr sc a) where
  put = put . getExpr
  get = Expr <$> get