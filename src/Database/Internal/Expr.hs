{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE KindSignatures, DataKinds, ViewPatterns, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses, TypeApplications #-}
module Database.Internal.Expr where

import qualified Database.Internal.PrimQuery as PQ
import Data.Aeson as A
import Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Foldable as F
import Data.String
import qualified Data.Text as T
import Data.Functor.Identity
import Data.Typeable
import Data.Scientific
import GHC.Exts
import Data.Typeable
import qualified Debug.Trace as DT
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Text.Read as R

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

instance (NumExpr a) => Num (Expr sc a) where
  fromInteger = exprFromInteger
  (*)      = binOp PQ.OpMul
  (+)      = binOp PQ.OpPlus
  (-)      = binOp PQ.OpMinus
  abs      = unOp PQ.OpAbs
  negate   = unOp PQ.OpAbs
  signum _ = undefined

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
  
instance (FractionalExpr a) => Fractional (Expr sc a) where
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
  (.==) a b = not_ (a ./= b)
  
  (./=) :: Expr sc a -> Expr sc a -> Expr sc Bool
  (./=) a b = not_ (a .== b)

infix 4 .==
infix 4 ./=

class OrdExpr a where
  (.>) :: Expr sc a -> Expr sc b -> Expr sc Bool
  (.<)  :: Expr sc a -> Expr sc b -> Expr sc Bool
  (.>=) :: Expr sc a -> Expr sc b -> Expr sc Bool
  (.<=) :: Expr sc a -> Expr sc b -> Expr sc Bool
  
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

dbDefault :: Expr sc a
dbDefault = Expr $ PQ.ConstExpr PQ.Default

toNullable :: Expr sc a -> Expr sc (Maybe a)
toNullable = unsafeCoerceExpr

matchNullable :: Expr sc b -> (Expr sc a -> Expr sc b) -> Expr sc (Maybe a) -> Expr sc b
matchNullable def f val = ifThenElse (isNull val) (f $ unsafeCoerceExpr val) def

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

text :: T.Text -> Expr sc T.Text
text = fromString . T.unpack


{- TODO
timestamptzAtTimeZone :: Expr sc UTCTime
                      -> Expr sc T.Text
                      -> Expr sc LocalTime
timestamptzAtTimeZone = binOp PQ.OpAtTimeZone
-}

instance EqExpr T.Text where
  a .== b = binOp PQ.OpEq a b

instance EqExpr Int where
  a .== b = binOp PQ.OpEq a b

instance (EqExpr a) => EqExpr (Maybe a) where
  a .== b = binOp PQ.OpEq a b

deriving instance (EqExpr a) => EqExpr (Identity a)

{-
instance (ToJSON a) => ToJSON (Expr sc a) where
  toJSON (Expr exp) = undefined

instance (FromJSON (ParseTag PQ.PrimExpr (Expr sc a))) => FromJSON (Expr sc a) where
  parseJSON v = undefined

data ParseTag t a = ParseTag a
                  deriving (Show)

invalidLit :: (Show act) => String -> act -> Parser a
invalidLit expected act = Prelude.fail $ "Invalid Literal. Expected: " ++ expected ++ ", Actual: " ++ (show act)

instance FromJSON (ParseTag PQ.Lit (Expr sc Int)) where
  parseJSON (A.Number n) = case floatingOrInteger n of
    Left f  -> invalidLit "Int" (typeOf (f :: Double))
    Right r -> pure $ ParseTag $ fromIntegral r

instance FromJSON (ParseTag PQ.Lit (Expr sc Word)) where
  parseJSON (A.Number n) = case floatingOrInteger n of
    Left f  -> invalidLit "Word" (typeOf (f :: Double))
    Right r -> pure $ ParseTag $ fromIntegral r    

instance FromJSON (ParseTag PQ.Lit (Expr sc Double)) where
  parseJSON (A.Number n) = case floatingOrInteger n of
    Left f  -> pure $ ParseTag $ fromRational $ toRational f
    Right r -> pure $ ParseTag $ fromIntegral r

instance FromJSON (ParseTag PQ.Lit (Expr sc Float)) where
  parseJSON (A.Number n) = case floatingOrInteger n of
    Left f  -> pure $ ParseTag $ fromRational $ toRational f
    Right r -> pure $ ParseTag $ fromIntegral r

instance FromJSON (ParseTag PQ.Lit (Expr sc Bool)) where
  parseJSON (A.Bool b) = if b then pure $ ParseTag true else pure $ ParseTag false
  parseJSON v          = typeMismatch "Bool" v

instance Typeable a => FromJSON (ParseTag PQ.Lit (Expr sc (Maybe a))) where
  parseJSON (A.Null) = pure $ ParseTag nothing
  parseJSON v          = typeMismatch (show $ typeRep (Proxy :: Proxy (Maybe a))) v

instance FromJSON (ParseTag PQ.Lit (Expr sc T.Text)) where
  parseJSON (A.String s) = pure $ ParseTag $ text s
  parseJSON v            = typeMismatch "Text" v  


{-
data Fn (a :: k) = Fn

class HasFun a f where
  hasFun :: Fn a -> f

instance (fn ~ (Expr sc a -> Expr sc a -> Expr sc a), NumExpr a)
         => HasFun 'PQ.OpPlus fn where
  hasFun _ = (+)

instance (fn ~ (Expr sc a -> Expr sc a -> Expr sc Bool), OrdExpr a)
         => HasFun 'PQ.OpLt fn where
  hasFun _ = (.<)  

data Ap f a = Ap

instance ( HasFun f (Expr sc a -> Expr sc r)
         ) => FromJSON (ParseTag (Fn f) (Expr sc a -> Expr sc r)) where
  parseJSON v = pure $ ParseTag $ hasFun (Fn :: Fn f)

type family ApRes f :: * where
  ApRes (a -> r) = ApRes r
  ApRes r       = r
  
class ApplyExpr f where
  applyExpr :: Value -> Parser f -> Parser (ApRes f)

instance ( ApplyExpr (Expr sc a)
         , NumExpr a
--         , ApplyExpr (Expr sc a -> Expr sc a -> Expr sc a)
         , ApplyExpr r
         ) => ApplyExpr (Expr sc a -> r) where
  applyExpr val fn = applyExpr val (fn Prelude.<*> parseJSON val)
  
{-
instance FromJSON (ParseTag (Ap (Expr sc a -> Expr sc r) (Expr sc a)) (Expr sc r)) where
  parseJSON v = 
  
instance FromJSON (ParseTag (Fn ()) (Expr sc a -> Expr sc r)) where
  parseJSON v = undefined

instance FromJSON (ParseTag (Fn ()) (Expr sc r)) where
  parseJSON v = undefined
-}
instance ( ApplyExpr (Expr sc a -> Expr sc a -> Expr sc a)
         , NumExpr a
         ) => FromJSON (ParseTag PQ.PrimExpr (Expr sc a)) where
  parseJSON v@(Object obj) | isBinOp obj = ParseTag <$> (applyExpr v $ (pure $ hasFun (Fn :: Fn 'PQ.OpPlus)))

{-
instance ( ApplyExpr (Expr sc a -> Expr sc a -> Expr sc a)
         , NumExpr a
         ) => FromJSON (ParseTag PQ.PrimExpr (Expr sc Bool)) where
  parseJSON v@(Object obj) | isBinOp obj = ParseTag <$> (applyExpr v $ (pure $ hasFun (Fn :: Fn 'PQ.OpPlus)))  
-}
-}

primExprFromJSON :: Value -> Parser PQ.PrimExpr
primExprFromJSON lit | isLitVal lit      = PQ.ConstExpr <$>  litFromJSON lit
primExprFromJSON (isTypedLit -> Just lit') = case lit' of
  Right (lit, ty) -> (PQ.CastExpr ty . PQ.ConstExpr) <$>  litFromJSON lit
  Left err        -> Prelude.fail err
primExprFromJSON (isBinOpVal -> Just bOpVal) = undefined
  

isTypedLit :: Value -> Maybe (Either String (Value, T.Text))
isTypedLit (Object obj) = case HM.toList obj of
  [("$type", Array vals)] -> case V.toList vals of
    [litVal, String ty] | isLitVal litVal -> Just $ Right (litVal, ty)
    [_,_] -> Just $ Left "Typed Literal show have second elem as type name"
  xs | HM.member "$type" obj  -> Just $ Left "Typed literal object should have exactly one elem"
  _  -> Nothing
isTypedLit _            = Nothing

isBinOpVal :: Value -> Maybe Value
isBinOpVal = undefined

binOps :: [T.Text]
binOps = ["$eq", "$lt", "$lte", "$gt", "$gte", "$ne"]

isBinOp :: Object -> Bool
isBinOp obj = case HM.toList obj of
  [(bop, val)] | bop `elem` binOps -> True
  _ -> False

{-
$<binOp> : [expr1, expr2]
$<unOp>  : expr1
$<aggOp> : [expr, order]
-}

primExprToJSON :: PQ.PrimExpr -> Value
primExprToJSON (PQ.ConstExpr lit) = litToJSON lit
primExprToJSON (PQ.AttrExpr sym) = A.String (PQ.renderSym sym)
primExprToJSON (PQ.BaseTableAttrExpr col) = A.String col
primExprToJSON (PQ.BinExpr bOp expr1 expr2) = object [bOpKey bOp A..= [
                                                         primExprToJSON expr1,
                                                         primExprToJSON expr2
                                                                      ]]
primExprToJSON (PQ.UnExpr uOp expr) = object [uOpKey uOp A..= primExprToJSON expr]
primExprToJSON (PQ.AggrExpr agOp expr ords) = undefined

tagVal :: Maybe T.Text -> Value -> Value
tagVal (Just t) v = object [t A..= v]
tagVal Nothing v  = v

isLitVal :: Value -> Bool
isLitVal = not . isTLVal
    
isTLVal :: Value -> Bool
isTLVal (Object _ ) = True
isTLVal (Array _ )  = True
isTLVal _           = False

instance FromJSON PQ.OrderOp where
  parseJSON = orderOpFromJSON

instance FromJSON PQ.PrimExpr where
  parseJSON = undefined

orderExprToJSON :: PQ.OrderExpr -> Value
orderExprToJSON (PQ.OrderExpr ordOp expr)
  = let exprVal' = primExprToJSON expr
        ordOpVal = orderOpToJSON ordOp
        exprVal  = case exprVal' of
          String s -> object [s A..= ordOpVal]
          _        -> object ["$by" A..= exprVal', "$op" A..=ordOpVal]      
    in object [orderTag A..= exprVal]

orderOpToJSON :: PQ.OrderOp -> Value
orderOpToJSON (PQ.OrderOp PQ.OpAsc PQ.NullsLast)   = A.Number 1
orderOpToJSON (PQ.OrderOp PQ.OpDesc PQ.NullsFirst) = A.Number (-1)
orderOpToJSON (PQ.OrderOp PQ.OpAsc PQ.NullsFirst)  = object
  [ orderDirTag A..= A.Number 1
  , orderNullTag A..= A.Number 1
  ]
orderOpToJSON (PQ.OrderOp PQ.OpDesc PQ.NullsLast)  = object
  [ orderDirTag A..= A.Number (-1)
  , orderNullTag A..= A.Number (-1)
  ]

orderExprFromJSON :: Value -> Parser PQ.OrderExpr
orderExprFromJSON v = withObject "OrderExpr" parseOrdE v
  where parseOrdE obj = case HM.toList obj of
          [(otag, val)] | otag == orderTag -> withObject "OrderOp" parseOrdExpr val
          _ -> Prelude.fail "Unable to parse OrderExpr"
        parseOrdExpr obj = case HM.size obj of
          1 -> do
            let [(col, opVal)] = HM.toList obj
            op <- orderOpFromJSON opVal
            return $ PQ.OrderExpr op (PQ.BaseTableAttrExpr col)
          2 -> do
            expr <- obj .: "$by"
            op <- obj .: "$op"
            return $ PQ.OrderExpr op expr
          _ -> Prelude.fail "Unable to parse Order Expression"
          

orderOpFromJSON :: Value -> Parser PQ.OrderOp
orderOpFromJSON (A.Number 1) = return (PQ.OrderOp PQ.OpAsc PQ.NullsLast)
orderOpFromJSON (A.Number (-1)) = return (PQ.OrderOp PQ.OpAsc PQ.NullsLast)
orderOpFromJSON (Object obj) = case HM.toList obj of
  [(dirT, Number 1), (nullT, Number 1)]
    | dirT == orderDirTag && nullT == orderNullTag ->
        return (PQ.OrderOp PQ.OpAsc PQ.NullsFirst)
  [(dirT, Number (-1)), (nullT, Number (-1))]
    | dirT == orderDirTag && nullT == orderNullTag ->
        return (PQ.OrderOp PQ.OpDesc PQ.NullsLast)
  _ -> Prelude.fail "Unable to parse Order Op"

orderTag,orderDirTag,orderNullTag :: T.Text
orderTag = "$order"
orderDirTag = "$dir"
orderNullTag = "$nulls"
  

litToJSON :: PQ.Lit -> Value
litToJSON (PQ.Null) = A.Null
litToJSON (PQ.Bool b) = A.Bool b
litToJSON (PQ.String s) = A.String s
litToJSON (PQ.Integer i) = A.Number $ fromIntegral i
litToJSON (PQ.Double d) = A.Number $ fromFloatDigits d
litToJSON PQ.Default    = String "DEFAULT"
litToJSON _             = undefined

litFromJSON :: Value -> Parser PQ.Lit
litFromJSON A.Null = return PQ.Null
litFromJSON (A.Bool b) = return $ PQ.Bool b
litFromJSON (A.String "DEFAULT") = return $ PQ.Default
litFromJSON (A.String s) = return $ PQ.String s
litFromJSON (A.Number n) = return $ either PQ.Double PQ.Integer $ floatingOrInteger n
litFromJSON _ = Prelude.fail "Unable to parse to Literal"

bOpKey :: PQ.BinOp -> T.Text
bOpKey PQ.OpEq = "$eq"

uOpKey :: PQ.UnOp -> T.Text
uOpKey PQ.OpNot = "$not"

  
{-  
data Expr (scopes :: [*]) (t :: *)
  = Col Symbol
  | ColRef (Tab scopes) Symbol
  | Num Nat
  | Str Symbol
  | Array [Expr scopes t]
  | SubScript (Expr scopes t) Nat
  | SubScript2d (Expr scopes t) Nat Nat
  | SubScript3d (Expr scopes t) Nat Nat Nat
  | Splice (Expr scopes t) Nat Nat
  | Sel (Expr scopes t) Symbol
  | BinOp BinOp (Expr scopes t) (Expr scopes t)
  | UnOp UnOp (Expr scopes t)
  | Symbol `Ap` [(Expr scopes t)]
            
data Cond (scopes :: [*]) (t :: *)
  = LT (Expr scopes t) (Expr scopes t)
  | GT (Expr scopes t) (Expr scopes t)
  | EQ (Expr scopes t) (Expr scopes t)
  | LE (Expr scopes t) (Expr scopes t)
  | GE (Expr scopes t) (Expr scopes t)
  | NE (Expr scopes t) (Expr scopes t)
  | And (Cond scopes t) (Cond scopes t)
  | Or (Cond scopes t) (Cond scopes t)
  | Not (Cond scopes t)
  | Between (Cond scopes t) (Cond scopes t)
  | NotBetween (Cond scopes t) (Cond scopes t)
  | IS IsCond        
data IsCond = Null
            | NotNull
            | Distinct
            | NotDistint

type UserR =
  '[ "name" ::: T.Text
   , "age"  ::: Int
   ]
-}

{-
-- Mathematical Operators
data (+) (expr1 :: Expr *) (expr2 :: Expr *)
data (-) (expr1 :: Expr *) (expr2 :: Expr *)
data (*) (expr1 :: Expr *) (expr2 :: Expr *)
data (/) (expr1 :: Expr *) (expr2 :: Expr *)
data (%) (expr1 :: Expr *) (expr2 :: Expr *)
data (^) (expr1 :: Expr *) (expr2 :: Expr *)

data SqaureRoot (expr :: Expr *)
data CubeRoot (expr :: Expr *)
data Fact (expr :: Expr *)
data Abs' (expr :: Expr *)
-}
{-
type family ExprType' (flds :: [*]) (expr :: Expr *) :: * where
  ExprType' 
-}
{-

"age" :? LT (Num 2 * (Col "b"))
Distinct '[Max "a", "b", "c"]
OrderBy '[Date "a"]

Filter '[ "age"              `Is` LT (Input "max_limit") && GE (Input "min_limit")
        , "user_id"          `Is` Eq (ColRef User "id")
        , DateOf "date_time" `Is` Eq (Input "serv_date")
        , DateOf "date_time" :? Eq (Input "serv_date")
        ] '["user"]

user.filter.max_limit = 60
user.filter.min_limit = 60
user.filter.serv_date = '10-10-2020'

Filter '["age" ::: NoInput, "date_time" ::: ]
LT ""
LT (ColRef Tab "")
LT (AsParam "foo")

{ "max_limit" :: Int
, "min_limit" :: Int
, "serv_date" :: Day
}
-}
-}

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
  
checkFieldType :: (Typeable t) => [T.Text] -> Proxy t -> ScopeRepMap -> Bool
checkFieldType colPieces t scrMap =
  DT.trace (show (colPieces,  scrMap, tyRep)) $ maybe False (== tyRep) (lookupField colPieces scrMap)
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
formatCol t
  | isCol t   = Just (splitCol t)
  | otherwise = Nothing

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
parseLitNull tRep t
  | t == "null" = hasMaybeWrapper tRep
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
parseLitString tRep t
  | tRep == typeRep (Proxy :: Proxy T.Text) && isLitStr t = pure (dropQuotes t)
  | otherwise                                             = exprParseErr

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
