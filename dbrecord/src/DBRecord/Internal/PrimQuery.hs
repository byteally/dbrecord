{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, KindSignatures, TypeFamilies, DataKinds #-}
-- |
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
--                Purely Agile Limited (c) 2014-2016
-- License     :  BSD-style

module DBRecord.Internal.PrimQuery where

import Prelude hiding (product)
import Data.Text          (Text)
import Data.ByteString    (ByteString)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
-- import Data.Binary
-- import Data.Aeson (FromJSON (..), ToJSON (..))
-- import qualified Data.Aeson as A
-- import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
-- import qualified Data.ByteString.Base64 as B64
import Data.Generics.Uniplate.Direct
import DBRecord.Internal.DBTypes
import Data.String


-- import DBRecord.Migration hiding (TableName)
-- import GHC.TypeLits
-- import Data.Functor.Const
-- import Data.Proxy

type TableName  = Text
type WindowName = Text
type Attribute  = Text
type Assoc      = [(Attribute, PrimExpr)]
type Scheme     = [Attribute]
type Name       = Text

data JoinType = LeftJoin
              | RightJoin
              | FullJoin
              | InnerJoin
              | CrossJoin
              deriving (Show, Read, Generic, Eq, Ord)

data BinType = Union | Intersection | Except | UnionAll | IntersectionAll | ExceptAll
             deriving (Show, Read, Generic, Eq, Ord)

data Sym = Sym { symPrefix :: [Text]
               , symField  :: Text
               } deriving (Show, Read, Generic, Eq, Ord)

type Lateral = Bool

data CTE p q = CTE [WithExpr p] q
             deriving (Show, Read, Generic, Eq, Ord)

data WithExpr p = WithExpr TableName [Attribute] p
              deriving (Show, Read, Generic, Eq, Ord)

data TableExpr p = PrimQuery p
                 | TableName TableId
                 | TableFun  Name [Attribute]
                 deriving (Show, Read, Generic, Eq, Ord)

queryExpr :: p -> TableExpr p
queryExpr = PrimQuery

data PrimQuery = Table (TableExpr PrimQuery) Clauses
               | Product (NEL.NonEmpty (TableExpr PrimQuery)) Clauses
               | Join JoinType Lateral (Maybe PrimExpr) (TableExpr PrimQuery) (TableExpr PrimQuery) Clauses
               | Binary BinType PrimQuery PrimQuery (Maybe Text)
               | CTEQuery (CTE PrimQuery PrimQuery)
               -- Values
               deriving (Show)

data InsertQuery = InsertQuery TableId [Attribute] (NEL.NonEmpty [PrimExpr]) [PrimExpr]
                 deriving (Show)

data UpdateQuery = UpdateQuery TableId [PrimExpr] Assoc [PrimExpr]
                 deriving (Show)

data DeleteQuery = DeleteQuery TableId [PrimExpr]
                 deriving (Show)

type Projection = (Text, PrimExpr)

data Clauses = Clauses { projections :: [Projection]
                       , criteria    :: [PrimExpr]
                       , windows     :: [WindowClause]
                       , groupbys    :: [PrimExpr]
                       , havings     :: [PrimExpr]
                       , orderbys    :: [OrderExpr]
                       , limit       :: Maybe PrimExpr
                       , offset      :: Maybe PrimExpr
                       , alias       :: Maybe Text  
                       } deriving (Show)

clauses :: Clauses
clauses =
  Clauses { projections = []
          , criteria    = []
          , windows     = []
          , groupbys    = []
          , havings     = []
          , orderbys    = []
          , limit       = Nothing
          , offset      = Nothing
          , alias       = Nothing
          }

data WindowClause = WindowClause
  { windowName    :: Text
  , wpartitionbys :: WindowPart
  } deriving (Show)

data WindowPart = WindowPart
  { wpartExpr :: [PrimExpr]
  , worderbys :: [OrderExpr]
  } deriving (Show)
             
data TableId = TableId
  { schema :: Name
  , tableName :: TableName
  } deriving (Show, Read, Generic, Eq, Ord)

data Lit = Null
         | Default
         | Bool Bool
         | String Text
         | Byte ByteString
         | Integer Integer
         | Double Double
         | Other Text
         deriving (Show, Read, Generic, Eq, Ord)

data BinOp = OpEq | OpLt | OpLtEq | OpGt | OpGtEq | OpNotEq
           | OpAnd | OpOr
           | OpLike | OpIn
           | OpOther String  
           | OpCat
           | OpPlus | OpMinus | OpMul | OpDiv | OpMod
           | OpBitNot | OpBitAnd | OpBitOr | OpBitXor
           | OpAsg | OpAtTimeZone
           deriving (Show, Read, Generic, Eq, Ord)

data UnOp = OpNot
          | OpIsNull
          | OpIsNotNull
          | OpLength
          | OpAbs
          | OpNegate
          | OpLower
          | OpUpper
          | OpOtherPrefix String
          | OpOtherPostfix String
          | OpOtherFun String
          deriving (Show, Read, Generic, Eq, Ord)

data AggrOp = AggrCount | AggrSum | AggrAvg | AggrMin | AggrMax
            | AggrStdDev | AggrStdDevP | AggrVar | AggrVarP
            | AggrBoolOr | AggrBoolAnd | AggrArr | AggrStringAggr PrimExpr
            | AggrOther String
            deriving (Show, Eq)

data LimitOp = LimitOp Int | OffsetOp Int | LimitOffsetOp Int Int
             deriving (Show, Read, Generic, Eq, Ord)
                     
data OrderExpr = OrderExpr OrderOp PrimExpr
               deriving (Show, Eq)

data OrderNulls = NullsFirst | NullsLast
                deriving (Show, Read, Generic, Eq, Ord)

data OrderDirection = OpAsc | OpDesc
                    deriving (Show, Read, Generic, Eq, Ord)

data OrderOp = OrderOp
  { orderDirection :: OrderDirection
  , orderNulls     :: OrderNulls
  } deriving (Show, Read, Generic, Eq, Ord)

data PrimExpr = AttrExpr Sym -- Eg?
              -- | OidExpr  Attribute
              | BaseTableAttrExpr Attribute
              | CompositeExpr     PrimExpr Attribute -- ^ Composite Type Query
              | BinExpr   BinOp PrimExpr PrimExpr
              | PrefixExpr   UnOp PrimExpr
              | PostfixExpr  UnOp PrimExpr
              | AggrExpr  AggrOp PrimExpr [OrderExpr] -- ^ Only for internals
              | ConstExpr Lit
              | CaseExpr [(PrimExpr,PrimExpr)] PrimExpr
              | ListExpr [PrimExpr]
              | ParamExpr (Maybe Name) PrimExpr
              | FunExpr Name [PrimExpr]
              | CastExpr DBType PrimExpr -- ^ Cast an expression to a given type.
              | DefaultInsertExpr   -- Indicate that we want to insert the
                                    -- default value into a column.
                                    -- TODO: I'm not sure this belongs
                                    -- here.  Perhaps a special type is
                                    -- needed for insert expressions.
              | ArrayExpr [PrimExpr] -- ^ ARRAY[..]
              | NamedWindowExpr WindowName PrimExpr -- OVER
              | AnonWindowExpr [PrimExpr] [OrderExpr] PrimExpr -- OVER
              | TableExpr PQFun PrimExpr
              | FlatComposite [Projection]
            -- For Raw Expressions
              | RawExpr T.Text
              deriving ({-Read,-} Show, Eq{-, Generic, Eq, Ord-})

newtype PQFun = PQFun { getPqFun :: PrimExpr -> PrimQuery }

pqFun :: (PrimExpr -> PrimQuery) -> PQFun
pqFun = PQFun

instance Show PQFun where
  show (PQFun f) = "PQ: <" ++ show (f (FlatComposite [])) ++ ">"

instance Eq PQFun where
  _ == _ = False
  
instance Uniplate PrimExpr where
  uniplate (AttrExpr s)           = plate AttrExpr |- s
  uniplate (BaseTableAttrExpr b)  = plate BaseTableAttrExpr |- b
  uniplate (CompositeExpr pe a)   = plate CompositeExpr |* pe |- a
  uniplate (BinExpr bop pe1 pe2)  = plate BinExpr |- bop |* pe1 |* pe2
  uniplate (PrefixExpr uop pe)    = plate PrefixExpr |- uop |* pe
  uniplate (PostfixExpr uop pe)   = plate PostfixExpr |- uop |* pe  
  uniplate (AggrExpr aop pe oes)  = plate AggrExpr |- aop |* pe ||+ oes
  uniplate (ConstExpr l)          = plate ConstExpr |- l
  uniplate (CaseExpr alts def)    = plate CaseExpr |- alts |* def -- TODO: alts
  uniplate (ListExpr pes)         = plate ListExpr ||* pes
  uniplate (ParamExpr n pe)       = plate ParamExpr |- n |* pe
  uniplate (FunExpr n pes)        = plate FunExpr |- n ||* pes
  uniplate (CastExpr n pe)        = plate CastExpr |- n |* pe
  uniplate (DefaultInsertExpr)    = plate DefaultInsertExpr
  uniplate (ArrayExpr pes)        = plate ArrayExpr ||* pes
  uniplate (TableExpr {})         = error "Panic: not implemented for TableExpr"
  uniplate (NamedWindowExpr n pe) = plate NamedWindowExpr |- n |* pe
  uniplate (AnonWindowExpr p o e) = plate AnonWindowExpr ||* p |- o |* e
  uniplate (FlatComposite tpes)   = plate FlatComposite ||+ tpes

-- NOTE: Orphan!
instance Biplate (Text, PrimExpr) PrimExpr where
  biplate (a, b) = plate (,) |- a |* b

instance Biplate OrderExpr PrimExpr where
  biplate (OrderExpr oop pe) = plate OrderExpr |- oop |* pe

data PrimQueryFold p = PrimQueryFold
  { table     :: TableExpr p -> Clauses -> p
  , product   :: NEL.NonEmpty (TableExpr p) -> Clauses -> p
  , join      :: JoinType -> Lateral -> Maybe PrimExpr -> TableExpr p -> TableExpr p -> Clauses -> p
  , binary    :: BinType -> p -> p -> Maybe Text -> p
  , cte       :: CTE p p -> p
  
  -- , values    :: [Sym] -> (NEL.NonEmpty [PrimExpr]) -> p
  -- , label     :: String -> p -> p
  -- , relExpr   :: PrimExpr -> [(Sym, PrimExpr)] -> p
  -- ^ A relation-valued expression
  }

baseTable :: PrimQueryFold p -> TableId -> Clauses -> p
baseTable p tId = table p (TableName tId)

innerJoin :: PrimQueryFold p -> PrimExpr -> TableExpr p -> TableExpr p -> Clauses -> p
innerJoin p e = join p InnerJoin False (Just e)

leftJoin :: PrimQueryFold p -> PrimExpr -> TableExpr p -> TableExpr p -> Clauses -> p
leftJoin p e = join p LeftJoin False (Just e)

primQueryFoldDefault :: PrimQueryFold PrimQuery
primQueryFoldDefault = PrimQueryFold
  { table     = Table
  , product   = Product
  , join      = Join
  , binary    = Binary
  , cte       = CTEQuery
  -- , values    = Values                
  -- , label     = Label
  -- , relExpr   = RelExpr
  }

foldPrimQuery :: PrimQueryFold p -> PrimQuery -> p
foldPrimQuery f = fix fold
  where fold self primQ = case primQ of
          Table ti cs              -> table     f (goTExpr self ti) cs
          Product qs cs            -> product   f (fmap (goTExpr self) qs) cs
          Join j lt cond q1 q2 cs  -> join      f j lt cond (goTExpr self q1) (goTExpr self q2) cs
          CTEQuery ctev            -> cte       f (goCTE self ctev)
          Binary b q1 q2 a         -> binary    f b (self q1) (self q2) a
          -- Values ss pes             -> values    f ss pes
          -- Label l pq                -> label     f l (self pq)
          -- RelExpr pe syms           -> relExpr   f pe syms

        goCTE self (CTE wexps pq)          = CTE (map (goWith self) wexps) (self pq)
        goWith self (WithExpr tn attrs pq) = WithExpr tn attrs (self pq)

        goTExpr self (PrimQuery p)      = PrimQuery (self p)
        goTExpr _    (TableName t)      = TableName t
        goTExpr _    (TableFun n attrs) = TableFun n attrs
                  
fix :: (t -> t) -> t
fix g = let x = g x in x

newtype InsertQueryFold p = InsertQueryFold
  { insertQ :: TableId -> [Attribute] -> (NEL.NonEmpty [PrimExpr]) -> [PrimExpr] -> p
  }

insertQueryFoldDefault :: InsertQueryFold InsertQuery
insertQueryFoldDefault = InsertQueryFold { insertQ = InsertQuery}

foldInsertQuery :: InsertQueryFold p -> InsertQuery -> p
foldInsertQuery f = fix fold
  where fold _self (InsertQuery tid attr vals rets) = insertQ f tid attr vals rets

newtype UpdateQueryFold p = UpdateQueryFold
  { updateQ :: TableId -> [PrimExpr] -> Assoc -> [PrimExpr] -> p
  }

updateQueryFoldDefault :: UpdateQueryFold UpdateQuery
updateQueryFoldDefault = UpdateQueryFold {updateQ = UpdateQuery}

foldUpdateQuery :: UpdateQueryFold p -> UpdateQuery -> p
foldUpdateQuery f = fix fold
  where fold _self (UpdateQuery ti cond assoc rets) = updateQ f ti cond assoc rets

newtype DeleteQueryFold p = DeleteQueryFold
 { deleteQ :: TableId -> [PrimExpr] -> p
 }

deleteQueryFoldDefault :: DeleteQueryFold DeleteQuery
deleteQueryFoldDefault = DeleteQueryFold {deleteQ = DeleteQuery} 

foldDeleteQuery :: DeleteQueryFold p -> DeleteQuery -> p
foldDeleteQuery f = fix fold
  where fold _self (DeleteQuery ti cond) = deleteQ f ti cond

isFieldExpr :: PrimExpr -> Bool
isFieldExpr (AttrExpr {})          = True
isFieldExpr (BaseTableAttrExpr {}) = True
isFieldExpr (CompositeExpr {})     = True
isFieldExpr _                      = False

inverseBinOp :: BinOp -> Maybe BinOp
inverseBinOp OpEq = Just OpNotEq
inverseBinOp OpNotEq = Just OpEq
inverseBinOp OpLt = Just OpGtEq
inverseBinOp OpLtEq = Just OpGt
inverseBinOp OpGt = Just OpLtEq
inverseBinOp OpGtEq = Just OpLt
inverseBinOp _ = Nothing

normaliseExpr :: PrimExpr -> PrimExpr
normaliseExpr = transform go
  where
    go orig@(PrefixExpr OpNot (BinExpr bop pe1 pe2)) = case inverseBinOp bop of
      Just revOp -> BinExpr revOp pe1 pe2
      Nothing    -> orig
    go origExpr@(BinExpr OpAnd org1@(BinExpr OpLtEq pe1 pe2) pe3) = case pe3 of
      BinExpr OpNotEq pe31 pe32
        | pe1 == pe31 && pe2 == pe32 -> BinExpr OpLt org1 pe2
        | otherwise                  -> origExpr
      PrefixExpr OpNot (BinExpr OpEq pe31 pe32)
        | pe1 == pe31 && pe2 == pe32 -> BinExpr OpLt org1 pe2
        | otherwise                  -> origExpr
      _                              -> origExpr
    go origExpr@(BinExpr OpOr org1@(PrefixExpr OpNot (BinExpr OpLtEq pe1 pe2)) pe3) = case pe3 of
      BinExpr OpEq pe31 pe32
        | pe1 == pe31 && pe2 == pe32 -> BinExpr OpGtEq org1 pe2
        | otherwise                  -> origExpr
      PrefixExpr OpNot (BinExpr OpNotEq pe31 pe32)
        | pe1 == pe31 && pe2 == pe32 -> BinExpr OpGtEq org1 pe2
        | otherwise                  -> origExpr
      _                              -> origExpr
    go pe = pe

renderSym :: Sym -> Text
renderSym (Sym pfx fld) = T.intercalate "." (pfx ++ [fld])

modifyClause :: (Clauses -> Clauses) -> PrimQuery -> PrimQuery
modifyClause f pq = case pq of
  Table t cs              -> Table t (f cs)
  Product qs cs           -> Product qs (f cs)
  Join j lt cond q1 q2 cs -> Join j lt cond q1 q2 (f cs)
  
  -- NOTE: no modification on Binary and CTEs
  _                       -> pq

getClause :: PrimQuery -> Clauses
getClause pq = case pq of
  Table _ cs           -> cs
  Product _ cs         -> cs
  Join _ _ _ _ _ cs    -> cs
  _                    -> error "panic: Not implemented for CTE / Binary"

toSym :: [T.Text] -> Maybe Sym
toSym flds = case flds of
  (_ : _) -> Just $ Sym { symPrefix = init flds
                        , symField  = last flds
                        }
  _       -> Nothing

unsafeToSym :: [T.Text] -> Sym
unsafeToSym = maybe (error "Panic: unsafeToSym") id . toSym 

symFromText :: T.Text -> Sym
symFromText = unsafeToSym . singleton
  where singleton x = [x]

instance IsString Sym where
  fromString = symFromText . T.pack
        
{-
instance Binary PrimExpr
instance Binary OrderExpr
instance Binary AggrOp
instance Binary OrderOp
instance Binary UnOp
instance Binary OrderDirection
instance Binary OrderNulls
instance Binary BinOp
instance Binary Lit
instance Binary Sym
instance Binary WindowPart

instance FromJSON PrimExpr
instance FromJSON OrderExpr
instance FromJSON AggrOp
instance FromJSON OrderOp
instance FromJSON UnOp
instance FromJSON OrderDirection
instance FromJSON OrderNulls
instance FromJSON BinOp
instance FromJSON Lit
instance FromJSON Sym
instance FromJSON WindowPart

instance FromJSON ByteString where
  parseJSON = A.withText "Order" go
    where go t = case B64.decode (encodeUtf8 t) of
            Left e  -> fail e
            Right o -> pure o

instance ToJSON PrimExpr
instance ToJSON OrderExpr
instance ToJSON AggrOp
instance ToJSON OrderOp
instance ToJSON UnOp
instance ToJSON OrderDirection
instance ToJSON OrderNulls
instance ToJSON BinOp
instance ToJSON Lit
instance ToJSON Sym
instance ToJSON WindowPart

instance ToJSON ByteString where
  -- toEncoding = E.string . B64.encode
  toJSON     = A.String . decodeUtf8 . B64.encode
-}

transformPE :: (PrimExpr -> PrimExpr) -> PrimExpr -> PrimExpr
transformPE = transform

newtype Expr (scopes :: [*]) (t :: *) = Expr { getExpr :: PrimExpr }
                                      deriving Show

unsafeCol :: [T.Text] -> Expr sc a
unsafeCol = Expr . AttrExpr . sym
  where sym = maybe (error "Panic: Empty col @col_") id . toSym


{-

class BackendExpr (b :: *) where
  type BackendExprType b :: *
  backendExpr :: proxy b -> PrimExpr -> BackendExprType b

-}

