{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveGeneric, FlexibleInstances #-}
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
import Data.Binary
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
import qualified Data.ByteString.Base64 as B64

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

data JoinType = LeftJoin | RightJoin | FullJoin | InnerJoin deriving (Show, Read, Generic)

data BinType = Union | Intersection | Except | UnionAll | IntersectionAll | ExceptAll
             deriving (Show, Read, Generic)

data Sym = Sym { symPrefix :: [Text]
               , symField  :: Text
               } deriving (Show, Read, Generic)

data PrimQuery = BaseTable TableId Clauses
               | Product (NEL.NonEmpty PrimQuery) Clauses
               | Join JoinType PrimExpr PrimQuery PrimQuery Clauses
               | Binary BinType PrimQuery PrimQuery Clauses
               -- Values
               deriving (Show, Read, Generic)

data InsertQuery = InsertQuery TableId [Attribute] (NEL.NonEmpty [PrimExpr])
                 deriving (Show, Read, Generic)

data UpdateQuery = UpdateQuery TableId [PrimExpr] Assoc
                 deriving (Show, Read, Generic)

data DeleteQuery = DeleteQuery TableId [PrimExpr]
                 deriving (Show, Read, Generic)


data Clauses = Clauses { projections :: [(Sym, PrimExpr)]
                       , criteria    :: [PrimExpr]
                       , windows     :: [WindowClause]
                       , groupbys    :: [PrimExpr]
                       , havings     :: [PrimExpr]
                       , orderbys    :: [OrderExpr]
                       , limit       :: Maybe PrimExpr
                       , offset      :: Maybe PrimExpr
                       , alias       :: Maybe Text  
                       } deriving (Show, Read, Generic)

data WindowClause = WindowClause
  { windowName    :: Text
  , wpartitionbys :: WindowPart
  } deriving (Show, Read, Generic)

data WindowPart = WindowPart
  { wpartExpr :: [PrimExpr]
  , worderbys :: [OrderExpr]
  } deriving (Show, Read, Generic)
             
data TableId = TableId
  { schema :: Name
  , tableName :: TableName
  } deriving (Show, Read, Generic)

data Lit = Null
         | Default
         | Bool Bool
         | String Text
         | Byte ByteString
         | Integer Integer
         | Double Double
         | Other Text
         deriving (Show, Read, Generic)

data BinOp = OpEq | OpLt | OpLtEq | OpGt | OpGtEq | OpNotEq
           | OpAnd | OpOr
           | OpLike | OpIn
           | OpOther String  
           | OpCat
           | OpPlus | OpMinus | OpMul | OpDiv | OpMod
           | OpBitNot | OpBitAnd | OpBitOr | OpBitXor
           | OpAsg | OpAtTimeZone
           deriving (Show, Read, Generic)

data UnOp = OpNot
          | OpIsNull
          | OpIsNotNull
          | OpLength
          | OpAbs
          | OpNegate
          | OpLower
          | OpUpper
          | UnOpOtherPrefix String
          | UnOpOtherFun String
          deriving (Show, Read, Generic)

data AggrOp = AggrCount | AggrSum | AggrAvg | AggrMin | AggrMax
            | AggrStdDev | AggrStdDevP | AggrVar | AggrVarP
            | AggrBoolOr | AggrBoolAnd | AggrArr | AggrStringAggr PrimExpr
            | AggrOther String
            deriving (Show, Read, Generic)

data LimitOp = LimitOp Int | OffsetOp Int | LimitOffsetOp Int Int
             deriving (Show, Read, Generic)
                     
data OrderExpr = OrderExpr OrderOp PrimExpr
               deriving (Show, Read, Generic)

data OrderNulls = NullsFirst | NullsLast
                deriving (Show, Read, Generic)

data OrderDirection = OpAsc | OpDesc
                    deriving (Show, Read, Generic)

data OrderOp = OrderOp
  { orderDirection :: OrderDirection
  , orderNulls     :: OrderNulls
  } deriving (Show, Read, Generic)

data PrimExpr = AttrExpr Sym -- Eg?
              -- | OidExpr  Attribute
              | BaseTableAttrExpr Attribute
              | CompositeExpr     PrimExpr Attribute -- ^ Composite Type Query
              | BinExpr   BinOp PrimExpr PrimExpr
              | UnExpr    UnOp PrimExpr
              | AggrExpr  AggrOp PrimExpr [OrderExpr] -- ^ Only for internals
              | ConstExpr Lit
              | CaseExpr [(PrimExpr,PrimExpr)] PrimExpr
              | ListExpr [PrimExpr]
              | ParamExpr (Maybe Name) PrimExpr
              | FunExpr Name [PrimExpr]
              | CastExpr Name PrimExpr -- ^ Cast an expression to a given type.
              | DefaultInsertExpr -- Indicate that we want to insert the
                -- default value into a column.
                                    -- TODO: I'm not sure this belongs
                                    -- here.  Perhaps a special type is
                                    -- needed for insert expressions.
              | ArrayExpr [PrimExpr] -- ^ ARRAY[..]
              | WindowExpr WindowName PrimExpr -- OVER   
              deriving (Read, Show, Generic)

data PrimQueryFold p = PrimQueryFold
  { baseTable :: TableId -> Clauses -> p
  , product   :: NEL.NonEmpty p -> Clauses -> p
  , join      :: JoinType -> PrimExpr -> p -> p -> Clauses -> p
  , binary    :: BinType  -> p -> p -> Clauses -> p
  
  -- , values    :: [Sym] -> (NEL.NonEmpty [PrimExpr]) -> p
  -- , label     :: String -> p -> p
  -- , relExpr   :: PrimExpr -> [(Sym, PrimExpr)] -> p
    -- ^ A relation-valued expression
  }

primQueryFoldDefault :: PrimQueryFold PrimQuery
primQueryFoldDefault = PrimQueryFold
  { baseTable = BaseTable
  , product   = Product
  , join      = Join
  -- , values    = Values
  , binary    = Binary
  -- , label     = Label
  -- , relExpr   = RelExpr
  }

foldPrimQuery :: PrimQueryFold p -> PrimQuery -> p
foldPrimQuery f = fix fold
  where fold self primQ = case primQ of
          BaseTable ti cs          -> baseTable f ti cs
          Product qs cs            -> product   f (fmap self qs) cs
          Join j cond q1 q2 cs     -> join      f j cond (self q1) (self q2) cs
          Binary b q1 q2 cs        -> binary  f b (self q1) (self q2) cs
          -- Values ss pes             -> values    f ss pes
          -- Label l pq                -> label     f l (self pq)
          -- RelExpr pe syms           -> relExpr   f pe syms
          
fix :: (t -> t) -> t
fix g = let x = g x in x

newtype InsertQueryFold p = InsertQueryFold
  { insertQ :: TableId -> [Attribute] -> (NEL.NonEmpty [PrimExpr]) -> p
  }

insertQueryFoldDefault :: InsertQueryFold InsertQuery
insertQueryFoldDefault = InsertQueryFold { insertQ = InsertQuery}

foldInsertQuery :: InsertQueryFold p -> InsertQuery -> p
foldInsertQuery f = fix fold
  where fold _self (InsertQuery tid attr vals) = insertQ f tid attr vals

newtype UpdateQueryFold p = UpdateQueryFold
  { updateQ :: TableId -> [PrimExpr] -> Assoc -> p
  }

updateQueryFoldDefault :: UpdateQueryFold UpdateQuery
updateQueryFoldDefault = UpdateQueryFold {updateQ = UpdateQuery}

foldUpdateQuery :: UpdateQueryFold p -> UpdateQuery -> p
foldUpdateQuery f = fix fold
  where fold _self (UpdateQuery ti cond assoc) = updateQ f ti cond assoc

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



renderSym :: Sym -> Text
renderSym (Sym pfx fld) = T.intercalate "." (pfx ++ [fld])

modifyClause :: (Clauses -> Clauses) -> PrimQuery -> PrimQuery
modifyClause f pq = case pq of
  BaseTable t cs       -> BaseTable t (f cs)
  Product qs cs        -> Product qs (f cs)
  Join j cond q1 q2 cs -> Join j cond q1 q2 (f cs)
  Binary bt q1 q2 cs -> Binary bt q1 q2 (f cs)

getClause :: PrimQuery -> Clauses
getClause pq = case pq of
  BaseTable _ cs       -> cs
  Product _ cs         -> cs
  Join _ _ _ _  cs     -> cs
  Binary _ _ _  cs     -> cs

toSym :: [T.Text] -> Maybe Sym
toSym flds = case flds of
  (_ : _) -> Just $ Sym { symPrefix = init flds
                        , symField  = last flds
                        }
  _       -> Nothing

unsafeToSym :: [T.Text] -> Sym
unsafeToSym = maybe (error "Panic: unsafeToSym") id . toSym 

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

