{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
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
import Data.Generics.Uniplate.Direct


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
               } deriving (Show, Read, Generic, Eq, Ord)

data PrimQuery = BaseTable TableId Clauses
               | Product (NEL.NonEmpty PrimQuery) Clauses
               | Join JoinType PrimExpr PrimQuery PrimQuery Clauses
               | Binary BinType PrimQuery PrimQuery Clauses
               -- Values
               deriving (Show, Read, Generic)

data InsertQuery = InsertQuery TableId [Attribute] (NEL.NonEmpty [PrimExpr]) [PrimExpr]
                 deriving (Show, Read, Generic)

data UpdateQuery = UpdateQuery TableId [PrimExpr] Assoc [PrimExpr]
                 deriving (Show, Read, Generic)

data DeleteQuery = DeleteQuery TableId [PrimExpr]
                 deriving (Show, Read, Generic)


type Projection = (Sym, PrimExpr)

data Clauses = Clauses { projections :: [Projection]
                       , criteria    :: [PrimExpr]
                       , windows     :: [WindowClause]
                       , groupbys    :: [PrimExpr]
                       , havings     :: [PrimExpr]
                       , orderbys    :: [OrderExpr]
                       , limit       :: Maybe PrimExpr
                       , offset      :: Maybe PrimExpr
                       , alias       :: Maybe Text  
                       } deriving (Show, Read, Generic)

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
          | UnOpOtherPrefix String
          | UnOpOtherFun String
          deriving (Show, Read, Generic, Eq, Ord)

data AggrOp = AggrCount | AggrSum | AggrAvg | AggrMin | AggrMax
            | AggrStdDev | AggrStdDevP | AggrVar | AggrVarP
            | AggrBoolOr | AggrBoolAnd | AggrArr | AggrStringAggr PrimExpr
            | AggrOther String
            deriving (Show, Read, Generic, Eq, Ord)

data LimitOp = LimitOp Int | OffsetOp Int | LimitOffsetOp Int Int
             deriving (Show, Read, Generic, Eq, Ord)
                     
data OrderExpr = OrderExpr OrderOp PrimExpr
               deriving (Show, Read, Generic, Eq, Ord)

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
              | FlatComposite [(Text, PrimExpr)]
              deriving (Read, Show, Generic, Eq, Ord)

instance Uniplate PrimExpr where
  uniplate (AttrExpr s)          = plate AttrExpr |- s
  uniplate (BaseTableAttrExpr b) = plate BaseTableAttrExpr |- b
  uniplate (CompositeExpr pe a)  = plate CompositeExpr |* pe |- a
  uniplate (BinExpr bop pe1 pe2) = plate BinExpr |- bop |* pe1 |* pe2
  uniplate (UnExpr uop pe)       = plate UnExpr |- uop |* pe
  uniplate (AggrExpr aop pe oes) = plate AggrExpr |- aop |* pe ||+ oes
  uniplate (ConstExpr l)         = plate ConstExpr |- l
  uniplate (CaseExpr alts def)   = plate CaseExpr |- alts |* def -- TODO: alts
  uniplate (ListExpr pes)        = plate ListExpr ||* pes
  uniplate (ParamExpr n pe)      = plate ParamExpr |- n |* pe
  uniplate (FunExpr n pes)       = plate FunExpr |- n ||* pes
  uniplate (CastExpr n pe)       = plate CastExpr |- n |* pe
  uniplate (DefaultInsertExpr)   = plate DefaultInsertExpr
  uniplate (ArrayExpr pes)       = plate ArrayExpr ||* pes
  uniplate (WindowExpr n pe)     = plate WindowExpr |- n |* pe
  uniplate (FlatComposite tpes)  = plate FlatComposite ||+ tpes

-- NOTE: Orphan!
instance Biplate (Text, PrimExpr) PrimExpr where
  biplate (a, b) = plate (,) |- a |* b

instance Biplate OrderExpr PrimExpr where
  biplate (OrderExpr oop pe) = plate OrderExpr |- oop |* pe

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
    go orig@(UnExpr OpNot (BinExpr bop pe1 pe2)) = case inverseBinOp bop of
      Just revOp -> BinExpr revOp pe1 pe2
      Nothing    -> orig
    go origExpr@(BinExpr OpAnd org1@(BinExpr OpLtEq pe1 pe2) pe3) = case pe3 of
      BinExpr OpNotEq pe31 pe32
        | pe1 == pe31 && pe2 == pe32 -> BinExpr OpLt org1 pe2
        | otherwise                  -> origExpr
      UnExpr OpNot (BinExpr OpEq pe31 pe32)
        | pe1 == pe31 && pe2 == pe32 -> BinExpr OpLt org1 pe2
        | otherwise                  -> origExpr
      _                              -> origExpr
    go origExpr@(BinExpr OpOr org1@(UnExpr OpNot (BinExpr OpLtEq pe1 pe2)) pe3) = case pe3 of
      BinExpr OpEq pe31 pe32
        | pe1 == pe31 && pe2 == pe32 -> BinExpr OpGtEq org1 pe2
        | otherwise                  -> origExpr
      UnExpr OpNot (BinExpr OpNotEq pe31 pe32)
        | pe1 == pe31 && pe2 == pe32 -> BinExpr OpGtEq org1 pe2
        | otherwise                  -> origExpr
      _                              -> origExpr
    go pe = pe

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

