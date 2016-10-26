{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
-- |
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
--                Purely Agile Limited (c) 2014-2016
-- License     :  BSD-style

module Database.Internal.PrimQuery where

import Prelude hiding (product)
import Data.Text          (Text)
import Data.ByteString    (ByteString)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T

-- import Database.Migration hiding (TableName)
-- import GHC.TypeLits
-- import Data.Functor.Const
-- import Data.Proxy

type TableName = Text
type Attribute = Text
type Assoc     = [(Attribute, PrimExpr)]
type Scheme    = [Attribute]
type Name      = Text

data JoinType = LeftJoin | RightJoin | FullJoin | InnerJoin deriving (Show, Read)

data Sym = Sym { symPrefix :: [Text]
               , symField  :: Text
               } deriving (Show, Read)

data PrimQuery = BaseTable TableId Clauses
               | Product (NEL.NonEmpty PrimQuery) Clauses
               | Join JoinType PrimExpr PrimQuery PrimQuery Clauses
               -- | Values
               deriving (Show, Read)

data Clauses = Clauses { projections :: [(Sym, PrimExpr)]
                       , criteria    :: [PrimExpr]
                       , groupbys    :: [PrimExpr]
                       , havings     :: [PrimExpr]
                       , orderbys    :: [OrderExpr]
                       , limit       :: Maybe Int
                       , offset      :: Maybe Int
                       , alias       :: Maybe Text  
                       } deriving (Show, Read)
                         
data TableId = TableId
  { schema :: Name
  , tableName :: TableName
  } deriving (Show, Read)

data Lit = Null
         | Default
         | Bool Bool
         | String Text
         | Byte ByteString
         | Integer Integer
         | Double Double
         | Other Text
         deriving (Show, Read)

data BinOp = OpEq | OpLt | OpLtEq | OpGt | OpGtEq | OpNotEq
           | OpAnd | OpOr
           | OpLike | OpIn
           | OpOther String  
           | OpCat
           | OpPlus | OpMinus | OpMul | OpDiv | OpMod
           | OpBitNot | OpBitAnd | OpBitOr | OpBitXor
           | OpAsg | OpAtTimeZone
           deriving (Show,Read)

data UnOp = OpNot
          | OpIsNull
          | OpIsNotNull
          | OpLength
          | OpAbs
          | OpNegate
          | OpLower
          | OpUpper
          | UnOpOther String
          deriving (Show,Read)

data AggrOp = AggrCount | AggrSum | AggrAvg | AggrMin | AggrMax
            | AggrStdDev | AggrStdDevP | AggrVar | AggrVarP
            | AggrBoolOr | AggrBoolAnd | AggrArr | AggrStringAggr PrimExpr
            | AggrOther String
            deriving (Show, Read)

data LimitOp = LimitOp Int | OffsetOp Int | LimitOffsetOp Int Int
             deriving (Show, Read)
                     
data OrderExpr = OrderExpr OrderOp PrimExpr
               deriving (Show,Read)

data OrderNulls = NullsFirst | NullsLast
                deriving (Show,Read)

data OrderDirection = OpAsc | OpDesc
                    deriving (Show,Read)

data OrderOp = OrderOp
  { orderDirection :: OrderDirection
  , orderNulls     :: OrderNulls
  } deriving (Show,Read)

data PrimExpr = AttrExpr Sym -- Eg?
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
              deriving (Read,Show)

data PrimQueryFold p = PrimQueryFold
  { baseTable :: TableId -> Clauses -> p
  , product   :: NEL.NonEmpty p -> Clauses -> p
  , join      :: JoinType -> PrimExpr -> p -> p -> Clauses -> p
  -- , values    :: [Sym] -> (NEL.NonEmpty [PrimExpr]) -> p
  -- , binary    :: BinOp -> [(Symbol, (PrimExpr, PrimExpr))] -> (p, p) -> p
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
  -- , binary    = Binary
  -- , label     = Label
  -- , relExpr   = RelExpr
  }

foldPrimQuery :: PrimQueryFold p -> PrimQuery -> p
foldPrimQuery f = fix fold
  where fold self primQ = case primQ of
          BaseTable ti cs          -> baseTable f ti cs
          Product qs cs            -> product   f (fmap self qs) cs
          Join j cond q1 q2 cs     -> join      f j cond (self q1) (self q2) cs
          -- Values ss pes             -> values    f ss pes
          -- Binary binop pes (q1, q2) -> binary    f binop pes (self q1, self q2)
          -- Label l pq                -> label     f l (self pq)
          -- RelExpr pe syms           -> relExpr   f pe syms
          
        fix g = let x = g x in x

renderSym :: Sym -> Text
renderSym (Sym pfx fld) = T.intercalate "." (pfx ++ [fld])

modifyClause :: (Clauses -> Clauses) -> PrimQuery -> PrimQuery
modifyClause f pq = case pq of
  BaseTable t cs       -> BaseTable t (f cs)
  Product qs cs        -> Product qs (f cs)
  Join j cond q1 q2 cs -> Join j cond q1 q2 (f cs)

getClause :: PrimQuery -> Clauses
getClause pq = case pq of
  BaseTable _ cs       -> cs
  Product _ cs         -> cs
  Join _ _ _ _  cs     -> cs

toSym :: [T.Text] -> Maybe Sym
toSym flds = case flds of
  (_ : _) -> Just $ Sym { symPrefix = init flds
                        , symField  = last flds
                        }
  _       -> Nothing

unsafeToSym :: [T.Text] -> Sym
unsafeToSym = maybe (error "Panic: unsafeToSym") id . toSym 
