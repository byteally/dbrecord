{-# LANGUAGE DataKinds, KindSignatures #-}
module Database.Internal.Postgres.Types where

import qualified Data.List.NonEmpty as NEL
import Data.Text

type TableName = Text
type Name      = Text

data Join = Join
  { jJoinType :: JoinType
  , jTables :: (SqlSelect, SqlSelect)
  , jCond :: SqlExpr
  } deriving (Show, Read, Eq)

data JoinType = LeftJoin
              | RightJoin
              | FullJoin
              | InnerJoin
              deriving (Show, Read, Eq)

data SqlTable = SqlTable
  { sqlTableSchemaName :: Maybe String
  , sqlTableName       :: String
  } deriving (Show, Read, Eq)
  
newtype SqlColumn = SqlColumn [Text]
                  deriving (Show, Read, Eq)
type SqlName = Text

data SqlType = SqlType Text
             | SqlType1 Text Int
             | SqlType2 Text Int Int
             deriving (Show, Read, Eq)


data SqlOrder = SqlOrder
  { sqlOrdDirection :: SqlOrdDirection
  , sqlNullOrd :: SqlNullOrd
  } deriving (Show, Read, Eq)
  
data SqlOrdDirection = SqlAsc
                     | SqlDesc
                     deriving (Show, Read, Eq)

data SqlNullOrd = SqlNullsFirst
                | SqlNullsLast
                deriving (Show, Read, Eq)

data SqlInsert = SqlInsert SqlTable [SqlColumn] (NEL.NonEmpty [SqlExpr])
                 deriving (Show, Read, Eq)
                          
data SqlUpdate = SqlUpdate SqlTable [(SqlColumn,SqlExpr)] [SqlExpr]
                 deriving (Show, Read, Eq)
                          
data SqlDelete = SqlDelete SqlTable [SqlExpr]
                 deriving (Show, Read, Eq)

type Alias = Maybe String

data SqlSelect = SqlProduct [SqlSelect] SelectFrom -- ^ product
               | SqlSelect SqlTable SelectFrom     -- ^ base case
               | SqlJoin Join SelectFrom           -- ^ join
               | SqlBin Binary Alias               -- ^ binary
               | SqlValues SqlValues Alias         -- ^ values
                 deriving (Show, Read,Eq)

data SelectFrom = SelectFrom
  { options   :: [String]                 -- ^ DISTINCT, ALL etc.
  , attrs     :: SelectAttrs              -- ^ result
  , criteria  :: [SqlExpr]                -- ^ WHERE
  , groupby   :: Maybe (NEL.NonEmpty SqlExpr) -- ^ GROUP BY
  , orderby   :: [(SqlExpr,SqlOrder)]
  , limit     :: Maybe Int
  , offset    :: Maybe Int
  , having    :: [SqlExpr]
  , alias     :: Alias
  , window    :: ()  -- TODO
  } deriving (Show, Read, Eq)
  
type SelectAttrs = Mark

data Mark = All
          | Columns (NEL.NonEmpty (SqlExpr, Maybe SqlColumn))
            deriving (Show, Read, Eq)

data SqlValues = SqlVals
  { vAttrs :: SelectAttrs
  , vValues :: [[SqlExpr]]
  } deriving (Show, Read, Eq)
  
data Binary = Binary
  {
    bOp :: SelectBinOp
  , bSelect1 :: SqlSelect
  , bSelect2 :: SqlSelect
  } deriving (Show, Read, Eq)

data Returning a = Returning a (NEL.NonEmpty SqlExpr)
                 deriving (Show, Read, Eq) 

data SqlExpr = ColumnSqlExpr  SqlColumn
             | BinSqlExpr     String SqlExpr SqlExpr
             | PrefixSqlExpr  String SqlExpr
             | PostfixSqlExpr String SqlExpr
             | FunSqlExpr     String [SqlExpr]
             | AggrFunSqlExpr String [SqlExpr] [(SqlExpr, SqlOrder)]  -- ^ Aggregate functions separate from normal functions.
             | ConstSqlExpr   String
             | CaseSqlExpr    (NEL.NonEmpty (SqlExpr,SqlExpr)) SqlExpr
             | ListSqlExpr    [SqlExpr]
             | ExistsSqlExpr  SqlSelect
             | ParamSqlExpr (Maybe SqlName) SqlExpr
             | PlaceHolderSqlExpr
             | ParensSqlExpr SqlExpr
             | CastSqlExpr String SqlExpr
             | CompositeSqlExpr SqlExpr String
             | ArraySqlExpr [SqlExpr]
             | DefaultSqlExpr
             deriving (Show, Read, Eq)


data SelectBinOp = Except
                 | ExceptAll
                 | Union
                 | UnionAll
                 | Intersect
                 | IntersectAll
                 deriving (Show, Read, Eq)

{- Record Level Bin Ops

data LimitOp = LimitOp Int | OffsetOp Int | LimitOffsetOp Int Int
             deriving Show
-}

