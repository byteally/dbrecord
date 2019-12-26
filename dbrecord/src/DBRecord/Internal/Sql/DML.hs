{-# LANGUAGE DeriveGeneric #-}
module DBRecord.Internal.Sql.DML where

import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NEL
import Data.Text
import GHC.Generics (Generic)
import DBRecord.Internal.DBTypes (DBType)

type TableName = Text
type Name      = Text

type Lateral = Bool

data Join = Join
  { jJoinType :: JoinType
  , jLateral  :: Lateral
  , jTables :: (SqlTableExpr, SqlTableExpr)
  , jCond :: Maybe SqlExpr
  } deriving (Show, Read, Eq)

data JoinType = LeftJoin
              | RightJoin
              | FullJoin
              | InnerJoin
              | CrossJoin
              deriving (Show, Read, Eq)

data SqlTableName = SqlTableName
  { sqlTableDbName     :: String
  , sqlTableSchemaName :: String
  , sqlTableName       :: String
  } deriving (Show, Read, Eq)
  
newtype SqlColumn = SqlColumn [Text]
                  deriving (Show, Read, Eq)
type SqlName = Text

newtype SqlOidName = SqlOidName Text
                   deriving (Read, Show, Eq)

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

data SqlInsert = SqlInsert SqlTableName [SqlColumn] (NEL.NonEmpty [SqlExpr]) (Maybe SqlConflict) [SqlExpr]
                 deriving (Show, Read, Eq)

data SqlConflict = SqlConflict SqlConflictTarget SqlConflictAction
                 deriving (Show, Read, Eq)

data SqlConflictTarget = SqlConflictConstraint Text
                       | SqlConflictColumn [SqlColumn]
                       | SqlConflictAnon
                       deriving (Show, Read, Eq)

data SqlConflictAction = SqlConflictDoNothing
                       | SqlConflictUpdate SqlUpdate
                       deriving (Show, Read, Eq)

data SqlUpdate = SqlUpdate SqlTableName [(SqlColumn,SqlExpr)] [SqlExpr] [SqlExpr]
                 deriving (Show, Read, Eq)
                          
data SqlDelete = SqlDelete SqlTableName [SqlExpr]
                 deriving (Show, Read, Eq)

type Alias = Maybe String

data SqlTableExpr = NestedSqlSelect SqlSelect
                  | SqlTabName SqlTableName
                  | SqlTabFun SqlName [SqlName]
                  deriving (Eq, Show, Read)

data SqlWith = SqlWith SqlName [SqlName] SqlSelect
             deriving (Show, Read, Eq)

data SqlSelect = SqlProduct [SqlTableExpr] SelectFrom       -- ^ product
               | SqlSelect (Maybe SqlTableExpr) SelectFrom  -- ^ base case
               | SqlJoin Join SelectFrom                -- ^ join
               | SqlBin Binary Alias                    -- ^ binary
               | SqlCTE [SqlWith] SqlSelect             -- ^ CTEs
               | SqlValues SqlValues Alias              -- ^ values
                 deriving (Show, Read,Eq)

data SelectFrom = SelectFrom
  { options   :: [String]                 -- ^ DISTINCT, ALL etc.
  , attrs     :: SelectAttrs              -- ^ result
  , windows   :: [WindowExpr]             -- ^ windows  
  , criteria  :: [SqlExpr]                -- ^ WHERE
  , groupby   :: Maybe (NEL.NonEmpty SqlExpr) -- ^ GROUP BY
  , orderby   :: [(SqlExpr,SqlOrder)]
  , limit     :: Maybe SqlExpr
  , offset    :: Maybe SqlExpr
  , having    :: [SqlExpr]
  , alias     :: Alias
  } deriving (Show, Read, Eq)

data WindowExpr = WindowExpr
  { wname   :: String
  , wpart   :: WindowPart
  } deriving (Show, Read, Eq)

data WindowPart = WindowPart
  { partExpr :: [SqlExpr]
  , wordbys  :: [(SqlExpr, SqlOrder)]
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
  { bOp :: SelectBinOp
  , bSelect1 :: SqlSelect
  , bSelect2 :: SqlSelect
  } deriving (Show, Read, Eq)

data SqlExpr = ColumnSqlExpr  SqlColumn
             -- | OidSqlExpr     SqlOidName
             | BinSqlExpr     BinOp SqlExpr SqlExpr
             | PrefixSqlExpr  UnOp  SqlExpr
             | PostfixSqlExpr UnOp SqlExpr
             | FunSqlExpr     String [SqlExpr]
             | AggrFunSqlExpr String [SqlExpr] [(SqlExpr, SqlOrder)]  -- ^ Aggregate functions separate from normal functions.
             | ConstSqlExpr   LitSql
             | CaseSqlExpr    (NEL.NonEmpty (SqlExpr,SqlExpr)) (Maybe SqlExpr)
             | ListSqlExpr    [SqlExpr]
             | ExistsSqlExpr  SqlSelect
             | ParamSqlExpr (Maybe SqlName) SqlExpr
             | PlaceHolderSqlExpr
             | ParensSqlExpr SqlExpr
             | CastSqlExpr DBType SqlExpr
             | CompositeSqlExpr SqlExpr String
             | ArraySqlExpr [SqlExpr]
             | TableSqlExpr SqlSelect
             | NamedWindowSqlExpr String SqlExpr
             | AnonWindowSqlExpr [SqlExpr] [(SqlExpr, SqlOrder)] SqlExpr
             | DefaultSqlExpr
             deriving (Show, Read, Eq)


data SelectBinOp = Except
                 | ExceptAll
                 | Union
                 | UnionAll
                 | Intersect
                 | IntersectAll
                 deriving (Show, Read, Eq)

data LitSql = NullSql
            | DefaultSql
            | BoolSql Bool
            | StringSql Text
            | ByteSql ByteString
            | IntegerSql Integer
            | DoubleSql Double
            | OtherSql Text
            deriving (Eq, Show, Read, Generic)


data BinOp = OpEq | OpLt | OpLtEq | OpGt | OpGtEq | OpNotEq
           | OpNotLt | OpNotGt
           | OpAnd | OpOr
           | OpLike | OpIn
           | OpAny | OpAll | OpExists | OpSome | OpBetween
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
          | OpPositive
          | OpBitwiseNot
          | OpLower
          | OpUpper
          | OpOtherPrefix String
          | OpOtherPostfix String
          | OpOtherFun String          
          deriving (Show, Read, Generic, Eq, Ord)
