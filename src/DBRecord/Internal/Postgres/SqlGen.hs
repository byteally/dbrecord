{-# LANGUAGE LambdaCase, OverloadedStrings #-}
-- |
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
--                Purely Agile Limited (c) 2014-2016
-- License     :  BSD-style

module DBRecord.Internal.Postgres.SqlGen where

import           Prelude hiding (product)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List.NonEmpty as NEL
import DBRecord.Internal.Postgres.Types hiding (alias, criteria, attrs)
import qualified DBRecord.Internal.Postgres.Types as PGT
import qualified DBRecord.Internal.PrimQuery as PQ
import qualified Data.Text as T
import qualified Data.Maybe as M

sql :: PQ.PrimQuery -> SqlSelect
sql = PQ.foldPrimQuery sqlQueryGenerator

sqlQueryGenerator :: PQ.PrimQueryFold SqlSelect
sqlQueryGenerator = PQ.PrimQueryFold
  { PQ.baseTable = baseTable
  , PQ.product   = product
  , PQ.join      = join
  -- , PQ.values    = values
  -- , PQ.binary    = binary
  -- , PQ.label     = label
  -- , PQ.relExpr   = relExpr
  }

newSelect :: SelectFrom
newSelect = SelectFrom {
  options   = [],
  PGT.attrs     = All,
  PGT.criteria  = [],
  groupby   = Nothing,
  orderby   = [],
  limit     = Nothing,
  offset    = Nothing,
  having    = [],
  windows   = [],
  PGT.alias = Nothing            
  }

baseClauses :: PQ.Clauses -> SelectFrom
baseClauses cs = 
    newSelect { PGT.attrs    = Columns (ensureColumns (map sqlBinding (PQ.projections cs)))
              , PGT.criteria = map toSqlExpr (PQ.criteria cs)
              , windows      = map toSqlWindow (PQ.windows cs)                 
              , groupby  = case (PQ.groupbys cs) of
                             [] -> Nothing
                             xs -> Just (NEL.fromList (map toSqlExpr xs))
              , having   = map toSqlExpr (PQ.havings cs)
              , limit    = PQ.limit cs
              , offset   = PQ.offset cs
              , orderby  = map toSqlOrder (PQ.orderbys cs)
              }

baseTable :: PQ.TableId -> PQ.Clauses -> SqlSelect
baseTable tabId cs = SqlSelect (sqlTab tabId) $ 
  (baseClauses cs) { PGT.alias  = (T.unpack <$> (PQ.alias cs)) }

  where sqlTab (PQ.TableId s tn) = SqlTable (Just (T.unpack s)) (T.unpack tn)

product :: NEL.NonEmpty SqlSelect -> PQ.Clauses -> SqlSelect
product tabs cs = SqlProduct (NEL.toList tabs) $            
  (baseClauses cs) { PGT.alias = T.unpack <$> (PQ.alias cs) } 

join :: PQ.JoinType -> PQ.PrimExpr -> SqlSelect -> SqlSelect -> PQ.Clauses -> SqlSelect
join jt e q1 q2 cs = SqlJoin (Join (joinType' jt) (q1, q2) (toSqlExpr e))
                              ((baseClauses cs) { PGT.alias  = (T.unpack <$> (PQ.alias cs)) })
                     
  where joinType' :: PQ.JoinType -> JoinType
        joinType' PQ.LeftJoin  = LeftJoin
        joinType' PQ.RightJoin = RightJoin
        joinType' PQ.FullJoin  = FullJoin
        joinType' PQ.InnerJoin = InnerJoin

sqlOrder :: SqlGenerator -> PQ.OrderExpr -> (SqlExpr,SqlOrder)
sqlOrder gen (PQ.OrderExpr o e) =
  (sqlExpr gen e, SqlOrder { sqlOrdDirection = o'
                           , sqlNullOrd      = orderNulls' })
  
    where o' = case PQ.orderDirection o of
            PQ.OpAsc  -> SqlAsc
            PQ.OpDesc -> SqlDesc
            
          orderNulls' = case PQ.orderNulls o of
            PQ.NullsFirst -> SqlNullsFirst
            PQ.NullsLast  -> SqlNullsLast

sqlWindow :: SqlGenerator -> PQ.WindowClause -> WindowExpr
sqlWindow gen (PQ.WindowClause wn p) =
  WindowExpr (T.unpack wn) (sqlPartition gen p)

sqlPartition :: SqlGenerator -> PQ.WindowPart -> WindowPart
sqlPartition gen (PQ.WindowPart es oes) =
  WindowPart (map (sqlExpr gen) es) (map (sqlOrder gen) oes)

-- TODO: should be sqlExpr
toSqlExpr :: PQ.PrimExpr -> SqlExpr
toSqlExpr = sqlExpr defaultSqlGenerator

toSqlOrder :: PQ.OrderExpr -> (SqlExpr,SqlOrder)
toSqlOrder = sqlOrder defaultSqlGenerator

toSqlWindow :: PQ.WindowClause -> WindowExpr
toSqlWindow = sqlWindow defaultSqlGenerator

data SqlGenerator = SqlGenerator
    {
     sqlUpdate      :: SqlTable -> [PQ.PrimExpr] -> PQ.Assoc -> SqlUpdate,
     sqlDelete      :: SqlTable -> [PQ.PrimExpr] -> SqlDelete,
     sqlInsert      :: SqlTable -> [PQ.Attribute] -> NEL.NonEmpty [PQ.PrimExpr] -> SqlInsert,
     sqlExpr        :: PQ.PrimExpr -> SqlExpr,
     sqlLiteral     :: PQ.Lit -> String,
     -- | Turn a string into a quoted string. Quote characters
     -- and any escaping are handled by this function.
     sqlQuote       :: String -> String
    }

defaultSqlGenerator :: SqlGenerator
defaultSqlGenerator = mkSqlGenerator defaultSqlGenerator

mkSqlGenerator :: SqlGenerator -> SqlGenerator
mkSqlGenerator gen = SqlGenerator
    {
     sqlUpdate      = defaultSqlUpdate      gen,
     sqlDelete      = defaultSqlDelete      gen,
     sqlInsert      = defaultSqlInsert      gen,
     sqlExpr        = defaultSqlExpr        gen,
     sqlLiteral     = defaultSqlLiteral     gen,
     sqlQuote       = defaultSqlQuote       gen
    }

defaultSqlExpr :: SqlGenerator -> PQ.PrimExpr -> SqlExpr
defaultSqlExpr gen expr = case expr of
  PQ.AttrExpr t          -> ColumnSqlExpr (sqlColumn t)
  PQ.BaseTableAttrExpr a -> ColumnSqlExpr (SqlColumn [a])
  PQ.CompositeExpr e x   -> CompositeSqlExpr (defaultSqlExpr gen e) (T.unpack x)
  PQ.BinExpr op e1 e2    ->
    let leftE = sqlExpr gen e1
        rightE = sqlExpr gen e2
        paren = ParensSqlExpr
        (expL, expR) = case (op, e1, e2) of
          (PQ.OpAnd, PQ.BinExpr PQ.OpOr _ _, PQ.BinExpr PQ.OpOr _ _)  -> (paren leftE, paren rightE)
          (PQ.OpOr, PQ.BinExpr PQ.OpAnd _ _, PQ.BinExpr PQ.OpAnd _ _) -> (paren leftE, paren rightE)
          (PQ.OpAnd, PQ.BinExpr PQ.OpOr _ _, _)                       -> (paren leftE, rightE)
          (PQ.OpAnd, _, PQ.BinExpr PQ.OpOr _ _)                       -> (leftE, paren rightE)
          (PQ.OpOr, PQ.BinExpr PQ.OpAnd _ _, _)                       -> (paren leftE, rightE)
          (PQ.OpOr, _, PQ.BinExpr PQ.OpAnd _ _)                       -> (leftE, paren rightE)
          (_, PQ.ConstExpr _, PQ.ConstExpr _)                         -> (leftE, rightE)
          (_, _, PQ.ConstExpr _)                                      -> (paren leftE, rightE)
          (_, PQ.ConstExpr _, _)                                      -> (leftE, paren rightE)
          _                                                           -> (paren leftE, paren rightE)
    in BinSqlExpr (showBinOp op) expL expR
       
  PQ.UnExpr op e         ->
    let (op',t) = sqlUnOp op
        e' = sqlExpr gen e
    in case t of
      UnOpFun     -> FunSqlExpr op' [e']
      UnOpPrefix  -> PrefixSqlExpr op' (ParensSqlExpr e')
      UnOpPostfix -> PostfixSqlExpr op' e'

  PQ.AggrExpr op e ord   ->
    let op'  = showAggrOp op
        e'   = sqlExpr gen e
        ord' = sqlOrder gen <$> ord
        moreAggrFunParams = case op of
          PQ.AggrStringAggr primE -> [sqlExpr gen primE]
          _ -> []
    in AggrFunSqlExpr op' (e' : moreAggrFunParams) ord'
  PQ.ConstExpr l         -> ConstSqlExpr (sqlLiteral gen l)
  PQ.CaseExpr cs e       ->
    let cs' = [(sqlExpr gen c, sqlExpr gen x)| (c,x) <- cs]
        e'  = sqlExpr gen e
    in case NEL.nonEmpty cs' of
      Just nel -> CaseSqlExpr nel e'
      Nothing  -> e'
      
  PQ.ListExpr es         -> ListSqlExpr (map (sqlExpr gen) es)
  PQ.ParamExpr n _       -> ParamSqlExpr n PlaceHolderSqlExpr
  PQ.FunExpr n exprs     -> FunSqlExpr (T.unpack n) (map (sqlExpr gen) exprs)
  PQ.CastExpr typ e1     -> CastSqlExpr (T.unpack typ) (sqlExpr gen e1)
  PQ.DefaultInsertExpr   -> DefaultSqlExpr
  PQ.ArrayExpr es        -> ArraySqlExpr (map (sqlExpr gen) es)
  PQ.WindowExpr w e      -> WindowSqlExpr (T.unpack w) (sqlExpr gen e)

showBinOp :: PQ.BinOp -> String
showBinOp  PQ.OpEq         = "="
showBinOp  PQ.OpLt         = "<"
showBinOp  PQ.OpLtEq       = "<="
showBinOp  PQ.OpGt         = ">"
showBinOp  PQ.OpGtEq       = ">="
showBinOp  PQ.OpNotEq      = "<>"
showBinOp  PQ.OpAnd        = "AND"
showBinOp  PQ.OpOr         = "OR"
showBinOp  PQ.OpLike       = "LIKE"
showBinOp  PQ.OpIn         = "IN"
showBinOp  (PQ.OpOther s)  = s
showBinOp  PQ.OpCat        = "||"
showBinOp  PQ.OpPlus       = "+"
showBinOp  PQ.OpMinus      = "-"
showBinOp  PQ.OpMul        = "*"
showBinOp  PQ.OpDiv        = "/"
showBinOp  PQ.OpMod        = "MOD"
showBinOp  PQ.OpBitNot     = "~"
showBinOp  PQ.OpBitAnd     = "&"
showBinOp  PQ.OpBitOr      = "|"
showBinOp  PQ.OpBitXor     = "^"
showBinOp  PQ.OpAsg        = "="
showBinOp  PQ.OpAtTimeZone = "AT TIME ZONE"

data UnOpType = UnOpFun | UnOpPrefix | UnOpPostfix

sqlUnOp :: PQ.UnOp -> (String,UnOpType)
sqlUnOp  PQ.OpNot         = ("NOT", UnOpPrefix)
sqlUnOp  PQ.OpIsNull      = ("IS NULL", UnOpPostfix)
sqlUnOp  PQ.OpIsNotNull   = ("IS NOT NULL", UnOpPostfix)
sqlUnOp  PQ.OpLength      = ("LENGTH", UnOpFun)
sqlUnOp  PQ.OpAbs         = ("@", UnOpFun)
sqlUnOp  PQ.OpNegate      = ("-", UnOpFun)
sqlUnOp  PQ.OpLower       = ("LOWER", UnOpFun)
sqlUnOp  PQ.OpUpper       = ("UPPER", UnOpFun)
sqlUnOp  (PQ.UnOpOther s) = (s, UnOpFun)

showAggrOp :: PQ.AggrOp -> String
showAggrOp PQ.AggrCount          = "COUNT"
showAggrOp PQ.AggrSum            = "SUM"
showAggrOp PQ.AggrAvg            = "AVG"
showAggrOp PQ.AggrMin            = "MIN"
showAggrOp PQ.AggrMax            = "MAX"
showAggrOp PQ.AggrStdDev         = "StdDev"
showAggrOp PQ.AggrStdDevP        = "StdDevP"
showAggrOp PQ.AggrVar            = "Var"
showAggrOp PQ.AggrVarP           = "VarP"
showAggrOp PQ.AggrBoolAnd        = "BOOL_AND"
showAggrOp PQ.AggrBoolOr         = "BOOL_OR"
showAggrOp PQ.AggrArr            = "ARRAY_AGG"
showAggrOp (PQ.AggrStringAggr _) = "STRING_AGG"
showAggrOp (PQ.AggrOther s)      = s

defaultSqlLiteral :: SqlGenerator -> PQ.Lit -> String
defaultSqlLiteral _ l =
    case l of
      PQ.Null       -> "NULL"
      PQ.Default    -> "DEFAULT"
      PQ.Bool True  -> "TRUE"
      PQ.Bool False -> "FALSE"
      PQ.Byte s     -> binQuote s
      PQ.String s   -> quote (T.unpack s)
      PQ.Integer i  -> show i
      PQ.Double d   -> if isNaN d then "'NaN'"
                       else if isInfinite d && d < 0 then "'-Infinity'"
                       else if isInfinite d && d > 0 then "'Infinity'"
                       else show d
      PQ.Other o    -> T.unpack o

defaultSqlQuote :: SqlGenerator -> String -> String
defaultSqlQuote _ s = quote s

quote :: String -> String
quote s = "E'" ++ concatMap escape s ++ "'"

escape :: Char -> String
escape '\NUL' = "\\0"
escape '\''   = "''"
escape '"'    = "\\\""
escape '\b'   = "\\b"
escape '\n'   = "\\n"
escape '\r'   = "\\r"
escape '\t'   = "\\t"
escape '\\'   = "\\\\"
escape c      = [c]

binQuote :: ByteString -> String
binQuote s = "E'\\\\x" ++ BS8.unpack (Base16.encode s) ++ "'"

defaultSqlUpdate :: SqlGenerator
                   -> SqlTable   -- ^ Table to update
                   -> [PQ.PrimExpr] -- ^ Conditions which must all be true for a row
                                 --   to be updated.
                   -> PQ.Assoc -- ^ Update the data with this.
                   -> SqlUpdate
defaultSqlUpdate gen tbl criteria assigns
        = SqlUpdate tbl (toSqlAssoc gen assigns) (map (sqlExpr gen) criteria)


defaultSqlInsert :: SqlGenerator
                   -> SqlTable
                   -> [PQ.Attribute]
                   -> NEL.NonEmpty [PQ.PrimExpr]
                   -> SqlInsert
defaultSqlInsert gen tbl attrs exprs =
  SqlInsert tbl (map toSqlColumn attrs) ((fmap . map) (sqlExpr gen) exprs)

defaultSqlDelete :: SqlGenerator
                   -> SqlTable
                   -> [PQ.PrimExpr] -- ^ Criteria which must all be true for a row
                                 --   to be deleted.
                   -> SqlDelete
defaultSqlDelete gen tbl criteria = SqlDelete tbl (map (sqlExpr gen) criteria)

toSqlAssoc :: SqlGenerator -> PQ.Assoc -> [(SqlColumn,SqlExpr)]
toSqlAssoc gen = map (\(attr,expr) -> (toSqlColumn attr, sqlExpr gen expr))

-- TODO: Examine status of PQ.Attribute (should it be a [Text]?)
toSqlColumn :: PQ.Attribute -> SqlColumn
toSqlColumn attr = SqlColumn [attr]

sqlBinding :: (PQ.Sym, PQ.PrimExpr) -> (SqlExpr, Maybe SqlColumn)
sqlBinding (t, pe) =
  (toSqlExpr pe, Just (sqlColumn t))

ensureColumns :: [(SqlExpr, Maybe a)] -> NEL.NonEmpty (SqlExpr, Maybe a)
ensureColumns = ensureColumnsGen (\x -> (x,Nothing))

ensureColumnsGen :: (SqlExpr -> a) -> [a] -> NEL.NonEmpty a
ensureColumnsGen f = M.fromMaybe (return . f $ ConstSqlExpr "0") . NEL.nonEmpty

sqlColumn :: PQ.Sym -> SqlColumn
sqlColumn e = SqlColumn (PQ.symPrefix e ++ [PQ.symField e])

infixr 8 .:

(.:) :: (r -> z) -> (a -> b -> r) -> a -> b -> z
(.:) f g x y = f (g x y)
