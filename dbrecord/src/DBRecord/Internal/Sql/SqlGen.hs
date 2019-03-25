{-# LANGUAGE LambdaCase, OverloadedStrings, TypeFamilies #-}
-- |
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
--                Purely Agile Limited (c) 2014-2016
-- License     :  BSD-style

module DBRecord.Internal.Sql.SqlGen where

import           Prelude hiding (product)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List.NonEmpty as NEL
import DBRecord.Internal.Sql.DML hiding (alias, criteria, attrs)
import qualified DBRecord.Internal.Sql.DML as DML
import qualified DBRecord.Internal.PrimQuery as PQ
import qualified Data.Text as T
import qualified Data.Maybe as M
import qualified DBRecord.Internal.PrimQuery as PQ

sql :: PQ.PrimQuery -> SqlSelect
sql = PQ.foldPrimQuery sqlQueryGenerator

updateSql :: PQ.UpdateQuery -> SqlUpdate
updateSql = PQ.foldUpdateQuery sqlUpdateGenerator

deleteSql :: PQ.DeleteQuery -> SqlDelete
deleteSql = PQ.foldDeleteQuery sqlDeleteGenerator

insertSql :: PQ.InsertQuery -> SqlInsert
insertSql = PQ.foldInsertQuery sqlInsertGenerator


sqlQueryGenerator :: PQ.PrimQueryFold SqlSelect
sqlQueryGenerator = PQ.PrimQueryFold
  { PQ.table     = table
  , PQ.product   = product
  , PQ.join      = join
  , PQ.binary    = binary
  , PQ.cte       = cte
  -- , PQ.values    = values
  -- , PQ.label     = label
  -- , PQ.relExpr   = relExpr
  }

sqlUpdateGenerator :: PQ.UpdateQueryFold SqlUpdate
sqlUpdateGenerator = PQ.UpdateQueryFold
  {PQ.updateQ = \tabId -> sqlUpdate defaultSqlGenerator $ toSqlTableName tabId
  }

sqlDeleteGenerator :: PQ.DeleteQueryFold SqlDelete
sqlDeleteGenerator = PQ.DeleteQueryFold
  { PQ.deleteQ = \tabId -> sqlDelete defaultSqlGenerator $ toSqlTableName tabId
  }

sqlInsertGenerator :: PQ.InsertQueryFold SqlInsert
sqlInsertGenerator = PQ.InsertQueryFold
  { PQ.insertQ = \tabId -> sqlInsert defaultSqlGenerator $ toSqlTableName tabId
  }  


newSelect :: SelectFrom
newSelect = SelectFrom {
  options   = [],
  DML.attrs     = All,
  DML.criteria  = [],
  groupby   = Nothing,
  orderby   = [],
  limit     = Nothing,
  offset    = Nothing,
  having    = [],
  windows   = [],
  DML.alias = Nothing            
  }

baseClauses :: PQ.Clauses -> SelectFrom
baseClauses cs = 
    newSelect { DML.attrs    = Columns (ensureColumns (map sqlBinding (project' (PQ.projections cs))))
              , DML.criteria = map toSqlExpr (PQ.criteria cs)
              , windows      = map toSqlWindow (PQ.windows cs)                 
              , groupby  = case (PQ.groupbys cs) of
                             [] -> Nothing
                             xs -> Just (NEL.fromList (map toSqlExpr xs))
              , having   = map toSqlExpr (PQ.havings cs)
              , limit    = toSqlExpr <$> (PQ.limit cs)
              , offset   = toSqlExpr <$> (PQ.offset cs)
              , orderby  = map toSqlOrder (PQ.orderbys cs)
              }

table :: PQ.TableExpr SqlSelect -> PQ.Clauses -> SqlSelect
table tab cs = SqlSelect (toSqlTable tab) $ 
  (baseClauses cs) { DML.alias  = (T.unpack <$> (PQ.alias cs)) }

toSqlTable :: PQ.TableExpr SqlSelect -> SqlTableExpr
toSqlTable (PQ.TableName tabId)     = SqlTabName (toSqlTableName tabId)
toSqlTable (PQ.TableFun name attrs) = SqlTabFun name attrs
toSqlTable (PQ.PrimQuery sq)        = NestedSqlSelect sq

toSqlTableName :: PQ.TableId -> SqlTableName
toSqlTableName (PQ.TableId s tn) = SqlTableName (Just (T.unpack s)) (T.unpack tn)

product :: NEL.NonEmpty (PQ.TableExpr SqlSelect) -> PQ.Clauses -> SqlSelect
product tabs cs = SqlProduct (map toSqlTable $ NEL.toList tabs) $            
  (baseClauses cs) { DML.alias = T.unpack <$> (PQ.alias cs) } 

cte :: PQ.CTE SqlSelect SqlSelect -> SqlSelect
cte (PQ.CTE withs s) =
  SqlCTE (map toSqlWith withs) s
  
  where toSqlWith (PQ.WithExpr tab cols p) = SqlWith tab cols p

join :: PQ.JoinType -> PQ.Lateral -> Maybe PQ.PrimExpr -> PQ.TableExpr SqlSelect -> PQ.TableExpr SqlSelect -> PQ.Clauses -> SqlSelect
join jt l e q1 q2 cs = SqlJoin (Join (joinType' jt) l (toSqlTable q1, toSqlTable q2) (fmap toSqlExpr e))
                                ((baseClauses cs) { DML.alias  = (T.unpack <$> (PQ.alias cs)) })
                     
  where joinType' :: PQ.JoinType -> JoinType
        joinType' PQ.LeftJoin  = LeftJoin
        joinType' PQ.RightJoin = RightJoin
        joinType' PQ.FullJoin  = FullJoin
        joinType' PQ.InnerJoin = InnerJoin
        joinType' PQ.CrossJoin = CrossJoin

binary :: PQ.BinType -> SqlSelect -> SqlSelect -> Maybe T.Text -> SqlSelect
binary bt q1 q2 =
  SqlBin (Binary (selectBinOp' bt)
           q1 q2
         ) . fmap T.unpack
                     
  where selectBinOp' :: PQ.BinType -> SelectBinOp
        selectBinOp' PQ.Union           = Union
        selectBinOp' PQ.Intersection    = Intersect
        selectBinOp' PQ.Except          = Except
        selectBinOp' PQ.UnionAll        = UnionAll
        selectBinOp' PQ.IntersectionAll = IntersectAll
        selectBinOp' PQ.ExceptAll       = ExceptAll

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

toSqlExpr :: PQ.PrimExpr -> SqlExpr
toSqlExpr = sqlExpr defaultSqlGenerator

toSqlOrder :: PQ.OrderExpr -> (SqlExpr,SqlOrder)
toSqlOrder = sqlOrder defaultSqlGenerator

toSqlWindow :: PQ.WindowClause -> WindowExpr
toSqlWindow = sqlWindow defaultSqlGenerator

data SqlGenerator = SqlGenerator
    {
     sqlUpdate      :: SqlTableName -> [PQ.PrimExpr] -> PQ.Assoc -> [PQ.PrimExpr] -> SqlUpdate,
     sqlDelete      :: SqlTableName -> [PQ.PrimExpr] -> SqlDelete,
     sqlInsert      :: SqlTableName -> [PQ.Attribute] -> NEL.NonEmpty [PQ.PrimExpr] -> [PQ.PrimExpr] -> SqlInsert,
     sqlExpr        :: PQ.PrimExpr -> SqlExpr,
     sqlLiteral     :: PQ.Lit -> LitSql,
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
  -- PQ.OidExpr a           -> OidSqlExpr    (SqlOidName a)
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
    in BinSqlExpr (sqlBinOp op) expL expR
       
  PQ.PrefixExpr op e         -> PrefixSqlExpr (sqlUnOp op) (sqlExpr gen e)
  PQ.PostfixExpr op e        -> PostfixSqlExpr (sqlUnOp op) (sqlExpr gen e)  

{-  
    let (op',t) = sqlUnOp op
        e' = sqlExpr gen e
    in case t of
      UnOpFun     -> FunSqlExpr op' [e']
      UnOpPrefix  -> PrefixSqlExpr op' e'      
      UnOpPrefixParen  -> PrefixSqlExpr op' (ParensSqlExpr e')
      UnOpPostfix -> PostfixSqlExpr op' e'
-}
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
      Just nel -> CaseSqlExpr nel (Just e')
      Nothing  -> e'
      
  PQ.ListExpr es         -> ListSqlExpr (map (sqlExpr gen) es)
  PQ.ParamExpr n _       -> ParamSqlExpr n PlaceHolderSqlExpr
  PQ.FunExpr n exprs     -> FunSqlExpr (T.unpack n) (map (sqlExpr gen) exprs)
  PQ.CastExpr typ e1     -> CastSqlExpr typ (sqlExpr gen e1)
  PQ.DefaultInsertExpr   -> DefaultSqlExpr
  PQ.ArrayExpr es        -> ArraySqlExpr (map (sqlExpr gen) es)
  PQ.TableExpr f pe      -> TableSqlExpr (sql (PQ.getPqFun f
                                               (updateFlatComposite pe)))
  PQ.NamedWindowExpr w e -> NamedWindowSqlExpr (T.unpack w) (sqlExpr gen e)
  PQ.AnonWindowExpr p o e -> AnonWindowSqlExpr (map (sqlExpr gen) p)
                                              (map (sqlOrder gen) o)
                                              (sqlExpr gen e)
  p                      -> error ("Panic: Unexpected flatcomposite" ++ show p)

updateFlatComposite :: PQ.PrimExpr -> PQ.PrimExpr
updateFlatComposite = unfoldFlatComposites . go
  where go (PQ.FlatComposite (e : es)) =
          go (snd e) ++ go (PQ.FlatComposite es)
        go (PQ.FlatComposite [])       =
          []
        go e                           =
          [e]
  

unfoldFlatComposites :: [PQ.PrimExpr] -> PQ.PrimExpr
unfoldFlatComposites =
  foldr unfoldFlatComposite (PQ.FlatComposite [])

unfoldFlatComposite :: PQ.PrimExpr -> PQ.PrimExpr -> PQ.PrimExpr
unfoldFlatComposite e@(PQ.AttrExpr (PQ.Sym pfx fld)) (PQ.FlatComposite xs) =
  go (pfx ++ [fld]) xs

  where go [f]  xs  = PQ.FlatComposite $ case lookup f xs of
                        Just _ -> map (\(fld, e') -> case fld == f of
                                       True  -> (fld, e)
                                       False -> (fld, e')
                                   ) xs
                        Nothing -> (xs ++ [(f, e)])
        go (f : fs) xs = PQ.FlatComposite $ case lookup f xs of
                        Just _ -> map (\(fld, e') -> case fld == f of
                                       True  -> case e' of
                                         PQ.FlatComposite xss -> (fld, go fs xss)
                                         _                    -> (fld, go fs [])
                                       False -> (fld, e')
                                   ) xs
                        Nothing -> (xs ++ [(f, go fs [])])
unfoldFlatComposite e _ = error $ "Panic: unexpected: " ++ show e
                        
  
sqlBinOp :: PQ.BinOp -> BinOp
sqlBinOp  PQ.OpEq         = OpEq
sqlBinOp  PQ.OpLt         = OpLt
sqlBinOp  PQ.OpLtEq       = OpLtEq
sqlBinOp  PQ.OpGt         = OpGt
sqlBinOp  PQ.OpGtEq       = OpGtEq
sqlBinOp  PQ.OpNotEq      = OpNotEq
sqlBinOp  PQ.OpAnd        = OpAnd
sqlBinOp  PQ.OpOr         = OpOr
sqlBinOp  PQ.OpLike       = OpLike
sqlBinOp  PQ.OpIn         = OpIn
sqlBinOp  (PQ.OpOther s)  = OpOther s
sqlBinOp  PQ.OpCat        = OpCat
sqlBinOp  PQ.OpPlus       = OpPlus
sqlBinOp  PQ.OpMinus      = OpMinus
sqlBinOp  PQ.OpMul        = OpMul
sqlBinOp  PQ.OpDiv        = OpDiv
sqlBinOp  PQ.OpMod        = OpMod
sqlBinOp  PQ.OpBitNot     = OpBitNot
sqlBinOp  PQ.OpBitAnd     = OpBitAnd
sqlBinOp  PQ.OpBitOr      = OpBitOr
sqlBinOp  PQ.OpBitXor     = OpBitXor
sqlBinOp  PQ.OpAsg        = OpAsg
sqlBinOp  PQ.OpAtTimeZone = OpAtTimeZone

sqlUnOp :: PQ.UnOp -> UnOp
sqlUnOp  PQ.OpNot              = OpNot
sqlUnOp  PQ.OpIsNull           = OpIsNull 
sqlUnOp  PQ.OpIsNotNull        = OpIsNotNull 
sqlUnOp  PQ.OpLength           = OpLength
sqlUnOp  PQ.OpAbs              = OpAbs
sqlUnOp  PQ.OpNegate           = OpNegate 
sqlUnOp  PQ.OpLower            = OpLower 
sqlUnOp  PQ.OpUpper            = OpUpper
sqlUnOp  (PQ.OpOtherPostfix s) = OpOtherPostfix s 
sqlUnOp  (PQ.OpOtherPrefix s)  = OpOtherPrefix s
sqlUnOp  (PQ.OpOtherFun s)     = OpOtherFun s

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

defaultSqlLiteral :: SqlGenerator -> PQ.Lit -> LitSql
defaultSqlLiteral _ l =
    case l of
      PQ.Null       -> NullSql
      PQ.Default    -> DefaultSql
      PQ.Bool b     -> BoolSql b
      PQ.Byte s     -> ByteSql s
      PQ.String s   -> StringSql s
      PQ.Integer i  -> IntegerSql i
      PQ.Double d   -> DoubleSql d
      PQ.Other o    -> OtherSql o

{-      
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
-}

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
                   -> SqlTableName   -- ^ Table to update
                   -> [PQ.PrimExpr] -- ^ Conditions which must all be true for a row
                                 --   to be updated.
                   -> PQ.Assoc -- ^ Update the data with this.
                   -> [PQ.PrimExpr] -- ^ Returning these expressions.
                   -> SqlUpdate
defaultSqlUpdate gen tbl criteria assigns rets
        = SqlUpdate tbl (toSqlAssoc gen assigns) (map (sqlExpr gen) criteria) (map (sqlExpr gen) rets)


defaultSqlInsert :: SqlGenerator
                   -> SqlTableName
                   -> [PQ.Attribute]
                   -> NEL.NonEmpty [PQ.PrimExpr]
                   -> [PQ.PrimExpr] -- ^ Returning these expressions.
                   -> SqlInsert
defaultSqlInsert gen tbl attrs exprs rets =
  SqlInsert tbl (map toSqlColumn attrs) ((fmap . map) (sqlExpr gen) exprs) (map (sqlExpr gen) rets)

defaultSqlDelete :: SqlGenerator
                   -> SqlTableName
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
ensureColumnsGen f = M.fromMaybe (return . f $ ConstSqlExpr (IntegerSql 0 )) . NEL.nonEmpty

sqlColumn :: PQ.Sym -> SqlColumn
sqlColumn e = SqlColumn (PQ.symPrefix e ++ [PQ.symField e])

infixr 8 .:

(.:) :: (r -> z) -> (a -> b -> r) -> a -> b -> z
(.:) f g x y = f (g x y)


--

primExprGen :: SqlExpr -> PQ.PrimExpr
primExprGen expr = case expr of
  ColumnSqlExpr (SqlColumn [pc]) -> PQ.BaseTableAttrExpr pc
  ColumnSqlExpr (SqlColumn pcs)  -> PQ.AttrExpr (PQ.unsafeToSym pcs)
  CompositeSqlExpr se n          -> PQ.CompositeExpr (primExprGen se) (T.pack n)
  ListSqlExpr ses                -> PQ.ListExpr (map primExprGen ses)
  FunSqlExpr n ses               -> funPrimExprGen n ses
  CastSqlExpr typ se             -> PQ.CastExpr typ (primExprGen se)
  DefaultSqlExpr                 -> PQ.DefaultInsertExpr
  ArraySqlExpr ses               -> PQ.ArrayExpr (map primExprGen ses)
  TableSqlExpr sq                -> error "TODO: not implemented for TableSqlExpr" -- PQ.TableExpr (primQueryGen sq)  
  NamedWindowSqlExpr w se        -> PQ.NamedWindowExpr (T.pack w) (primExprGen se)
  AnonWindowSqlExpr ps os se     -> error "TODO: not implemented for AnonWindowSqlExpr"
  BinSqlExpr op sel ser          -> error "TODO: not implemented for BinSqlExpr" 
  PrefixSqlExpr op se            -> error "TODO: not implemented for PrefixSqlExpr" 
  PostfixSqlExpr op se           -> error "TODO: not implemented for PostfixSqlExpr" 
  ParensSqlExpr se               -> primExprGen se
  ConstSqlExpr c                 -> constPrimExprGen c
  AggrFunSqlExpr n ses ords      -> aggrFunPrimExprGen n ses ords
  e                              -> pqMappingFail e
  
funPrimExprGen :: String -> [SqlExpr] -> PQ.PrimExpr
funPrimExprGen n ses = PQ.FunExpr (T.pack n) (map primExprGen ses)

{-
binPrimExprGen :: String -> SqlExpr -> SqlExpr -> PQ.PrimExpr
binPrimExprGen binOp l r = PQ.BinExpr binPrimExpr (primExprGen l) (primExprGen r)
  where binPrimExpr = case binOp of
                        "="   -> PQ.OpEq
                        "<"   -> PQ.OpLt
                        "<="  -> PQ.OpLtEq
                        ">"   -> PQ.OpGt
                        ">="  -> PQ.OpGtEq
                        "<>"  -> PQ.OpNotEq
                        "AND" -> PQ.OpAnd
                        "OR"  -> PQ.OpOr
                        "LIKE" -> PQ.OpLike
                        "IN"   -> PQ.OpIn
                        "||"   -> PQ.OpCat
                        "+"    -> PQ.OpPlus
                        "-"    -> PQ.OpMinus
                        "*"    -> PQ.OpMul
                        "/"    -> PQ.OpDiv
                        "MOD"  -> PQ.OpMod
                        "~"    -> PQ.OpBitNot
                        "&"    -> PQ.OpBitAnd
                        "|"    -> PQ.OpBitOr
                        "^"    -> PQ.OpBitXor
                        "="    -> PQ.OpAsg
                        "AT TIME ZONE" -> PQ.OpAtTimeZone
                        s      -> PQ.OpOther s

prefixPrimExprGen :: String -> SqlExpr -> PQ.PrimExpr
prefixPrimExprGen op e =
  PQ.UnExpr (primOp op) (primExprGen e)

    where primOp "NOT"         = PQ.OpNot
          primOp _             = error "Panic: not a known prefix operator"

postfixPrimExprGen :: String -> SqlExpr -> PQ.PrimExpr
postfixPrimExprGen op e =
  PQ.UnExpr (primOp op) (primExprGen e)

    where primOp "IS NULL"     = PQ.OpIsNull
          primOp "IS NOT NULL" = PQ.OpIsNotNull
          primOp _             = error "Panic: not a known postfix operator"
-}

constPrimExprGen :: LitSql -> PQ.PrimExpr
constPrimExprGen lsq = PQ.ConstExpr $ 
  case lsq of
    NullSql -> PQ.Null
    DefaultSql -> PQ.Default
    BoolSql b  -> PQ.Bool b
    StringSql s -> PQ.String s
    ByteSql b -> PQ.Byte b
    IntegerSql i -> PQ.Integer i
    DoubleSql d -> PQ.Double d
    OtherSql t -> PQ.Other t

aggrFunPrimExprGen :: String -> [SqlExpr] -> [(SqlExpr, SqlOrder)] -> PQ.PrimExpr
aggrFunPrimExprGen op args =
  pqMappingSkipped "agg expr"
  -- PQ.AggrExpr aggrOp 

pqMappingSkipped :: String -> a
pqMappingSkipped msg = error $ "Incomplete mapping for: " ++ show msg

pqMappingFail :: SqlExpr -> PQ.PrimExpr
pqMappingFail e = error $ "Panic: No mapping for SqlExpr :" ++ show e ++ " to PrimExpr"
{-  
  PQ.FunExpr n exprs     -> FunSqlExpr (T.unpack n) (map (sqlExpr gen) exprs)
  PQ.CastExpr typ e1     -> CastSqlExpr (T.unpack typ) (sqlExpr gen e1)
  PQ.DefaultInsertExpr   -> DefaultSqlExpr
  PQ.ArrayExpr es        -> ArraySqlExpr (map (sqlExpr gen) es)
  PQ.WindowExpr w e      -> WindowSqlExpr (T.unpack w) (sqlExpr gen e)
  
-}

{-  
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
      UnOpPrefix  -> PrefixSqlExpr op' e'      
      UnOpPrefixParen  -> PrefixSqlExpr op' (ParensSqlExpr e')
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

-}
primQueryGen :: SqlSelect -> PQ.PrimQuery
primQueryGen = undefined

{-
data Sql

instance PQ.BackendExpr Sql where
  type BackendExprType Sql = SqlExpr
  backendExpr _ = defaultSqlExpr defaultSqlGenerator
-}

genSqlExpr :: PQ.PrimExpr -> SqlExpr
genSqlExpr = defaultSqlExpr defaultSqlGenerator

project' :: [PQ.Projection] -> [(PQ.Sym, PQ.PrimExpr)]
project' exprs =
  map concatPQSym $ concatMap (go []) exprs

  where go tags (tag, expr) = case expr of
          PQ.FlatComposite iexps -> concatMap (go (tag : tags)) iexps
          _                      -> [(tag : tags, expr)]

        concatPQSym (tags, e) =
          let sym = PQ.unsafeToSym (reverse tags)
          in case e of
               PQ.AttrExpr {} | not (singleton tags) -> (sym, PQ.AttrExpr sym)
                              | otherwise          -> (sym, e)
               _                                   -> (sym, e)

        singleton [x] = True
        singleton _   = False
