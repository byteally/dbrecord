-- |
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
--                Purely Agile Limited (c) 2014-2016
-- License     :  BSD-style

module DBRecord.MSSQL.Internal.Sql.Pretty
  ( renderQuery
  , renderDelete
  , renderInsert
  , renderUpdate
  , renderExpr
  , ppExpr
  , ppMSSQLType
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as Base16
import DBRecord.Internal.Sql.DML hiding (alias, criteria)
import qualified DBRecord.Internal.Sql.DML as DML
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import Data.Foldable (toList)
import Data.Monoid ((<>))
import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ (Doc, ($$), (<+>), text, empty,
                                  parens, comma, punctuate,
                                  hcat, vcat, brackets, doubleQuotes,
                                  hsep, equals, char, empty, render,
                                  space)
import DBRecord.Schema.Interface
import qualified Data.List as L

ppSelect :: SqlSelect -> Doc
ppSelect select = case select of
  SqlProduct sqSels selectFrom    -> ppSelectWith selectFrom (Just (ppProduct sqSels))
  SqlSelect tab selectFrom        -> ppSelectWith selectFrom (ppTableExpr <$> tab)
  SqlJoin joinSt selectFrom       -> ppSelectWith selectFrom (Just (ppJoin joinSt))
  SqlBin binSt as                 -> ppSelectBinary binSt as
  SqlCTE withs sql                -> ppSelectCTE withs sql
  SqlValues vals als              -> ppAs (text <$> als) $ ppSelectValues vals
  -- SqlBin bin als               -> ppAs (text <$> als) $ ppSelectBinary bin

ppSelectWith :: SelectFrom -> Maybe Doc -> Doc
ppSelectWith from tabDoc =
    ppAs (doubleQuotes . text <$> DML.alias from) $
    parens $ 
      text "SELECT"
  <+> ppAttrs (attrs from)
  $$  ppTab
  $$  ppWhere (DML.criteria from)
  $$  ppWindows (windows from)  
  $$  ppGroupBy (groupby from)
  $$  ppHaving (having from)
  $$  ppOrderBy (orderby from)
  $$  ppOffsetFetch (offset from) (limit from)

  where ppTab = case tabDoc of
          Nothing  -> empty
          Just doc -> text "FROM " <+> doc

ppProduct :: [SqlTableExpr] -> Doc
ppProduct = ppTables

ppAttrs :: SelectAttrs -> Doc
ppAttrs All            = text "*"
ppAttrs (Columns cols) = (commaV nameAs . toList) cols

nameAs :: (SqlExpr, Maybe SqlColumn) -> Doc
nameAs (expr, n) = ppAs (fmap unColumn n) (ppExpr expr)
  where unColumn (SqlColumn s) = ppAliasedCol (map T.unpack s)
        
ppTables :: [SqlTableExpr] -> Doc
ppTables []   = empty
ppTables tabs = commaV ppTableExpr tabs

ppSelectBinary :: Binary -> Alias -> Doc
ppSelectBinary bin als =
  let selBin =    ppSelect (bSelect1 bin)
               $$ ppSelBinOp (bOp bin)
               $$ ppSelect (bSelect2 bin)
  in case als of
    Nothing -> selBin
    Just as -> ppAs (Just $ doubleQuotes . text $ as) (parens selBin)
  
ppSelBinOp :: SelectBinOp -> Doc
ppSelBinOp op = text $ case op of
  Union        -> "UNION"
  UnionAll     -> "UNION ALL"
  Except       -> "EXCEPT"
  ExceptAll    -> error "Panic: EXCEPT ALL not supported in MSSQL"
  Intersect    -> "INTERSECT"
  IntersectAll -> error "Panic: INTERSECT ALL not supported in MSSQL"

ppSelectCTE :: [SqlWith] -> SqlSelect -> Doc
ppSelectCTE sqWiths sel = text "WITH" <+> commaV ppSqlWith sqWiths
                         $$ ppSelect sel
  where ppSqlWith (SqlWith tabn atts isel) =
              text (T.unpack tabn) <+> ppAtts atts
          $$  text "AS"
          $$  ppSelect isel
        ppAtts [] = empty
        ppAtts as = parens . commaH (text . T.unpack) $ as

ppJoin :: Join -> Doc
ppJoin joinSt = ppJoinedTabs
  where ppJoinedTabs = parens (   
                       ppTableExpr s1
                   $$  ppJoinType (jJoinType joinSt) (jLateral joinSt)
                   $$  ppTableExpr s2
                   $$  ppOn (jCond joinSt)
                   )
                   
        (s1, s2) = jTables joinSt
        ppOn Nothing  = empty
        ppOn (Just e) =   text "ON"
                      $$  ppExpr e

ppJoinType :: JoinType -> Lateral -> Doc
ppJoinType LeftJoin  False = text "LEFT OUTER JOIN"
ppJoinType RightJoin False = text "RIGHT OUTER JOIN"
ppJoinType FullJoin  False = text "FULL OUTER JOIN"
ppJoinType InnerJoin False = text "INNER JOIN"
ppJoinType CrossJoin False = text "CROSS JOIN"
ppJoinType LeftJoin  True  = text "OUTER APPLY"
ppJoinType CrossJoin True  = text "CROSS APPLY"
ppJoinType _ _ = error "Panic: impossible case @ppJoinType"

ppSelectValues :: SqlValues -> Doc
ppSelectValues v = text "SELECT"
                   <+> ppAttrs (vAttrs v)
                   $$  text "FROM"
                   $$  ppValues (vValues v)

ppValues :: [[SqlExpr]] -> Doc
ppValues vals = ppAs (Just (text "V")) (parens (text "VALUES" $$ commaV ppValuesRow vals))

ppValuesRow :: [SqlExpr] -> Doc
ppValuesRow = parens . commaH ppExpr

ppWindows :: [WindowExpr] -> Doc
ppWindows [] = empty
ppWindows ws = hsep (map ppWindow ws)
  where ppWindow (WindowExpr wn parts) =
              text "WINDOW"
          <+> text wn
          <+> text "AS"
          <+> parens (ppPartition parts)

ppPartition :: WindowPart -> Doc
ppPartition (WindowPart [] [])
  = empty
ppPartition (WindowPart es [])
  = text "PARTITION BY" <+> commaH ppExpr es
ppPartition (WindowPart es oeds)
  = text "PARTITION BY" <+> commaH ppExpr es <+> text "ORDER BY" <+> commaH ppOrd oeds
  
ppWhere :: [SqlExpr] -> Doc
ppWhere []    = empty
ppWhere exprs = text "WHERE" <+>  hsep (intersperse (text "AND")
                                        (map (parens . ppExpr) exprs))

ppGroupBy :: Maybe (NEL.NonEmpty SqlExpr) -> Doc
ppGroupBy Nothing      = empty
ppGroupBy (Just exprs) = go (toList exprs)
  where
    go es = text "GROUP BY" <+> ppGroupAttrs es
    ppGroupAttrs es = commaV (ppExpr . deliteral) es

ppHaving :: [SqlExpr] -> Doc
ppHaving []    = empty
ppHaving exprs = go (toList exprs)
  where
    go es = text "HAVING" <+> ppGroupAttrs es
    ppGroupAttrs es = commaV (ppExpr . deliteral) es

ppOrderBy :: [(SqlExpr,SqlOrder)] -> Doc
ppOrderBy []   = empty
ppOrderBy ords = text "ORDER BY" <+> commaV ppOrd ords

ppOrd :: (SqlExpr, SqlOrder) -> Doc
ppOrd (e, o) = ppExpr (deliteral e) <+> ppOrdDir o

ppOrdDir :: SqlOrder -> Doc
ppOrdDir sqlOrd = text $ case sqlOrdDirection sqlOrd of
  SqlAsc  -> "ASC"
  SqlDesc -> "DESC"

ppOffsetFetch :: Maybe SqlExpr -> Maybe SqlExpr -> Doc
ppOffsetFetch Nothing Nothing = empty
ppOffsetFetch (Just off) (Just lim) =
  text "OFFSET " <> ppExpr off <> text " ROWS" $$
  text "FETCH NEXT "  <> ppExpr lim <> text " ROWS ONLY"
ppOffsetFetch Nothing mlim =
  ppOffsetFetch (Just (ConstSqlExpr (IntegerSql 0))) mlim  
ppOffsetFetch (Just off) Nothing =
  text "OFFSET " <> ppExpr off <> text " ROWS"

-- ppOid :: SqlOidName -> Doc
-- ppOid (SqlOidName n) = quotes (doubleQuotes (text (T.unpack n)))

ppColumn :: SqlColumn -> Doc
ppColumn (SqlColumn s) =
  case map T.unpack s of
    [x]      -> doubleQuotes (text x)
    (x : xs) -> doubleQuotes (text x) <> char '.' <> ppAliasedCol xs
    _        -> error "Panic: Column cannot be empty"


ppTableExpr :: SqlTableExpr -> Doc
ppTableExpr (NestedSqlSelect sql)     = ppSelect sql
ppTableExpr (SqlTabName sqltab)       = ppTableName sqltab
ppTableExpr (SqlTabFun funName args)  = ppTableFun funName args

ppTableFun :: SqlName -> [SqlName] -> Doc
ppTableFun funN args = text (T.unpack funN) <> parens (hsep (map (text . T.unpack) args))

ppTableName :: SqlTableName -> Doc
ppTableName (SqlTableName db sc tab) = 
    quoted db <> dot <> quoted sc <> dot <> quoted tab
  where
    quoted = doubleQuotes . text
    dot = text "."

ppExpr :: SqlExpr -> Doc
ppExpr expr =
  case expr of
    ColumnSqlExpr c     -> ppColumn c
    -- OidSqlExpr s        -> ppOid s
    CompositeSqlExpr s x -> parens (ppExpr s) <> text "." <> text x
    ParensSqlExpr e -> parens (ppExpr e)
    BinSqlExpr op e1 e2 -> ppExpr e1 <+> ppBinOp op <+> ppExpr e2
    PrefixSqlExpr op e  -> ppPrefixExpr op e
    PostfixSqlExpr op e -> ppPostfixExpr op e
    FunSqlExpr f es     -> text f <> parens (commaH ppExpr es)
    AggrFunSqlExpr f es ord -> text f <> parens (commaH ppExpr es <+> ppOrderBy ord)
    ConstSqlExpr c      -> ppLiteral c
    CaseSqlExpr cs el   -> text "CASE" <> space <> vcat (toList (fmap ppWhen cs))
      <> ppElse el <> space <> text "END"
      where ppWhen (w,t) = text "WHEN" <+> ppExpr w
                       <+> text "THEN" <+> ppExpr t
            ppElse (Just e) = space <> (text "ELSE" <+> ppExpr e)
            ppElse Nothing  = space 
    ListSqlExpr es      -> parens (commaH ppExpr es)
    ParamSqlExpr _ v -> ppExpr v
    PlaceHolderSqlExpr -> text "?"
    CastSqlExpr typ e -> text "CAST" <> parens (ppExpr e <+> text "AS" <+> text (ppMSSQLType typ))
    DefaultSqlExpr    -> text "DEFAULT"
    ArraySqlExpr es -> text "ARRAY" <> brackets (commaH ppExpr es)
    TableSqlExpr sql -> parens (ppSelect sql)
    ExistsSqlExpr s     -> text "EXISTS" <+> parens (ppSelect s)
    NamedWindowSqlExpr w e -> ppExpr e <+> text "OVER" <+> text w
    AnonWindowSqlExpr p o e -> ppExpr e <+> text "OVER" <+> parens (partPP p <> ppOrderBy o)
      where partPP     [] = empty
            partPP     xs = text "PARTITION BY" <+> (commaH ppExpr xs <> space)

ppBinOp :: BinOp -> Doc
ppBinOp = text . go
  where go OpEq         = "="
        go OpLt         = "<"
        go OpLtEq       = "<="
        go OpGt         = ">"
        go OpGtEq       = ">="
        go OpNotEq      = "<>"
        go OpAnd        = "AND"
        go OpOr         = "OR"
        go OpLike       = "LIKE"
        go OpIn         = "IN"
        go (OpOther s)  = s
        go OpCat        = "||"
        go OpPlus       = "+"
        go OpMinus      = "-"
        go OpMul        = "*"
        go OpDiv        = "/"
        go OpMod        = "%"
        go OpBitNot     = "~"
        go OpBitAnd     = "&"
        go OpBitOr      = "|"
        go OpBitXor     = "^"
        go OpAsg        = "="
        go OpAtTimeZone = "AT TIME ZONE"
        go _            = error "Panic: not implemented @ppBinOp"

ppPrefixExpr :: UnOp -> SqlExpr -> Doc
ppPrefixExpr op e = go op
  where go OpNot              = text "NOT" <> parens (ppExpr e)
        go OpLength           = text "LENGTH" <> parens (ppExpr e)
        go OpAbs              = text "ABS" <> parens (ppExpr e)
        go OpNegate           = text "-" <> parens (ppExpr e)
        go OpLower            = text "LOWER" <> parens (ppExpr e)
        go OpUpper            = text "UPPER" <> parens (ppExpr e)
        go (OpOtherFun s)     = text s <> parens (ppExpr e)
        go (OpOtherPrefix s)  = text s <+> (ppExpr e)
        go _                  = error "Panic: unsupported combination @ppPrefixExpr"

ppPostfixExpr :: UnOp -> SqlExpr -> Doc
ppPostfixExpr op e = go op
  where go OpIsNull           = ppExpr e <+> text "IS NULL"
        go OpIsNotNull        = ppExpr e <+> text "IS NOT NULL"
        go (OpOtherPostfix s) = ppExpr e <+> text s 
        
        go _              = error "Panic: unsupported combination @ppPostfixExpr"

ppInsert :: SqlInsert -> Doc
ppInsert (SqlInsert table names values _cfts rets)
    = text "INSERT INTO" <+> ppTableName table
      <+> parens (commaV ppColumn names)
      $$ text "VALUES" <+> commaV (\v -> parens (commaV ppExpr v))
                                  (toList values)
      $$ ppReturning rets

ppUpdate :: SqlUpdate -> Doc
ppUpdate (SqlUpdate table assigns criteria rets)
        = text "UPDATE" <+> ppTableName table
        $$ text "SET" <+> commaV ppAssign assigns
        $$ ppWhere criteria
        $$ ppReturning rets
    where
      ppAssign (c,e) = ppColumn c <+> equals <+> ppExpr e

ppDelete :: SqlDelete -> Doc
ppDelete (SqlDelete table criteria) =
    text "DELETE FROM" <+> ppTableName table $$ ppWhere criteria
    
ppReturning :: [SqlExpr] -> Doc
ppReturning []   = empty
ppReturning rets =
  text "RETURNING"
  <+> commaV ppExpr (toList rets)

deliteral :: SqlExpr -> SqlExpr
deliteral expr@(ConstSqlExpr _) = FunSqlExpr "COALESCE" [expr]
deliteral expr                  = expr

commaH :: (a -> Doc) -> [a] -> Doc
commaH f = hcat . punctuate comma . map f

commaV :: (a -> Doc) -> [a] -> Doc
commaV f = vcat . punctuate comma . map f

ppAs :: Maybe Doc -> Doc -> Doc
ppAs Nothing      expr = expr
ppAs (Just alias) expr = expr <+> hsep [text "as", alias]

-- TODO: This name is absurdly wrong
ppAliasedCol :: [String] -> Doc
ppAliasedCol = doubleQuotes . hcat . punctuate aliasSep . map text

aliasSep :: Doc
aliasSep = char '_'

ppLiteral :: LitSql -> Doc
ppLiteral l =
  case l of
    NullSql -> text "NULL"
    DefaultSql -> text "DEFAULT"
    BoolSql True -> text "(1 = 1)"
    BoolSql False -> text "(0 = 1)"
    ByteSql s -> binQuote s
    StringSql s -> text (quote (T.unpack s))
    IntegerSql i -> text (show i)
    DoubleSql d -> if isNaN d then text "'NaN'"
                  else if isInfinite d && d < 0 then text "'-Infinity'"
                  else if isInfinite d && d > 0 then text "'Infinity'"
                  else text (show d)
    OtherSql s -> text (T.unpack s)
-- testPP doc = render doc

binQuote :: ByteString -> Doc
binQuote s = text "'\\\\x" <> text (BS8.unpack (Base16.encode s)) <> text "'"

quote :: String -> String
quote s = "'" ++ concatMap escape s ++ "'"

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
          
renderQuery :: SqlSelect -> String
renderQuery = render . ppSelect

renderDelete :: SqlDelete -> String
renderDelete = render . ppDelete

renderInsert :: SqlInsert -> String
renderInsert = render . ppInsert

renderUpdate :: SqlUpdate -> String
renderUpdate = render . ppUpdate

renderExpr :: SqlExpr -> String
renderExpr = render . ppExpr

--

ppMSSQLType :: DBType -> String
ppMSSQLType = go
  where go DBInt2                   = "smallint"
        go DBInt4                   = "int"
        go DBInt8                   = "bigint"
        go (DBNumeric p s)          = "numeric (" ++ show p ++ ", " ++ show s ++ ")"
        go (DBFloat n)              = "float (" ++ show n ++ ")"
        go (DBChar i)               = "nchar (" ++ show i ++ ")"
        go (DBVarchar (Left _))     = "nvarchar (max)"
        go (DBVarchar (Right i))    = "nvarchar (" ++ show i ++ ")"                
        go DBText                   = "ntext"
        go (DBBinary i)             = "binary (" ++ show i ++ ")"
        go (DBVarbinary (Left Max)) = "varbinary (max)"
        go (DBVarbinary (Right i))  = "varbinary (" ++ show i ++ ")"
                                     
        go (DBTimestamptz i)        = "datetimeoffset (" ++ show i ++ ")"
        go (DBTimestamp i)          = "datetime2 (" ++ show i ++ ")"
        go DBDate                   = "date"
        go (DBTime i)               = "time (" ++ show i ++ ")"
        -- go DBUuid             = "UUID"
        -- go DBJson             = "JSON"
        -- go DBJsonB            = "JSONB"        
        -- go (DBArray t)        = go t ++ "[]"
        go (DBBit i)                = "bit (" ++ show i ++ ")"
        go (DBNullable t)           = go t
        go (DBCustomType (DBTypeName t args))       = T.unpack (doubleQuote t) ++ ppArgs args
        go t                        = error $ "Panic: not implemented : " ++ show t

        ppArgs []  = ""
        ppArgs xs  = "(" ++ L.intercalate "," (map ppArg xs) ++ ")"

        ppArg (TextArg t)    = T.unpack t
        ppArg (IntegerArg i) = show i
