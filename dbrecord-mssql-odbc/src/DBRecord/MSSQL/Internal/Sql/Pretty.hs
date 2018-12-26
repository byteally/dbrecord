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
  , ppMSSQLExpr
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
                                  quotes, space)
import DBRecord.Schema.Interface
import DBRecord.Internal.DBTypes
import qualified Data.List as L

ppSelect :: SqlSelect -> Doc
ppSelect select = case select of
  SqlProduct sqSels selectFrom -> ppSelectWith selectFrom (ppProduct sqSels) 
  SqlSelect tab selectFrom     -> ppSelectWith selectFrom (ppTable tab)
  SqlJoin joinSt selectFrom    -> ppSelectWith selectFrom (ppJoin joinSt)
  SqlBin binSt selectFrom      -> ppSelectWith selectFrom (ppSelectBinary binSt)
  SqlCTE withs sql selectFrom  -> ppSelectWith selectFrom (ppSelectCTE withs sql)  
  SqlValues vals als           -> ppAs (text <$> als) $ ppSelectValues vals
  -- SqlBin bin als               -> ppAs (text <$> als) $ ppSelectBinary bin

ppSelectWith :: SelectFrom -> Doc -> Doc
ppSelectWith from tabDoc =
    ppAs (doubleQuotes . text <$> DML.alias from) $
    parens $ 
      text "SELECT"
  <+> ppAttrs (attrs from)
  $$  text "FROM " <+> tabDoc
  $$  ppWhere (DML.criteria from)
  $$  ppWindows (windows from)  
  $$  ppGroupBy (groupby from)
  $$  ppHaving (having from)
  $$  ppOrderBy (orderby from)
  $$  ppOffsetFetch (offset from) (limit from)

ppProduct :: [SqlSelect] -> Doc
ppProduct = ppTables

ppAttrs :: SelectAttrs -> Doc
ppAttrs All            = text "*"
ppAttrs (Columns cols) = (commaV nameAs . toList) cols

nameAs :: (SqlExpr, Maybe SqlColumn) -> Doc
nameAs (expr, name) = ppAs (fmap unColumn name) (ppMSSQLExpr expr)
  where unColumn (SqlColumn s) = ppAliasedCol (map T.unpack s)
        
ppTables :: [SqlSelect] -> Doc
ppTables []   = empty
ppTables tabs = commaV ppSelect tabs

ppSelectBinary :: Binary -> Doc
ppSelectBinary bin = ppSelect (bSelect1 bin)
                    $$ ppBinOp (bOp bin)
                    $$ ppSelect (bSelect2 bin)

ppBinOp :: SelectBinOp -> Doc
ppBinOp op = text $ case op of
  Union        -> "UNION"
  UnionAll     -> "UNIONALL"
  Except       -> "EXCEPT"
  ExceptAll    -> "EXCEPTALL"
  Intersect    -> "INTERSECT"
  IntersectAll -> "INTERSECTALL"

ppSelectCTE :: [SqlWith] -> SqlSelect -> Doc
ppSelectCTE = undefined

ppJoin :: Join -> Doc
ppJoin joinSt = ppJoinedTabs
  where ppJoinedTabs = parens (   
                       ppSelect s1
                   $$  ppJoinType (jJoinType joinSt) (jLateral joinSt)
                   $$  ppSelect s2
                   $$  ppOn (jCond joinSt)
                   )
                   
        (s1, s2) = jTables joinSt
        ppOn Nothing  = empty
        ppOn (Just e) =   text "ON"
                      $$  ppMSSQLExpr e


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
ppValuesRow = parens . commaH ppMSSQLExpr

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
  = text "PARTITION BY" <+> commaH ppMSSQLExpr es
ppPartition (WindowPart es oeds)
  = text "PARTITION BY" <+> commaH ppMSSQLExpr es <+> text "ORDER BY" <+> commaH ppOrd oeds
  
ppWhere :: [SqlExpr] -> Doc
ppWhere []    = empty
ppWhere exprs = text "WHERE" <+>  hsep (intersperse (text "AND")
                                        (map (parens . ppMSSQLExpr) exprs))

ppGroupBy :: Maybe (NEL.NonEmpty SqlExpr) -> Doc
ppGroupBy Nothing      = empty
ppGroupBy (Just exprs) = go (toList exprs)
  where
    go es = text "GROUP BY" <+> ppGroupAttrs es
    ppGroupAttrs es = commaV (ppMSSQLExpr . deliteral) es

ppHaving :: [SqlExpr] -> Doc
ppHaving []    = empty
ppHaving exprs = go (toList exprs)
  where
    go es = text "HAVING" <+> ppGroupAttrs es
    ppGroupAttrs es = commaV (ppMSSQLExpr . deliteral) es

ppOrderBy :: [(SqlExpr,SqlOrder)] -> Doc
ppOrderBy []   = empty
ppOrderBy ords = text "ORDER BY" <+> commaV ppOrd ords

ppOrd :: (SqlExpr, SqlOrder) -> Doc
ppOrd (e, o) = ppMSSQLExpr (deliteral e) <+> ppOrdDir o

ppOrdDir :: SqlOrder -> Doc
ppOrdDir sqlOrd = text $ case sqlOrdDirection sqlOrd of
  SqlAsc  -> "ASC"
  SqlDesc -> "DESC"

ppOffsetFetch :: Maybe SqlExpr -> Maybe SqlExpr -> Doc
ppOffsetFetch Nothing Nothing = empty
ppOffsetFetch (Just off) (Just lim) =
  text "OFFSET " <> ppMSSQLExpr off <> text " ROWS" $$
  text "FETCH NEXT "  <> ppMSSQLExpr lim <> text " ROWS ONLY"
ppOffsetFetch Nothing mlim =
  ppOffsetFetch (Just (ConstSqlExpr (IntegerSql 0))) mlim  
ppOffsetFetch (Just off) Nothing =
  text "OFFSET " <> ppMSSQLExpr off <> text " ROWS"

-- ppOid :: SqlOidName -> Doc
-- ppOid (SqlOidName n) = quotes (doubleQuotes (text (T.unpack n)))

ppColumn :: SqlColumn -> Doc
ppColumn (SqlColumn s) =
  case map T.unpack s of
    [x]      -> doubleQuotes (text x)
    (x : xs) -> doubleQuotes (text x) <> char '.' <> ppAliasedCol xs
    _        -> error "Panic: Column cannot be empty"

ppTable :: SqlTable -> Doc
ppTable st = case sqlTableSchemaName st of
    Just sn -> doubleQuotes (text sn) <> text "." <> tname
    Nothing -> tname
  where
    tname = doubleQuotes (text (sqlTableName st))

ppMSSQLExpr :: SqlExpr -> Doc
ppMSSQLExpr expr =
  case expr of
    ColumnSqlExpr c     -> ppColumn c
    -- OidSqlExpr s        -> ppOid s
    CompositeSqlExpr s x -> parens (ppMSSQLExpr s) <> text "." <> text x
    ParensSqlExpr e -> parens (ppMSSQLExpr e)
    BinSqlExpr op e1 e2 -> ppMSSQLExpr e1 <+> ppOp op <+> ppMSSQLExpr e2
    PrefixSqlExpr op e  -> text op <+> ppMSSQLExpr e
    PostfixSqlExpr op e -> ppMSSQLExpr e <+> text op
    FunSqlExpr f es     -> ppFunName f <> parens (commaH ppMSSQLExpr es)
    AggrFunSqlExpr f es ord -> text f <> parens (commaH ppMSSQLExpr es <+> ppOrderBy ord)
    ConstSqlExpr c      -> ppLiteral c
    CaseSqlExpr cs el   -> text "CASE" <> space <> vcat (toList (fmap ppWhen cs))
      <> ppElse el <> space <> text "END"
      where ppWhen (w,t) = text "WHEN" <+> ppMSSQLExpr w
                       <+> text "THEN" <+> ppMSSQLExpr t
            ppElse (Just e) = space <> (text "ELSE" <+> ppMSSQLExpr e)
            ppElse Nothing  = space 
    ListSqlExpr es      -> parens (commaH ppMSSQLExpr es)
    ParamSqlExpr _ v -> ppMSSQLExpr v
    PlaceHolderSqlExpr -> text "?"
    CastSqlExpr typ e -> text "CAST" <> parens (ppMSSQLExpr e <+> text "AS" <+> text (ppMSSQLType typ))
    DefaultSqlExpr    -> text "DEFAULT"
    ArraySqlExpr es -> text "ARRAY" <> brackets (commaH ppMSSQLExpr es)
    ExistsSqlExpr s     -> text "EXISTS" <+> parens (ppSelect s)
    NamedWindowSqlExpr w e -> ppMSSQLExpr e <+> text "OVER" <+> text w
    AnonWindowSqlExpr p o e -> ppMSSQLExpr e <+> text "OVER" <+> parens (partPP p <> ppOrderBy o)
      where partPP     [] = empty
            partPP     xs = text "PARTITION BY" <+> (commaH ppMSSQLExpr xs <> space)



  where ppOp "MOD" = text "%"
        ppOp  v    = text v

        ppFunName "@" = text "abs"
        ppFunName v   = text v

ppInsert :: SqlInsert -> Doc
ppInsert (SqlInsert table names values rets)
    = text "INSERT INTO" <+> ppTable table
      <+> parens (commaV ppColumn names)
      $$ text "VALUES" <+> commaV (\v -> parens (commaV ppMSSQLExpr v))
                                  (toList values)
      $$ ppReturning rets

ppUpdate :: SqlUpdate -> Doc
ppUpdate (SqlUpdate table assigns criteria rets)
        = text "UPDATE" <+> ppTable table
        $$ text "SET" <+> commaV ppAssign assigns
        $$ ppWhere criteria
        $$ ppReturning rets
    where
      ppAssign (c,e) = ppColumn c <+> equals <+> ppMSSQLExpr e

ppDelete :: SqlDelete -> Doc
ppDelete (SqlDelete table criteria) =
    text "DELETE FROM" <+> ppTable table $$ ppWhere criteria
    
ppReturning :: [SqlExpr] -> Doc
ppReturning []   = empty
ppReturning rets =
  text "RETURNING"
  <+> commaV ppMSSQLExpr (toList rets)

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
    BoolSql True -> error "Panic: impossible boolean true literal"
    BoolSql False -> error "Panic: impossible boolean false literal"
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
        go (DBTypeName t args)      = T.unpack (doubleQuote t) ++ ppArgs args
        go (DBCustomType t _)       = go t
        go t                        = error $ "Panic: not implemented : " ++ show t

        ppArgs []  = ""
        ppArgs xs  = "(" ++ L.intercalate "," (map ppArg xs) ++ ")"

        ppArg (TextArg t)    = T.unpack t
        ppArg (IntegerArg i) = show i
