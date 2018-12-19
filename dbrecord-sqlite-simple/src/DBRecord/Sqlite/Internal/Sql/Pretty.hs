-- |
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
--                Purely Agile Limited (c) 2014-2016
-- License     :  BSD-style

module DBRecord.Sqlite.Internal.Sql.Pretty
  ( renderQuery
  , renderDelete
  , renderInsert
  , renderUpdate
  , ppSqliteExpr
  , ppSqliteType
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

ppSelect :: SqlSelect -> Doc
ppSelect select = case select of
  SqlProduct sqSels selectFrom -> ppSelectWith selectFrom (ppProduct sqSels) 
  SqlSelect tab selectFrom     -> ppSelectWith selectFrom (ppTable tab)
  SqlJoin joinSt selectFrom    -> ppSelectWith selectFrom (ppJoin joinSt)
  SqlBin binSt selectFrom      -> ppSelectWith selectFrom (ppSelectBinary binSt)  
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
  $$  ppOrderBy (orderby from)
  $$  ppLimit (limit from)
  $$  ppOffset (offset from)

ppProduct :: [SqlSelect] -> Doc
ppProduct = ppTables

ppAttrs :: SelectAttrs -> Doc
ppAttrs All            = text "*"
ppAttrs (Columns cols) = (commaV nameAs . toList) cols

nameAs :: (SqlExpr, Maybe SqlColumn) -> Doc
nameAs (expr, name) = ppAs (fmap unColumn name) (ppSqliteExpr expr)
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

ppJoin :: Join -> Doc
ppJoin joinSt = ppJoinedTabs
  where ppJoinedTabs = parens (   
                       ppSelect s1
                   $$  ppJoinType (jJoinType joinSt)
                   $$  ppSelect s2
                   $$  text "ON"
                   $$  ppSqliteExpr (jCond joinSt)
                   )
                   
        (s1, s2) = jTables joinSt

ppJoinType :: JoinType -> Doc
ppJoinType LeftJoin   = text "LEFT OUTER JOIN"
ppJoinType RightJoin  = text "RIGHT OUTER JOIN"
ppJoinType FullJoin   = text "FULL OUTER JOIN"
ppJoinType InnerJoin  = text "INNER JOIN"


ppSelectValues :: SqlValues -> Doc
ppSelectValues v = text "SELECT"
                   <+> ppAttrs (vAttrs v)
                   $$  text "FROM"
                   $$  ppValues (vValues v)

ppValues :: [[SqlExpr]] -> Doc
ppValues vals = ppAs (Just (text "V")) (parens (text "VALUES" $$ commaV ppValuesRow vals))

ppValuesRow :: [SqlExpr] -> Doc
ppValuesRow = parens . commaH ppSqliteExpr

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
  = text "PARTITION BY" <+> commaH ppSqliteExpr es
ppPartition (WindowPart es oeds)
  = text "PARTITION BY" <+> commaH ppSqliteExpr es <+> text "ORDER BY" <+> commaH ppOrd oeds
  
ppWhere :: [SqlExpr] -> Doc
ppWhere []    = empty
ppWhere exprs = text "WHERE" <+>  hsep (intersperse (text "AND")
                                        (map (parens . ppSqliteExpr) exprs))

ppGroupBy :: Maybe (NEL.NonEmpty SqlExpr) -> Doc
ppGroupBy Nothing      = empty
ppGroupBy (Just exprs) = go (toList exprs)
  where
    go es = text "GROUP BY" <+> ppGroupAttrs es
    ppGroupAttrs es = commaV (ppSqliteExpr . deliteral) es

ppOrderBy :: [(SqlExpr,SqlOrder)] -> Doc
ppOrderBy []   = empty
ppOrderBy ords = text "ORDER BY" <+> commaV ppOrd ords

ppOrd :: (SqlExpr, SqlOrder) -> Doc
ppOrd (e, o) = ppSqliteExpr (deliteral e)
                    <+> ppOrdDir o
                    <+> ppNullOrd o

ppOrdDir :: SqlOrder -> Doc
ppOrdDir sqlOrd = text $ case sqlOrdDirection sqlOrd of
  SqlAsc  -> "ASC"
  SqlDesc -> "DESC"

ppNullOrd :: SqlOrder -> Doc
ppNullOrd sqlOrd = text $ case sqlNullOrd sqlOrd of
  SqlNullsFirst -> "NULLS FIRST"
  SqlNullsLast  -> "NULLS LAST"

ppLimit :: Maybe SqlExpr -> Doc
ppLimit Nothing    = empty
ppLimit (Just lmt) = text "LIMIT " <> ppSqliteExpr lmt

ppOffset :: Maybe SqlExpr -> Doc
ppOffset Nothing    = empty
ppOffset (Just off) = text "OFFSET " <> ppSqliteExpr off

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

ppSqliteExpr :: SqlExpr -> Doc
ppSqliteExpr expr =
  case expr of
    ColumnSqlExpr c     -> ppColumn c
    -- OidSqlExpr s        -> ppOid s
    CompositeSqlExpr s x -> parens (ppSqliteExpr s) <> text "." <> text x
    ParensSqlExpr e -> parens (ppSqliteExpr e)
    BinSqlExpr op e1 e2 -> ppSqliteExpr e1 <+> text op <+> ppSqliteExpr e2
    PrefixSqlExpr op e  -> text op <+> ppSqliteExpr e
    PostfixSqlExpr op e -> ppSqliteExpr e <+> text op
    FunSqlExpr f es     -> text f <> parens (commaH ppSqliteExpr es)
    AggrFunSqlExpr f es ord -> text f <> parens (commaH ppSqliteExpr es <+> ppOrderBy ord)
    ConstSqlExpr c      -> ppLiteral c
    CaseSqlExpr cs el   -> text "CASE" <> space <> vcat (toList (fmap ppWhen cs))
      <> ppElse el <> space <> text "END"
      where ppWhen (w,t) = text "WHEN" <+> ppSqliteExpr w
                       <+> text "THEN" <+> ppSqliteExpr t
            ppElse (Just e) = space <> (text "ELSE" <+> ppSqliteExpr e)
            ppElse Nothing  = space 
    ListSqlExpr es      -> parens (commaH ppSqliteExpr es)
    ParamSqlExpr _ v -> ppSqliteExpr v
    PlaceHolderSqlExpr -> text "?"
    CastSqlExpr typ e -> text "CAST" <> parens (ppSqliteExpr e <+> text "AS" <+> text typ)
    DefaultSqlExpr    -> text "DEFAULT"
    ArraySqlExpr es -> text "ARRAY" <> brackets (commaH ppSqliteExpr es)
    ExistsSqlExpr s     -> text "EXISTS" <+> parens (ppSelect s)
    WindowSqlExpr w e -> ppSqliteExpr e <+> text "OVER" <+> text w 

ppInsert :: SqlInsert -> Doc
ppInsert (SqlInsert table names values rets)
    = text "INSERT INTO" <+> ppTable table
      <+> parens (commaV ppColumn names)
      $$ text "VALUES" <+> commaV (\v -> parens (commaV ppSqliteExpr v))
                                  (toList values)
      $$ ppReturning rets

ppUpdate :: SqlUpdate -> Doc
ppUpdate (SqlUpdate table assigns criteria rets)
        = text "UPDATE" <+> ppTable table
        $$ text "SET" <+> commaV ppAssign assigns
        $$ ppWhere criteria
        $$ ppReturning rets
    where
      ppAssign (c,e) = ppColumn c <+> equals <+> ppSqliteExpr e

ppDelete :: SqlDelete -> Doc
ppDelete (SqlDelete table criteria) =
    text "DELETE FROM" <+> ppTable table $$ ppWhere criteria
    
ppReturning :: [SqlExpr] -> Doc
ppReturning []   = empty
ppReturning rets =
  text "RETURNING"
  <+> commaV ppSqliteExpr (toList rets)

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
    BoolSql True -> text "TRUE"
    BoolSql False -> text "FALSE"
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
binQuote s = text "E'\\\\x" <> text (BS8.unpack (Base16.encode s)) <> text "'"

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
          
renderQuery :: SqlSelect -> String
renderQuery = render . ppSelect

renderDelete :: SqlDelete -> String
renderDelete = render . ppDelete

renderInsert :: SqlInsert -> String
renderInsert = render . ppInsert

renderUpdate :: SqlUpdate -> String
renderUpdate = render . ppUpdate

--

-- NOTE: Untested. 
ppSqliteType :: DBType -> String
ppSqliteType = go
  where go DBInt2             = "int"
        go DBInt4             = "int"
        go DBInt8             = "int"
        go DBBool             = "int"
        go DBFloat4           = "real"        
        go DBFloat8           = "real"
        -- go (DBChar i)         = "nchar (" ++ show i ++ " )"
        go DBText             = "text"
        -- TODO: fix size of varbinary
        go DBByteArr          = "blob"
        -- go DBTimestamptz      = "datetimeoffset"
        -- go DBInterval         = "INTERVAL"
        -- go DBCiText           = "CITEXT"
        -- go DBDate             = "date"
        -- go DBTime             = "time"
        -- go DBTimestamp        = "datetime2"
        -- go DBUuid             = "UUID"
        -- go DBJson             = "JSON"
        -- go DBJsonB            = "JSONB"        
        -- go (DBArray t)        = go t ++ "[]"
        go (DBNullable t)     = go t
        go (DBTypeName t)     = T.unpack (doubleQuote (T.pack t))
        go (DBCustomType t _) = go t
        go _                  = error "Panic: not implemented"

