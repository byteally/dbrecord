-- |
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
--                Purely Agile Limited (c) 2014-2016
-- License     :  BSD-style

module DBRecord.Postgres.Internal.Sql.Pretty
  ( renderQuery
  , renderDelete
  , renderInsert
  , renderUpdate
  , ppPGExpr
  , ppPGType
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
import qualified Data.List as L

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
  $$  ppHaving (having from)  
  $$  ppOrderBy (orderby from)
  $$  ppLimit (limit from)
  $$  ppOffset (offset from)

ppProduct :: [SqlSelect] -> Doc
ppProduct = ppTables

ppAttrs :: SelectAttrs -> Doc
ppAttrs All            = text "*"
ppAttrs (Columns cols) = (commaV nameAs . toList) cols

nameAs :: (SqlExpr, Maybe SqlColumn) -> Doc
nameAs (expr, name) = ppAs (fmap unColumn name) (ppPGExpr expr)
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
                   $$  ppPGExpr (jCond joinSt)
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
ppValuesRow = parens . commaH ppPGExpr

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
  = text "PARTITION BY" <+> commaH ppPGExpr es
ppPartition (WindowPart es oeds)
  = text "PARTITION BY" <+> commaH ppPGExpr es <+> text "ORDER BY" <+> commaH ppOrd oeds
  
ppWhere :: [SqlExpr] -> Doc
ppWhere []    = empty
ppWhere exprs = text "WHERE" <+>  hsep (intersperse (text "AND")
                                        (map (parens . ppPGExpr) exprs))

ppGroupBy :: Maybe (NEL.NonEmpty SqlExpr) -> Doc
ppGroupBy Nothing      = empty
ppGroupBy (Just exprs) = go (toList exprs)
  where
    go es = text "GROUP BY" <+> ppGroupAttrs es
    ppGroupAttrs es = commaV (ppPGExpr . deliteral) es

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
ppOrd (e, o) = ppPGExpr (deliteral e)
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
ppLimit (Just lmt) = text "LIMIT " <> ppPGExpr lmt

ppOffset :: Maybe SqlExpr -> Doc
ppOffset Nothing    = empty
ppOffset (Just off) = text "OFFSET " <> ppPGExpr off

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

ppPGExpr :: SqlExpr -> Doc
ppPGExpr expr =
  case expr of
    ColumnSqlExpr c     -> ppColumn c
    -- OidSqlExpr s        -> ppOid s
    CompositeSqlExpr s x -> parens (ppPGExpr s) <> text "." <> text x
    ParensSqlExpr e -> parens (ppPGExpr e)
    BinSqlExpr op e1 e2 -> ppPGExpr e1 <+> text op <+> ppPGExpr e2
    PrefixSqlExpr op e  -> text op <+> ppPGExpr e
    PostfixSqlExpr op e -> ppPGExpr e <+> text op
    FunSqlExpr f es     -> text f <> parens (commaH ppPGExpr es)
    AggrFunSqlExpr f es ord -> text f <> parens (commaH ppPGExpr es <+> ppOrderBy ord)
    ConstSqlExpr c      -> ppLiteral c
    CaseSqlExpr cs el   -> text "CASE" <> space <> vcat (toList (fmap ppWhen cs))
      <> ppElse el <> space <> text "END"
      where ppWhen (w,t) = text "WHEN" <+> ppPGExpr w
                       <+> text "THEN" <+> ppPGExpr t
            ppElse (Just e) = space <> (text "ELSE" <+> ppPGExpr e)
            ppElse Nothing  = space 
    ListSqlExpr es      -> parens (commaH ppPGExpr es)
    ParamSqlExpr _ v -> ppPGExpr v
    PlaceHolderSqlExpr -> text "?"
    CastSqlExpr typ e -> text "CAST" <> parens (ppPGExpr e <+> text "AS" <+> text (ppPGType typ))
    DefaultSqlExpr    -> text "DEFAULT"
    ArraySqlExpr es -> text "ARRAY" <> brackets (commaH ppPGExpr es)
    ExistsSqlExpr s     -> text "EXISTS" <+> parens (ppSelect s)
    NamedWindowSqlExpr w e -> ppPGExpr e <+> text "OVER" <+> text w
    AnonWindowSqlExpr p o e -> ppPGExpr e <+> text "OVER" <+> parens (partPP p <> ppOrderBy o)
      where partPP     [] = empty
            partPP     xs = text "PARTITION BY" <+> (commaH ppPGExpr xs <> space)

ppInsert :: SqlInsert -> Doc
ppInsert (SqlInsert table names values rets)
    = text "INSERT INTO" <+> ppTable table
      <+> parens (commaV ppColumn names)
      $$ text "VALUES" <+> commaV (\v -> parens (commaV ppPGExpr v))
                                  (toList values)
      $$ ppReturning rets

ppUpdate :: SqlUpdate -> Doc
ppUpdate (SqlUpdate table assigns criteria rets)
        = text "UPDATE" <+> ppTable table
        $$ text "SET" <+> commaV ppAssign assigns
        $$ ppWhere criteria
        $$ ppReturning rets
    where
      ppAssign (c,e) = ppColumn c <+> equals <+> ppPGExpr e

ppDelete :: SqlDelete -> Doc
ppDelete (SqlDelete table criteria) =
    text "DELETE FROM" <+> ppTable table $$ ppWhere criteria
    
ppReturning :: [SqlExpr] -> Doc
ppReturning []   = empty
ppReturning rets =
  text "RETURNING"
  <+> commaV ppPGExpr (toList rets)

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

ppPGType :: DBType -> String
ppPGType = go
  where go DBInt2                       = "SMALLINT"
        go DBInt4                       = "INTEGER"
        go DBInt8                       = "BIGINT"
        go (DBNumeric p s)              = "NUMERIC (" ++ show p ++ ", " ++ show s ++ ")"
        go DBBool                       = "BOOLEAN"
        go (DBFloat i) | i < 25 && i >= 0 = "REAL"
                       | i >= 53         = "DOUBLE PRECISION"
                       | otherwise      = error "Panic: outside allowed range @ppPGType DBFloat"
        go (DBChar i)                   = "CHARACTER (" ++ show i ++ ")"
        go (DBVarchar i)                = "CHARACTER VARYING (" ++ show i ++ ")"
        go DBText                       = "TEXT"
        go (DBTimestamp i)              = "TIMESTAMP (" ++ show i ++ ")"        
        go (DBTimestamptz i)            = "TIMESTAMP (" ++ show i ++ ") WITH TIME ZONE"
        go (DBTime i)                   = "TIME (" ++ show i ++ ")"
        go (DBTimetz i)                 = "TIME (" ++ show i ++ ") WITH TIME ZONE"
        go (DBInterval _ i)             = "INTERVAL (" ++ show i ++ ")"
        go (DBBinary _)                 = "BYTEA"        
        go (DBVarbinary _)              = "BYTEA"
        go (DBBit n)                    = "BIT (" ++ show n ++ ")"
        go (DBVarbit n)                 = "BIT VARYING (" ++ show n ++ ")"
        go DBCiText                     = "CITEXT"
        go DBDate                       = "DATE"
        go DBUuid                       = "UUID"
        go DBJson                       = "JSON"
        go DBJsonB                      = "JSONB"
        go DBXml                        = "XML"
        go (DBArray t)                  = go t ++ "[]"
        go (DBNullable t)               = go t
        go (DBTypeName t args)          = T.unpack (doubleQuote t) ++ ppArgs args
        go (DBCustomType t _)           = go t

        ppArgs []  = ""
        ppArgs xs  = "(" ++ L.intercalate "," (map ppArg xs) ++ ")"

        ppArg (TextArg t)    = T.unpack t
        ppArg (IntegerArg i) = show i
