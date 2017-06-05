-- |
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
--                Purely Agile Limited (c) 2014-2016
-- License     :  BSD-style

module DBRecord.Internal.Postgres.Pretty where

import DBRecord.Internal.Postgres.Types hiding (alias, criteria)
import qualified DBRecord.Internal.Postgres.Types as PGT
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import Data.Foldable (toList)
import Data.Monoid ((<>))
import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ (Doc, ($$), (<+>), text, empty,
                                  parens, comma, punctuate,
                                  hcat, vcat, brackets, doubleQuotes,
                                  hsep, equals, char, empty, render)

ppSelect :: SqlSelect -> Doc
ppSelect select = case select of
  SqlProduct sqSels selectFrom -> ppSelectWith selectFrom (ppProduct sqSels) 
  SqlSelect tab selectFrom     -> ppSelectWith selectFrom (ppTable tab)
  SqlJoin joinSt selectFrom      -> ppSelectWith selectFrom (ppJoin joinSt)
  SqlValues vals als           -> ppAs (text <$> als) $ ppSelectValues vals
  SqlBin bin als               -> ppAs (text <$> als) $ ppSelectBinary bin

ppSelectWith :: SelectFrom -> Doc -> Doc
ppSelectWith from tabDoc =
    ppAs (doubleQuotes . text <$> PGT.alias from) $
    parens $ 
      text "SELECT"
  <+> ppAttrs (attrs from)
  $$  text "FROM " <+> tabDoc
  $$  ppWhere (PGT.criteria from)
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
nameAs (expr, name) = ppAs (fmap unColumn name) (ppSqlExpr expr)
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
                   $$  ppSqlExpr (jCond joinSt)
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
ppValuesRow = parens . commaH ppSqlExpr

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
  = text "PARTITION BY" <+> commaH ppSqlExpr es
ppPartition (WindowPart es oeds)
  = text "PARTITION BY" <+> commaH ppSqlExpr es <+> text "ORDER BY" <+> commaH ppOrd oeds
  
ppWhere :: [SqlExpr] -> Doc
ppWhere []    = empty
ppWhere exprs = text "WHERE" <+>  hsep (intersperse (text "AND")
                                        (map (parens . ppSqlExpr) exprs))

ppGroupBy :: Maybe (NEL.NonEmpty SqlExpr) -> Doc
ppGroupBy Nothing      = empty
ppGroupBy (Just exprs) = go (toList exprs)
  where
    go es = text "GROUP BY" <+> ppGroupAttrs es
    ppGroupAttrs es = commaV (ppSqlExpr . deliteral) es

ppOrderBy :: [(SqlExpr,SqlOrder)] -> Doc
ppOrderBy []   = empty
ppOrderBy ords = text "ORDER BY" <+> commaV ppOrd ords

ppOrd :: (SqlExpr, SqlOrder) -> Doc
ppOrd (e, o) = ppSqlExpr (deliteral e)
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
ppLimit (Just lmt) = text "LIMIT " <> ppSqlExpr lmt

ppOffset :: Maybe SqlExpr -> Doc
ppOffset Nothing    = empty
ppOffset (Just off) = text "OFFSET " <> ppSqlExpr off

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

ppSqlExpr :: SqlExpr -> Doc
ppSqlExpr expr =
  case expr of
    ColumnSqlExpr c     -> ppColumn c
    CompositeSqlExpr s x -> parens (ppSqlExpr s) <> text "." <> text x
    ParensSqlExpr e -> parens (ppSqlExpr e)
    BinSqlExpr op e1 e2 -> ppSqlExpr e1 <+> text op <+> ppSqlExpr e2
    PrefixSqlExpr op e  -> text op <+> ppSqlExpr e
    PostfixSqlExpr op e -> ppSqlExpr e <+> text op
    FunSqlExpr f es     -> text f <> parens (commaH ppSqlExpr es)
    AggrFunSqlExpr f es ord -> text f <> parens (commaH ppSqlExpr es <+> ppOrderBy ord)
    ConstSqlExpr c      -> text c
    CaseSqlExpr cs el   -> text "CASE" <+> vcat (toList (fmap ppWhen cs))
      <+> text "ELSE" <+> ppSqlExpr el <+> text "END"
      where ppWhen (w,t) = text "WHEN" <+> ppSqlExpr w
                       <+> text "THEN" <+> ppSqlExpr t
    ListSqlExpr es      -> parens (commaH ppSqlExpr es)
    ParamSqlExpr _ v -> ppSqlExpr v
    PlaceHolderSqlExpr -> text "?"
    CastSqlExpr typ e -> text "CAST" <> parens (ppSqlExpr e <+> text "AS" <+> text typ)
    DefaultSqlExpr    -> text "DEFAULT"
    ArraySqlExpr es -> text "ARRAY" <> brackets (commaH ppSqlExpr es)
    ExistsSqlExpr s     -> text "EXISTS" <+> parens (ppSelect s)
    WindowSqlExpr w e -> ppSqlExpr e <+> text "OVER" <+> text w 

ppInsert :: SqlInsert -> Doc
ppInsert (SqlInsert table names values)
    = text "INSERT INTO" <+> ppTable table
      <+> parens (commaV ppColumn names)
      $$ text "VALUES" <+> commaV (\v -> parens (commaV ppSqlExpr v))
                                  (toList values)

ppUpdate :: SqlUpdate -> Doc
ppUpdate (SqlUpdate table assigns criteria)
        = text "UPDATE" <+> ppTable table
        $$ text "SET" <+> commaV ppAssign assigns
        $$ ppWhere criteria
    where
      ppAssign (c,e) = ppColumn c <+> equals <+> ppSqlExpr e

ppDelete :: SqlDelete -> Doc
ppDelete (SqlDelete table criteria) =
    text "DELETE FROM" <+> ppTable table $$ ppWhere criteria
                                   
ppInsertReturning :: Returning SqlInsert -> Doc
ppInsertReturning (Returning insert returnExprs) =
  ppInsert insert
  $$ text "RETURNING"
  <+> commaV ppSqlExpr (toList returnExprs)

ppUpdateReturning :: Returning SqlUpdate -> Doc
ppUpdateReturning (Returning update returnExprs) =
  ppUpdate update
  $$ text "RETURNING"
  <+> commaV ppSqlExpr (toList returnExprs)

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

-- testPP doc = render doc

renderQuery :: SqlSelect -> String
renderQuery = render . ppSelect

renderDelete :: SqlDelete -> String
renderDelete = render . ppDelete

renderInsert :: SqlInsert -> String
renderInsert = render . ppInsert

renderInsertRet :: Returning SqlInsert -> String
renderInsertRet = render . ppInsertReturning

renderUpdate :: SqlUpdate -> String
renderUpdate = render . ppUpdate

renderUpdateRet :: Returning SqlUpdate -> String
renderUpdateRet = render . ppUpdateReturning

