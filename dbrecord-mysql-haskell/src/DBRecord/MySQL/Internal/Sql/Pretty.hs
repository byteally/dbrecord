-- |
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
--                Purely Agile Limited (c) 2014-2016
-- License     :  BSD-style

module DBRecord.MySQL.Internal.Sql.Pretty
  ( renderQuery
  , renderDelete
  , renderInsert
  , renderUpdate
  , ppMysqlExpr
  , ppMysqlType
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
  SqlSelect tab selectFrom     -> ppSelectWith selectFrom (ppTableExpr tab)
  SqlJoin joinSt selectFrom    -> ppSelectWith selectFrom (ppJoin joinSt)
  SqlBin binSt                 -> ppSelectBinary binSt
  SqlCTE withs sql             -> ppSelectCTE withs sql
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

ppProduct :: [SqlTableExpr] -> Doc
ppProduct = ppTables

ppAttrs :: SelectAttrs -> Doc
ppAttrs All            = text "*"
ppAttrs (Columns cols) = (commaV nameAs . toList) cols

nameAs :: (SqlExpr, Maybe SqlColumn) -> Doc
nameAs (expr, name) = ppAs (fmap unColumn name) (ppMysqlExpr expr)
  where unColumn (SqlColumn s) = ppAliasedCol (map T.unpack s)
        
ppTables :: [SqlTableExpr] -> Doc
ppTables []   = empty
ppTables tabs = commaV ppTableExpr tabs

ppSelectBinary :: Binary -> Doc
ppSelectBinary bin = ppSelect (bSelect1 bin)
                    $$ ppSelectBinOp (bOp bin)
                    $$ ppSelect (bSelect2 bin)

ppSelectBinOp :: SelectBinOp -> Doc
ppSelectBinOp op = text $ case op of
  Union        -> "UNION"
  UnionAll     -> "UNIONALL"
  Except       -> "EXCEPT"
  ExceptAll    -> "EXCEPTALL"
  Intersect    -> "INTERSECT"
  IntersectAll -> "INTERSECTALL"

ppSelectCTE :: [SqlWith] -> SqlSelect -> Doc
ppSelectCTE sqWiths sel = text "WITH" <+> commaV ppSqlWith sqWiths
                         $$ ppSelect sel
  where ppSqlWith (SqlWith tabn attrs isel) =
              text (T.unpack tabn) <+> ppAttrs attrs
          $$  text "AS"
          $$  ppSelect isel
        ppAttrs [] = empty
        ppAttrs as = parens . commaH (text . T.unpack) $ as

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
                      $$  ppMysqlExpr e
        

ppJoinType :: JoinType -> Lateral -> Doc
ppJoinType LeftJoin   False = text "LEFT OUTER JOIN"
ppJoinType RightJoin  False = text "RIGHT OUTER JOIN"
ppJoinType FullJoin   False = text "FULL OUTER JOIN"
ppJoinType InnerJoin  False = text "INNER JOIN"
ppJoinType CrossJoin  False = text "CROSS JOIN"
ppJoinType  _         True  = error "Panic: impossible case @ppJoinType"


ppSelectValues :: SqlValues -> Doc
ppSelectValues v = text "SELECT"
                   <+> ppAttrs (vAttrs v)
                   $$  text "FROM"
                   $$  ppValues (vValues v)

ppValues :: [[SqlExpr]] -> Doc
ppValues vals = ppAs (Just (text "V")) (parens (text "VALUES" $$ commaV ppValuesRow vals))

ppValuesRow :: [SqlExpr] -> Doc
ppValuesRow = parens . commaH ppMysqlExpr

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
  = text "PARTITION BY" <+> commaH ppMysqlExpr es
ppPartition (WindowPart es oeds)
  = text "PARTITION BY" <+> commaH ppMysqlExpr es <+> text "ORDER BY" <+> commaH ppOrd oeds
  
ppWhere :: [SqlExpr] -> Doc
ppWhere []    = empty
ppWhere exprs = text "WHERE" <+>  hsep (intersperse (text "AND")
                                        (map (parens . ppMysqlExpr) exprs))

ppGroupBy :: Maybe (NEL.NonEmpty SqlExpr) -> Doc
ppGroupBy Nothing      = empty
ppGroupBy (Just exprs) = go (toList exprs)
  where
    go es = text "GROUP BY" <+> ppGroupAttrs es
    ppGroupAttrs es = commaV (ppMysqlExpr . deliteral) es

ppHaving :: [SqlExpr] -> Doc
ppHaving []    = empty
ppHaving exprs = go (toList exprs)
  where
    go es = text "HAVING" <+> ppGroupAttrs es
    ppGroupAttrs es = commaV (ppMysqlExpr . deliteral) es

ppOrderBy :: [(SqlExpr,SqlOrder)] -> Doc
ppOrderBy []   = empty
ppOrderBy ords = text "ORDER BY" <+> commaV ppOrd ords

ppOrd :: (SqlExpr, SqlOrder) -> Doc
ppOrd (e, o) = ppMysqlExpr (deliteral e)
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
ppLimit (Just lmt) = text "LIMIT " <> ppMysqlExpr lmt

ppOffset :: Maybe SqlExpr -> Doc
ppOffset Nothing    = empty
ppOffset (Just off) = text "OFFSET " <> ppMysqlExpr off

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
ppTableName st = case sqlTableSchemaName st of
    Just sn -> doubleQuotes (text sn) <> text "." <> tname
    Nothing -> tname
  where
    tname = doubleQuotes (text (sqlTableName st))

ppMysqlExpr :: SqlExpr -> Doc
ppMysqlExpr expr =
  case expr of
    ColumnSqlExpr c     -> ppColumn c
    -- OidSqlExpr s        -> ppOid s
    CompositeSqlExpr s x -> parens (ppMysqlExpr s) <> text "." <> text x
    ParensSqlExpr e -> parens (ppMysqlExpr e)
    BinSqlExpr op e1 e2 -> ppMysqlExpr e1 <+> ppBinOp op <+> ppMysqlExpr e2
    PrefixSqlExpr op e  -> ppPrefixExpr op e
    PostfixSqlExpr op e -> ppPostfixExpr op e
    FunSqlExpr f es     -> text f <> parens (commaH ppMysqlExpr es)
    AggrFunSqlExpr f es ord -> text f <> parens (commaH ppMysqlExpr es <+> ppOrderBy ord)
    ConstSqlExpr c      -> ppLiteral c
    CaseSqlExpr cs el   -> text "CASE" <> space <> vcat (toList (fmap ppWhen cs))
      <> ppElse el <> space <> text "END"
      where ppWhen (w,t) = text "WHEN" <+> ppMysqlExpr w
                       <+> text "THEN" <+> ppMysqlExpr t
            ppElse (Just e) = space <> (text "ELSE" <+> ppMysqlExpr e)
            ppElse Nothing  = space 
    ListSqlExpr es      -> parens (commaH ppMysqlExpr es)
    ParamSqlExpr _ v -> ppMysqlExpr v
    PlaceHolderSqlExpr -> text "?"
    CastSqlExpr typ e -> text "CAST" <> parens (ppMysqlExpr e <+> text "AS" <+> text (ppMysqlType typ))
    DefaultSqlExpr    -> text "DEFAULT"
    ArraySqlExpr es -> text "ARRAY" <> brackets (commaH ppMysqlExpr es)
    ExistsSqlExpr s     -> text "EXISTS" <+> parens (ppSelect s)
    NamedWindowSqlExpr w e -> ppMysqlExpr e <+> text "OVER" <+> text w
    AnonWindowSqlExpr p o e -> ppMysqlExpr e <+> text "OVER" <+> parens (partPP p <> ppOrderBy o)
      where partPP     [] = empty
            partPP     xs = text "PARTITION BY" <+> (commaH ppMysqlExpr xs <> space)


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

ppPrefixExpr :: UnOp -> SqlExpr -> Doc
ppPrefixExpr op e = go op
  where go OpNot              = text "NOT" <> parens (ppMysqlExpr e)
        go OpLength           = text "LENGTH" <> parens (ppMysqlExpr e)
        go OpAbs              = text "ABS" <> parens (ppMysqlExpr e)
        go OpNegate           = text "-" <> parens (ppMysqlExpr e)
        go OpLower            = text "LOWER" <> parens (ppMysqlExpr e)
        go OpUpper            = text "UPPER" <> parens (ppMysqlExpr e)
        go (OpOtherFun s)     = text s <> parens (ppMysqlExpr e)
        go (OpOtherPrefix s)  = text s <+> (ppMysqlExpr e)
        go _                  = error "Panic: unsupported combination @ppPrefixExpr"

ppPostfixExpr :: UnOp -> SqlExpr -> Doc
ppPostfixExpr op e = go op
  where go OpIsNull           = ppMysqlExpr e <+> text "IS NULL"
        go OpIsNotNull        = ppMysqlExpr e <+> text "IS NOT NULL"
        go (OpOtherPostfix s) = ppMysqlExpr e <+> text s 
        
        go _              = error "Panic: unsupported combination @ppPostfixExpr"

ppInsert :: SqlInsert -> Doc
ppInsert (SqlInsert table names values rets)
    = text "INSERT INTO" <+> ppTableName table
      <+> parens (commaV ppColumn names)
      $$ text "VALUES" <+> commaV (\v -> parens (commaV ppMysqlExpr v))
                                  (toList values)
      $$ ppReturning rets

ppUpdate :: SqlUpdate -> Doc
ppUpdate (SqlUpdate table assigns criteria rets)
        = text "UPDATE" <+> ppTableName table
        $$ text "SET" <+> commaV ppAssign assigns
        $$ ppWhere criteria
        $$ ppReturning rets
    where
      ppAssign (c,e) = ppColumn c <+> equals <+> ppMysqlExpr e

ppDelete :: SqlDelete -> Doc
ppDelete (SqlDelete table criteria) =
    text "DELETE FROM" <+> ppTableName table $$ ppWhere criteria
    
ppReturning :: [SqlExpr] -> Doc
ppReturning []   = empty
ppReturning rets =
  text "RETURNING"
  <+> commaV ppMysqlExpr (toList rets)

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
ppMysqlType :: DBType -> String
ppMysqlType = go
  where go DBInt2                   = "SMALLINT"
        go DBInt4                   = "INT"
        go DBInt8                   = "BIGINT"
        go (DBNumeric p s)          = "NUMERIC (" ++ show p ++ ", " ++ show s ++ ")"
        go (DBFloat n)              = "FLOAT (" ++ show n ++ ")"        
        go (DBChar i)               = "CHAR (" ++ show i ++ " )"
        go (DBVarchar i)            = "VARCHAR (" ++ show i ++ " )"
        go (DBBinary i)             = "BINARY (" ++ show i ++ ")"
        go (DBVarbinary (Right i))  = "VARBINARY (" ++ show i ++ ")"
        go (DBTimestamp i)          = "DATETIME"
        go DBDate                   = "DATE"
        go (DBTime i)               = "TIME"
        go (DBBit i)                = "BIT (" ++ show i ++ ")"        
        
        go (DBNullable t)           = go t
        go (DBTypeName t args)      = T.unpack (doubleQuote t) ++ ppArgs args
        go (DBCustomType t _)       = go t
        go _                        = error "Panic: not implemented @ppMysqlType"

        ppArgs []  = ""
        ppArgs xs  = "(" ++ L.intercalate "," (map ppArg xs) ++ ")"

        ppArg (TextArg t)    = T.unpack t
        ppArg (IntegerArg i) = show i


