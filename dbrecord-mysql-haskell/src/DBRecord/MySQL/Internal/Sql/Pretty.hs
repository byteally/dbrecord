{-# LANGUAGE OverloadedStrings #-}

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
  , ppExpr
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
                                  hcat, vcat, brackets,
                                   hsep, equals, char, empty, render,
                                  space)
import DBRecord.Schema.Interface
import qualified Data.List as L


ppSelect :: SqlSelect -> Doc
ppSelect select = case select of
  SqlProduct sqSels selectFrom -> ppSelectWith selectFrom (Just (ppProduct sqSels)) 
  SqlSelect tab selectFrom     -> ppSelectWith selectFrom (ppTableExpr <$> tab)
  SqlJoin joinSt selectFrom    -> ppSelectWith selectFrom (Just (ppJoin joinSt))
  SqlBin binSt as              -> ppSelectBinary binSt as
  SqlCTE withs sql             -> ppSelectCTE withs sql
  SqlValues vals als           -> ppAs (text <$> als) $ ppSelectValues vals
  -- SqlBin bin als               -> ppAs (text <$> als) $ ppSelectBinary bin

ppSelectWith :: SelectFrom -> Maybe Doc -> Doc
ppSelectWith from tabDoc =
    ppAs (backtickQuotes . text <$> DML.alias from) $
    parens $ 
      text "SELECT"
  <+> ppAttrs (attrs from)
  $$  ppTab
  $$  ppWhere (DML.criteria from)
  $$  ppWindows (windows from)  
  $$  ppGroupBy (groupby from)
  $$  ppHaving (having from)  
  $$  ppOrderBy (orderby from)
  $$  ppLimit (limit from)
  $$  ppOffset (offset from)

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
ppSelectBinary bin as =
  let selBin =    ppSelect (bSelect1 bin)
               $$ ppSelBinOp (bOp bin)
               $$ ppSelect (bSelect2 bin)
  in case as of
    Nothing  -> selBin
    Just als -> ppAs (Just $ backtickQuotes . text $ als) (parens selBin)


ppSelBinOp :: SelectBinOp -> Doc
ppSelBinOp op = text $ case op of
  Union        -> "UNION"
  UnionAll     -> "UNIONALL"
  Except       -> "EXCEPT"
  ExceptAll    -> "EXCEPTALL"
  Intersect    -> "INTERSECT"
  IntersectAll -> "INTERSECTALL"

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
ppOrd (e, o) = ppExpr (deliteral e)
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
ppLimit (Just lmt) = text "LIMIT " <> ppExpr lmt

ppOffset :: Maybe SqlExpr -> Doc
ppOffset Nothing    = empty
ppOffset (Just off) = text "OFFSET " <> ppExpr off

-- ppOid :: SqlOidName -> Doc
-- ppOid (SqlOidName n) = quotes (doubleQuotes (text (T.unpack n)))

ppColumn :: SqlColumn -> Doc
ppColumn (SqlColumn s) =
  case map T.unpack s of
    [x]      -> backtickQuotes (text x)
    (x : xs) -> backtickQuotes (text x) <> char '.' <> ppAliasedCol xs
    _        -> error "Panic: Column cannot be empty"

ppTableExpr :: SqlTableExpr -> Doc
ppTableExpr (NestedSqlSelect sql)     = ppSelect sql
ppTableExpr (SqlTabName sqltab)       = ppTableName sqltab
ppTableExpr (SqlTabFun funName args)  = ppTableFun funName args

ppTableFun :: SqlName -> [SqlName] -> Doc
ppTableFun funN args = text (T.unpack funN) <> parens (hsep (map (text . T.unpack) args))

ppTableName :: SqlTableName -> Doc
ppTableName (SqlTableName db _ tab) =
  quoted db <> text "." <> quoted tab
  where
    quoted = backtickQuotes . text

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
    CastSqlExpr typ e -> text "CAST" <> parens (ppExpr e <+> text "AS" <+> text (ppMysqlType typ))
    DefaultSqlExpr    -> text "DEFAULT"
    ArraySqlExpr es -> text "ARRAY" <> brackets (commaH ppExpr es)
    ExistsSqlExpr s     -> text "EXISTS" <+> parens (ppSelect s)
    NamedWindowSqlExpr w e -> ppExpr e <+> text "OVER" <+> text w
    AnonWindowSqlExpr p o e -> ppExpr e <+> text "OVER" <+> parens (partPP p <> ppOrderBy o)
      where partPP     [] = empty
            partPP     xs = text "PARTITION BY" <+> (commaH ppExpr xs <> space)
    TableSqlExpr {} -> error "Panic: table sql expr not implemented @ppExpr"


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
ppInsert (SqlInsert table names values Nothing rets)
    = text "INSERT IGNORE INTO" <+> ppTableName table
      <+> parens (commaV ppColumn names)
      $$ text "VALUES" <+> commaV (\v -> parens (commaV ppExpr v))
                                  (toList values)
      $$ ppReturning rets
ppInsert (SqlInsert table names values (Just (SqlConflict _ SqlConflictDoNothing)) rets)
    = text "INSERT IGNORE INTO" <+> ppTableName table
      <+> parens (commaV ppColumn names)
      $$ text "VALUES" <+> commaV (\v -> parens (commaV ppExpr v))
                                  (toList values)
      $$ ppReturning rets
ppInsert (SqlInsert table names values (Just (SqlConflict _ (SqlConflictUpdate upd))) rets)
    = text "INSERT INTO" <+> ppTableName table
      <+> parens (commaV ppColumn names)
      $$ text "VALUES" <+> commaV (\v -> parens (commaV ppExpr v))
                                  (toList values)
      $$ text "ON DUPLICATE KEY" <+> ppUpdate upd
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
ppAliasedCol = backtickQuotes . hcat . punctuate aliasSep . map text

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
        go (DBTimestamp _)          = "DATETIME"
        go DBDate                   = "DATE"
        go (DBTime _)               = "TIME"
        go (DBBit i)                = "BIT (" ++ show i ++ ")"        
        
        go (DBNullable t)           = go t
        go (DBTypeName t args)      = T.unpack (backtickQuoteTxt t) ++ ppArgs args
        go (DBCustomType t _)       = go t
        go x                        = error $ "Panic: not implemented @ppMysqlType" ++ (show x)

        ppArgs []  = ""
        ppArgs xs  = "(" ++ L.intercalate "," (map ppArg xs) ++ ")"

        ppArg (TextArg t)    = T.unpack t
        ppArg (IntegerArg i) = show i

        backtickQuoteTxt a   = "`" <> a <> "`"
        
backtickQuotes :: Doc -> Doc
backtickQuotes d = text "`" <> d <> text "`"
