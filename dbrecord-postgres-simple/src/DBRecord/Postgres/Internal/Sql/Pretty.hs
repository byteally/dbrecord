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
  , renderExpr
  , ppExpr
  , ppPGType
  , ppPGOIDType
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as Base16
-- import DBRecord.Driver
--import DBRecord.Internal.Types hiding (DBTypeK (..), TypeArgK (..), DBTypeNameK (..))
import DBRecord.Internal.DBTypes hiding (toNullable)
import DBRecord.Internal.Sql.DML hiding (alias, criteria)
import qualified DBRecord.Internal.Sql.DML as DML
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import Data.Foldable (toList)
import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ (Doc, ($$), (<+>), text, empty,
                                  parens, comma, punctuate,
                                  hcat, vcat, brackets, doubleQuotes,
                                  hsep, equals, char, empty, render,
                                  space)
import qualified Data.List as L
import           DBRecord.Types (PGOIDType(..))

ppSelect :: SqlSelect -> Doc
ppSelect select = case select of
  SqlProduct sqSels selectFrom -> ppSelectWith selectFrom (Just (ppProduct sqSels))
  SqlSelect tab selectFrom     -> ppSelectWith selectFrom (ppTableExpr <$> tab)
  SqlJoin joinSt selectFrom    -> ppSelectWith selectFrom (Just (ppJoin joinSt))
  SqlJoins joins selectFrom    -> ppSelectWith selectFrom (Just (ppJoins joins))
  SqlBin binSt as              -> ppSelectBinary binSt as
  SqlCTE withs sql             -> ppSelectCTE withs sql  
  SqlValues vals als           -> ppAs (text <$> als) $ ppSelectValues vals
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
    Nothing -> selBin
    Just als -> ppAs (Just $ doubleQuotes . text $ als) (parens selBin)

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
        ppAtts as = go as

        go = parens . commaH (text . T.unpack)

ppJoin :: Join -> Doc
ppJoin joinSt = ppJoinedTabs
  where ppJoinedTabs = parens (   
                       ppTableExpr s1
                   $$  ppJoinType (jJoinType joinSt) <> ppLateral (jLateral joinSt)
                   $$  ppTableExpr s2
                   $$  ppOn (jCond joinSt)
                   )
                   
        (s1, s2) = jTables joinSt

ppOn :: Maybe SqlExpr -> Doc
ppOn Nothing  = empty
ppOn (Just e) =   text "ON"
                  $$  ppExpr e
        
ppJoins :: InlineJoin -> Doc
ppJoins ijs = case ijs of
  InlineJoinBase t -> ppTableExpr t
  InlineJoinL js jt lat t on -> parens ( ppJoins js
                                       $$ ppJoinType jt <> ppLateral lat
                                       $$ ppTableExpr t
                                       $$ ppOn (Just on)
                                       )
  InlineJoinR t jt lat js on -> parens ( ppTableExpr t
                                       $$ ppJoinType jt <> ppLateral lat
                                       $$ ppJoins js
                                       $$ ppOn (Just on)
                                       )

ppLateral :: Lateral -> Doc
ppLateral True = space <> text "LATERAL" <> space
ppLateral False = empty

ppJoinType :: JoinType -> Doc
ppJoinType LeftJoin   = text "LEFT OUTER JOIN"
ppJoinType RightJoin  = text "RIGHT OUTER JOIN"
ppJoinType FullJoin   = text "FULL OUTER JOIN"
ppJoinType InnerJoin  = text "INNER JOIN"
ppJoinType CrossJoin  = text "CROSS JOIN"


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
ppTableName (SqlTableName _db sc tab) =
  quoted sc <> dot <> quoted tab
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
    CastSqlExpr typ e -> text "CAST" <> parens (ppExpr e <+> text "AS" <+> text (ppPGType typ))
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
        go OpMod        = "MOD"
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
        go OpAbs              = text "@" <> parens (ppExpr e)
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

ppConflict :: Maybe SqlConflict -> Doc
ppConflict Nothing =
  empty
ppConflict (Just (SqlConflict mtgt act)) =
  text "ON CONFLICT" <> ppTgt mtgt <> space <> ppAct act

  where ppTgt (SqlConflictConstraint ctx) =
          space <> text "ON CONSTRAINT" <> space <> text (T.unpack ctx)
        ppTgt  (SqlConflictColumn cols) =
          space <> parens (commaH ppColumn cols)
        ppTgt SqlConflictAnon =
          empty

        ppAct SqlConflictDoNothing = text "DO NOTHING"
        ppAct (SqlConflictUpdate upd) =
          text "DO" <> space <> ppUpdate' False upd

ppInsert :: SqlInsert -> Doc
ppInsert (SqlInsert table names values mconflict rets)
    = text "INSERT INTO" <+> ppTableName table
      <+> parens (commaV ppColumn names)
      $$ text "VALUES" <+> commaV (\v -> parens (commaV ppExpr v))
                                  (toList values)
      $$ ppConflict mconflict
      $$ ppReturning rets

ppUpdate :: SqlUpdate -> Doc
ppUpdate = ppUpdate' True 

ppUpdate' :: Bool -> SqlUpdate -> Doc
ppUpdate' b (SqlUpdate table assigns criteria rets)
        = text "UPDATE" <> (if b then space <> ppTableName table else space)
        $$ text "SET" <+> commaV ppAssign assigns
        $$ ppWhere criteria
        $$ ppReturning rets
    where
      ppAssign (c,e) = ppColumn c <+> equals <+> ppExpr e      

ppDelete :: SqlDelete -> Doc
ppDelete (SqlDelete table criteria rets) =
    text "DELETE FROM" <+> ppTableName table
    $$ ppWhere criteria
    $$ ppReturning rets
    
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

renderExpr :: SqlExpr -> String
renderExpr = render . ppExpr

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
        go DBLTree                      = "LTREE"
        go (DBArray t)                  = go t ++ "[]"
        go (DBNullable t)               = go t
        go (OtherBuiltInType tn)        = ppDbTypeName tn
        go (DBCustomType scn tn)        = T.unpack (doubleQuote scn) <> dot <> ppDbTypeName tn

        ppDbTypeName (DBTypeName t args) = T.unpack (doubleQuote t) ++ ppArgs args

        ppArgs []  = ""
        ppArgs xs  = "(" ++ L.intercalate "," (map ppArg xs) ++ ")"

        ppArg (TextArg t)    = T.unpack t
        ppArg (IntegerArg i) = show i

        dot = "."


ppPGOIDType :: PGOIDType -> String
ppPGOIDType PGOID = "oid"
ppPGOIDType RegProc = "regproc"
ppPGOIDType RegProcedure = "regprocedure"
ppPGOIDType RegOper = "regoper"
ppPGOIDType RegOperator = "regoperator"
ppPGOIDType RegClass = "regclass"
ppPGOIDType RegType = "regtype"
ppPGOIDType RegRole = "regrole"
ppPGOIDType RegNamespace = "regnamespace"
ppPGOIDType RegConfig = "regconfig"
ppPGOIDType RegDictionary = "regdictionary"
