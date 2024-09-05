{-# LANGUAGE OverloadedStrings          #-}
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
  , renderType
  , render

  , ppExpr
  , ppPGType
  , ppDBTypeName
  , ppPGOIDType
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as Base16
import DBRecord.Internal.Types (DBType (..), DBTypeName (..), TypeNameQual (..), TypeArg(..))
import DBRecord.Internal.Sql.DML hiding (alias, criteria)
import qualified DBRecord.Internal.Sql.DML as DML
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import Data.Foldable (toList)
import Data.List (intersperse)
import Prettyprinter.Render.Text ( renderStrict )
import Prettyprinter ((<+>), emptyDoc, parens, comma, punctuate,
                      hcat, vcat, brackets, dquotes, encloseSep,
                      hsep, equals, layoutPretty, defaultLayoutOptions,
                      space, pretty
                     )
import qualified Prettyprinter as PP
import           DBRecord.Types (PGOIDType(..))

type Doc = PP.Doc ()

ppSelect :: SqlSelect -> Doc
ppSelect select = case select of
  SqlProduct sqSels selectFrom -> ppSelectWith selectFrom (Just (ppProduct sqSels))
  SqlSelect tab selectFrom     -> ppSelectWith selectFrom (ppTableExpr <$> tab)
  SqlJoin joinSt selectFrom    -> ppSelectWith selectFrom (Just (ppJoin joinSt))
  SqlJoins joins selectFrom    -> ppSelectWith selectFrom (Just (ppJoins joins))
  SqlBin binSt as              -> ppSelectBinary binSt as
  SqlCTE withs sql             -> ppSelectCTE withs sql  
  SqlValues vals als           -> ppAs (pretty <$> als) $ ppSelectValues vals
  -- SqlBin bin als               -> ppAs (pretty <$> als) $ ppSelectBinary bin

ppSelectWith :: SelectFrom -> Maybe Doc -> Doc
ppSelectWith from tabDoc =
    ppAs (dquotes . pretty <$> DML.alias from) $
    parens $
    vcat [ "SELECT" <+> ppOptions (options from) <+> ppAttrs (attrs from)
         , ppTab
         , ppWhere (DML.criteria from)
         , ppWindows (windows from)  
         , ppGroupBy (groupby from)
         , ppHaving (having from)  
         , ppOrderBy (orderby from)
         , ppLimit (limit from)
         , ppOffset (offset from)
         ]

  where ppTab = case tabDoc of
          Nothing  -> emptyDoc
          Just doc ->  "FROM " <+> doc

ppProduct :: [SqlTableExpr] -> Doc
ppProduct = ppTables

ppAttrs :: SelectAttrs -> Doc
ppAttrs All            =  "*"
ppAttrs (Columns cols) = (commaV nameAs . toList) cols

ppOptions :: Maybe SelectOption -> Doc
ppOptions =
  maybe emptyDoc go

  where
    go SelectAll                  = space <>  "ALL"
    go (SelectDistinct Nothing)   = space <>  "DISTINCT" 
    go (SelectDistinct (Just vs)) = space <> ( "DISTINCT" <+> commaH ppExpr (NEL.toList vs))

nameAs :: (SqlExpr, Maybe SqlColumn) -> Doc
nameAs (expr, n) = ppAs (fmap unColumn n) (ppExpr expr)
  where unColumn (SqlColumn s) = ppAliasedCol (map T.unpack s)
        
ppTables :: [SqlTableExpr] -> Doc
ppTables []   = emptyDoc
ppTables tabs = commaV ppTableExpr tabs

ppSelectBinary :: Binary -> Alias -> Doc
ppSelectBinary bin as =
  let selBin = vcat [ ppSelect (bSelect1 bin)
                    , ppSelBinOp (bOp bin)
                    , ppSelect (bSelect2 bin)
                    ]
  in case as of
    Nothing -> selBin
    Just als -> ppAs (Just $ dquotes . pretty $ als) (parens selBin)

ppSelBinOp :: SelectBinOp -> Doc
ppSelBinOp op = case op of
  Union        -> "UNION"
  UnionAll     -> "UNIONALL"
  Except       -> "EXCEPT"
  ExceptAll    -> "EXCEPTALL"
  Intersect    -> "INTERSECT"
  IntersectAll -> "INTERSECTALL"

ppSelectCTE :: [SqlWith] -> SqlSelect -> Doc
ppSelectCTE sqWiths sel =
  vcat [ "WITH" <+> commaV ppSqlWith sqWiths
       , ppSelect sel
       ]
  
  where ppSqlWith (SqlWith tabn atts isel) =
              vcat [ pretty tabn <+> ppAtts atts
                   , "AS"
                   , ppSelect isel
                   ]
        ppAtts [] = emptyDoc
        ppAtts as = go as

        go = parens . commaH pretty

ppJoin :: Join -> Doc
ppJoin joinSt = ppJoinedTabs
  where ppJoinedTabs = parens (vcat [ ppTableExpr s1
                                    , ppJoinType (jJoinType joinSt) <> ppLateral (jLateral joinSt)
                                    , ppTableExpr s2
                                    , ppOn (jCond joinSt)
                                    ]
                              )
                   
        (s1, s2) = jTables joinSt

ppOn :: Maybe SqlExpr -> Doc
ppOn Nothing  = emptyDoc
ppOn (Just e) = vcat [ "ON"
                     , ppExpr e
                     ]
        
ppJoins :: InlineJoin -> Doc
ppJoins ijs = case ijs of
  InlineJoinBase t -> ppTableExpr t
  InlineJoinL js jt lat t on -> parens ( vcat [ ppJoins js
                                              , ppJoinType jt <> ppLateral lat
                                              , ppTableExpr t
                                              , ppOn (Just on)
                                              ]
                                       )
  InlineJoinR t jt lat js on -> parens ( vcat [ ppTableExpr t
                                              , ppJoinType jt <> ppLateral lat
                                              , ppJoins js
                                              , ppOn (Just on)
                                              ]
                                       )

ppLateral :: Lateral -> Doc
ppLateral True = space <>  "LATERAL" <> space
ppLateral False = emptyDoc

ppJoinType :: JoinType -> Doc
ppJoinType LeftJoin   =  "LEFT OUTER JOIN"
ppJoinType RightJoin  =  "RIGHT OUTER JOIN"
ppJoinType FullJoin   =  "FULL OUTER JOIN"
ppJoinType InnerJoin  =  "INNER JOIN"
ppJoinType CrossJoin  =  "CROSS JOIN"


ppSelectValues :: SqlValues -> Doc
ppSelectValues v =
  vcat [ "SELECT"  <+> ppAttrs (vAttrs v)
       , "FROM"
       , ppValues (vValues v)
       ]

ppValues :: [[SqlExpr]] -> Doc
ppValues vals = ppAs (Just ( "V")) (parens ( vcat ["VALUES",  commaV ppValuesRow vals]))

ppValuesRow :: [SqlExpr] -> Doc
ppValuesRow = parens . commaH ppExpr

ppWindows :: [WindowExpr] -> Doc
ppWindows [] = emptyDoc
ppWindows ws = hsep (map ppWindow ws)
  where ppWindow (WindowExpr wn parts) =
               "WINDOW"
          <+> pretty wn
          <+>  "AS"
          <+> parens (ppPartition parts)

ppPartition :: WindowPart -> Doc
ppPartition (WindowPart [] [])
  = emptyDoc
ppPartition (WindowPart es [])
  =  "PARTITION BY" <+> commaH ppExpr es
ppPartition (WindowPart es oeds)
  =  "PARTITION BY" <+> commaH ppExpr es <+>  "ORDER BY" <+> commaH ppOrd oeds
  
ppWhere :: [SqlExpr] -> Doc
ppWhere []    = emptyDoc
ppWhere exprs =  "WHERE" <+>  hsep (intersperse ( "AND")
                                        (map (parens . ppExpr) exprs))

ppGroupBy :: Maybe (NEL.NonEmpty SqlExpr) -> Doc
ppGroupBy Nothing      = emptyDoc
ppGroupBy (Just exprs) = go (toList exprs)
  where
    go es =  "GROUP BY" <+> ppGroupAttrs es
    ppGroupAttrs es = commaV (ppExpr . deliteral) es

ppHaving :: [SqlExpr] -> Doc
ppHaving []    = emptyDoc
ppHaving exprs = go (toList exprs)
  where
    go es =  "HAVING" <+> ppGroupAttrs es
    ppGroupAttrs es = commaV (ppExpr . deliteral) es

ppOrderBy :: [(SqlExpr,SqlOrder)] -> Doc
ppOrderBy []   = emptyDoc
ppOrderBy ords =  "ORDER BY" <+> commaV ppOrd ords

ppOrd :: (SqlExpr, SqlOrder) -> Doc
ppOrd (e, o) = ppExpr (deliteral e)
                    <+> ppOrdDir o
                    <+> ppNullOrd o

ppOrdDir :: SqlOrder -> Doc
ppOrdDir sqlOrd = case sqlOrdDirection sqlOrd of
  SqlAsc  -> "ASC"
  SqlDesc -> "DESC"

ppNullOrd :: SqlOrder -> Doc
ppNullOrd sqlOrd = case sqlNullOrd sqlOrd of
  SqlNullsFirst -> "NULLS FIRST"
  SqlNullsLast  -> "NULLS LAST"

ppLimit :: Maybe SqlExpr -> Doc
ppLimit Nothing    = emptyDoc
ppLimit (Just lmt) =  "LIMIT " <> ppExpr lmt

ppOffset :: Maybe SqlExpr -> Doc
ppOffset Nothing    = emptyDoc
ppOffset (Just off) =  "OFFSET " <> ppExpr off

-- ppOid :: SqlOidName -> Doc
-- ppOid (SqlOidName n) = quotes (dquotes (pretty n))

ppColumn :: SqlColumn -> Doc
ppColumn (SqlColumn s) =
  case map T.unpack s of
    [x]      -> dquotes (pretty x)
    (x : xs) -> dquotes (pretty x) <> pretty '.' <> ppAliasedCol xs
    _        -> error "Panic: Column cannot be empty"

ppTableExpr :: SqlTableExpr -> Doc
ppTableExpr (NestedSqlSelect sql)     = ppSelect sql
ppTableExpr (SqlTabName sqltab)       = ppTableName sqltab
ppTableExpr (SqlTabFun funName args)  = ppTableFun funName args

ppTableFun :: SqlName -> [SqlName] -> Doc
ppTableFun funN args = pretty funN <> parens (hsep (map pretty args))

ppTableName :: SqlTableName -> Doc
ppTableName (SqlTableName _db sc tab) =
  quoted sc <> dot <> quoted tab
  where
    quoted = dquotes . pretty
    dot =  "."

ppExpr :: SqlExpr -> Doc
ppExpr expr =
  case expr of
    ColumnSqlExpr c     -> ppColumn c
    -- OidSqlExpr s        -> ppOid s
    CompositeSqlExpr s x -> parens (ppExpr s) <>  "." <> pretty x
    ParensSqlExpr e -> parens (ppExpr e)
    BinSqlExpr op e1 e2 -> ppExpr e1 <+> ppBinOp op <+> ppExpr e2
    PrefixSqlExpr op e  -> ppPrefixExpr op e
    PostfixSqlExpr op e -> ppPostfixExpr op e
    FunSqlExpr f es     -> pretty f <> parens (commaH ppExpr es)
    AggrFunSqlExpr f es ord -> pretty f <> parens (commaH ppExpr es <+> ppOrderBy ord)
    ConstSqlExpr c      -> ppLiteral c
    CaseSqlExpr cs el   ->  "CASE" <> space <> vcat (toList (fmap ppWhen cs))
      <> ppElse el <> space <>  "END"
      where ppWhen (w,t) =  "WHEN" <+> ppExpr w
                       <+>  "THEN" <+> ppExpr t
            ppElse (Just e) = space <> ( "ELSE" <+> ppExpr e)
            ppElse Nothing  = space 
    ListSqlExpr es      -> parens (commaH ppExpr es)
    ParamSqlExpr _ v -> ppExpr v
    PlaceHolderSqlExpr ->  "?"
    CastSqlExpr typ e ->  "CAST" <> parens (ppExpr e <+>  "AS" <+> ppPGType typ)
    DefaultSqlExpr    ->  "DEFAULT"
    ArraySqlExpr es ->  "ARRAY" <> brackets (commaH ppExpr es)
    TableSqlExpr sql -> parens (ppSelect sql)    
    ExistsSqlExpr s     ->  "EXISTS" <+> parens (ppSelect s)
    NamedWindowSqlExpr w e -> ppExpr e <+>  "OVER" <+> pretty w
    AnonWindowSqlExpr p o e -> ppExpr e <+>  "OVER" <+> parens (partPP p <> ppOrderBy o)
      where partPP     [] = emptyDoc
            partPP     xs =  "PARTITION BY" <+> (commaH ppExpr xs <> space)
    RowSqlExpr es      ->  "ROW" <> parens (commaH ppExpr es)
            

ppBinOp :: BinOp -> Doc
ppBinOp = go
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
        go (OpOther s)  = pretty s
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
  where go OpNot              =  "NOT" <> parens (ppExpr e)
        go OpLength           =  "LENGTH" <> parens (ppExpr e)
        go OpAbs              =  "@" <> parens (ppExpr e)
        go OpNegate           =  "-" <> parens (ppExpr e)
        go OpLower            =  "LOWER" <> parens (ppExpr e)
        go OpUpper            =  "UPPER" <> parens (ppExpr e)
        go (OpOtherFun s)     = pretty s <> parens (ppExpr e)
        go (OpOtherPrefix s)  = pretty s <+> (ppExpr e)
        go _                  = error "Panic: unsupported combination @ppPrefixExpr"

ppPostfixExpr :: UnOp -> SqlExpr -> Doc
ppPostfixExpr op e = go op
  where go OpIsNull           = ppExpr e <+>  "IS NULL"
        go OpIsNotNull        = ppExpr e <+>  "IS NOT NULL"
        go (OpOtherPostfix s) = ppExpr e <+> pretty s 
        
        go _              = error "Panic: unsupported combination @ppPostfixExpr"

ppConflict :: Maybe SqlConflict -> Doc
ppConflict Nothing =
  emptyDoc
ppConflict (Just (SqlConflict mtgt act)) =
   "ON CONFLICT" <> ppTgt mtgt <> space <> ppAct act

  where ppTgt (SqlConflictConstraint ctx) =
          space <>  "ON CONSTRAINT" <> space <> pretty ctx
        ppTgt  (SqlConflictColumn cols) =
          space <> parens (commaH ppColumn cols)
        ppTgt SqlConflictAnon =
          emptyDoc

        ppAct SqlConflictDoNothing =  "DO NOTHING"
        ppAct (SqlConflictUpdate upd) =
           "DO" <> space <> ppUpdate' False upd

ppInsert :: SqlInsert -> Doc
ppInsert (SqlInsert table names values mconflict rets)
    =  "INSERT INTO" <+> ppTableName table <+> parens (commaV ppColumn names) <+>
       vcat [ "VALUES" <+> commaV (\v -> parens (commaV ppExpr v)) (toList values)
            , ppConflict mconflict
            , ppReturning rets
            ]

ppUpdate :: SqlUpdate -> Doc
ppUpdate = ppUpdate' True 

ppUpdate' :: Bool -> SqlUpdate -> Doc
ppUpdate' b (SqlUpdate table assigns criteria rets)
        =
  "UPDATE" <> (if b then space <> ppTableName table else space) <+>
  vcat [ "SET" <+> commaV ppAssign assigns
       , ppWhere criteria
       , ppReturning rets
       ]
  
    where
      ppAssign (c,e) = ppColumn c <+> equals <+> ppExpr e      

ppDelete :: SqlDelete -> Doc
ppDelete (SqlDelete table criteria rets) =
  "DELETE FROM" <+>
  vcat [ ppTableName table
       , ppWhere criteria
       , ppReturning rets
       ]
    
ppReturning :: [SqlExpr] -> Doc
ppReturning []   = emptyDoc
ppReturning rets =
   "RETURNING"
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
ppAs (Just alias) expr = expr <+> hsep [ "as", alias]

-- TODO: This name is absurdly wrong
ppAliasedCol :: [String] -> Doc
ppAliasedCol = dquotes . hcat . punctuate aliasSep . map pretty

aliasSep :: Doc
aliasSep = pretty '_'


ppLiteral :: LitSql -> Doc
ppLiteral l =
  case l of
    NullSql ->  "NULL"
    DefaultSql ->  "DEFAULT"
    BoolSql True ->  "TRUE"
    BoolSql False ->  "FALSE"
    ByteSql s -> binQuote s
    StringSql s -> quote s
    IntegerSql i -> pretty i
    DoubleSql d -> if isNaN d then  "'NaN'"
                  else if isInfinite d && d < 0 then  "'-Infinity'"
                  else if isInfinite d && d > 0 then  "'Infinity'"
                  else pretty d
    OtherSql s -> pretty s
-- testPP doc = render doc

binQuote :: ByteString -> Doc
binQuote s =  "E'\\\\x" <> pretty (BS8.unpack (Base16.encode s)) <>  "'"

quote :: T.Text -> Doc
quote s = "E'" <> pretty (T.concatMap escape s) <> "'"

escape :: Char -> T.Text
escape '\NUL' = "\\0"
escape '\''   = "''"
escape '"'    = "\\\""
escape '\b'   = "\\b"
escape '\n'   = "\\n"
escape '\r'   = "\\r"
escape '\t'   = "\\t"
escape '\\'   = "\\\\"
escape c      = T.singleton c
          
renderQuery :: SqlSelect -> T.Text
renderQuery = render . ppSelect

renderDelete :: SqlDelete -> T.Text
renderDelete = render . ppDelete

renderInsert :: SqlInsert -> T.Text
renderInsert = render . ppInsert

renderUpdate :: SqlUpdate -> T.Text
renderUpdate = render . ppUpdate

renderExpr :: SqlExpr -> T.Text
renderExpr = render . ppExpr

renderType :: DBType -> T.Text
renderType = render . ppPGType

render :: Doc -> T.Text
render = renderStrict . layoutPretty defaultLayoutOptions

--

ppPGType :: DBType -> Doc
ppPGType = go
  where go DBInt2                       = "SMALLINT"
        go DBInt4                       = "INTEGER"
        go DBInt8                       = "BIGINT"
        go DBFloat4                     = "FLOAT4"
        go DBFloat8                     = "FLOAT8"
        go (DBNumeric p s)              = "NUMERIC" <> encloseSep "(" ")" comma [pretty p, pretty s]
        go DBBool                       = "BOOLEAN"
        go (DBFloat i) | i < 54 && i > 0 = "FLOAT" <> parens (pretty i)
                       | i == 0          = "FLOAT"
                       | otherwise      = error "Panic: outside allowed range @ppPGType DBFloat"
        go (DBChar i)                   = "CHARACTER" <> parens (pretty i)
        go (DBVarchar i)                = "CHARACTER VARYING" <> parens (varCharSz i)
        go DBText                       = "TEXT"
        go (DBTimestamp i)              = "TIMESTAMP" <> parens (pretty i)        
        go (DBTimestamptz i)            = "TIMESTAMP" <> parens (pretty i) <+> "WITH TIME ZONE"
        go (DBTime i)                   = "TIME" <> parens (pretty i)
        go (DBTimetz i)                 = "TIME" <> parens (pretty i) <+> "WITH TIME ZONE"
        go (DBInterval _ i)             = "INTERVAL" <> parens (pretty i)
        go (DBBinary _)                 = "BYTEA"        
        go (DBVarbinary _)              = "BYTEA"
        go (DBBit n)                    = "BIT" <> parens (pretty n)
        go (DBVarbit n)                 = "BIT VARYING" <> parens (pretty n)
        go DBCiText                     = "CITEXT"
        go DBDate                       = "DATE"
        go DBUuid                       = "UUID"
        go DBJson                       = "JSON"
        go DBJsonB                      = "JSONB"
        go DBXml                        = "XML"
        go DBLTree                      = "LTREE"
        go (DBArray t)                  = go t <> "[]"
        go (DBNullable t)               = go t
        go (OtherType tn)               = ppDBTypeName tn

        varCharSz (Left _)  = "MAX"
        varCharSz (Right i) = pretty i 


ppDBTypeName :: DBTypeName -> Doc
ppDBTypeName (DBTypeName qual t args) = case qual of
  NoQualification -> ppDbTypeName t args
  SchemaQualified scn -> dquotes (pretty scn) <> dot <> ppDbTypeName t args
  DBQualified _ scn -> dquotes (pretty scn) <> dot <> ppDbTypeName t args
  where
    ppDbTypeName t' args' = dquotes (pretty t') <> ppArgs args'

    ppArgs []  = emptyDoc
    ppArgs xs  = encloseSep "(" ")" comma (map ppArg xs)


    ppArg (TextArg t')    = pretty t'
    ppArg (IntegerArg i)  = pretty i

    dot = "."

ppPGOIDType :: PGOIDType -> T.Text
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
