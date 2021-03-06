{-# LANGUAGE OverloadedStrings #-}
module DBRecord.Postgres.Internal.Sql.Parser
  ( sqlExpr
  , parsePGType
  , SizeInfo (..)
  , defSizeInfo
  ) where

import DBRecord.Internal.Sql.DML
-- import qualified DBRecord.Internal.Postgres.Types as PGT
import Data.Attoparsec.Text hiding (number)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Applicative
import qualified Data.List.NonEmpty as NEL
import DBRecord.Internal.DBTypes (DBType (..), DBTypeName (..))
import DBRecord.Internal.Types (Max (..))
import Data.Functor (($>))

sqlExpr :: Parser SqlExpr
sqlExpr =
  (
   postfixOrWindowExpr <|>
   binSqlExpr          <|>
   -- compositeExpr  <|>
   prefixSqlExpr       <|>
   termSqlExpr)        -- <*
   -- endOfInput

    where binSqlExpr = do
            e1 <- termSqlExpr
            b  <- Left <$> binOp <|> Right <$> (symbol "::" $> ())
            case b of
              Right _ -> do
                ty <- typeExpr
                pure (CastSqlExpr ty e1)
              Left op -> do
                e2 <- sqlExpr
                pure (BinSqlExpr op e1 e2)
            
          postfixOrWindowExpr = do
            e <-  binSqlExpr <|> prefixSqlExpr <|> termSqlExpr
            op <- eitherP (symbol "OVER" *> word) postfixOp
            case op of
              Left w -> pure (NamedWindowSqlExpr w e)
              Right opv -> pure (PostfixSqlExpr opv e)

          prefixSqlExpr = do
            op <- prefixOp
            e <-  sqlExpr
            pure (PrefixSqlExpr op e)

termSqlExpr :: Parser SqlExpr
termSqlExpr =
  arraySqlExpr                                                        <|>
  defaultExpr                                                         <|>
  placeholderExpr                                                     <|>
  castExpr                                                            <|>
  caseExpr                                                            <|>
  ParensSqlExpr     <$> (parens sqlExpr)                              <|>  
  ListSqlExpr       <$> parens (sepByComma sqlExpr)                   <|>
  -- aggrFunSqlExpr                                                      <|>  
  FunSqlExpr        <$> funName <*> parens (sepByComma sqlExpr)       <|>
  ColumnSqlExpr     <$> column                                        <|>
  ConstSqlExpr      <$> literal  
  -- ParamSqlExpr
  -- ExistsSqlExpr
  -- 

caseExpr :: Parser SqlExpr
caseExpr = do
  _ <- symbol "CASE"
  cbs <- some $ do
    _ <- symbol "WHEN"
    c <- sqlExpr
    _ <- symbol "THEN"
    b <- sqlExpr
    return (c, b)
  me <- option Nothing (Just <$> (symbol "ELSE" *> sqlExpr))
  _ <- symbol "END"
  pure (CaseSqlExpr (NEL.fromList cbs) me)

funName :: Parser String
funName = identifier

{-
aggrFunSqlExpr :: Parser SqlExpr
aggrFunSqlExpr = do
  n <- funName
  (es, obys) <- parens $ do
    es <- sepByComma sqlExpr
    obys <- orderBy
    pure (es, obys)
  pure (AggrFunSqlExpr n es obys)
-}

arraySqlExpr :: Parser SqlExpr
arraySqlExpr = do
  _ <- symbol "ARRAY"
  es <- brackets (sepByComma sqlExpr)
  pure (ArraySqlExpr es)

castExpr :: Parser SqlExpr
castExpr = do
  _ <- symbol "CAST"
  (e, typ) <- parens $ do
    e <- sqlExpr
    _ <- symbol "AS"
    typ <- typeExpr
    pure (e, typ)
  pure (CastSqlExpr typ e)

-- TODO: Assuming columns are of "foo"."bar<op>baz..."
column :: Parser SqlColumn
column = do
  h <- (doubleQuoted identifier <|> identifier)
  hs <- (singleton <$> (char '.' *> (doubleQuoted identifier <|> identifier))) <|> (pure [])
  pure (SqlColumn (T.pack h : map T.pack hs))

prefixOp :: Parser UnOp
prefixOp =
  symbol "NOT"    $> OpNot       <|>
  symbol "LENGTH" $> OpLength    <|>
  symbol "@"      $> OpAbs       <|>
  symbol "-"      $> OpNegate    <|>
  symbol "LOWER"  $> OpLower     <|>
  symbol "UPPER"  $> OpUpper
  -- NOTE: missing custom prefixes

postfixOp :: Parser UnOp
postfixOp =
  symbol "IS" *> symbol "NULL"                 $> OpIsNull    <|>
  symbol "IS" *> symbol "NOT" *> symbol "NULL" $> OpIsNotNull
  -- NOTE: missing custom postfixes

binOp :: Parser BinOp
binOp =  
  symbol "+"    $> OpPlus   <|>
  symbol "="    $> OpEq     <|>  
  symbol "-"    $> OpMinus  <|>
  symbol ">="   $> OpGtEq   <|>
  symbol "<="   $> OpLtEq   <|>  
  symbol ">"    $> OpGt     <|>
  symbol "<"    $> OpLt     <|>
  symbol "||"   $> OpCat    <|>
  symbol "AND"  $> OpAnd    <|>
  symbol "OR"   $> OpOr     <|>
  symbol "LIKE" $> OpLike   <|>
  symbol "IN"   $> OpIn     <|>      
  symbol "/"    $> OpDiv    <|>    
  symbol "*"    $> OpMul    <|>  
  symbol "~"    $> OpBitNot <|>    
  symbol "&"    $> OpBitAnd <|>
  symbol "^"    $> OpBitXor <|>
  -- symbol "::"   <|>  
  symbol "AT" *> symbol "TIME" *> symbol "ZONE" $> OpAtTimeZone

{-
orderBy :: Parser [(SqlExpr, SqlOrder)]
orderBy = option [] (symbol "ORDER" *> symbol "BY" *> sepByComma ord)

ord :: Parser (SqlExpr, SqlOrder)
ord = do
  (,) <$> termSqlExpr <*> sqlOrder
-}

placeholderExpr :: Parser SqlExpr
placeholderExpr =
  symbol "?" *> pure PlaceHolderSqlExpr

{-
sqlOrder :: Parser SqlOrder
sqlOrder = do
  dir <- (symbol "ASC"  *> pure SqlAsc <|>
          symbol "DESC" *> pure SqlDesc
        )
  nullOrd <- (symbol "NULLS" *> symbol "FIRST" *> pure SqlNullsFirst <|>
              symbol "NULLS" *> symbol "LAST"  *> pure SqlNullsLast
              )
  pure (SqlOrder dir nullOrd)
-}

typeExpr :: Parser DBType
typeExpr = undefined

{-
ordDir :: Parser SqlOrder
ordDir = undefined

nullOrd :: Parser SqlOrder
nullOrd = undefined

limit :: Parser SqlExpr
limit = undefined
-}

defaultExpr :: Parser SqlExpr
defaultExpr = symbol "DEFAULT" *> pure DefaultSqlExpr

lexeme :: Parser a -> Parser a
lexeme p = skipSpace *> p

symbol :: Text -> Parser Text
symbol = lexeme . string
         
identifier :: Parser String
identifier = lexeme (many1 (letter <|> char '_'))

word :: Parser String
word = lexeme (many1 letter)

number :: Parser String
number = lexeme (many1 digit)

singleton :: a -> [a]
singleton a = [a]

openParen :: Parser ()
openParen = void (lexeme (char '('))

closeParen :: Parser ()
closeParen = void (lexeme (char ')'))

openBracket :: Parser ()
openBracket = void (lexeme (char '['))

closeBracket :: Parser ()
closeBracket = void (lexeme (char ']'))

doubleQuote :: Parser ()
doubleQuote = void (lexeme (char '"'))

singleQuote :: Parser ()
singleQuote = void (lexeme (char '\''))

between :: Parser b -> Parser c -> Parser a -> Parser a
between open close p = do
  open *> (p <* close)

doubleQuoted :: Parser a -> Parser a
doubleQuoted = between doubleQuote doubleQuote

quoted :: Parser a -> Parser a
quoted = between singleQuote singleQuote

parens :: Parser a -> Parser a
parens = between openParen closeParen

brackets :: Parser a -> Parser a
brackets = between openBracket closeBracket

void :: Parser a -> Parser ()
void a = a *> pure ()

sepByComma :: Parser a -> Parser [a]
sepByComma p  = p `sepBy` (lexeme (char ','))

literal :: Parser LitSql
literal =
  symbol "NULL"    *> pure NullSql    <|>
  symbol "DEFAULT" *> pure DefaultSql <|>
  boolLit                             <|>
  stringLit                           <|>
  oidLit                              <|>
  integerLit                          -- <|>
  -- doubleLit 


    where boolLit = (
            symbol "TRUE"            <|>
            quoted (symbol "true")
            ) *> pure (BoolSql True) <|>
            (
            symbol "FALSE"           <|>
            quoted (symbol "false")
            ) *> pure (BoolSql False)
          integerLit = (IntegerSql . read . concat) <$> many1 number
          stringLit  = (StringSql . T.pack) <$> quoted word
          oidLit     = (StringSql . T.pack) <$> quoted (doubleQuoted identifier)
          
data SizeInfo = SizeInfo { szCharacterLength :: Maybe Integer
                         , szNumericPrecision :: Maybe Integer
                         , szNumericScale :: Maybe Integer
                         , szDateTimePrecision :: Maybe Integer
                         , szIntervalPrecision :: Maybe Integer
                         } deriving (Show, Eq)

defSizeInfo :: SizeInfo
defSizeInfo = SizeInfo Nothing Nothing Nothing Nothing Nothing
                                    
parsePGType :: String -> Bool -> SizeInfo -> String -> DBType
parsePGType scn nullInfo sz = wrapNullable nullInfo . go
  where go "smallint"                  = DBInt2
        go "integer"                   = DBInt4
        go "bigint"                    = DBInt8
        go "boolean"                   = DBBool
        go "double precision"          = case szNumericPrecision sz of
                                           Nothing -> DBFloat 53
                                           Just v  -> DBFloat v
        go "real"                      = case szNumericPrecision sz of
                                           Nothing -> DBFloat 24
                                           Just v  -> DBFloat v
        go "numeric"                   = case (,) <$> szNumericPrecision sz <*> szNumericScale sz of
                                           Just (pr, sc) -> DBNumeric pr sc
                                           _             -> error "Panic: numeric must specify precision and scale"
        go "character"                 = case szCharacterLength sz of
                                           Just v -> DBChar v
                                           _      -> error "Panic: character must specify a size"
        go "character varying"         = case szCharacterLength sz of
                                           Just v -> DBVarchar (Right v)
                                           _      -> error "Panic: varying character must specify a size"                                           
        go "text"                      = DBText
        go "bytea"                     = DBVarbinary (Left Max)
        go "bit"                       = case szCharacterLength sz of
                                           Just v  -> DBBit v
                                           Nothing -> error "Panic: bit must specify a size"      
        go "bit varying"               = case szCharacterLength sz of
                                           Just v -> DBVarbit v
                                           Nothing -> error "Panic: bit varying must specify a size"         
        
        go "timestamp with time zone"  = case szDateTimePrecision sz of
                                           Just v  -> DBTimestamptz v
                                           Nothing -> DBTimestamptz 6
        go "timestamp"                 = case szDateTimePrecision sz of
                                           Just v  -> DBTimestamp v
                                           Nothing -> DBTimestamp 6
        go "timestamp without time zone"
                                       = case szDateTimePrecision sz of
                                           Just v  -> DBTimestamp v
                                           Nothing -> DBTimestamp 6
                                           
        go "time with time zone"       = case szDateTimePrecision sz of
                                           Just v  -> DBTimetz v
                                           Nothing -> DBTimetz 6
        go "time"                      = case szDateTimePrecision sz of
                                           Just v  -> DBTime v
                                           Nothing -> DBTime 6
        go "time without time zone"    = case szDateTimePrecision sz of
                                           Just v  -> DBTime v
                                           Nothing -> DBTime 6                                           
                                           
        go "interval"                  = case szIntervalPrecision sz of
                                           Just v -> DBInterval Nothing v
                                           Nothing -> DBInterval Nothing 6
        go "citext"                    = DBCiText
        go "date"                      = DBDate
        go "uuid"                      = DBUuid
        go "xml"                       = DBXml
        go "jsonb"                     = DBJsonB
        go "json"                      = DBJson
        go v | isArray v               = DBArray (parseArray v)
             | otherwise               = DBCustomType (T.pack scn) (DBTypeName (T.pack v) [])


        isArray v = case splitAt 2 v of
          ("[]", _) -> True
          _         -> False

        parseArray = go . drop 3 

        wrapNullable True a  = DBNullable a
        wrapNullable False a = a
