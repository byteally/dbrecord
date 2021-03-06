{-# LANGUAGE OverloadedStrings #-}
module DBRecord.MySQL.Internal.Sql.Parser where

{-
  ( sqlExpr
  , parseMySQLType
  , SizeInfo (..)
  , defSizeInfo
  ) where

import DBRecord.Internal.Sql.DML
import qualified Data.Text as T
import Control.Applicative
import Data.Attoparsec.Text hiding (number)
import Data.Text (Text)
import Data.Char (isAlpha, isDigit)
import qualified Debug.Trace as DT
import qualified Data.List.NonEmpty as NEL
import DBRecord.Internal.DBTypes (DBType (..))

sqlExpr :: Parser SqlExpr
sqlExpr = undefined
  (
   postfixOrWindowExpr <|>
   binSqlExpr          <|>
   -- compositeExpr  <|>
   prefixSqlExpr       <|>
   termSqlExpr)        -- <*
   -- endOfInput

    where binSqlExpr = do
            e1 <- termSqlExpr
            b  <- binOp
            case b of
              "::" -> do
                ty <- typeExpr
                pure (CastSqlExpr ty e1)
              _ -> do
                e2 <- sqlExpr
                pure (BinSqlExpr b e1 e2)
            
          postfixOrWindowExpr = do
            e <-  binSqlExpr <|> prefixSqlExpr <|> termSqlExpr
            op <- eitherP (symbol "OVER" *> word) postfixOp
            case op of
              Left w -> pure (WindowSqlExpr w e)
              Right op -> pure (PostfixSqlExpr op e)

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

aggrFunSqlExpr :: Parser SqlExpr
aggrFunSqlExpr = do
  n <- funName
  (es, obys) <- parens $ do
    es <- sepByComma sqlExpr
    obys <- orderBy
    pure (es, obys)
  pure (AggrFunSqlExpr n es obys)
            
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

-- TODO: Identify prefix operators  
prefixOp :: Parser String
prefixOp = symbol "NOT" *> pure "NOT"

-- TODO: Identify postfix operators  
postfixOp :: Parser String
postfixOp =
  symbol "IS" *> symbol "NULL" *> pure "IS NULL"                      <|>
  symbol "IS" *> symbol "NOT"  *> symbol "NULL" *> pure "IS NOT NULL"
  
-- TODO: Identity binary operators
binOp :: Parser String
binOp = fmap T.unpack $ 
  symbol "+"    <|>
  symbol "="    <|>  
  symbol "-"    <|>
  symbol ">="   <|>
  symbol "<="   <|>  
  symbol ">"    <|>
  symbol "<"    <|>
  symbol "||"   <|>
  symbol "AND"  <|>
  symbol "OR"   <|>
  symbol "LIKE" <|>
  symbol "IN"   <|>      
  symbol "/"    <|>    
  symbol "*"    <|>  
  symbol "~"    <|>    
  symbol "&"    <|>
  symbol "^"    <|>
  symbol "::"   <|>  
  symbol "AT"   

orderBy :: Parser [(SqlExpr, SqlOrder)]
orderBy = option [] (symbol "ORDER" *> symbol "BY" *> sepByComma ord)

ord :: Parser (SqlExpr, SqlOrder)
ord = do
  (,) <$> termSqlExpr <*> sqlOrder

placeholderExpr :: Parser SqlExpr
placeholderExpr =
  symbol "?" *> pure PlaceHolderSqlExpr

sqlOrder :: Parser SqlOrder
sqlOrder = do
  dir <- (symbol "ASC"  *> pure SqlAsc <|>
          symbol "DESC" *> pure SqlDesc
        )
  nullOrd <- (symbol "NULLS" *> symbol "FIRST" *> pure SqlNullsFirst <|>
              symbol "NULLS" *> symbol "LAST"  *> pure SqlNullsLast
              )
  pure (SqlOrder dir nullOrd)

typeExpr :: Parser DBType
typeExpr = undefined

ordDir :: Parser SqlOrder
ordDir = undefined

nullOrd :: Parser SqlOrder
nullOrd = undefined

limit :: Parser SqlExpr
limit = undefined

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
                         

parseMySQLType :: Bool -> SizeInfo -> String -> DBType
parseMySQLType nullInfo sizeInfo = wrapNullable nullInfo . go
  where go "smallint" =   DBInt2                  
        go "int"      =   DBInt4                  
        go "bigint"   =   DBInt8                  
        go "numeric"  =  case (,) <$> szNumericPrecision sizeInfo <*> szNumericScale sizeInfo of
                            Just (pr, sc) -> DBNumeric pr sc
                            _             -> error "Panic: numeric must specify precision and scale"       
        go "decimal"  =  case (,) <$> szNumericPrecision sizeInfo <*> szNumericScale sizeInfo of
                            Just (pr, sc) -> DBNumeric pr sc
                            _             -> error "Panic: decimal must specify precision and scale"             
        go "float"    =  case szNumericPrecision sizeInfo of
                            Nothing -> DBFloat 53
                            Just v  -> DBFloat v
        go "char"     =  case szCharacterLength sizeInfo of
                            Nothing -> DBChar 53
                            Just v  -> DBChar v              
        go "varchar"  =  case szCharacterLength sizeInfo of
                            Just v  -> DBVarchar (Right v)              
                            _      -> error "Panic: varying character must specify a size"                                           
        go "binary"   =  case szCharacterLength sizeInfo of
                            Just v  -> DBBinary v              
                            _      -> error "Panic: binary character must specify a size"                                                      
        go "varbinary"=  case szCharacterLength sizeInfo of
                            Just v  -> (DBVarbinary (Right v))             
                            _      -> error "Panic: varying binary character must specify a size"                                           
           
        go "datetime" =  case szDateTimePrecision sizeInfo of
                            Just v  -> DBTimestamp v
                            Nothing -> DBTimestamp 6       
        go "date"     =  DBDate                  
        go "time"     =  case szDateTimePrecision sizeInfo of
                            Just v  -> DBTime v
                            Nothing -> DBTime 6           
        go "bit"      =  case szCharacterLength sizeInfo of
                            Just v -> DBBit v
                            Nothing -> error "Panic: bit must specify a size" 
        
        -- go (DBTypeName t args)      = T.unpack (doubleQuote t) ++ ppArgs args
        -- go (DBCustomType t _)       = go t
        go x                        = error $  "Panic: not implemented @ppMysqlType" ++ (show x)

        -- ppArgs []  = ""
        -- ppArgs xs  = "(" ++ L.intercalate "," (map ppArg xs) ++ ")"

        -- ppArg (TextArg t)    = T.unpack t
        -- ppArg (IntegerArg i) = show i
        wrapNullable True a  = DBNullable a
        wrapNullable False a = a
-}
