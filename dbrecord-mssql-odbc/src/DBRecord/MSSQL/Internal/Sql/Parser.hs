{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module DBRecord.MSSQL.Internal.Sql.Parser
  ( sqlExpr
  , parseMSSQLType
  , SizeInfo (..)
  , defSizeInfo
  ) where

import DBRecord.Internal.Sql.DML
import Data.Attoparsec.Text hiding (number)
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A
import Data.Text (Text)
import Control.Applicative
import Data.Char (isAlpha, isDigit)
import qualified Debug.Trace as DT
import qualified Data.List.NonEmpty as NEL
import DBRecord.Internal.DBTypes (DBType (..))
import Data.Functor (($>))
import qualified Data.List as DL

import qualified DBRecord.Internal.Types as Type

sqlExpr :: Parser SqlExpr
sqlExpr =
  (
   postfixOrWindowExpr <|>
   binSqlExpr          <|>
   -- compositeExpr  <|>
   prefixSqlExpr       <|>
   termSqlExpr
   )
   -- <*
   -- endOfInput

    where binSqlExpr = do
            e1 <- termSqlExpr
            b  <- Left <$> binOp <|> Right <$> (symbol "::" $> ())
            case b of
              Right _ -> do
                ty <- typeExpr
                pure (CastSqlExpr ty e1)
              Left binOpRes -> do
                e2 <- sqlExpr
                pure $ {-transformBinExprByPrecedence-} (BinSqlExpr binOpRes e1 e2)
            
          postfixOrWindowExpr = do
            e <- binSqlExpr <|> prefixSqlExpr <|> termSqlExpr
            epRes <- eitherP (symbol "OVER" *> anonWindowExpr e
                            ) postfixOp
            case epRes of
              Left w   -> pure w
              Right op -> pure (PostfixSqlExpr op e)

          anonWindowExpr e = DT.traceShow (show e) $ parens $ do
            pbys <- optional ( symbol "PARTITION" *> symbol "BY" *>
                              sepBy1 sqlExpr comma
                            )
            ords <- orderBy
            -- NOTE: ROW(s) are also possible here
            pure (AnonWindowSqlExpr (maybe [] id pbys)
                                    ords
                  e)
                          
          prefixSqlExpr = do
            op <- prefixOp
            e <-  sqlExpr
            pure (PrefixSqlExpr op e)

termSqlExpr :: Parser SqlExpr
termSqlExpr =
  -- arraySqlExpr                                                        <|>
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
funName = do
  funNameQual <- identifier <|> brackets identifier
  funName <- (singleton <$> (char '.' *> (brackets identifier <|> identifier))) <|> pure []
  -- TODO: FunSqlExpr needs to be changed or we need to intercalate a separator.
  pure (concat $ funNameQual : funName)


aggrFunSqlExpr :: Parser SqlExpr
aggrFunSqlExpr = do
  n <- funName
  (es, obys) <- parens $ do
    es <- sepByComma sqlExpr
    -- obys <- orderBy
    pure (es, []) -- obys)
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

-- Note :For MSSQL, Columns can also be of the form [foo ], "foo", or just foo.
column :: Parser SqlColumn
column = do
  pieces <- sepBy1 columnPiece (char '.')
  pure (SqlColumn (map T.pack pieces))

  where columnPiece = doubleQuoted identifier <|>
                      brackets     identifier <|>
                      identifier

-- -- TODO: Identify prefix operators  
-- prefixOp :: Parser String
-- prefixOp = symbol "NOT" *> pure "NOT"

prefixOp :: Parser UnOp
prefixOp =
  symbol "NOT"    $> OpNot       <|>
  symbol "LENGTH" $> OpLength    <|>
  symbol "-"      $> OpNegate    <|>
  symbol "LOWER"  $> OpLower     <|>
  symbol "UPPER"  $> OpUpper     <|>
  -- NOTE: missing custom prefixes
  -- Added to support MSSQL + prefix Op
  symbol "+"      $> OpPositive  <|>
  symbol "~"      $> OpBitwiseNot



postfixOp :: Parser UnOp
postfixOp =
  symbol "IS" *> symbol "NULL"                 $> OpIsNull    <|>  
  symbol "IS" *> symbol "NOT" *> symbol "NULL" $> OpIsNotNull
  -- NOTE: missing custom postfixes
  

binOp :: Parser BinOp
binOp =  
  symbol "+"    $> OpPlus   <|>
  symbol "="    $> OpEq     <|>
  symbol "<>"   $> OpNotEq  <|>  
  symbol "!="   $> OpNotEq  <|>  
  symbol "-"    $> OpMinus  <|>
  symbol ">="   $> OpGtEq   <|>
  symbol "<="   $> OpLtEq   <|>  
  symbol ">"    $> OpGt     <|>
  symbol "<"    $> OpLt     <|>
  symbol "!<"   $> OpNotLt  <|>
  symbol "!>"   $> OpNotGt  <|>
  symbol "||"   $> OpCat    <|>
  symbol "AND"  $> OpAnd    <|>
  symbol "OR"   $> OpOr     <|>
  symbol "LIKE" $> OpLike   <|>
  symbol "IN"   $> OpIn     <|>
  symbol "ALL"     $> OpAll      <|>
  symbol "ANY"     $> OpAny      <|>
  symbol "EXISTS"  $> OpExists   <|>
  symbol "SOME"    $> OpSome     <|>
  symbol "BETWEEN" $> OpBetween  <|>

  symbol "/"    $> OpDiv    <|>
  symbol "%"    $> OpMod    <|>
  symbol "*"    $> OpMul    <|>  
  symbol "~"    $> OpBitNot <|>    
  symbol "&"    $> OpBitAnd <|>
  symbol "|"    $> OpBitOr  <|>
  symbol "^"    $> OpBitXor <|>
  -- symbol "::"   <|>  
  symbol "AT" *> symbol "TIME" *> symbol "ZONE" $> OpAtTimeZone

  
orderBy :: Parser [(SqlExpr, {-Maybe-} SqlOrder)]
orderBy = option [] (symbol "KORDER" *> symbol "BY" *> sepBy1 ord comma)

ord :: Parser (SqlExpr, {-Maybe-} SqlOrder)
ord = do
  (,) <$> sqlExpr <*> sqlOrder

placeholderExpr :: Parser SqlExpr
placeholderExpr =
  symbol "?" *> pure PlaceHolderSqlExpr

sqlOrder :: Parser SqlOrder
sqlOrder = do
  dir <- optional (symbol "ASC"  *> pure SqlAsc <|>
                  symbol "DESC" *> pure SqlDesc
                 )
  nullOrd <- optional (symbol "NULLS" *> symbol "FIRST" *> pure SqlNullsFirst <|>
                      symbol "NULLS" *> symbol "LAST"  *> pure SqlNullsLast
                     )
  pure (SqlOrder (maybe SqlAsc id dir) (maybe SqlNullsLast id nullOrd))

typeExpr :: Parser DBType
typeExpr =
  DBInt2 <$ symbol "SMALLINT" <|>
  DBInt4 <$ symbol "INT"      <|>
  DBInt8 <$ symbol "BIGINT"   <|>
  DBText <$ symbol "NTEXT"    <|>
  DBDate <$ symbol "DATE"     <|>  

  numericType                 <|>
  floatType                   <|>
  binaryType                  <|>
  varbinaryType               <|>
  ncharType                   <|>
  nvarcharType                <|>
  timestamptzType             <|>
  timestampType               <|>
  timeType                    <|>
  bitType                     <|>
  customType                  

  
  where numericType =
          symbol "NUMERIC" *>
          ( mkNumericType   <$>
           ( do
               mv <- optional (openParen *> number)
               case mv of
                 Just v -> 
                   Just . (v, ) <$> optional (comma *> number) <* closeParen
                 Nothing -> pure Nothing
            
           )
          )
          
        mkNumericType Nothing              = DBNumeric 0 0
        mkNumericType (Just (v , Nothing)) = DBNumeric (read v) 0
        mkNumericType (Just (v, Just v'))  = DBNumeric (read v) (read v')

        typeWithOptParam s d f p =
          symbol s *>
          fmap (maybe (f d) f) (optional (parens p))

        typeWithOptParamNum s f =
          typeWithOptParam s 0 f (read <$> number)

        floatType  = typeWithOptParamNum "FLOAT" DBFloat
        binaryType = typeWithOptParamNum "BINARY" DBBinary
        timestamptzType =
          typeWithOptParamNum "DATETIMEOFFSET" DBTimestamptz
        timestampType =
          typeWithOptParamNum "DATETIME2" DBTimestamp
        timeType =
          typeWithOptParamNum "TIME" DBTime
        bitType =
          typeWithOptParamNum "BIT" DBBit
        ncharType =
          typeWithOptParamNum "CHAR" DBChar
        nvarcharType = typeWithOptParam "NVARCHAR"
                       (Right 0)
                       DBVarchar
                       ( Left Type.Max <$  symbol "MAX" <|>
                         Right . read  <$> number
                       )
        varbinaryType = typeWithOptParam "VARBINARY"
                       (Right 0)
                       DBVarbinary
                       ( Left Type.Max <$  symbol "MAX" <|>
                         Right . read  <$> number
                       )
        customType = undefined
        
        mkFloatType Nothing  = DBFloat 0
        mkFloatType (Just v) = DBFloat (read v)

        
          

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
sepByComma p  = p `sepBy` comma

comma :: Parser Char
comma = lexeme (char ',')

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
          stringLit  = (StringSql . T.pack) <$> ( skipSpace *>
                                                  char '\'' *> 
                                                  manyTill anyChar (char '\'')
                                                )
          oidLit     = (StringSql . T.pack) <$> quoted (doubleQuoted identifier)
          

data SizeInfo = SizeInfo { szCharacterLength :: Maybe Integer
                         , szNumericPrecision :: Maybe Integer
                         , szNumericScale :: Maybe Integer
                         , szDateTimePrecision :: Maybe Integer
                         , szIntervalPrecision :: Maybe Integer
                         } deriving (Show, Eq)

defSizeInfo :: SizeInfo
defSizeInfo = SizeInfo Nothing Nothing Nothing Nothing Nothing

parseMSSQLType :: Bool -> SizeInfo -> String -> DBType
parseMSSQLType nullInfo sz = wrapNullable nullInfo . go
 where  go "smallint"                  = DBInt2
        go "int"                       = DBInt4
        go "bigint"                    = DBInt8
        go "numeric"                   = case (,) <$> szNumericPrecision sz <*> szNumericScale sz of
                                           Just (pr, sc) -> DBNumeric pr sc
                                           _             -> error "Panic: numeric must specify precision and scale"
        go "float"                     = case szNumericPrecision sz of
                                           Nothing -> DBFloat 24
                                           Just v  -> DBFloat v
        go "nchar"                     = case szCharacterLength sz of
                                           Just v -> DBChar v
                                           _      -> error "Panic: character must specify a size"                                           
        go "nvarchar (max)"            = DBVarchar (Left Type.Max) 
        go "nvarchar"                  = case szCharacterLength sz of
                                           Just v -> DBVarchar (Right v)
                                           _      -> error "Panic: varying character must specify a size" 
        go "ntext"                     = DBText
        go "binary"                    = case szCharacterLength sz of -- TODO: Verify this check
                                           Just v -> DBBinary v
                                           _      -> error "Panic: Binary type must specify a size or length" 
        go "varbinary (max)"           = DBVarbinary (Left Type.Max)
        go "varbinary"                 = DBVarbinary (Left Type.Max) -- TODO : Check for sz part?
        go "datetimeoffset"            = case szDateTimePrecision sz of
                                           Just v  -> DBTimestamptz v
                                           Nothing -> DBTimestamptz 6
        go "datetime2"                 = case szDateTimePrecision sz of
                                           Just v  -> DBTimestamp v
                                           Nothing -> DBTimestamp 6
        go "date"                      = DBDate
        go "time"                      = case szDateTimePrecision sz of
                                           Just v  -> DBTime v
                                           Nothing -> DBTime 6
        go "bit"                       = case szCharacterLength sz of
                                          Just v -> DBBit v
                                          _      -> error "Panic: character must specify a size"

       -- Custom Type 
        go t                           = DBCustomType (DBTypeName (T.pack t) []) False

        -- Add Nullable info
        wrapNullable True a  = DBNullable a
        wrapNullable False a = a                                          


data ExpWrap = Expr SqlExpr
             | Op   BinOp
  deriving (Show, Eq)

data Assoc = LeftAssoc | RightAssoc 
  deriving Show
type Prec = Int

transformBinExprByPrecedence :: SqlExpr -> SqlExpr
transformBinExprByPrecedence initialExpr = 
  case initialExpr of
    BinSqlExpr fstBinOp op1 op2 ->
      (go . flatten) initialExpr
    e -> e

 where 
  flatten :: SqlExpr -> [ExpWrap]
  flatten (BinSqlExpr binOp exp1 exp2)   = flatten exp1 ++ [ Op binOp ] ++ flatten exp2
  flatten x = [Expr x]

  go :: {- Map BinOp (Assoc, Prec) -> -} [ExpWrap] -> SqlExpr -- [ExpWrap]
  go xs =
    let ops = map (\(Op c) -> c) $
              filter (\x -> case x of
                         Op {} -> True
                         _     -> False) xs
    in case DL.reverse $ DL.sort ops of -- Get the current lowest precedence operator
         [] -> 
          case xs of
            Expr sqlExp:[] -> sqlExp
            Op _:[] -> error "Panic! Did not expect an Op here, since filter function was empty!"
            _ -> error $ "Encountered a list of ExpWrap (we need to handle this case)" ++ (show xs)
         firstOp:_ -> 
          let ixs = DL.elemIndices (Op firstOp) xs
          in case ixs of
               [ix] -> BinSqlExpr firstOp (go $ DL.take (ix +1) xs) (go $ DL.drop (ix + 2) xs)
               multipleIx -> undefined




