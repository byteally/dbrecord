{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Database.PostgreSQL.Simple.FromField.Composite
  ( module Database.PostgreSQL.Simple.FromField.Composite
  ) where

import           Control.Applicative
import           Control.Exception (Exception)
import           Control.Monad.State.Strict
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as Char8
import           Data.Int
import           Data.Kind
import qualified Data.List as L
import           Data.Ratio
import           Data.Text (Text)
import           Data.Text.Encoding ( decodeUtf8' )
import           Data.Typeable
import qualified Data.UUID as UUID
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.Types (Null (..))
import           GHC.Real (infinity, notANumber)
import qualified Data.Aeson as A

class FromComposite (t :: Type) where
  fromComposite :: CompositeParser t

instance (FromCompositeField a, FromCompositeField b) => FromComposite (a,b) where
  fromComposite =
    (,) <$> compositeField <*> compositeField

instance (FromCompositeField a, FromCompositeField b, FromCompositeField c) => FromComposite (a,b,c) where
  fromComposite =
    (,,) <$> compositeField <*> compositeField <*> compositeField

instance (FromCompositeField a, FromCompositeField b, FromCompositeField c, FromCompositeField d) => FromComposite (a,b,c,d) where
  fromComposite =
    (,,,) <$> compositeField <*> compositeField <*> compositeField <*> compositeField

instance (FromCompositeField a, FromCompositeField b, FromCompositeField c, FromCompositeField d, FromCompositeField e) => FromComposite (a,b,c,d,e) where
  fromComposite =
    (,,,,) <$> compositeField <*> compositeField <*> compositeField <*> compositeField <*> compositeField

instance (FromCompositeField a, FromCompositeField b, FromCompositeField c, FromCompositeField d, FromCompositeField e, FromCompositeField f) => FromComposite (a,b,c,d,e,f) where
  fromComposite =
    (,,,,,) <$> compositeField <*> compositeField <*> compositeField <*> compositeField <*> compositeField <*> compositeField

class FromCompositeField (t :: Type) where
  fromCompositeField :: CompositeFieldParser t

newtype CompositeParser t = CompositeParser {runCompParser :: StateT CPState Conversion t}
  deriving newtype (Functor, Applicative, Monad, Alternative)

data CPState = CPState
  { cField' :: !(Either Field CompositeField)
  , parsedFields :: !(Vector (Maybe ByteString))
  , currentField :: !Int
  }

type CompositeFieldParser (t :: Type) = CompositeField -> Maybe ByteString -> Conversion t

getAllParsedFields :: CompositeParser (Vector (Maybe ByteString))
getAllParsedFields = CompositeParser (gets parsedFields)

lookupCompositeField :: Int -> CompositeParser (Maybe (Maybe ByteString))
lookupCompositeField ix = CompositeParser (gets ((V.!? ix) . parsedFields))

getCompositeFieldInfo :: CompositeParser CompositeField
getCompositeFieldInfo = CompositeParser $ do
  cpst <- get
  pure $ CompositeField { cField = cField' cpst
                        , cPos = currentField cpst
                        }

consumeCurrentField :: CompositeParser (Int, Maybe ByteString)
consumeCurrentField = do
  f <- getCompositeFieldInfo
  (curr, fldE) <- CompositeParser $ do
    curr <- gets currentField
    pFlds <- gets parsedFields
    modify' $ \s -> s {currentField = curr + 1}
    pure (curr, maybe (Left $ "Trying tolookup fields more than available no. of fields: " ++ (show $ V.length pFlds)) Right (pFlds V.!? curr))
  case fldE of
    Left e -> CompositeParser $ lift $ returnCompositeError ConversionFailed f e
    Right fld -> pure (curr, fld)

compositeToField :: (FromComposite t, Typeable t) => FieldParser t
compositeToField = compositeToFieldWith fromComposite

compositeToFieldWith :: Typeable t => CompositeParser t -> FieldParser t
compositeToFieldWith compP f = \case
  Nothing -> returnError UnexpectedNull f ""
  Just bs -> case A.parseOnly (parseCompositeFields <* A.endOfInput) bs of
    Left err -> returnError ConversionFailed f (show (err, Char8.unpack bs))
    Right flds -> evalStateT (runCompParser compP) (CPState (Left f) flds 0)

compositeToCompositeField :: (FromComposite t, Typeable t) => CompositeFieldParser t
compositeToCompositeField = compositeToCompositeFieldWith fromComposite

compositeToCompositeFieldWith :: Typeable t => CompositeParser t -> CompositeFieldParser t
compositeToCompositeFieldWith compP f = \case
  Nothing -> returnCompositeError UnexpectedNull f ""
  Just bs -> case A.parseOnly (parseCompositeFields <* A.endOfInput) bs of
    Left err -> returnCompositeError ConversionFailed f err
    Right flds -> evalStateT (runCompParser compP) (CPState (Right f) flds 0)

compositeField :: forall t.FromCompositeField t => CompositeParser t
compositeField = compositeFieldWith (fromCompositeField @t)

compositeFieldWith :: CompositeFieldParser t -> CompositeParser t
compositeFieldWith fp = do
  cfld <- getCompositeFieldInfo
  (_currIx, fldBS) <- consumeCurrentField
  CompositeParser $ lift $ fp cfld fldBS

optionalCompositeFieldParser :: CompositeFieldParser t -> CompositeFieldParser (Maybe t)
optionalCompositeFieldParser fp f = \case
  Nothing -> pure Nothing
  bs' -> Just <$> fp f bs'

data CompositeField = CompositeField { cField :: !(Either Field CompositeField)
                                     , cPos :: !Int
                                     }

arrayCompositeFieldParser :: forall a .(Typeable a) => CompositeFieldParser a -> CompositeFieldParser (Vector a)
arrayCompositeFieldParser cfp f bs = do
  bss <- attoCompositeFieldParser (braces $ commaSep byteContentArr) f bs
  V.fromList <$> (mapM (cfp f) bss)
{-# INLINE arrayCompositeFieldParser #-}

-- ^ Parsers

parseCompositeFields :: A.Parser (Vector (Maybe ByteString))
parseCompositeFields = V.fromList <$> (parens $ commaSep byteContent)

array :: A.Parser a -> A.Parser [a]
array p = do
  _ <- A.char '{'
  vs <- many p
  _ <- A.char '}'
  pure vs

parens :: A.Parser a -> A.Parser a
parens = between (A.char '(') (A.char ')')
{-# INLINE parens  #-}

braces :: A.Parser a -> A.Parser a
braces = between (A.char '{') (A.char '}')
{-# INLINE braces  #-}

quotes :: A.Parser a -> A.Parser a
quotes = between (A.char '"') (A.char '"')
{-# INLINE quotes  #-}

commaSep :: A.Parser a -> A.Parser [a]
commaSep p  = p `A.sepBy'` (A.char ',')
{-# INLINE commaSep #-}

between :: A.Parser open -> A.Parser close -> A.Parser a -> A.Parser a
between open close p = open *> p <* close
{-# INLINE between #-}


-- | Recognizes a quoted string.
quoted :: A.Parser ByteString
quoted = quotes (A.option "" contents)
  where
    escQ = A.char '"' *> A.char '"'
    esc = A.char '\\' *> (A.char '\\' <|> A.char '"')
    unQ = A.takeWhile1 (A.notInClass "\"\\")
    contents = mconcat <$> many (unQ
                                 <|> Char8.singleton <$> esc
                                 <|> Char8.singleton <$> escQ)

-- | Recognizes a plain string literal, not containing comma, quotes, or parens.
plain :: A.Parser ByteString
plain = A.takeWhile (A.notInClass ",\"()")

plainArr :: A.Parser ByteString
plainArr = A.takeWhile (A.notInClass ",\"{}")


byteContent :: A.Parser (Maybe ByteString)
byteContent = (Just <$> quoted) <|> (fmap (\bs -> if Char8.null bs then Nothing else Just bs) plain)

byteContentArr :: A.Parser (Maybe ByteString)
byteContentArr = (Just <$> quoted) <|> (fmap (\bs -> if Char8.null bs then Nothing else Just bs) plainArr)


returnCompositeError :: forall a err . (Typeable a, Exception err)
            => (String -> Maybe Oid -> String -> String -> String -> err)
            -> CompositeField -> String -> Conversion a
returnCompositeError mkErr f msg = do
  tyn <- either (fmap Char8.unpack . typename) (const $ pure "") $ cField f
  conversionError $ mkErr tyn (Nothing) "" (show (typeOf (undefined :: a))) msg

attoCompositeFieldParser :: forall a. (Typeable a)
     => A.Parser a
     -- ^ An attoparsec parser.
     -> CompositeFieldParser a
attoCompositeFieldParser p f = \case
  Nothing -> returnCompositeError UnexpectedNull f ""
  Just s -> case A.parseOnly (p <* A.endOfInput) s of
    Left err -> returnCompositeError ConversionFailed f (L.intercalate "|" [err, Char8.unpack s])
    Right  v -> pure v

instance FromCompositeField Int where
  fromCompositeField = attoCompositeFieldParser $ A.signed A.decimal

instance FromCompositeField Int8 where
  fromCompositeField = attoCompositeFieldParser $ A.signed A.decimal

instance FromCompositeField Int16 where
  fromCompositeField = attoCompositeFieldParser $ A.signed A.decimal

instance FromCompositeField Int32 where
  fromCompositeField = attoCompositeFieldParser $ A.signed A.decimal

instance FromCompositeField Int64 where
  fromCompositeField = attoCompositeFieldParser $ A.signed A.decimal

instance FromCompositeField Word where
  fromCompositeField = attoCompositeFieldParser $ A.signed A.decimal

instance FromCompositeField Word8 where
  fromCompositeField = attoCompositeFieldParser $ A.signed A.decimal

instance FromCompositeField Word16 where
  fromCompositeField = attoCompositeFieldParser $ A.signed A.decimal

instance FromCompositeField Word32 where
  fromCompositeField = attoCompositeFieldParser $ A.signed A.decimal

instance FromCompositeField Word64 where
  fromCompositeField = attoCompositeFieldParser $ A.signed A.decimal

instance FromCompositeField Float where
  fromCompositeField = attoCompositeFieldParser $ (realToFrac <$> pg_double)

instance FromCompositeField Double where
  fromCompositeField = attoCompositeFieldParser $ pg_double

instance FromCompositeField (Ratio Integer) where
  fromCompositeField = attoCompositeFieldParser $ pg_rational

instance FromCompositeField Text where
  fromCompositeField = nonNullCompositeField (either conversionError pure . decodeUtf8')

instance FromCompositeField Bool where
  fromCompositeField f = nonNullCompositeField (\case
    "t" -> pure True
    "f" -> pure False
    s -> returnCompositeError ConversionFailed f (Char8.unpack s)) f

instance FromCompositeField ByteString where
  fromCompositeField f = nonNullCompositeField pure f

instance FromCompositeField UUID.UUID where
  fromCompositeField f = nonNullCompositeField go f

    where
      go s =
        maybe (returnCompositeError ConversionFailed f (Char8.unpack s)) pure . UUID.fromASCIIBytes $ s

instance FromCompositeField A.Value where
  fromCompositeField f = nonNullCompositeField go f

    where
      go bs =
        case A.eitherDecodeStrict' bs of
          Left  err -> returnCompositeError ConversionFailed f err
          Right val -> pure val        

instance FromCompositeField Null where
  fromCompositeField f = \case
    Nothing -> pure Null
    Just _ -> returnCompositeError ConversionFailed f "data is not null"

instance FromCompositeField t => FromCompositeField (Maybe t) where
  fromCompositeField _ Nothing = pure Nothing
  fromCompositeField cf bs = Just <$> fromCompositeField @t cf bs

instance (FromCompositeField t, Typeable t) => FromCompositeField (V.Vector t) where
  fromCompositeField = arrayCompositeFieldParser (fromCompositeField @t)

instance (FromCompositeField t, Typeable t) => FromCompositeField [t] where
  fromCompositeField = (fmap . fmap . fmap) V.toList $ arrayCompositeFieldParser (fromCompositeField @t)

nonNullCompositeField :: forall a . (Typeable a)
  => (ByteString -> Conversion a) -> CompositeFieldParser a
nonNullCompositeField cvt _ (Just bs) = cvt bs
nonNullCompositeField _ f _ = returnCompositeError UnexpectedNull f ""
{-# INLINE nonNullCompositeField #-}

-- From postgresql-simple:Database.PostgreSQL.Simple.FromField
pg_double :: A.Parser Double
pg_double
    =   (A.string "NaN"       *> pure ( 0 / 0))
    <|> (A.string "Infinity"  *> pure ( 1 / 0))
    <|> (A.string "-Infinity" *> pure (-1 / 0))
    <|> A.double

pg_rational :: A.Parser Rational
pg_rational
    =   (A.string "NaN"       *> pure notANumber )
    <|> (A.string "Infinity"  *> pure infinity   )
    <|> (A.string "-Infinity" *> pure (-infinity))
    <|> A.rational
