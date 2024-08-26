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
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as Char8
import           Data.Text
import           Data.Text.Encoding ( decodeUtf8' )
import           Data.Kind
import           Data.Int
import           Data.Word
import           Data.Typeable
import           Data.Ratio
import           Control.Exception (Exception)
-- import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
-- import           Database.PostgreSQL.Simple.FromRow
-- import           Database.PostgreSQL.Simple.Internal
import           GHC.Real (infinity, notANumber)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad.State.Strict


{-
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Lazy as LT
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.UUID.Types   (UUID)
import qualified Data.UUID.Types as UUID
import           Data.Scientific (Scientific)
-}


class FromComposite (t :: Type) where
  fromComposite :: CompositeParser t

class FromCompositeField (t :: Type) where
  fromCompositeField :: CompositeFieldParser t

newtype CompositeParser t = CompositeParser {runCompParser :: StateT CPState Conversion t}
  deriving newtype (Functor, Applicative, Monad)

data CPState = CPState
  { cField' :: !Field
  , parsedFields :: !(Vector ByteString)
  , currentField :: !Int
  }

type CompositeFieldParser (t :: Type) = CompositeField -> Maybe ByteString -> Conversion t

getAllParsedFields :: CompositeParser (Vector ByteString)
getAllParsedFields = CompositeParser (gets parsedFields)

lookupCompositeField :: Int -> CompositeParser (Maybe ByteString)
lookupCompositeField ix = CompositeParser (gets ((V.!? ix) . parsedFields))

getCompositeFieldInfo :: CompositeParser CompositeField
getCompositeFieldInfo = CompositeParser $ do
  cpst <- get
  pure $ CompositeField { cField = cField' cpst
                        }

consumeCurrentField :: CompositeParser Int
consumeCurrentField = CompositeParser $ do
  curr <- gets currentField
  modify' $ \s -> s {currentField = curr + 1}
  pure curr

compositeToField :: (FromComposite t, Typeable t) => FieldParser t
compositeToField = compositeToFieldWith fromComposite

compositeToFieldWith :: Typeable t => CompositeParser t -> FieldParser t
compositeToFieldWith compP f = \case
  Nothing -> returnError UnexpectedNull f ""
  Just bs -> case A.parseOnly parseCompositeFields bs of
    Left err -> returnError ConversionFailed f err
    Right flds -> evalStateT (runCompParser compP) (CPState f flds 0)

compositeField :: forall t.FromCompositeField t => CompositeParser t
compositeField = compositeFieldWith (fromCompositeField @t)

compositeFieldWith :: CompositeFieldParser t -> CompositeParser t
compositeFieldWith fp = do
  cfld <- getCompositeFieldInfo
  currIx <- consumeCurrentField
  fldBS <- lookupCompositeField currIx
  CompositeParser $ lift $ fp cfld fldBS

{-
T <$> compositeField
  <*> compositeFieldWith $ nestedComposite @NCT
  <*> compositeField
-}


data CompositeField = CompositeField { cField :: !Field}

-- ^ Parsers

parseCompositeFields :: A.Parser (Vector ByteString)
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

commaSep :: A.Parser a -> A.Parser [a]
commaSep p  = p `A.sepBy'` (A.char ',')
{-# INLINE commaSep #-}

between :: A.Parser open -> A.Parser close -> A.Parser a -> A.Parser a
between open close p = open *> p <* close
{-# INLINE between #-}


optQuoted :: A.Parser a -> A.Parser a
optQuoted p =
  (q *> p <* q) <|> p

  where
    q = A.char '"' <|> (A.char '\\' *> A.char '"')

-- | Recognizes a quoted string.
quoted :: A.Parser ByteString
quoted = A.char '"' *> A.option "" contents <* A.char '"'
  where
    esc = A.char '\\' *> (A.char '\\' <|> A.char '"')
    unQ = A.takeWhile1 (A.notInClass "\"\\") -- TODO: use `takeWhile1`, null field is blank
    contents = mconcat <$> many (unQ <|> Char8.singleton <$> esc)

-- | Recognizes a plain string literal, not containing comma, quotes, or parens.
plain :: A.Parser ByteString
plain = A.takeWhile1 (A.notInClass ",\"()")

-- TODO: Clarify
-- plain_ :: A.Parser ByteString
-- plain_ = A.takeWhile (A.notInClass ",")

-- textContent :: A.Parser Text
-- textContent = decodeUtf8 <$> byteContent

byteContent :: A.Parser ByteString
byteContent = quoted <|> plain


returnCompositeError :: forall a err . (Typeable a, Exception err)
            => (String -> Maybe Oid -> String -> String -> String -> err)
            -> CompositeField -> String -> Conversion a
returnCompositeError mkErr f msg = do
  tyn <- Char8.unpack <$> (typename $ cField f)
  conversionError $ mkErr tyn (Just $ typeOid $ cField f) "" (show (typeOf (undefined :: a))) msg

attoCompositeFieldParser :: forall a. (Typeable a)
     => A.Parser a
     -- ^ An attoparsec parser.
     -> CompositeFieldParser a
attoCompositeFieldParser p f = \case
  Nothing -> returnCompositeError UnexpectedNull f ""
  Just s -> case A.parseOnly p s of
    Left err -> returnCompositeError ConversionFailed f err
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

instance FromCompositeField t => FromCompositeField (Maybe t) where
  fromCompositeField _ Nothing = pure Nothing
  fromCompositeField cf bs = Just <$> fromCompositeField @t cf bs


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
