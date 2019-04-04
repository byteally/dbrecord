{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module DBRecord.MySQL.Internal.FromField where

import Database.MySQL.Base
import Data.Int
import Data.Word
import Control.Exception
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime)
import Data.Functor.Identity
import qualified Data.ByteString.Char8 as B8

newtype FieldParser a =
  FieldParser { runFieldParser :: ColumnDef -> MySQLValue -> Either ResultError a }

instance Functor FieldParser where
  fmap f (FieldParser fp) = FieldParser $ \def val -> fmap f (fp def val)

fieldParser :: (ColumnDef -> MySQLValue -> Either ResultError a) -> FieldParser a
fieldParser = FieldParser

data ResultError
  = Incompatible { errColumnName :: Text
                 , errHaskellType :: Text
                 , errMessage :: Text
                 }
    
  | ConversionFailed { --errColumnName :: Text
                     --, errHaskellType :: Text
                       errMessage :: Text }
  deriving (Eq, Show)

instance Exception ResultError

class FromField a where
  fromField :: FieldParser a

instance (FromField a) => FromField (Identity a) where
  fromField = Identity <$> fromField

instance FromField Int where
  fromField = intConvert "Int"

instance FromField Int8 where
  fromField = fieldParser $ \def val ->
    case val of
      MySQLInt8 i -> pure (fromIntegral i)
      _           -> Left $ conversionFailed "Int8" val def

instance FromField Int16 where
  fromField = fieldParser $ \def val ->
    case val of
      MySQLInt16 i -> pure (fromIntegral i)
      _            -> Left $ conversionFailed "Int16" val def

instance FromField Int32 where
  fromField = fieldParser $ \def val ->
    case val of
      MySQLInt32 i -> pure (fromIntegral i)
      _            -> Left $ conversionFailed "Int32" val def

instance FromField Int64 where
  fromField = fieldParser $ \def val ->
    case val of
      MySQLInt64 i -> pure (fromIntegral i)
      _            -> Left $ conversionFailed "Int64" val def

instance FromField Word where
  fromField = wordConvert "Word"

instance FromField Word8 where
  fromField = fieldParser $ \def val ->
    case val of
      MySQLInt8U i -> pure (fromIntegral i)
      _            -> Left $ conversionFailed "Word8" val def

instance FromField Word16 where
  fromField = fieldParser $ \def val ->
    case val of
      MySQLInt16U i -> pure (fromIntegral i)
      _             -> Left $ conversionFailed "Word16" val def

instance FromField Word32 where
  fromField = fieldParser $ \def val ->
    case val of
      MySQLInt32U i -> pure (fromIntegral i)
      _             -> Left $ conversionFailed "Word32" val def

instance FromField Word64 where
  fromField = fieldParser $ \def val ->
    case val of
      MySQLInt64U i -> pure (fromIntegral i)
      _             -> Left $ conversionFailed "Word64" val def

instance FromField Float where
  fromField = fieldParser $ \def val ->
    case val of
      MySQLFloat f -> pure f
      _ -> Left $ conversionFailed "Float" val def
      
instance FromField Double where
  fromField = fieldParser $ \def val ->
    case val of
      MySQLDouble d -> pure d
      -- MySQLDecimal d -> pure 0.0 -- fromIntegral d
      _ -> Left $ conversionFailed "Double" val def

instance FromField Text where
  fromField = fieldParser $ \def val ->
    case val of
      MySQLText t -> pure t
      _ -> Left $ conversionFailed "Text" val def

instance FromField String where
  fromField = fieldParser $ \def val ->
    case val of
      MySQLText t -> pure (T.unpack t)
      _ -> Left $ conversionFailed "String" val def

instance FromField BS.ByteString where
  fromField = fieldParser $ \def val ->
    case val of
      MySQLBytes t -> pure t
      _ -> Left $ conversionFailed "ByteString" val def

instance FromField Day where
  fromField = fieldParser $ \def val ->
    case val of
      MySQLDate d -> pure d
      _ -> Left $ conversionFailed "Day" val def

instance FromField LocalTime where
  fromField = fieldParser $ \def val ->
    case val of
      MySQLDateTime d  -> pure d
      MySQLTimeStamp d -> pure d
      _ -> Left $ conversionFailed "LocalTime" val def

{-
instance Result Scientific where
  convert def val =
    case val of
      MySQLDecimal d -> d
      MySQLFloat f   -> fromFloatDigits f
      MySQLDouble f  -> fromFloatDigits f
      MySQLInt8U i   -> fromIntegral i
      MySQLInt16U i  -> fromIntegral i
      MySQLInt32U i  -> fromIntegral i
      MySQLInt64U i  -> fromIntegral i
      MySQLInt8 i    -> fromIntegral i
      MySQLInt16 i   -> fromIntegral i
      MySQLInt32 i   -> fromIntegral i
      MySQLInt64 i   -> fromIntegral i
      _ -> throw $ conversionFailed "Scientific" val def
-}

instance (FromField a) =>
         FromField (Maybe a) where
  fromField = fieldParser $ \def val ->
    case val of
      MySQLNull -> pure Nothing
      _         -> Just <$> runFieldParser fromField def val

intConvert :: Num a => Text -> FieldParser a
intConvert t = fieldParser $ \def val ->
  case val of
    MySQLInt8 i   -> pure (fromIntegral i)
    MySQLInt16 i  -> pure (fromIntegral i)
    MySQLInt32 i  -> pure (fromIntegral i)
    MySQLInt64 i  -> pure (fromIntegral i)
    _ -> Left $ conversionFailed t val def

wordConvert :: Num a => Text -> FieldParser a
wordConvert t = fieldParser $ \def val ->
  case val of
    MySQLInt8U i  -> pure (fromIntegral i)
    MySQLInt16U i -> pure (fromIntegral i)
    MySQLInt32U i -> pure (fromIntegral i)
    MySQLInt64U i -> pure (fromIntegral i)
    _ -> Left $ conversionFailed t val def

conversionFailed :: Text -> MySQLValue -> ColumnDef -> ResultError
conversionFailed t v def =
  Incompatible
    (T.pack . show . B8.unpack . columnOrigName $ def)
    t
    ("Could not convert: " <> T.pack (show v))
