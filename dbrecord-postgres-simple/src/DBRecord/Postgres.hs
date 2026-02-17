{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, StandaloneDeriving, DataKinds, UndecidableInstances, DerivingStrategies, ScopedTypeVariables, TypeOperators, FlexibleContexts, FlexibleInstances, OverloadedStrings, PolyKinds #-}
module DBRecord.Postgres
       ( module DBRecord.Postgres.Internal.Query
       , module Database.PostgreSQL.Simple
       , module Database.PostgreSQL.Simple.FromField
       , Key (..)
       , Json (..)
       , LTree (..)
       , fromPGEnum
       , fromPGEnum'
       , enumToMap
       , GEnumToMap (..)
       , fromFieldHelper
       , fromPGRow
       , byteContent
       , textContent
       ) where

import Control.Applicative
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Builder as Bin
import qualified Data.List.NonEmpty as NE
import DBRecord.Types
import DBRecord.Internal.Types (DbK (Postgres))
import DBRecord.Internal.DBTypes
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (fromPGArray)
import Data.Functor.Identity (Identity(..))
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField (FromField (..), FieldParser, returnError, typename, Conversion, ResultError(..), fromJSONField)
import Database.PostgreSQL.Simple.FromField as PGF (Field)
import Database.PostgreSQL.Simple.ToField (ToField (..), Action (..))
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 as ASCII
import qualified Data.List as L
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable
import GHC.Generics
import DBRecord.Postgres.Internal.Query
import Record
import GHC.OverloadedLabels
import GHC.TypeLits

deriving newtype instance (FromField a) => FromField (Key (k :: j) a)

instance (FromField a) => FromRow (Identity a) where
  fromRow = Identity <$> field

instance (FromJSON a, Typeable a) => FromField (Json a) where
  fromField f dat = Json <$> fromJSONField f dat

instance (ToJSON a) => ToField (Json a) where
  toField (Json a) = Plain . Bin.lazyByteString . encode $ a

instance (FromField a, Typeable a) => FromField [a] where
  fromField f v = fromPGArray <$> fromField f v

instance (FromField a, Typeable a) => FromField (NE.NonEmpty a) where
  fromField f v = NE.fromList <$> fromField f v

fromPGEnum :: forall a.(Generic a, Typeable a, Enum a, GEnumToMap (Rep a), Show a) => ByteString -> FieldParser a
fromPGEnum = fromPGEnum' (enumToMap (Proxy :: Proxy a))

fromPGEnum' :: forall a. (Typeable a) => [(a, (Int, String))] -> ByteString -> FieldParser a
fromPGEnum' _ _tab f Nothing = returnError UnexpectedNull f ""
fromPGEnum' enMap tab f (Just fval) = do
  tName <- typename f
  if tName == tab || tName == ASCII.pack "_" `ASCII.append` tab
    then case L.find (\(_, (_, ename)) -> fval == ASCII.pack ename) enMap of
          Just (en,_) -> return en
          _         -> returnError ConversionFailed f (show fval)
    else returnError Incompatible f ("Wrong database type for " ++ (show $ (typeRep (Proxy :: Proxy a), tab)) ++ ", saw: " ++ show tName)

enumToMap :: (Generic a, Enum a, GEnumToMap (Rep a), Show a) => Proxy a -> [(a, (Int, String))]
enumToMap a = let kvs = gEnumToMap (prep a)
                  prep :: Proxy a -> Proxy (Rep a a)
                  prep = const Proxy
              in fmap (\(fa, _cname) -> let a' = to fa in (a', (fromEnum a', show a'))) kvs

class GEnumToMap f where
  gEnumToMap :: Proxy (f a) -> [(f a, String)]
instance (GEnumToMap f) => GEnumToMap (D1 c f) where
  gEnumToMap _p = fmap (\(fa, n) -> (M1 fa, n)) $ gEnumToMap Proxy
instance (GEnumToMap f, GEnumToMap g) => GEnumToMap (f :+: g) where
  gEnumToMap _p = let l1 = fmap (\(fa, n) -> (L1 fa, n)) $ gEnumToMap Proxy
                      r1 = fmap (\(fa, n) -> (R1 fa, n)) $ gEnumToMap Proxy
                  in l1 ++ r1
instance (GEnumToMap f, Constructor c) => GEnumToMap (C1 c f) where
  gEnumToMap _p = let cname = conName (undefined :: (C1 c f) a)
                  in case gEnumToMap Proxy of
                       [(con, _)] -> [(M1 con, cname)]
                       _          -> error "Panic: GEnumToMap is only defined for nullary constructors"
instance GEnumToMap U1 where
  gEnumToMap _ = [(U1, "")]

fromFieldHelper :: (Typeable a1, Show a2) => ByteString -> (a2 -> Maybe a1) -> PGF.Field -> Maybe a2 -> Conversion a1
fromFieldHelper fieldname _ f Nothing = returnError UnexpectedNull f ("Expected " <> show fieldname)
fromFieldHelper fieldname fn f (Just fval) = do
  tName <- typename f
  if tName == fieldname || tName == ASCII.pack "_" `ASCII.append` fieldname
    then case fn fval of
      Just en -> return en
      _ -> returnError ConversionFailed f (show fval)
    else returnError Incompatible f ("Wrong database type for " ++ (show fieldname) ++ ", saw: " ++ show tName)

textContent :: A.Parser Text
textContent = decodeUtf8 <$> byteContent

byteContent :: A.Parser ByteString
byteContent = quoted <|> plain

-- | Recognizes a quoted string.
quoted :: A.Parser ByteString
quoted = A.char '"' *> A.option "" contents <* A.char '"'
  where
    esc = A.char '\\' *> (A.char '\\' <|> A.char '"')
    unQ = A.takeWhile1 (A.notInClass "\"\\")
    contents = mconcat <$> many (unQ <|> ASCII.singleton <$> esc)

-- | Recognizes a plain string literal, not containing comma, quotes, or parens.
plain :: A.Parser ByteString
plain = A.takeWhile1 (A.notInClass ",\"()")

fromPGRow :: Typeable a => String -> A.Parser a -> PGF.Field -> Maybe ByteString -> Conversion a
fromPGRow _ _ f Nothing = returnError UnexpectedNull f ""
fromPGRow fname parser f (Just bs) = do
  typename' <- typename f
  if typename' /= ASCII.pack fname
    then returnError Incompatible f ("Wanted " <> fname <> ", got " <> show typename')
    else case A.parseOnly parser bs of
           Left err -> returnError ConversionFailed f err
           Right a  -> pure a

instance FromRow (Rec '[]) where
  fromRow = pure end

instance ( FromRow (Rec xs)
         , KnownSymbol fn
         , Typeable ft
         , FromRow (AnnEntity (ToDBType 'Postgres ft) (AutoCodec 'Postgres ft) () ft)
         ) => FromRow (Rec ('(fn, ft) ': xs)) where
  fromRow = do
    hd <- getEntity <$> fromRow @(AnnEntity (ToDBType 'Postgres ft) (AutoCodec 'Postgres ft) () ft) -- field @ft
    rst <- fromRow @(Rec xs)
    pure (fromLabel @fn .= hd .& rst)
