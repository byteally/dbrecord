{-# LANGUAGE TypeFamilies, DataKinds, DeriveGeneric, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module DBRecord.Postgres.Internal.Types where

import qualified DBRecord.Internal.Types as Type
import DBRecord.Internal.DBTypes
import Data.Text
import GHC.TypeLits
import GHC.Generics
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Expr
import Data.String
import qualified Data.Text as T
import Data.ByteString (ByteString)
import DBRecord.Internal.Types (DbK (..))
import Data.Proxy

import qualified DBRecord.Internal.PrimQuery as PQ
import qualified Data.Text.Encoding as T

instance FromField Interval where                                                                                                                    
  fromField f Nothing   = returnError UnexpectedNull f ""
  fromField f (Just val) = do                                                 
    tName <- typename f
    if tName == "interval"
      then case T.decodeUtf8' val  of
        Left err -> returnError ConversionFailed f (show err)
        Right a -> return (Interval a)
      else returnError Incompatible f ("Wrong database type for Interval, saw: " ++ show tName)
