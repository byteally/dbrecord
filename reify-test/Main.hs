{-# LANGUAGE OverloadedStrings #-}
module Main where

import DBRecord.Internal.Migration.Validation
import qualified Database.PostgreSQL.Simple as PGS

main = do
  dbInfo <- getPostgresDbSchemaInfo "public" defHints localConnectInfo
  print dbInfo


  where localConnectInfo = PGS.ConnectInfo "127.0.0.1" 5432 "sreenidhi" "password" "test_dbrecord"
