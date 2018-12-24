{-# LANGUAGE OverloadedStrings #-}
module Main where

import DBRecord.Postgres.Internal.Reify
import qualified Database.PostgreSQL.Simple as PGS
import Data.Function 

main = do
  let hints = defHints                        
  dbInfo <- getPostgresDbSchemaInfo "public" hints localConnectInfo
  print dbInfo

  where localConnectInfo = PGS.ConnectInfo "127.0.0.1" 5432 "sreenidhi" "password" "chinook"
