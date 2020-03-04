{-# LANGUAGE OverloadedStrings #-}
module Main where

import DBRecord.Postgres.Internal.Reify
import Database.PostgreSQL.Simple 
import Data.Function 

main = do
  -- let hints = defHints
  getPostgresDbSchemaInfo localConnectInfo  

  where localConnectInfo =
          defaultConnectInfo { connectHost = "localhost"
                             , connectPassword = "postgres"
                             , connectDatabase = "dbrecord_test"
                             , connectUser = "postgres"
                             
                             }
