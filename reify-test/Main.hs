{-# LANGUAGE OverloadedStrings #-}
module Main where

import DBRecord.Internal.Migration.Validation
import qualified Database.PostgreSQL.Simple as PGS
import Data.Function 

main = do
  let hints = defHints & tableNameHint "empsalary" "Emp_Salary"
  dbInfo <- getPostgresDbSchemaInfo "public" hints localConnectInfo
  print dbInfo


  where localConnectInfo = PGS.ConnectInfo "127.0.0.1" 5432 "sreenidhi" "password" "MyHuntPath"
