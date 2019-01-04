{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts     #-}

module Main where

import Test.Chinook.MSSQL.Database
import Test.Chinook.MySQL.Database
import Test.Chinook.Postgres.Database
import Test.Chinook.Sqlite.Database
import Test.Chinook.Models

import DBRecord.Sqlite.Internal.Query
import DBRecord.Postgres.Internal.Query
import DBRecord.Query

import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.SQLite.Simple as SQS

import Data.Proxy
import Control.Monad.IO.Class

type instance DBM ChinookPG     = PostgresDB ChinookPG
type instance Driver (PostgresDB ChinookPG) = PGS

type instance DBM ChinookSqlite = SqliteDB ChinookSqlite
type instance Driver (SqliteDB ChinookSqlite)  = SQS

main = do
  let connInfo = PGS.ConnectInfo "127.0.0.1" 5432 "sreenidhi" "password" "chinook"
  runPGSession connInfo (queries (Proxy :: Proxy ChinookPG))  

  let sqlitePath = "/path/to/sqlite"
  runSqliteSession sqlitePath (queries (Proxy :: Proxy ChinookSqlite))  

  -- runMSSQLSession  (queries (Proxy :: Proxy ChinookMSSQL))
  -- runMySqlSession  (queries (Proxy :: Proxy ChinookMySQL))
  

-- runMSSQLSession :: DBM ChinookMSSQL a -> IO a
-- runMSSQLSession = runSession (MSSQLConfig ()) . runMSSQLDB

runPGSession :: PGS.ConnectInfo -> DBM ChinookPG a -> IO a
runPGSession connInfo dbAct = do
  pool <- pgDefaultPool connInfo
  runSession (PGSConfig pool) (runPostgresDB dbAct)

-- runMySQLSession :: DBM ChinookMySQL a -> IO a
-- runMySQLSession = runSession (MySQLConfig ()) . runMySQLDB

runSqliteSession :: FilePath -> DBM ChinookSqlite a -> IO a
runSqliteSession connInfo dbAct = do
  pool <- sqliteDefaultPool connInfo
  runSession (SQSConfig pool) (runSqliteDB dbAct)

-- type instance DBM ChinookMSSQL =
--   MSSQLDB ChinookMSSQL
-- type instance Driver (MSSQLDB ChinookMSSQL) =
--   PGS
