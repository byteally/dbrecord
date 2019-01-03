module Main where

import Test.Chinook.MSSQL.Database
import Test.Chinook.MySql.Database
import Test.Chinook.Postgres.Database
import Test.Chinook.Sqlite.Database
import Test.Chinook.Models

import DBRecord.MSSQL.Internal.Query
import DBRecord.MSSQL.Internal.Types

import DBRecord.Postgres.Internal.Query
import DBRecord.Postgres.Internal.Types

main = do
  runMSSQLSession  (queries (Proxy :: Proxy ChinookMSSQL))
  runPGSession     (queries (Proxy :: Proxy ChinookPG))  
  -- runMySqlSession  (queries (Proxy :: Proxy ChinookMySQL))
  -- runSqliteSession (queries (Proxy :: Proxy ChinookSqlite))  

queries :: Proxy db -> DBM db ()
queries = do
  return ()

runMSSQLSession :: DBM ChinookMSSQL a -> IO a
runMSSQLSession = runTransaction (MSSQLConfig ()) . runMSSQLDB

runPGSession :: PGS.Connection -> DBM ChinookPG a -> IO a
runPGSession conn = runTransaction (PGS conn) . runPGDB

runMySQLSession :: DBM ChinookMySQL a -> IO a
runMySQLSession = runTransaction (MySQLConfig ()) . runMySQLDB

-- runSqliteSession :: DBM ChinookSqlite a -> IO a
-- runSqliteSession = runTransaction (SqliteConfig ()) . runSqliteDB

type instance DBM ChinookPG =
  PostgresDB ChinookPG
type instance Driver (PostgresDB ChinookPG) =
  PGS

type instance DBM ChinookMSSQL =
  MSSQLDB ChinookMSSQL
type instance Driver (MSSQLDB ChinookMSSQL) =
  PGS
