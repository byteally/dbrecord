{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module DBRecord.Postgres.Internal.Query where

import qualified DBRecord.Internal.Sql.SqlGen as PG
import qualified DBRecord.Postgres.Internal.Sql.Pretty as PG
import qualified DBRecord.Postgres.Internal.Transaction as PG
import Database.PostgreSQL.Simple as PGS
import Database.PostgreSQL.Simple.FromRow as PGS
import DBRecord.Query
import Data.Pool (withResource)
import Data.String
import Control.Monad.Reader
import Data.Pool

type instance FromDBRow PGS a = FromRow a
type instance ToDBRow   PGS a = ToRow a

data PGS cfg where
  PGS :: PGS.Connection -> PGS PGS.Connection

instance Session PGS where
  data SessionConfig PGS cfg where
    PGSConfig :: Pool PGS.Connection -> SessionConfig PGS PGS.Connection  
  runSession (PGSConfig pool) dbact f =
    withResource pool (\conn -> f (PGS conn) (runReaderT dbact $ PGS conn))
  
instance HasTransaction PGS where
  withTransaction (PGS conn) = PGS.withTransaction conn

instance HasUpdateRet PGS where
  dbUpdateRet (PGS conn) updateQ = do
    let updateSQL = PG.renderUpdate $ PG.updateSql $ updateQ
    putStrLn updateSQL
    returningWith fromRow conn (fromString updateSQL) ([]::[()])

instance HasUpdate PGS where
  dbUpdate (PGS conn) updateQ = do
    let updateSQL = PG.renderUpdate $ PG.updateSql $ updateQ
    putStrLn updateSQL
    execute_ conn (fromString updateSQL)

instance HasQuery PGS where
  dbQuery (PGS conn) primQ = do
    let sqlQ= PG.renderQuery $ PG.sql primQ
    putStrLn sqlQ
    query_ conn (fromString sqlQ)

instance HasInsert PGS where
  dbInsert (PGS conn) insQ = do
    let insSQL = PG.renderInsert $ PG.insertSql insQ
    putStrLn insSQL
    execute_ conn (fromString insSQL)

instance HasInsertRet PGS where
  dbInsertRet (PGS conn) insQ = do
    let insSQL = PG.renderInsert $ PG.insertSql insQ
    putStrLn insSQL
    returningWith fromRow conn (fromString insSQL) ([]::[()])

instance HasDelete PGS where
  dbDelete (PGS conn) deleteQ = do
    let delSQL = PG.renderDelete $ PG.deleteSql $ deleteQ
    putStrLn delSQL
    execute_ conn (fromString delSQL)

