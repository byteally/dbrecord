{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DBRecord.Postgres.Internal.Query where

import qualified DBRecord.Internal.Sql.SqlGen as PG
import qualified DBRecord.Postgres.Internal.Sql.Pretty as PG
import Database.PostgreSQL.Simple as PGS
import Database.PostgreSQL.Simple.FromRow as PGS
import DBRecord.Query
import Data.Pool
import Data.String
import Control.Monad.Reader

newtype PostgresDBT (db :: *) m a = PostgresDBT { runPostgresDB :: ReaderT (PGS PGS.Connection) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (PGS PGS.Connection))

type PostgresDB db = PostgresDBT db IO

instance DBDecoder PGS where
  type FromDBRowParser PGS = RowParser
  type FromDBRow PGS       = FromRow
  dbDecoder _ _ = fromRow
  
type instance ToDBRow   PGS a = ToRow a

data PGS cfg where
  PGS :: PGS.Connection -> PGS PGS.Connection

instance Session PGS where
  data SessionConfig PGS cfg where
    PGSConfig :: Pool PGS.Connection -> SessionConfig PGS PGS.Connection  
  runSession_ (PGSConfig pool) dbact f =
    withResource pool (\conn -> f (PGS conn) (runReaderT dbact $ PGS conn))
  
instance HasTransaction PGS where
  withTransaction (PGS conn) = PGS.withTransaction conn

instance HasUpdateRet PGS where
  dbUpdateRetWith parser (PGS conn) updateQ = do
    let updateSQL = PG.renderUpdate $ PG.updateSql $ updateQ
    returningWith parser conn (fromString updateSQL) ([]::[()])

instance HasUpdate PGS where
  dbUpdate (PGS conn) updateQ = do
    let updateSQL = PG.renderUpdate $ PG.updateSql $ updateQ
    execute_ conn (fromString updateSQL)

instance HasQuery PGS where
  dbQueryWith parser (PGS conn) primQ = do
    let sqlQ = PG.renderQuery $ PG.sql primQ
    queryWith_ parser conn (fromString sqlQ)

instance HasInsert PGS where
  dbInsert (PGS conn) insQ = do
    let insSQL = PG.renderInsert $ PG.insertSql insQ
    execute_ conn (fromString insSQL)

instance HasInsertRet PGS where
  dbInsertRetWith parser (PGS conn) insQ = do
    let insSQL = PG.renderInsert $ PG.insertSql insQ
    returningWith parser conn (fromString insSQL) ([]::[()])

instance HasDelete PGS where
  dbDelete (PGS conn) deleteQ = do
    let delSQL = PG.renderDelete $ PG.deleteSql $ deleteQ
    execute_ conn (fromString delSQL)

pgDefaultPool :: ConnectInfo -> IO (Pool Connection)
pgDefaultPool connectInfo = createPool (PGS.connect connectInfo) PGS.close 10 5 10
