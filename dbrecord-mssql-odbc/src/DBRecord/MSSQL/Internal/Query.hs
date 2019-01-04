{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module DBRecord.MSSQL.Internal.Query where

import DBRecord.Query
import qualified DBRecord.MSSQL.Internal.Sql.Pretty as MSSQL
import qualified DBRecord.Internal.Sql.SqlGen as MSSQL
import Control.Monad.Reader
import qualified DBRecord.Internal.PrimQuery as PQ
import Database.MsSQL as MSSQL

newtype MSSQLDBT m (db :: *) a = MSSQLDBT { runMSSQLDB :: ReaderT (PGS PGS.Connection) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (PGS PGS.Connection))

type MSSQLDB = MSSQLDBT IO

data MSSQL cfg where
  MSSQL :: MSSQL.Connection -> MSSQL MSSSQL.Connection

type instance FromDBRow MSSQL a = (FromRow a)
type instance ToDBRow MSSQL a   = (ToRow a)

instance Session MSSQL where
  data SessionConfig MSSQL cfg where
    MSSQLConfig :: Pool MSSSQL.Connection -> SessionConfig MSSQL MSSQL.Connection 
  runSession_ (MSSQLConfig pool) dbact f =
    withResource pool (\conn -> f (MSSQL conn) (runReaderT dbact $ MSSQL conn))

{-  
instance HasTransaction PGS where
  withTransaction (PGS conn) = PGS.withTransaction conn
-}

renderQuery :: PQ.PrimQuery -> String
renderQuery = MSSQL.renderQuery . MSSQL.sql

instance HasUpdate MSSQL where
  dbUpdate (MSSQL conn) updateQ = do
    let updateSQL = MSSQL.renderUpdate $ MSSQL.updateSql $ updateQ    
    fmap (either throwIO id) (execute conn (fromString updateSQL))

instance HasQuery MSSQL where
  dbQuery (MSSQL conn) primQ = do
    let sqlQ= renderQuery primQ
    fmap (either throwIO V.toList) (query conn (fromString sqlQ))

instance HasInsert MSSQL where
  dbInsert MSSQL insQ = do
    let insSQL = MSSQL.renderInsert $ MSSQL.insertSql insQ
    fmap (either throwIO id) (execute conn (fromString insSQL))

instance HasDelete MSSQL where
  dbDelete MSSQL deleteQ = do
    let delSQL = PG.renderDelete $ PG.deleteSql $ deleteQ
    fmap (either throwIO id) (execute conn (fromString delSQL))

mssqlDefaultPool :: ConnectInfo -> IO (Pool Connection)
mssqlDefaultPool connectInfo = createPool (handleException $ MSSQL.connect connectInfo) MSSQL.close 10 5 10
  where handleException = either throwIO id
