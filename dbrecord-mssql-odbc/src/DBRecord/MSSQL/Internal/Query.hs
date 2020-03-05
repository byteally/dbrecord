{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module DBRecord.MSSQL.Internal.Query where

import DBRecord.Query
import qualified DBRecord.MSSQL.Internal.Sql.Pretty as MSSQL
import qualified DBRecord.Internal.Sql.SqlGen as MSSQL
import Control.Monad.Reader
import qualified DBRecord.Internal.PrimQuery as PQ
import Database.MsSQL as MSSQL hiding (Session)
import Data.Pool
import qualified Data.Vector as V
import Control.Exception (throwIO)
import Data.String

newtype MSSQLDBT m (db :: *) a = MSSQLDBT { runMSSQLDB :: ReaderT (MSSQL MSSQL.Connection) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (MSSQL MSSQL.Connection))

type MSSQLDB = MSSQLDBT IO

instance DBDecoder MSSQL where
  type FromDBRowParser MSSQL   = RowParser
  type FromDBRow MSSQL         = FromRow
  
  dbDecoder _ _ = fromRow
          
data MSSQL cfg where
  MSSQL :: MSSQL.Connection -> MSSQL MSSQL.Connection

-- type instance ToDBRow MSSQL a   = (ToRow a)

instance Session MSSQL where
  data SessionConfig MSSQL cfg where
    MSSQLConfig :: Pool MSSQL.Connection -> SessionConfig MSSQL MSSQL.Connection 
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
    res <- execute conn (fromString updateSQL)
    either throwIO pure res

instance HasQuery MSSQL where
  dbQueryWith par (MSSQL conn) primQ = do
    let sqlQ = renderQuery primQ
    -- liftIO $ putStrLn sqlQ
    res <- queryWith par conn (fromString sqlQ)
    either throwIO (pure . V.toList) res

instance HasInsert MSSQL where
  dbInsert (MSSQL conn) insQ = do
    let insSQL = MSSQL.renderInsert $ MSSQL.insertSql insQ
    res <- execute conn (fromString insSQL)
    either throwIO pure res

instance HasInsertRet MSSQL where
  dbInsertRetWith parser (MSSQL conn) insQ = do
    let insSQL = MSSQL.renderInsert $ MSSQL.insertSql insQ
    res <- queryWith parser conn (fromString insSQL)
    either throwIO (pure . V.toList) res
    
instance HasDelete MSSQL where
  dbDelete (MSSQL conn) deleteQ = do
    let delSQL = MSSQL.renderDelete $ MSSQL.deleteSql $ deleteQ
    res <- execute conn (fromString delSQL)
    either throwIO pure res

mssqlDefaultPool :: ConnectInfo -> IO (Pool Connection)
mssqlDefaultPool conn =
  createPool
  (handleException =<< MSSQL.connect conn)
  (handleException <=< MSSQL.disconnect) 10 5 10

  where handleException = either throwIO pure

