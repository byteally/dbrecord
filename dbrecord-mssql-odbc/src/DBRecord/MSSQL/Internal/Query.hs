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
import Database.MSSQL as MSSQL
import Data.Pool
import qualified Data.Vector as V
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

instance HasTransaction MSSQL where
  withTransaction (MSSQL conn) = MSSQL.withTransaction conn

renderQuery :: PQ.PrimQuery -> String
renderQuery = MSSQL.renderQuery . MSSQL.sql

instance HasUpdate MSSQL where
  dbUpdate (MSSQL conn) updateQ = do
    let updateSQL = MSSQL.renderUpdate $ MSSQL.updateSql $ updateQ    
    execute conn (fromString updateSQL)

instance HasUpdateRet MSSQL where
  dbUpdateRetWith parser (MSSQL conn) updateQ = do
    let updateSQL = MSSQL.renderUpdate $ MSSQL.updateSql $ updateQ
    fmap V.toList (queryWith parser conn (fromString updateSQL))

instance HasQuery MSSQL where
  dbQueryWith par (MSSQL conn) primQ = do
    let sqlQ = renderQuery primQ
    fmap V.toList (queryWith par conn (fromString sqlQ))

instance HasInsert MSSQL where
  dbInsert (MSSQL conn) insQ = do
    let insSQL = MSSQL.renderInsert $ MSSQL.insertSql insQ
    execute conn (fromString insSQL)

instance HasInsertRet MSSQL where
  dbInsertRetWith parser (MSSQL conn) insQ = do
    let insSQL = MSSQL.renderInsert $ MSSQL.insertSql insQ
    fmap V.toList (queryWith parser conn (fromString insSQL))
    
instance HasDelete MSSQL where
  dbDelete (MSSQL conn) deleteQ = do
    let delSQL = MSSQL.renderDelete $ MSSQL.deleteSql $ deleteQ
    execute conn (fromString delSQL)

mssqlDefaultPool :: ConnectInfo -> IO (Pool Connection)
mssqlDefaultPool conn =
  createPool
  (MSSQL.connect conn)
  MSSQL.disconnect 10 5 10


