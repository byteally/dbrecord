{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module DBRecord.MSSQL.Internal.Query where

import DBRecord.Query
import qualified DBRecord.MSSQL.Internal.Sql.Pretty as MSSQL
import qualified DBRecord.Internal.Sql.SqlGen as MSSQL
import Control.Monad.Reader
import qualified DBRecord.Internal.PrimQuery as PQ

data MSSQL cfg where
  MSSQL :: MSSQL cfg

type instance FromDBRow MSSQL a = ()
type instance ToDBRow MSSQL a   = ()

renderQuery :: PQ.PrimQuery -> String
renderQuery = MSSQL.renderQuery . MSSQL.sql

instance HasUpdateRet MSSQL where
  dbUpdateRet MSSQL updateQ = do
    let updateSQL = MSSQL.renderUpdate $ MSSQL.updateSql $ updateQ
    putStrLn updateSQL
    undefined

instance HasUpdate MSSQL where
  dbUpdate MSSQL updateQ = do
    let updateSQL = MSSQL.renderUpdate $ MSSQL.updateSql $ updateQ
    putStrLn updateSQL
    undefined

instance HasQuery MSSQL where
  dbQuery MSSQL primQ = do
    let sqlQ= renderQuery primQ
    putStrLn sqlQ
    return []

instance HasInsert MSSQL where
  dbInsert MSSQL insQ = do
    let insSQL = MSSQL.renderInsert $ MSSQL.insertSql insQ
    putStrLn insSQL
    undefined 

instance HasInsertRet MSSQL where
  dbInsertRet MSSQL insQ = do
    let insSQL = MSSQL.renderInsert $ MSSQL.insertSql insQ
    putStrLn insSQL
    undefined

instance HasDelete MSSQL where
  dbDelete MSSQL deleteQ = do
    -- let delSQL = PG.renderDelete $ PG.deleteSql $ deleteQ
    -- putStrLn delSQL
    undefined

instance Session MSSQL where
  data SessionConfig MSSQL cfg = MSSQLConfig ()
  runSession (MSSQLConfig cfg) act = runReaderT act MSSQL
  
instance HasTransaction MSSQL where
  withTransaction = undefined 
