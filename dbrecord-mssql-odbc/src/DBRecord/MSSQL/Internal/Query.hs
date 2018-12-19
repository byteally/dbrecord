module DBRecord.MSSQL.Internal.Query where

{-
type instance FromDBRow (PGS) a = (FromRow a)
type instance ToDBRow (PGS) a = (ToRow a)

data PGS cfg where
  PGS :: PGS.Connection -> PGS PGS.Connection

instance Session PGS where
  data SessionConfig PGS cfg where
    PGSConfig :: PG.Config_ -> SessionConfig PGS PGS.Connection
  runSession (PGSConfig cfg) dbact =
    withResource (PG.connectionPool cfg) $ \conn -> runReaderT dbact $ PGS conn
  
instance HasTransaction PGS where
  withTransaction (PGS conn) dbact = PGS.withTransaction conn dbact

-}

data MSSQL cfg where
  MSSQL :: MSSQL MSSQL.Connection

instance HasUpdateRet MSSQL where
  dbUpdateRet (MSSQL conn) updateQ = do
    let updateSQL = PG.renderUpdate $ PG.updateSql $ updateQ
    putStrLn updateSQL
    returningWith fromRow conn (fromString updateSQL) ([]::[()])

instance HasUpdate MSSQL where
  dbUpdate (MSSQL conn) updateQ = do
    let updateSQL = PG.renderUpdate $ PG.updateSql $ updateQ
    putStrLn updateSQL
    execute_ conn (fromString updateSQL)

instance HasQuery MSSQL where
  dbQuery (MSSQL conn) primQ = do
    let sqlQ= PG.renderQuery $ PG.sql primQ
    putStrLn sqlQ
    query_ conn (fromString sqlQ)

instance HasInsert MSSQL where
  dbInsert (MSSQL conn) insQ = do
    let insSQL = PG.renderInsert $ PG.insertSql insQ
    putStrLn insSQL
    execute_ conn (fromString insSQL)

instance HasInsertRet MSSQL where
  dbInsertRet (MSSQL conn) insQ = do
    let insSQL = PG.renderInsert $ PG.insertSql insQ
    putStrLn insSQL
    returningWith fromRow conn (fromString insSQL) ([]::[()])

instance HasDelete MSSQL where
  dbDelete (MSSQL conn) deleteQ = do
    let delSQL = PG.renderDelete $ PG.deleteSql $ deleteQ
    putStrLn delSQL
    execute_ conn (fromString delSQL)

