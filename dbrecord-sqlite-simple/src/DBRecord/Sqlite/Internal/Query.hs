{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module DBRecord.Sqlite.Internal.Query where

import qualified DBRecord.Internal.Sql.SqlGen as SQ
import qualified DBRecord.Sqlite.Internal.Sql.Pretty as SQ
import Database.SQLite.Simple as SQS
import Database.SQLite.Simple.FromRow as SQS
import Database.SQLite.Simple.FromField
import DBRecord.Query
import Data.Pool
import Control.Monad.Reader
import Data.String
import Data.Functor.Identity

newtype SqliteDBT m (db :: *) a = SqliteDBT { runSqliteDB :: ReaderT (SQS SQS.Connection) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (SQS SQS.Connection))

type SqliteDB = SqliteDBT IO

type instance ToDBRow   SQS a = ToRow a

instance DBDecoder SQS where
  type FromDBRow SQS       = FromRow  
  type FromDBRowParser SQS = RowParser

  dbDecoder _ _ = fromRow

data SQS cfg where
  SQS :: SQS.Connection -> SQS SQS.Connection

instance Session SQS where
  data SessionConfig SQS cfg where
    SQSConfig :: Pool SQS.Connection -> SessionConfig SQS SQS.Connection
  runSession_ (SQSConfig pool) dbact f =
    withResource pool (\conn -> f (SQS conn) (runReaderT dbact $ SQS conn))

instance HasTransaction SQS where
  withTransaction (SQS conn) = SQS.withTransaction conn

{-
instance HasUpdateRet SQS where
  dbUpdateRet (SQS conn) updateQ = do
    let updateSQL = SQ.renderUpdate $ SQ.updateSql $ updateQ
    putStrLn updateSQL
    returningWith fromRow conn (fromString updateSQL) ([]::[()])
-}

instance HasUpdate SQS where
  dbUpdate (SQS conn) updateQ = do
    let updateSQL = SQ.renderUpdate $ SQ.updateSql $ updateQ
    putStrLn updateSQL
    execute_ conn (fromString updateSQL)
    pure 0

instance HasQuery SQS where
  dbQueryWith parser (SQS conn) primQ = do
    let sqlQ = SQ.renderQuery $ SQ.sql primQ
    putStrLn sqlQ
    queryWith_ parser conn (fromString sqlQ)

instance HasInsert SQS where
  dbInsert (SQS conn) insQ = do
    let insSQL = SQ.renderInsert $ SQ.insertSql insQ
    putStrLn insSQL
    execute_ conn (fromString insSQL)
    pure 0

{-
instance HasInsertRet SQS where
  dbInsertRet (SQS conn) insQ = do
    let insSQL = SQ.renderInsert $ SQ.insertSql insQ
    putStrLn insSQL
    returningWith fromRow conn (fromString insSQL) ([]::[()])
-}

instance HasDelete SQS where
  dbDelete (SQS conn) deleteQ = do
    let delSQL = SQ.renderDelete $ SQ.deleteSql $ deleteQ
    putStrLn delSQL
    execute_ conn (fromString delSQL)
    pure 0

sqliteDefaultPool :: FilePath -> IO (Pool Connection)
sqliteDefaultPool path = createPool (SQS.open path) SQS.close 10 5 10

instance (FromField a) => FromField (Identity a) where
  fromField f = Identity <$> fromField f
