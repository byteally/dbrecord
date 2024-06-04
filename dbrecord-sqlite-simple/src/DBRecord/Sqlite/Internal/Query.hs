{-# OPTIONS_GHC -Wno-orphans            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module DBRecord.Sqlite.Internal.Query where

import           Control.Monad.Reader
import qualified DBRecord.Internal.Sql.SqlGen as SQ
import           DBRecord.Query
import qualified DBRecord.Sqlite.Internal.Sql.Pretty as SQ
import           Data.Functor.Identity
import           Data.Kind
import           Data.Pool
import           Data.String
import           Database.SQLite.Simple as SQS
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow as SQS
import qualified UnliftIO as U


newtype SqliteDBT m (db :: Type) a = SqliteDBT { runSqliteDB :: ReaderT SQS m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader SQS)

type SqliteDB = SqliteDBT IO

type instance ToDBRow   SQS a = ToRow a

instance DBDecoder SQS where
  type FromDBRow SQS       = FromRow  
  type FromDBRowParser SQS = RowParser

  dbDecoder _ _ = fromRow

data SQS where
  SQS :: SQS.Connection -> SQS

instance Session SQS where
  data SessionConfig SQS where
    SQSConfig :: Pool SQS.Connection -> SessionConfig SQS
  runSession_ (SQSConfig pool) dbact f =
    withResource pool (\conn -> f (SQS conn) (runReaderT dbact $ SQS conn))

instance HasTransaction SQS where
  withTransaction (SQS conn) ma =
    U.withRunInIO (\f -> SQS.withTransaction conn (f ma))

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
