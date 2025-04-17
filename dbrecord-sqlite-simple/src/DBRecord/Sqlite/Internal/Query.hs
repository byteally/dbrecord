{-# LANGUAGE CPP                        #-}
{-# OPTIONS_GHC -Wno-orphans            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module DBRecord.Sqlite.Internal.Query where

import           Control.Monad.Reader
import           DBRecord.Driver
import qualified DBRecord.Internal.Sql.SqlGen as SQ
import qualified DBRecord.Sqlite.Internal.Sql.Pretty as SQ
import           Data.Functor.Identity
import           Data.Kind ( Type)
import qualified Data.Pool as P
import           Data.String
import qualified Data.Vector as V
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
    SQSConfig :: P.Pool SQS.Connection -> SessionConfig SQS
  runSession_ (SQSConfig pool) dbact f =
    U.withRunInIO (\f0 -> P.withResource pool (\conn -> f0 (f (SQS conn) (runReaderT dbact $ SQS conn))))

instance HasTransaction SQS where
  withTransaction (SQS conn) dbact =
    U.withRunInIO (\f -> SQS.withTransaction conn (f dbact))

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
    execute_ conn (fromString updateSQL)
    pure 0

instance HasQuery SQS where
  dbQueryWith parser (SQS conn) primQ = do
    let sqlQ = SQ.renderQuery $ SQ.sql primQ
    V.fromList <$> queryWith_ parser conn (fromString sqlQ)

instance HasInsert SQS where
  dbInsert (SQS conn) insQ = do
    let insSQL = SQ.renderInsert $ SQ.insertSql insQ
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
    execute_ conn (fromString delSQL)
    pure 0

sqliteDefaultPool :: FilePath -> IO (P.Pool Connection)
sqliteDefaultPool path = 
#if MIN_VERSION_resource_pool(0,4,0)
  P.newPool (P.defaultPoolConfig (SQS.open path) SQS.close 1000 24)
#elif MIN_VERSION_resource_pool(0,3,0)
  P.newPool cfg
  where
    cfg = P.PoolConfig { P.createResource = SQS.open path
                       , P.freeResource = SQS.close
                       , P.poolCacheTTL = 5
                       , P.poolMaxResources = 10
                       }
#else
  P.createPool (SQS.open path) SQS.close 10 5 10
#endif

instance (FromField a) => FromField (Identity a) where
  fromField f = Identity <$> fromField f
