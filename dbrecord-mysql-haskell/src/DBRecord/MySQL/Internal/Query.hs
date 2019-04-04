{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DBRecord.MySQL.Internal.Query where

import qualified Database.MySQL.Base as MySQL
import qualified DBRecord.MySQL.Internal.Sql.Pretty as MySQL
import qualified DBRecord.Internal.Sql.SqlGen as MySQL
import DBRecord.Query
import Data.Pool
import Control.Monad.Reader
import DBRecord.MySQL.Internal.FromRow
import Control.Exception
import Control.Monad.Trans.Except
import qualified System.IO.Streams as SS
import Data.String

data MySQL cfg where
  MySQL :: MySQL.MySQLConn -> MySQL MySQL.MySQLConn

newtype MySQLDBT m (db :: *) a = MySQLDBT { runMySQLDB :: ReaderT (MySQL MySQL.MySQLConn) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (MySQL MySQL.MySQLConn))

type MySQLDB = MySQLDBT IO

-- type instance ToDBRow MySQL a       = ToRow a

instance DBDecoder MySQL where
  type FromDBRowParser MySQL = RowParser
  type FromDBRow MySQL       = FromRow
  
  dbDecoder _ _  = fromRow

instance Session MySQL where
  data SessionConfig MySQL cfg where
    MySQLConfig :: Pool MySQL.MySQLConn -> SessionConfig MySQL MySQL.MySQLConn
  runSession_ (MySQLConfig pool) dbact f =
    withResource pool (\conn -> f (MySQL conn) (runReaderT dbact $ MySQL conn))

instance HasUpdate MySQL where
  dbUpdate (MySQL conn) updateQ = do
    let updateSQL = MySQL.renderUpdate $ MySQL.updateSql $ updateQ    
    (fromIntegral . MySQL.okAffectedRows) <$> MySQL.execute_ conn (fromString updateSQL)

instance HasQuery MySQL where
  dbQueryWith par (MySQL conn) primQ = do
    let sqlQ = MySQL.renderQuery $ MySQL.sql primQ
    liftIO $ putStrLn sqlQ
    queryWith par conn (fromString sqlQ)

instance HasInsert MySQL where
  dbInsert (MySQL conn) insQ = do
    let insSQL = MySQL.renderInsert $ MySQL.insertSql insQ
    (fromIntegral . MySQL.okAffectedRows) <$> MySQL.execute_ conn (fromString insSQL)
    
instance HasDelete MySQL where
  dbDelete (MySQL conn) deleteQ = do
    let delSQL = MySQL.renderDelete $ MySQL.deleteSql deleteQ
    (fromIntegral . MySQL.okAffectedRows) <$> MySQL.execute_ conn (fromString delSQL)

mysqlDefaultPool :: MySQL.ConnectInfo -> IO (Pool MySQL.MySQLConn)
mysqlDefaultPool connectInfo =
  createPool
  (MySQL.connect connectInfo)
  (\conn -> MySQL.close conn) 10 5 10

{-
query :: FromRow a => MySQL.MySQLConn -> MySQL.Query -> IO [a]
query = queryWith fromRow 
-}

queryWith :: RowParser a -> MySQL.MySQLConn -> MySQL.Query -> IO [a]
queryWith par conn q = do
  (defs, ss) <- MySQL.query_ conn q
  res <- runExceptT $ go defs ss
  case res of
    Left e     -> throwIO e
    Right res' -> pure res'
  
  where go defs ss = do
          mval <- liftIO $ SS.read ss
          case mval of
            Just val ->
              (:) <$> exceptT (evalParser par defs val)
                  <*> go defs ss
            Nothing  -> pure []


