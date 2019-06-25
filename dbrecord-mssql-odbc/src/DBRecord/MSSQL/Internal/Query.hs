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
import Database.MsSQL as MSSQL hiding (Session, RowBufferType)
import qualified Database.MsSQL as M
import Data.Pool
import qualified Data.Vector as V
import Control.Exception (throwIO)
import Data.String
import Data.Dynamic
import Data.Profunctor
import Data.Record (HListF (..), Rec (..), (:::))

newtype MSSQLDBT m (db :: *) a = MSSQLDBT { runMSSQLDB :: ReaderT (MSSQL MSSQL.Connection) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (MSSQL MSSQL.Connection))

type MSSQLDB = MSSQLDBT IO

class (FromRow a, SQLBindCol (M.RowBufferType a)) => FromDBRowCtx a 
instance (FromRow a, SQLBindCol (M.RowBufferType a)) => FromDBRowCtx a 

newtype RowBufferType a = RowBufferType { getRowBufferType :: M.RowBufferType a }

getRowBufferType :: RowBufferType a -> M.RowBufferType a
getRowBufferType (RBOther a) = a

data MSRowParser rb t = MSRowParser { rowParser :: RowParser rb t
                                    , sqlColBinder :: Dict SQLBindCol rb
                                    }

instance 


{-
data RowBufferType a where
  RBTuple :: RowBufferType a -> RowBufferType b -> RowBufferType (a, b)
  RBHCons :: RowBufferType a -> RowBufferType (HListF xs) ->
            RowBufferType (HListF (fld ::: a ': xs))
  RBRCons :: RowBufferType (o a) -> RowBufferType (Rec os xs) ->
            RowBufferType (Rec (o ': os) (fld ::: a ': xs))
  RBHNil  :: RowBufferType (HListF '[])
  RBRNil  :: RowBufferType (Rec '[] '[])
  RBOther :: M.RowBufferType a -> RowBufferType a
-}

instance DBDecoder MSSQL where
  type FromDBRowParser MSSQL   = MSRowParser
  type FromDBRow MSSQL         = FromDBRowCtx
  type RowParserInput MSSQL    = RowBufferType
  
  dbDecoder _ _ = lmap getRowBufferType fromRow
          
data MSSQL cfg where
  MSSQL :: MSSQL.Connection -> MSSQL MSSQL.Connection

type instance ToDBRow MSSQL a   = (ToRow a)

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
    res <- queryWith (lmap conv par) undefined conn (fromString sqlQ)
    either throwIO (pure . V.toList) res

instance HasInsert MSSQL where
  dbInsert (MSSQL conn) insQ = do
    let insSQL = MSSQL.renderInsert $ MSSQL.insertSql insQ
    res <- execute conn (fromString insSQL)
    either throwIO pure res
    
instance HasDelete MSSQL where
  dbDelete (MSSQL conn) deleteQ = do
    let delSQL = MSSQL.renderDelete $ MSSQL.deleteSql $ deleteQ
    res <- execute conn (fromString delSQL)
    either throwIO pure res

mssqlDefaultPool :: ConnectInfo -> IO (Pool Connection)
mssqlDefaultPool connectInfo =
  createPool
  (handleException =<< MSSQL.connect connectInfo)
  (\conn -> handleException =<< MSSQL.disconnect conn) 10 5 10

  where handleException = either throwIO pure

conv :: M.RowBufferType a -> RowBufferType a 
conv = undefined
