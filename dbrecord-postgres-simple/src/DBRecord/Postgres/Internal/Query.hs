{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}

module DBRecord.Postgres.Internal.Query where

import           Control.Monad.Reader
import qualified DBRecord.Internal.Sql.SqlGen as PG
import           DBRecord.Internal.Types
import qualified DBRecord.Postgres.Internal.Sql.Pretty as PG
import           DBRecord.Query
import           DBRecord.Types
import           Data.Functor.Identity
import           Data.Pool
import           Data.String
import           Database.PostgreSQL.Simple as PGS
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow as PGS
import qualified UnliftIO as U

newtype PostgresDBT (db :: *) m a = PostgresDBT { runPostgresDB :: ReaderT PGS m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadReader PGS, U.MonadUnliftIO)

type PostgresDB db = PostgresDBT db IO

instance DBDecoder PGS where
  type FromDBRowParser PGS = RowParser
  type FromDBRow PGS       = FromRow
  dbDecoder _ _ = fromRow

type instance ToDBRow PGS a = ToRow a

data PGS where
  PGS :: PGS.Connection -> PGS

instance Session PGS where
  data SessionConfig PGS where
    PGSConfig :: Pool PGS.Connection -> SessionConfig PGS
  runSession_ (PGSConfig pool) dbact f = do
    withResource pool (\conn -> f (PGS conn) (runReaderT dbact (PGS conn)))

instance HasTransaction PGS where
  withTransaction (PGS conn) dbact =
    U.withRunInIO (\f -> PGS.withTransaction conn (f dbact))

instance HasUpdateRet PGS where
  dbUpdateRetWith parser (PGS conn) updateQ = do
    let updateSQL = PG.renderUpdate $ PG.updateSql $ updateQ
    queryWith_ parser conn (fromString updateSQL)

instance HasUpdate PGS where
  dbUpdate (PGS conn) updateQ = do
    let updateSQL = PG.renderUpdate $ PG.updateSql $ updateQ
    execute_ conn (fromString updateSQL)

instance HasQuery PGS where
  dbQueryWith parser (PGS conn) primQ = do
    let sqlQ = PG.renderQuery $ PG.sql primQ
    queryWith_ parser conn (fromString sqlQ)

instance HasInsert PGS where
  dbInsert (PGS conn) insQ = do
    let insSQL = PG.renderInsert $ PG.insertSql insQ
    execute_ conn (fromString insSQL)

instance HasInsertRet PGS where
  dbInsertRetWith parser (PGS conn) insQ = do
    let insSQL = PG.renderInsert $ PG.insertSql insQ
    queryWith_ parser conn (fromString insSQL)

instance HasDelete PGS where
  dbDelete (PGS conn) deleteQ = do
    let delSQL = PG.renderDelete $ PG.deleteSql $ deleteQ
    execute_ conn (fromString delSQL)

pgDefaultPool :: ConnectInfo -> IO (Pool Connection)
pgDefaultPool connectInfo = createPool (PGS.connect connectInfo) PGS.close 10 5 10

#if MIN_VERSION_postgresql_simple(0,6,3)
#else
instance (FromField a) => FromField (Identity a) where
  fromField f m = Identity <$> fromField f m
#endif

instance (FromField a) => FromField (fld ::: a) where
  fromField f m = Field <$> fromField f m

instance (FromField v) => FromField (Key t v) where
  fromField f m = Key <$> fromField f m

instance (FromField a) => FromRow (Identity a) where
  fromRow = Identity <$> field

instance ( FromField (f x)
         , FromRow (HList f a)
         ) => FromRow (HList f (x ': a)) where
  fromRow = (:&) <$> field <*> fromRow

instance FromRow (HList f '[]) where
  fromRow = pure Nil

