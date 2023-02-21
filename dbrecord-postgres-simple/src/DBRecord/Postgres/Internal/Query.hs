{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

module DBRecord.Postgres.Internal.Query
       ( module DBRecord.Postgres.Internal.Query
       , module DBRecord.Postgres.Internal.RegClass
       ) where

import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Control.Monad.Trans.Control as U
import qualified DBRecord.Internal.Sql.SqlGen as PG
import           DBRecord.Internal.Types
import           DBRecord.Postgres.Internal.RegClass
import qualified DBRecord.Postgres.Internal.Sql.Pretty as PG
import           DBRecord.Old.Query
import           DBRecord.Types
import           DBRecord.Driver
import           Data.Functor.Identity
import qualified Data.Pool as P
import           Data.String
import           Database.PostgreSQL.Simple as PGS
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow as PGS
import qualified UnliftIO as U
import           Data.Kind
import           GHC.Generics
import           Data.Proxy
import qualified Data.List as L
import           Data.ByteString.Char8 as ASCII
import           Data.Typeable

newtype PostgresDBT (db :: Type) m a = PostgresDBT { runPostgresDB :: ReaderT PGS m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadReader PGS, U.MonadUnliftIO, MonadThrow, MonadCatch)

deriving newtype instance (U.MonadBaseControl IO m, MonadBase IO m) => U.MonadBaseControl IO (PostgresDBT db m)
deriving newtype instance (MonadBase IO m) => MonadBase IO (PostgresDBT db m)

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
    PGSConfig :: P.Pool PGS.Connection -> SessionConfig PGS
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

runPGExpr :: Expr sc a -> String
runPGExpr = PG.renderExpr . PG.toSqlExpr . getExpr

pgDefaultPool :: ConnectInfo -> IO (P.Pool Connection)
pgDefaultPool connectInfo =
#if MIN_VERSION_resource_pool(0,3,0)
  P.newPool cfg
  where
    cfg = P.PoolConfig { P.createResource = PGS.connect connectInfo
                       , P.freeResource = PGS.close
                       , P.poolCacheTTL = 5
                       , P.poolMaxResources = 10
                       }
#else
  P.createPool (PGS.connect connectInfo) PGS.close 10 5 10
#endif

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

-- | Implementation based on MonadUnliftIO
withResource :: (U.MonadUnliftIO m) => P.Pool a -> (a -> m r) -> m r
withResource p k = U.withRunInIO $ \f -> P.withResource p (\a -> f $ k a)

fromPGEnum :: forall a.(Generic a, Typeable a, Enum a, GEnumToMap (Rep a), Show a) => ByteString -> FieldParser a
fromPGEnum _tab f Nothing = returnError UnexpectedNull f ""
fromPGEnum tab f (Just val) = do
  tName <- typename f
  if tName == tab || tName == (ASCII.pack "_") `ASCII.append` tab
    then case L.find (\(_, (_, ename)) -> val == ASCII.pack ename) $ enumToMap (Proxy :: Proxy a)  of
          Just (en,_) -> return en
          _         -> returnError ConversionFailed f (show val)
    else returnError Incompatible f ("Wrong database type for " ++ (show $ (typeRep (Proxy :: Proxy a), tab)) ++ ", saw: " ++ show tName)

enumToMap :: (Generic a, Enum a, GEnumToMap (Rep a), Show a) => Proxy a -> [(a, (Int, String))]
enumToMap a = let kvs = gEnumToMap (prep a)
                  prep :: Proxy a -> Proxy (Rep a a)
                  prep = const Proxy
              in fmap (\(fa, _cname) -> let a' = to fa in (a', (fromEnum a', show a'))) kvs

class GEnumToMap f where
  gEnumToMap :: Proxy (f a) -> [(f a, String)]
instance (GEnumToMap f) => GEnumToMap (D1 c f) where
  gEnumToMap _p = fmap (\(fa, n) -> (M1 fa, n)) $ gEnumToMap Proxy
instance (GEnumToMap f, GEnumToMap g) => GEnumToMap (f :+: g) where
  gEnumToMap _p = let l1 = fmap (\(fa, n) -> (L1 fa, n)) $ gEnumToMap Proxy
                      r1 = fmap (\(fa, n) -> (R1 fa, n)) $ gEnumToMap Proxy
                  in l1 ++ r1
instance (GEnumToMap f, Constructor c) => GEnumToMap (C1 c f) where
  gEnumToMap _p = let cname = conName (undefined :: (C1 c f) a)                      
                  in case gEnumToMap Proxy of
                       [(con, _)] -> [(M1 con, cname)]
                       _ -> error "Panic: Expected a Singleton" 
instance GEnumToMap U1 where
  gEnumToMap _ = [(U1, "")]
