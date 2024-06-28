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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

module DBRecord.Postgres.Internal.Query
       ( module DBRecord.Postgres.Internal.Query
       , module DBRecord.Postgres.Internal.RegClass
       ) where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Control.Monad.Trans.Control as U
import qualified DBRecord.Internal.Sql.SqlGen as PG
import           DBRecord.Internal.Table (MQuery, execMQuery)
import           DBRecord.Postgres.Internal.RegClass
import qualified DBRecord.Postgres.Internal.Sql.Pretty as PG
-- import           DBRecord.Old.Query
import           DBRecord.Types
import           DBRecord.Driver
 -- TODO: Internal Modules
import           DBRecord.Internal.Types
import           DBRecord.Internal.DBTypes
import           DBRecord.Internal.Expr
import           Data.Functor.Identity
import qualified Data.Pool as P
import           Data.String
import           Database.PostgreSQL.Simple as PGS
import           Database.PostgreSQL.Simple.Types (PGArray (..))
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow as PGS
import qualified UnliftIO as U
import           Data.Kind
import           GHC.Generics
import           Data.Proxy
-- import qualified Data.List as L
-- import           Data.ByteString.Char8 as ASCII
import           Data.Typeable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

newtype PostgresDBT (db :: Type) m a = PostgresDBT { runPostgresDB :: ReaderT PGS m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadReader PGS, U.MonadUnliftIO, MonadThrow, MonadCatch)

deriving newtype instance (U.MonadBaseControl IO m, MonadBase IO m) => U.MonadBaseControl IO (PostgresDBT db m)
deriving newtype instance (MonadBase IO m) => MonadBase IO (PostgresDBT db m)

type PostgresDB db = PostgresDBT db IO

instance DBDecoder PGS where
  type FromDBRowParser PGS = RowParser
  type FromDBRow PGS       = FromRowGen
  dbDecoder _ _ = fromRowGen

type instance ToDBRow PGS a = ToRow a

newtype AnnEntity (dbobj :: DBObjK) (isAuto :: Bool) a = AnnEntity {getEntity :: a}

class FromRowGen a where
  fromRowGen :: RowParser a

instance FromRow (AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) a) => FromRowGen a where
  fromRowGen = getEntity <$> fromRow @(AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) a)

instance (FromField a) => FromRow (AnnEntity ('NativeTypeObj dbk) auto a) where
  fromRow = AnnEntity <$> field
  {-# INLINE fromRow #-}

instance (FromField a) => FromRow (AnnEntity ('NullableObjOf a ('NativeTypeObj dbk)) auto (Maybe a)) where
  fromRow = AnnEntity <$> field
  {-# INLINE fromRow #-}

instance (Generic a, GFromRowOpt (Rep a)) => FromRow (AnnEntity ('NullableObjOf a 'TableObj) 'True (Maybe a)) where
  fromRow = (AnnEntity . fmap to) <$> gfromRowOpt @(Rep a)
  {-# INLINE fromRow #-}

instance (FromField a, Typeable a) => FromRow (AnnEntity ('NullableObjOf [a] ('ArrayObjOf arrElt ('NativeTypeObj eldbk))) 'True (Maybe [a])) where
  fromRow = AnnEntity <$> (fieldWith $ optionalField (\v cn -> fromPGArray <$> (fromField v cn)))
  {-# INLINE fromRow #-}

instance (Generic a, GFromRowOpt (Rep a)) => FromRow (AnnEntity ('ArrayObjOf a 'TableObj) 'True (V.Vector a)) where
  fromRow = (AnnEntity . maybe V.empty V.singleton . getEntity) <$> fromRow @(AnnEntity ('NullableObjOf a 'TableObj) 'True (Maybe a))
  {-# INLINE fromRow #-}

instance (FromField a, Typeable a) => FromRow (AnnEntity ('ArrayObjOf a ('NativeTypeObj dbk)) auto [a]) where
  fromRow = AnnEntity <$> (fromPGArray <$> fieldWith fromField)
  {-# INLINE fromRow #-}

instance (Generic a, GFromRow (Rep a)) => FromRow (AnnEntity 'TableObj 'True a) where
  fromRow = (AnnEntity . to) <$> gfromRow @(Rep a)
  {-# INLINE fromRow #-}

instance (FromRow a) => FromRow (AnnEntity 'TableObj 'False a) where
  fromRow = AnnEntity <$> fromRow @a
  {-# INLINE fromRow #-}

-- TODO: Complete the following instance impl
instance (UDFromField t udRep) => FromRow (AnnEntity ('UDTypeObj udRep) 'True t) where
  fromRow = AnnEntity <$> fieldWith (udFromField @t (Proxy @udRep))

instance (UDFromField t udRep) => FromRow (AnnEntity ('NullableObjOf t ('UDTypeObj udRep)) 'True (Maybe t)) where
  fromRow = AnnEntity <$> fieldWith (optionalField $ udFromField @t (Proxy @udRep))

instance (UDFromField t udRep, Typeable t) => FromRow (AnnEntity ('ArrayObjOf t ('UDTypeObj udRep)) 'True [t]) where
  fromRow = AnnEntity <$> fieldWith (\v cn -> fromPGArray <$> pgArrayFieldParser (udFromField @t (Proxy @udRep)) v cn)

instance (FromField t) => FromRow (AnnEntity ('UDTypeObj udRep) 'False t) where
  fromRow = AnnEntity <$> fieldWith (fromField @t)

class UDFromField (t :: Type) (udtMap :: UDTypeK) where
  udFromField :: Proxy udtMap -> FieldParser t

instance (Typeable t, DBRepr 'Postgres t) => UDFromField t ('UDEnum enk) where
  udFromField _ fld =
    let udTyN = _getTypeName (typeName @'Postgres @t)
    in \case
      Nothing -> returnError UnexpectedNull fld ""
      Just val' -> case T.decodeUtf8' val' of
        Left ex -> returnError Incompatible fld (show ex)
        Right _cn -> do
          tName <- typename fld
          if tName == T.encodeUtf8 udTyN
            then undefined
            else returnError Incompatible fld ("Expected: " ++ (T.unpack udTyN) ++ ", Actual: " ++ show tName)

instance UDFromField t ('TaggedSumMono enk ct 'FlatRec) where
  udFromField = undefined
  

{-
instance (SingI tyAliasM, SingE tyAliasM, SingI conAliases, SingE conAliases, Typeable t, Generic t, GFromEnum (Rep t)) => UDFromField t ('EnumType tyAliasM conAliases) where
  udFromField _ f =
    let
      tyAliasM = fromSing (sing :: Sing tyAliasM)
      conAliases = HM.fromList $ fmap (\(k,v) -> (v,k)) $ fromSing (sing :: Sing conAliases)
      tab = T.encodeUtf8 $ maybe (T.pack $ show $ typeRep (Proxy @t)) id tyAliasM
    in \case
      Nothing -> returnError UnexpectedNull f ""
      Just val' -> case T.decodeUtf8' val' of
        Left ex -> returnError Incompatible f (show ex)
        Right val -> do
          tName <- typename f
          if tName == tab || tName == (ASCII.pack "_") `ASCII.append` tab
            then case HM.lookup val conAliases >>= genFromEnum (Proxy @t) of
                   Just en -> return en
                   _       -> returnError ConversionFailed f (show val)
            else returnError Incompatible f ("Wrong database type for " ++ (show $ (typeRep (Proxy :: Proxy t), tab)) ++ ", saw: " ++ show tName)

genFromEnum :: forall t. (Generic t, GFromEnum (Rep t)) => Proxy t -> T.Text -> Maybe t
genFromEnum _ con = to <$> gFromEnum (Proxy @(Rep t)) con

class GFromEnum (f :: Type ->Type) where
  gFromEnum :: Proxy f -> T.Text -> Maybe (f a)

instance GFromEnum f => GFromEnum (D1 c f) where
  gFromEnum _ con = M1 <$> gFromEnum (Proxy @f) con

instance (GFromEnum f, GFromEnum g) => GFromEnum (f :+: g) where
  gFromEnum _ con = (L1 <$> gFromEnum (Proxy @f) con) <|>
                    (R1 <$> gFromEnum (Proxy @g) con)

instance (Constructor c) => GFromEnum (C1 c U1) where
  gFromEnum _ con
    | con == (T.pack $ conName (undefined :: (C1 c f) a)) = Just (M1 U1)
    | otherwise = Nothing
-}

-- Type class for default implementation of FromRow using generics
class GFromRow f where
    gfromRow :: RowParser (f p)

instance GFromRow f => GFromRow (M1 c i f) where
    gfromRow = M1 <$> gfromRow

instance (GFromRow f, GFromRow g) => GFromRow (f :*: g) where
    gfromRow = liftA2 (:*:) gfromRow gfromRow

instance (FromRow (AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) a)) => GFromRow (K1 R a) where
    gfromRow = (K1 . getEntity) <$> fromRow @(AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) a)

instance GFromRow U1 where
    gfromRow = pure U1


class GFromRowOpt f where
    gfromRowOpt :: RowParser (Maybe (f p))

instance GFromRowOpt f => GFromRowOpt (M1 c i f) where
    gfromRowOpt = (fmap M1) <$> gfromRowOpt

instance (GFromRowOpt f, GFromRowOpt g) => GFromRowOpt (f :*: g) where
    gfromRowOpt = liftA2 (\l r -> liftA2 (:*:) l r) gfromRowOpt gfromRowOpt

instance (FromRow (AnnEntity ('NullableObjOf a (ToDBType 'Postgres a)) (AutoCodec 'Postgres a) (Maybe a))) => GFromRowOpt (K1 R a) where
    gfromRowOpt = (fmap K1 . getEntity) <$> fromRow @(AnnEntity ('NullableObjOf a (ToDBType 'Postgres a)) (AutoCodec 'Postgres a) (Maybe a))

instance GFromRowOpt U1 where
    gfromRowOpt = pure $ Just U1

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

instance HasDeleteRet PGS where
  dbDeleteRetWith parser (PGS conn) deleteQ = do
    let delSQL = PG.renderDelete $ PG.deleteSql $ deleteQ
    queryWith_ parser conn (fromString delSQL)

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

instance (FromField v) => FromField (Key (t :: k) v) where
  fromField f m = Key <$> fromField f m

instance (FromField a) => FromRow (Identity a) where
  fromRow = Identity <$> field

-- | Implementation based on MonadUnliftIO
withResource :: (U.MonadUnliftIO m) => P.Pool a -> (a -> m r) -> m r
withResource p k = U.withRunInIO $ \f -> P.withResource p (\a -> f $ k a)

showMQuery :: MQuery sc r -> String
showMQuery mQ = execMQuery
  (PG.renderInsert . PG.insertSql)
  (PG.renderUpdate . PG.updateSql)
  (PG.renderDelete . PG.deleteSql)
  "" mQ
