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
{-# LANGUAGE DuplicateRecordFields      #-}

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
import qualified DBRecord.Internal.Table as DBRI
import           DBRecord.Postgres.Internal.RegClass
import qualified DBRecord.Postgres.Internal.Sql.Pretty as PG
-- import           DBRecord.Old.Query
import           DBRecord.Types
import           DBRecord.Driver
 -- TODO: Internal Modules
import           DBRecord.Internal.Types
import           DBRecord.Internal.DBTypes
-- import           DBRecord.Internal.Expr
import           Data.Functor.Identity
import qualified Data.Pool as P
import           Data.String
import           Database.PostgreSQL.Simple as PGS
import           Database.PostgreSQL.Simple.Types (PGArray (..))
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow as PGS
import qualified Database.PostgreSQL.Simple.Internal as PGSInt
import qualified UnliftIO as U
import           Data.Kind
import           GHC.Generics
import           GHC.TypeLits as GHC
import qualified Data.Aeson as A
import           Data.Proxy
import           Data.Int
import qualified Data.List as L
import qualified Data.ByteString.Char8 as Char8
import           Data.Typeable
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Control.Monad.Trans.State.Strict

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

newtype AnnEntity (dbobj :: DBObjK) (isAuto :: Bool) (meta :: Type) a = AnnEntity {getEntity :: a}
  deriving Functor

class FromRowGen a where
  fromRowGen :: RowParser a

instance FromRow (AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) () a) => FromRowGen a where
  fromRowGen = getEntity <$> fromRow @(AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) () a)

instance (FromField a) => FromRow (AnnEntity ('NativeTypeObj dbk) auto meta a) where
  fromRow = AnnEntity <$> field
  {-# INLINE fromRow #-}

instance (FromField a) => FromRow (AnnEntity ('NullableObjOf a ('NativeTypeObj dbk)) auto meta (Maybe a)) where
  fromRow = AnnEntity <$> field
  {-# INLINE fromRow #-}

instance (Generic a, GFromRowOpt (Rep a)) => FromRow (AnnEntity ('NullableObjOf a 'TableObj) 'True meta (Maybe a)) where
  fromRow = (AnnEntity . fmap to) <$> gfromRowOpt @(Rep a)
  {-# INLINE fromRow #-}

instance (FromField a, Typeable a) => FromRow (AnnEntity ('NullableObjOf [a] ('ArrayObjOf arrElt ('NativeTypeObj eldbk))) 'True meta (Maybe [a])) where
  fromRow = AnnEntity <$> (fieldWith $ optionalField (\v cn -> fromPGArray <$> (fromField v cn)))
  {-# INLINE fromRow #-}

instance (Generic a, GFromRowOpt (Rep a)) => FromRow (AnnEntity ('ArrayObjOf a 'TableObj) 'True meta (V.Vector a)) where
  fromRow = (AnnEntity . maybe V.empty V.singleton . getEntity) <$> fromRow @(AnnEntity ('NullableObjOf a 'TableObj) 'True meta (Maybe a))
  {-# INLINE fromRow #-}

instance (FromField a, Typeable a) => FromRow (AnnEntity ('ArrayObjOf a ('NativeTypeObj dbk)) auto meta [a]) where
  fromRow = AnnEntity <$> (fromPGArray <$> fieldWith fromField)
  {-# INLINE fromRow #-}

instance (Generic a, GFromRow (Rep a)) => FromRow (AnnEntity 'TableObj 'True meta a) where
  fromRow = (AnnEntity . to) <$> gfromRow @(Rep a)
  {-# INLINE fromRow #-}

instance (FromRow a) => FromRow (AnnEntity 'TableObj 'False meta a) where
  fromRow = AnnEntity <$> fromRow @a
  {-# INLINE fromRow #-}

data SupportType (sup :: DBSupportK)

-- TODO: Complete the following instance impl
instance (Generic t, GFromRow (Rep t)) => FromRow (AnnEntity ('UDTypeObj ('UDRec 'FlatRec)) 'True (SupportType 'Synthesized) t) where
  fromRow = (AnnEntity . to) <$> gfromRow @(Rep t)

instance (Generic t, GFromSumOfRow (Rep t), MatchEnumTag enk, DBRepr 'Postgres t) => FromRow (AnnEntity ('UDTypeObj ('TaggedSum enk 'FlatRec)) 'True (SupportType 'Synthesized) t) where
  fromRow = do
    ctag <- field
    let mat cn = matchEnumTag (Proxy @enk) ctag (conAliases @'Postgres @t) cn
    ((fmap to) <$> gFromSumOfRow @(Rep t) mat) >>= \case
      Nothing -> error "Panic: [DBR-123]: Atleast one of the constructor should match"
      Just r -> pure $ AnnEntity r


instance ( Generic t, GFromSumOfRow (Rep t), MatchEnumTag enk, DBRepr 'Postgres t
         ) => FromRow (AnnEntity ('UDTypeObj ('TaggedSumMono enk ct 'FlatRec)) 'True (SupportType 'Synthesized) t) where
  fromRow = do
    ctag <- field
    let mat cn = matchEnumTag (Proxy @enk) ctag (conAliases @'Postgres @t) cn
    ((fmap to) <$> gFromSumOfRow @(Rep t) mat) >>= \case
      Nothing -> error "Panic: [DBR-123]: Atleast one of the constructor should match"
      Just r -> pure $ AnnEntity r

instance (Generic t, GFromSumOfRow (Rep t)) => FromRow (AnnEntity ('UDTypeObj ('SumOfCol 'FlatRec)) 'True (SupportType 'Synthesized) t) where
  fromRow = ((fmap to) <$> gFromSumOfRow @(Rep t) (const True)) >>= \case
    Nothing -> error "Panic: [DBR-123]: Atleast one of the constructor should match"
    Just r -> pure $ AnnEntity r

instance (UDFromField t udRep) => FromRow (AnnEntity ('UDTypeObj udRep) 'True (SupportType 'Native) t) where
  fromRow = AnnEntity <$> fieldWith (udFromField @t (Proxy @udRep))

instance (FromRow (AnnEntity ('UDTypeObj udRep) 'True (SupportType (GetDBSupportOf udRep)) t)) => FromRow (AnnEntity ('UDTypeObj udRep) 'True () t) where
  fromRow = fmap (AnnEntity . getEntity) $ fromRow @(AnnEntity ('UDTypeObj udRep) 'True (SupportType (GetDBSupportOf udRep)) t)

instance (UDFromField t udRep) => FromRow (AnnEntity ('NullableObjOf t ('UDTypeObj udRep)) 'True meta (Maybe t)) where
  fromRow = AnnEntity <$> fieldWith (optionalField $ udFromField @t (Proxy @udRep))

instance (UDFromField t udRep, Typeable t) => FromRow (AnnEntity ('ArrayObjOf t ('UDTypeObj udRep)) 'True meta [t]) where
  fromRow = AnnEntity <$> fieldWith (\v cn -> fromPGArray <$> pgArrayFieldParser (udFromField @t (Proxy @udRep)) v cn)

instance (FromField t) => FromRow (AnnEntity ('UDTypeObj udRep) 'False meta t) where
  fromRow = AnnEntity <$> fieldWith (fromField @t)

class UDFromField (t :: Type) (udtMap :: UDTypeK) where
  udFromField :: Proxy udtMap -> FieldParser t

instance (Typeable t, DBRepr 'Postgres t, Matcher 'Postgres t ~ 'EnumMatcher t, ParseEnum enk) => UDFromField t ('UDEnum enk) where
  udFromField _ fld =
    let
      expTyName = _getTypeName (typeName @'Postgres @t)
      EnumMatchRep {ctors = cs} = sumRepr (Proxy @'( 'Postgres, t))
    in \case
      Nothing -> returnError UnexpectedNull fld ""
      Just val' -> do
        actTyName <- typename fld
        if actTyName == T.encodeUtf8 expTyName
          then do
          case parseEnum (Proxy @enk) val' (conAliases @'Postgres @t) cs of
            Left ex -> returnError Incompatible fld (show ex)
            Right ct -> pure ct
          else returnError Incompatible fld (L.concat ["Expected: "
                                                      , T.unpack expTyName
                                                      , ", Actual: "
                                                      , Char8.unpack actTyName
                                                      ])

class ParseEnum (enk :: UDEnumK) where
  parseEnum :: Proxy enk -> ByteString -> ConAliases 'Postgres t -> [(T.Text, t)] -> Either String t

instance ParseEnum 'EnumType where
  parseEnum _ bs caliases ctors = case T.decodeUtf8' bs of
    Left ex -> Left $ show ex
    Right ev -> case L.find (\(cn, _ct) -> ev == lookupConText cn caliases) ctors of
      Nothing -> Left $ "[DBR-123] Enum value not matched! Value: " ++ (T.unpack ev) ++ " Expecting one of: " ++ (show $ fmap (\(cn, _) -> lookupConText cn caliases) ctors)
      Just (_, ct) -> Right ct


instance ParseEnum 'EnumText where
  parseEnum _ = parseEnum (Proxy @'EnumType)

instance ParseEnum 'EnumNum where
  parseEnum _ bs caliases ctors = case parseInt8 bs of
    Left ex -> error ex
    -- TODO: Fix h.c
    Right ev -> case L.find (\(cn, _ct) -> ev == lookupConNum cn 0 caliases) ctors of
      Nothing -> Left $ "[DBR-123] Enum value not matched! Value: " ++ (show ev) ++ " Expecting one of: " ++ (show $ fmap (\(cn, _) -> lookupConText cn caliases) ctors)
      Just (_, ct) -> Right ct

class MatchEnumTag (enk :: UDEnumK) where
  matchEnumTag :: Proxy enk -> ByteString -> ConAliases 'Postgres t -> T.Text -> Bool

instance MatchEnumTag 'EnumType where
  matchEnumTag _ bs caliases cn = case T.decodeUtf8' bs of
    Left ex -> error $ show ex
    Right ev -> ev == lookupConText cn caliases

instance MatchEnumTag 'EnumText where
  matchEnumTag _ = matchEnumTag (Proxy @'EnumType)

instance MatchEnumTag 'EnumNum where
  matchEnumTag _ bs caliases cn = case parseInt8 bs of
    Left ex -> error ex
    -- TODO: Fix h.c
    Right ev -> ev == lookupConNum cn 0 caliases

parseInt8 :: ByteString -> Either String Int64
parseInt8 bs = Atto.parseOnly (Atto.signed Atto.decimal) bs
--

rowToCompositeFieldParser :: RowParser a -> FieldParser a
rowToCompositeFieldParser (PGSInt.RP rp) = \fld bs -> do
  let
    PGSInt.Field {result = res, column = _col} = fld
    r = PGSInt.Row {row = 0, rowresult = res}
  maybe (pure ()) (PGSInt.liftConversion . putStrLn . Char8.unpack) $ bs
  evalStateT (runReaderT rp r) 0

instance (Generic t, GFromRow (Rep t)) => UDFromField t ('UDRec 'CompositeRec) where
  udFromField _ = rowToCompositeFieldParser $ to <$> gfromRow @(Rep t)

instance (TypeError ('GHC.Text "TODO: UDRec for JsonRec")) => UDFromField t ('UDRec 'JsonRec) where
  udFromField = error "TODO"

instance (A.FromJSON t, Typeable t) => UDFromField t ('SerializedBlob ('JsonContent 'Nothing)) where
  udFromField _ = fromJSONField

instance UDFromField t ('TaggedSum enk 'CompositeRec) where
  udFromField = undefined

instance (TypeError ('GHC.Text "TODO: UDRec for JsonRec")) => UDFromField t ('TaggedSum enk 'JsonRec) where
  udFromField = error "TODO"

instance UDFromField t ('TaggedSumMono enk ct 'CompositeRec) where
  udFromField = undefined

instance (TypeError ('GHC.Text "TODO: UDRec for JsonRec")) => UDFromField t ('TaggedSumMono enk ct 'JsonRec) where
  udFromField = error "TODO"

instance UDFromField t ('SumOfCol 'CompositeRec) where
  udFromField = undefined

-- Type class for default implementation of FromRow using generics
-- TODO: Uses Fields of DBRepr and HasField
class GFromRow f where
    gfromRow :: RowParser (f p)

instance GFromRow f => GFromRow (M1 c i f) where
    gfromRow = M1 <$> gfromRow

instance (GFromRow f, GFromRow g) => GFromRow (f :*: g) where
    gfromRow = liftA2 (:*:) gfromRow gfromRow

instance (FromRow (AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) () a)) => GFromRow (K1 R a) where
    gfromRow = (K1 . getEntity) <$> fromRow @(AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) () a)

instance GFromRow U1 where
    gfromRow = pure U1


class GFromRowOpt f where
  gfromRowOpt :: RowParser (Maybe (f p))

instance GFromRowOpt f => GFromRowOpt (M1 c i f) where
  gfromRowOpt = (fmap M1) <$> gfromRowOpt

instance (GFromRowOpt f, GFromRowOpt g) => GFromRowOpt (f :*: g) where
  gfromRowOpt = liftA2 (\l r -> liftA2 (:*:) l r) gfromRowOpt gfromRowOpt

instance (FromRow (AnnEntity ('NullableObjOf a (ToDBType 'Postgres a)) (AutoCodec 'Postgres a) () (Maybe a))) => GFromRowOpt (K1 k a) where
  gfromRowOpt = (fmap K1 . getEntity) <$> fromRow @(AnnEntity ('NullableObjOf a (ToDBType 'Postgres a)) (AutoCodec 'Postgres a) () (Maybe a))

instance GFromRowOpt U1 where
  gfromRowOpt = pure $ Just U1

class GFromSumOfRow f where
  gFromSumOfRow :: (T.Text -> Bool) -> RowParser (Maybe (f p))

instance GFromSumOfRow f => GFromSumOfRow (D1 d f) where
  gFromSumOfRow mat = (fmap M1) <$> gFromSumOfRow mat

instance (GFromSumOfRow f, GFromSumOfRow g) => GFromSumOfRow (f :+: g) where
  gFromSumOfRow mat = liftA2 (\l r -> asum [L1 <$> l, R1 <$> r]) (gFromSumOfRow @f mat) (gFromSumOfRow mat)

-- data Nullified a(t :: Type) = Nullified

-- fromNullified :: Nullified t -> Maybe t
-- fromNullified _ = Nothing

instance ( KnownSymbol cn
         , FromRow (AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) () a)
         , FromRow (AnnEntity ('NullableObjOf a (ToDBType 'Postgres a)) (AutoCodec 'Postgres a) () (Maybe a))
         ) => GFromSumOfRow (C1 ('MetaCons cn p isr) (S1 s (K1 k a))) where
  gFromSumOfRow mat =
    let cn = T.pack $ symbolVal (Proxy @cn)
    in if mat cn
       then ((Just . M1 . M1 . K1 . getEntity)) <$> fromRow @(AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) () a)
       else (fmap (M1 . M1 . K1) . getEntity) <$> fromRow @(AnnEntity ('NullableObjOf a (ToDBType 'Postgres a)) (AutoCodec 'Postgres a) () (Maybe a))
    -- TODO: Skipped value should only be NULL otherwise, follwing error should be thrown
    -- TODO: "Panic: [DBR-123]: Constructor " ++ (T.unpack cn) ++ " when not matched, it's argument should be null"


instance (KnownSymbol cn) => GFromSumOfRow (C1 ('MetaCons cn p isr) U1) where
  gFromSumOfRow mat =
    let cn = T.pack $ symbolVal (Proxy @cn)
    in if mat cn
       then pure $ Just $ M1 U1
       else pure Nothing


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
#if MIN_VERSION_resource_pool(0,4,0)
  P.newPool (P.defaultPoolConfig (PGS.connect connectInfo) PGS.close 5 10)
#elif MIN_VERSION_resource_pool(0,3,0)
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

showQuery :: DBRI.Query' qt sc r -> String
showQuery q = PG.renderQuery $ PG.sql $ DBRI.execQuery q
