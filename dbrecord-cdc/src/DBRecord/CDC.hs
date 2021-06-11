{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ConstraintKinds            #-}
module DBRecord.CDC where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Maybe
import Data.Proxy
import Data.Bits
import Data.Typeable
import GHC.Generics
import GHC.Exts
import GHC.TypeLits
import GHC.Records
import qualified GHC.Records as GRec
import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple as PGS hiding (query)
import qualified Database.PostgreSQL.Simple.FromField as PG
import           Database.PostgreSQL.Simple.FromField hiding ( Text )
import           Database.PostgreSQL.Simple.FromRow
import qualified Database.PostgreSQL.Simple.FromRow as PG
import Record
import qualified Data.TMap as TMap
import qualified Data.TypeRepMap as TRMap
import qualified Data.HashMap.Strict as HM
import qualified StmContainers.Map as STM
import           Control.Concurrent ( threadDelay )
import           Control.Concurrent.Async ( Async, async, link, AsyncCancelled )
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Vector ( Vector )
import qualified Data.Vector as V

import           DBRecord.Internal.Common
import qualified DBRecord.Internal.PrimQuery as PQ
import qualified DBRecord.Internal.Query as Q
import           DBRecord.Internal.Schema hiding ( TableName )
import           DBRecord.Internal.Types hiding ( Field )
import           DBRecord.Postgres.Internal.Query
import           DBRecord.Query as Q ( update, get, withPrimaryKey, (.==), (.~), constExpr )
import           DBRecord.Query hiding ( project )
import qualified DBRecord.Schema as DBS
import qualified ListT as ListT
import Data.List
import Data.Kind
import qualified Focus as F
import Text.Printf

class CDC sc where
  type CDCTables sc :: [Type]

data CDCTable = CDCTable
  { cdcTableId :: Int
  , sourceSchema :: Text -- regclass
  , sourceTable :: Text -- regclass
  , accessRole :: Text
  , indexName :: Text -- regclass
  , createdAt :: UTCTime
  } deriving (Show, Eq)

data CDCCapturedColumn = CDCCapturedColumn
  { cdcTableId :: Int
  , columnName :: Text  -- regclass
  , columnType :: Text  -- regclass
  , columnOrdinal :: Int
  , createdAt :: UTCTime  
  } deriving (Show, Eq)

data CDCDDLHistory = CDCDDLHistory
  { cdcTableId :: Int
  , requiredColumnUpdate :: Bool
  , ddlCommand :: Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  } deriving (Show, Eq)


data CdcType =
    Inserted
  | Deleted
  | OldUpdated  
  | NewUpdated
  deriving stock (Show, Eq, Ord, Generic)

-- TODO: -> CdcData
data RawCdcData a = RawCdcData
  { rawCdcId :: Int
  , rawCdcType :: CdcType
  , rawCdcBitmask :: Int
  , rawCdcCreatedAt :: UTCTime
  , rawCdcData :: a
  } deriving (Show)

-- TODO: -> CdcChangeSet
data CdcData a =
    CdcUpdate { cdcDataOld :: a, cdcDataNew :: a }
  | CdcInsert { cdcData :: a }
  | CdcDelete { cdcData :: a }
  deriving (Functor, Show)

data CDCSubscription = CDCSubscription
  { cdcTableId :: Int
  , subscriptionId :: Int
  , subscriptionName :: Text
  , columnOrdinals :: Text
  , isSequential :: Bool
  , retryNegAckedSequentially :: Bool
  , lastestPositiveAckDataId :: Maybe Int
  , lastestNegativeAckDataId :: Maybe Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  } deriving (Show, Eq)

data CDCSubscriberCommit = CDCSubscriberCommit
  { subscriptionId :: Int
  , currentDataId :: Int
  , startDataId :: Maybe Int
  , positiveAck :: Bool
  , negativeAck :: Bool
  , createdAt :: UTCTime
  } deriving (Show, Eq)


instance FromField CdcType where
  fromField = cdcTypePGFromField

cdcTypePGFromField :: PG.FieldParser CdcType
cdcTypePGFromField f mdata = do
    n <- PG.typename f
    if n /= "cdc_type"
    then PG.returnError PG.Incompatible f ""
    else case mdata of
        Nothing -> PG.returnError PG.UnexpectedNull f ""
        Just bs -> case parseCdcType bs of
            Nothing -> PG.returnError PG.ConversionFailed f (show bs)
            Just x  -> return x

parseCdcType :: ByteString -> Maybe CdcType
parseCdcType "Inserted" = Just Inserted
parseCdcType "Deleted" = Just Deleted
parseCdcType "NewUpdated" = Just NewUpdated
parseCdcType "OldUpdated" = Just OldUpdated

parseCdcData :: Vector (RawCdcData a) -> Vector (CdcData a)
parseCdcData = uncurry pairUpdates . foldr go ([], HM.empty)
  where go v@(RawCdcData { rawCdcType = Inserted }) (cdcs, upds) = (toInsertedCdc v : cdcs, upds)
        go v@(RawCdcData { rawCdcType = Deleted }) (cdcs, upds) = (toDeletedCdc v : cdcs, upds)
        go v@(RawCdcData { rawCdcType = NewUpdated, rawCdcId = id0 }) (cdcs, upds) =
          (cdcs, HM.insertWith (<>) id0 [v] upds) 
        go v@(RawCdcData { rawCdcType = OldUpdated, rawCdcId = id0 }) (cdcs, upds) =
          (cdcs, HM.insertWith (<>) id0 [v] upds)

        toInsertedCdc RawCdcData { rawCdcData = cdcData } = CdcInsert { cdcData }
        toDeletedCdc RawCdcData { rawCdcData = cdcData } = CdcDelete { cdcData }        

        pairUpdates :: [CdcData a] -> HM.HashMap Int [RawCdcData a] -> Vector (CdcData a)
        pairUpdates cdcs =
          V.fromList . (cdcs ++) . HM.foldrWithKey go0 []

          where
            go0 k vs acc =
              case sortBy (\a1 a2 -> compare (rawCdcType a1) (rawCdcType a2)) vs of
                [old, new] -> pure (CdcUpdate { cdcDataOld = rawCdcData old
                                              , cdcDataNew = rawCdcData new
                                              }) <> acc
                _          -> error $ "Panic: Invariant violated @parseCdcData. unexpected entry @ key: " ++ show k ++ " : " ++ show (map rawCdcId vs)



instance ( FromRowHK t (GenFields t (Rep t))
         ) => FromRow (RawCdcData (HK Maybe t)) where
  fromRow = do
    rawCdcId <- field
    rawCdcType <- field
    rawCdcBitmask <- field
    rawCdcCreatedAt <- field
    rawCdcData <- fromRowHK rawCdcBitmask
    pure (RawCdcData { .. })


fromRowHK :: forall f t. (FromRowHK t (GenFields t (Rep t))) => Int -> RowParser (HK Maybe t)
fromRowHK bitmask = fromRowHK' (Proxy @(GenFields t (Rep t))) bitmask 0 (pure (HK TRMap.empty))

class FromRowHK t (fcs :: [Constraint]) where
  fromRowHK' :: Proxy fcs -> Int -> Int -> RowParser (HK Maybe t) -> RowParser (HK Maybe t)

instance FromRowHK t '[] where
  fromRowHK' _ _ _ hkt = hkt

instance ( r ~ t
         , FromRowHK t fcs
         , KnownSymbol fld
         , Typeable a
         , FromField a
         , NullUpdater (IsMaybe a) a
         ) => FromRowHK t (GRec.HasField (fld :: Symbol) r a ': fcs) where
  fromRowHK' _ bitmask pos rp = do
    (HK tmap) <- rp
    v <- field @(Maybe a)
    let v0 = updateNull (Proxy :: Proxy (IsMaybe a)) (testBit bitmask pos) v
    fromRowHK' (Proxy @fcs) bitmask (pos + 1) (pure (HK $ TRMap.insert (Field @fld <$> v0) tmap))

type family IsMaybe a where
  IsMaybe (Maybe _) = 'True
  IsMaybe _         = 'False

class NullUpdater (isM :: Bool) a where
  updateNull :: Proxy isM -> Bool -> Maybe a -> Maybe a

instance NullUpdater 'True (Maybe a) where
  updateNull _ True Nothing = Just Nothing
  updateNull _ False Nothing = Nothing
  updateNull _ _ (Just a) = Just a

instance NullUpdater 'False a where
  updateNull _ _ a = a


---

data Manager = 
  Manager { handlerMap :: STM.Map TableName TableHandler
          , pollerThread :: Async ()
          }

data Command =
    RegisterHandler Manager
  | RemoveHandler HandlerRef

type ManagerCtx sc tab (cols :: [Symbol]) keys scopes rets =
  ( Table sc tab
  , SingCtx sc tab
  , SingCtxSc sc
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , Generic tab
  , Break (NoGeneric tab) (Rep tab)
  , Break0 (NoSchema sc) (SchemaDB sc)
  , SingI cols
  , SingE cols
  , FromRowHK tab (GenFields tab (Rep tab))
  , SingE (ColumnNames sc tab)
  , SingI (ColumnNames sc tab)
  , SingE (OriginalTableFieldInfo sc tab)
  , SingI (OriginalTableFieldInfo sc tab)
  , DistSubHK cols Maybe (Sub (HK Maybe tab) cols)
  , Project (HK Maybe tab) cols
  , Typeable tab
  , Typeable sc
  , Eq (Sub tab cols)  
  )

registerHandler :: forall sc tab (cols :: [Symbol]) keys scopes rets.
  ( ManagerCtx sc tab cols keys scopes rets
  ) =>
  Handler sc tab cols ->
  Manager             ->
  IO HandlerRef
registerHandler hdl (Manager { handlerMap }) = do
  let qtab = Q.getTableId (Proxy @sc) (Proxy @tab)
      cdcTabHandler = TableHandler (pure (Handler' hdl))
  atomically $ STM.focus (F.insertOrMerge mergeTableHandler' cdcTabHandler) (PQ.tableName qtab) handlerMap
  -- TODO: generate
  pure (HandlerRef { getHandlerRef = 0 })
    
removeHandler ::
  HandlerRef ->
  Manager    ->
  IO ()
removeHandler = undefined
  

type TableName = Text

-- | A Handler is invoked when a CDC entry is made, if it was registered previously.
newtype Handler sc tab cols = Handler { getHandler :: Vector (CdcData (Sub tab cols)) -> IO () }

-- | A reference to a Handler.
newtype HandlerRef = HandlerRef { getHandlerRef :: Int {- TODO: UUID -} }
                   deriving (Show, Eq)

data Handler' sc tab where
  Handler' :: ( DistSubHK cols Maybe (Sub (HK Maybe tab) cols)
              , Project (HK Maybe tab) cols
              , Eq (Sub tab cols)
              ) => Handler sc tab cols  -> Handler' sc tab

data TableHandler where
  TableHandler :: ( Table sc tab
                  , FromRowHK tab (GenFields tab (Rep tab))
                  , SingE (ColumnNames sc tab)
                  , SingI (ColumnNames sc tab)
                  , SingE (OriginalTableFieldInfo sc tab)
                  , SingI (OriginalTableFieldInfo sc tab)
                  , Typeable tab
                  , Typeable sc
                  ) => [Handler' sc tab] -> TableHandler

mergeTableHandler' :: TableHandler -> TableHandler -> TableHandler
mergeTableHandler' t1 t2 =
  -- NOTE: This should be an impossible case  
  fromJust (mergeTableHandler t1 t2)

mergeTableHandler :: TableHandler -> TableHandler -> Maybe TableHandler
mergeTableHandler  (TableHandler (hs1 :: [Handler' sc1 t1])) (TableHandler (hs2 :: [Handler' sc2 t2])) = do
  Refl <- eqT @t1 @t2
  Refl <- eqT @sc1 @sc2
  pure (TableHandler (hs1 <> hs2))

-- TODO: maybe callbacky to catch exceptions?
watch :: SessionConfig PGS -> Word -> IO Manager
watch pool delayInSecs = do
  handlerMap <- atomically $ STM.new
  pollerThread <- async $ poller pool delayInSecs handlerMap
  -- listenerThread <- async $ listener 
  pure (Manager { handlerMap, pollerThread }) 

listener :: IO ()
listener = pure ()

poller :: SessionConfig PGS -> Word -> STM.Map TableName TableHandler -> IO ()
poller pool delayInSecs handlerMap = forever $ do
  runTransactionalDB pool pollCdc
  threadDelay (fromIntegral delayInSecs * 1000000)      
  
  where
    pollCdc = do
      vs <- liftIO $ atomically $ ListT.toList (STM.listT handlerMap)
      mapM_ (uncurry pollChangeLogTab) vs

    -- NOTE: Any exception in *one* handler will cause the whole
    --       changeset to be processed again.
    pollChangeLogTab :: TableName -> TableHandler -> PostgresDBT sc IO ()
    pollChangeLogTab tn (TableHandler (handlers :: [Handler' sc tab])) = do
      let cols = headColInfos (Proxy @sc) (Proxy @tab)
      (res :: [RawCdcData (HK Maybe t)]) <- rawQ (fromString $ updateRetQ tn cols)
      forM handlers $ \(Handler' (Handler hdl :: Handler sc tab cols)) -> do
        let fun =
              V.filter filterSameUpdate .
              parseCdcData .
              V.fromList .
              catMaybes .
              map (atCdcData (fromHK . toHKOfSub . project @cols))
        liftIO $ hdl (fun res)
      pure ()

      where
        filterSameUpdate CdcUpdate { cdcDataOld, cdcDataNew }
          = not (cdcDataOld == cdcDataNew)
        
        atCdcData f v@RawCdcData { rawCdcData = a } =
          (\v0 -> v { rawCdcData = v0 }) <$> f a
        updateRetQ tn cols = ""
{-        
        updateRetQ tn cols =
          [i|
              UPDATE
                usfoodimports.#{ tabName tn }
              SET
                cdc_is_read = TRUE
              WHERE
                cdc_is_read = FALSE
              RETURNING
                cdc_id,
                cdc_type,
                cdc_is_read,
                cdc_created_at,
                cdc_bitmask,                
                #{ mkCols cols }
          |]
-}
        mkCols =
          T.intercalate "," . map (_dbName . _columnNameInfo)

        tabName tn =
          "cdc_" <> tn


rawQ :: FromRow a => Query -> PostgresDBT sc IO [a]
rawQ q = do
  (PGS conn) <- ask
  liftIO $ query_ conn q

runTransactionalDB = undefined


cdcMigrationSQL :: Text
cdcMigrationSQL = ""

runCdcMigration :: IO ()
runCdcMigration = do
  pure ()


-- SQL

-- CREATE

createCdcType :: String -> String
createCdcType sc = printf "\
\CREATE TYPE %s.cdc_change_type AS ENUM (\
\'Inserted',\
\'Deleted',\
\'OldUpdated',\
\'NewUpdated'\
\);" sc


createCDCTable :: String -> String
createCDCTable sc = printf "\
\CREATE TABLE IF NOT EXISTS %s.cdc_table (\
\ id SERIAL PRIMARY KEY,\
\ source_schema text NOT NULL,\
\ source_table text NOT NULL,\
\ access_role text NOT NULL,\
\ index_name text NOT NULL,\
\ created_at timestamp with time zone NOT NULL DEFAULT now()\
\);" sc

createCDCCapturedColumn :: String -> String
createCDCCapturedColumn sc = printf "\
\CREATE TABLE IF NOT EXISTS %s.cdc_captured_column (\
\ cdc_table_id SERIAL PRIMARY KEY,\
\ column_name text NOT NULL,\
\ column_type text NOT NULL,\
\ column_ordinal int NOT NULL,\
\ created_at timestamp with time zone NOT NULL DEFAULT now()\
\);" sc

createCDCDDLHistory :: String -> String
createCDCDDLHistory sc = printf "\
\CREATE TABLE IF NOT EXISTS %s.cdc_ddl_history (\
\ cdc_table_id SERIAL PRIMARY KEY,\
\ required_column_update boolean NOT NULL,\
\ ddl_command text NOT NULL,\
\ created_at timestamp with time zone NOT NULL DEFAULT now(),\
\ updated_at timestamp with time zone NOT NULL DEFAULT now()\
\);" sc

createCDCData :: forall db tab.String -> String -> String
createCDCData sc tab = printf "\
\CREATE TABLE IF NOT EXISTS %s.cdc_data_%s (\
\ id SERIAL PRIMARY KEY,\
\ change_type %s.cdc_change_type NOT NULL,\
\ update_mask int NOT NULL,\
\ created_at timestamp with time zone NOT NULL DEFAULT now(), %s\
\);" sc tab sc srcCols
  where
    srcCols :: String
    srcCols = "id1 int NOT NULL"

-- DROP
dropCDCTable :: String -> String
dropCDCTable sc = printf "DROP TABLE IF EXISTS %s.cdc_table;" sc

dropCDCCapturedColumn :: String -> String
dropCDCCapturedColumn sc = printf "DROP TABLE IF EXISTS %s.cdc_captured_column;" sc

dropCDCDDLHistory :: String -> String
dropCDCDDLHistory sc = printf "DROP TABLE IF EXISTS %s.cdc_ddl_history;" sc
    
dropCdcType :: String -> String
dropCdcType sc = printf "DROP TYPE %s.cdc_change_type;" sc

dropCDCData :: String -> String -> String
dropCDCData sc tab = printf "DROP TABLE IF EXISTS %s.cdc_data_%s;" sc tab
