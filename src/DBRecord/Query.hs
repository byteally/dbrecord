{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE ExplicitForAll          #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses           #-}
{-# LANGUAGE FunctionalDependencies          #-}

-- | 

module DBRecord.Query
       ( module DBRecord.Internal.Order
       , module DBRecord.Internal.Expr
       , module DBRecord.Internal.Predicate
       , get, getBy, getAll
       , delete
       , insert, insert_, insertMany, insertMany_, insertRet, insertManyRet
       , update, update_
       , count
       , (.~) , (%~)
       , DBM
       , Driver
       , PGS(..)
       , withResource
       , runSession, SessionConfig(..)
       , runTransaction
       , Page(..)
       ) where

import DBRecord.Schema

import DBRecord.Internal.Order
import DBRecord.Internal.Expr
import DBRecord.Internal.Predicate
import DBRecord.Internal.Common
import DBRecord.Internal.Schema hiding (insert, delete)
import DBRecord.Internal.PrimQuery  hiding (insertQ, updateQ, deleteQ)
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Types
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Int
import GHC.Exts
import GHC.TypeLits
import Data.Functor.Identity
import qualified DBRecord.Internal.Postgres.SqlGen as PG
import qualified DBRecord.Internal.Postgres.Pretty as PG
import qualified DBRecord.Transaction as PG
import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.PostgreSQL.Simple as PGS
import Database.PostgreSQL.Simple.FromRow as PGS
import Data.Pool (withResource)
import DBRecord.Internal.Lens ((^.))

import GHC.Generics

type family DBM (db :: *) = (r :: * -> *) | r -> db

type family Driver (dbm :: * -> *) = (r :: * -> *) | r -> dbm


type family MkPredFn (tab :: *) (args :: [Symbol]) (val :: *) (flds :: [*]) :: [Either ErrorMessage *] where
  MkPredFn tab (fn ': fs) chkFun flds = Note (ColNotFoundMsg fn tab) (FMapMaybe (Expr flds) (FindField flds fn)) ': MkPredFn tab fs chkFun flds
  MkPredFn tab '[] r flds = '[ 'Right (Expr flds Bool)]

  
type family UnifyPkPredicate (db :: *) (tab :: *) (pred :: *) :: Constraint where
  UnifyPkPredicate db tab pred = UnifyOrErr (SeqEither (MkPredFn tab (PrimaryKey db tab) pred (OriginalTableFields tab))) pred

type family UnifyUqPredicate (db :: *) (tab :: *) (pred :: *) (uniqs :: Either ErrorMessage [Symbol]) :: Constraint where
  UnifyUqPredicate db tab _ ('Left err) = TypeError err
  UnifyUqPredicate db tab pred ('Right uniqs) = UnifyOrErr (SeqEither (MkPredFn tab uniqs pred (OriginalTableFields tab))) pred


data Page = Offset Int | Limit Int | OffsetLimit Int Int
          deriving (Show, Eq)

-- Newtype used to tag a Table with a value
newtype ColVal tab a = ColVal a
                     deriving (Show, Eq, Num)

newtype Updated tab (sc :: [*]) = Updated {getUpdateMap :: HashMap Attribute PrimExpr}
  deriving (Show)

pattern EmptyUpdate :: Updated tab sc
pattern EmptyUpdate <- (HM.null . getUpdateMap -> True) where
  EmptyUpdate = Updated HM.empty



(.~) :: forall fn sc val tab.
        ( UnifyField sc fn val ('Text "Unable to find column " ':<>: 'ShowType fn)
        , sc ~ (OriginalTableFields tab)
        , KnownSymbol fn
        ) => Col fn -> Expr sc val -> Updated tab sc -> Updated tab sc
(.~) colVal expr (Updated updates) = Updated $ HM.insert (T.pack $ symbolVal colVal) (getExpr expr) updates

infixr 4 .~


(%~) :: forall fn sc val db tab.
        ( UnifyField sc fn val ('Text "Unable to find column " ':<>: 'ShowType fn)
        , sc ~ (OriginalTableFields tab)
        , KnownSymbol fn
        , Table db tab
        , SingCtx db tab
        , SingCtxDb db
        ) => Proxy db -> Col fn -> (Expr sc val -> Expr sc val) -> Updated tab sc -> Updated tab sc
(%~) _pdb col' exprFn updates = (.~) col' (exprFn $ col (Proxy :: Proxy (DBTag db tab fn))) updates

infixr 4 %~


type family FromDBRow (driver :: * -> *) (a :: *) :: Constraint
type family ToDBRow (driver :: * -> *) (a :: *) :: Constraint
  
type instance FromDBRow (PGS) a = (FromRow a)
type instance ToDBRow (PGS) a = (ToRow a)

data PGS cfg where
  PGS :: PGS.Connection -> PGS PGS.Connection

class HasUpdate (driver :: * -> *) where
  dbUpdate :: driver cfg -> UpdateQuery -> IO Int64

class HasUpdateRet (driver :: * -> *) where
  dbUpdateRet :: (FromDBRow driver a) => driver cfg -> UpdateQuery -> IO [a]

class HasDelete (driver :: * -> *) where
  dbDelete :: driver cfg -> DeleteQuery -> IO Int64

class HasQuery (driver :: * -> *) where
  dbQuery :: (FromDBRow driver a) => driver cfg -> PrimQuery -> IO [a]

class HasInsert (driver :: * -> *) where
  dbInsert :: driver cfg -> InsertQuery -> IO Int64

class HasInsertRet (driver :: * -> *) where
  dbInsertRet :: (FromDBRow driver a) => driver cfg -> InsertQuery -> IO [a]

class Session (driver :: * -> *) where
  data SessionConfig (driver :: * -> *) cfg :: *
  runSession :: SessionConfig driver cfg -> ReaderT (driver cfg) IO a -> IO a

instance Session PGS where
  data SessionConfig PGS cfg where
    PGSConfig :: PG.Config_ -> SessionConfig PGS PGS.Connection
  runSession (PGSConfig cfg) dbact =
    withResource (PG.connectionPool cfg) $ \conn -> runReaderT dbact $ PGS conn
  


class HasTransaction (driver :: * -> *) where
  withTransaction :: driver cfg ->  IO a -> IO a

instance HasTransaction PGS where
  withTransaction (PGS conn) dbact = PGS.withTransaction conn dbact

runTransaction :: (Session driver, HasTransaction driver) => SessionConfig driver cfg -> ReaderT (driver cfg) IO a -> IO a
runTransaction sessionCfg dbact = runSession sessionCfg $ do
  ReaderT (\cfg -> DBRecord.Query.withTransaction cfg $ runReaderT dbact cfg)
  

instance HasUpdateRet PGS where
  dbUpdateRet (PGS conn) updateQ = do
    let updateSQL = PG.renderUpdate $ PG.updateSql $ updateQ
    putStrLn updateSQL
    returningWith fromRow conn (fromString updateSQL) ([]::[()])

instance HasUpdate PGS where
  dbUpdate (PGS conn) updateQ = do
    let updateSQL = PG.renderUpdate $ PG.updateSql $ updateQ
    putStrLn updateSQL
    execute_ conn (fromString updateSQL)

instance HasQuery PGS where
  dbQuery (PGS conn) primQ = do
    let sqlQ= PG.renderQuery $ PG.sql primQ
    putStrLn sqlQ
    query_ conn (fromString sqlQ)

instance HasInsert PGS where
  dbInsert (PGS conn) insQ = do
    let insSQL = PG.renderInsert $ PG.insertSql insQ
    putStrLn insSQL
    execute_ conn (fromString insSQL)

instance HasInsertRet PGS where
  dbInsertRet (PGS conn) insQ = do
    let insSQL = PG.renderInsert $ PG.insertSql insQ
    putStrLn insSQL
    returningWith fromRow conn (fromString insSQL) ([]::[()])

instance HasDelete PGS where
  dbDelete (PGS conn) deleteQ = do
    let delSQL = PG.renderDelete $ PG.deleteSql $ deleteQ
    putStrLn delSQL
    execute_ conn (fromString delSQL)

class HasCol db tab sc (t :: *) where
  hasCol :: Proxy (DBTag db tab t) -> Proxy sc -> Expr sc t

instance ( KnownSymbol fld
         , Table db tab
         , UnifyField sc fld t ('Text "Unable to find column " ':<>: 'ShowType fld)
         , All SingE (ColumnNames db tab)
         , All SingE (GetFieldInfo (DB db) (GenTabFields (Rep tab)))
         , SingI (GetFieldInfo (DB db) (GenTabFields (Rep tab)))
         , SingI (ColumnNames db tab)
         ) => HasCol db tab sc (fld ::: t) where
  hasCol _ _ = coerceExpr (col (Proxy @(DBTag db tab fld)) :: Expr sc t)

applyEqs :: forall xs sc tab db.
             ( All (HasCol db tab sc) xs
             , All ConstExpr xs
             , All EqExpr xs
             ) =>
             Proxy (DBTag db tab ()) ->
             HList Identity xs ->
             Expr sc Bool
applyEqs p (Identity ce :& ces) =
    constExpr ce .== hasCol (proxy ce) (Proxy @sc) .&&
    applyEqs p ces

    where proxy :: forall x. (HasCol db tab sc x) => x -> Proxy (DBTag db tab x)
          proxy _ = Proxy
applyEqs _ Nil = true
  
-- class ApplyExpr (db :: *) (tab :: *) (cols :: [Symbol]) fn r where
--   applyExpr :: Proxy db -> Proxy tab -> Proxy cols -> fn -> r

-- instance ( fn ~ (Expr (OriginalTableFields tab) a -> res)
--          , r ~ res 
--          , KnownSymbol cn
--          , UnifyField
--            (OriginalTableFields tab)
--            (cn ::: a)
--            ('Text "Unable to find column " ':<>: 'ShowType cn)
--          , ApplyExpr db tab cols res r
--          ) => ApplyExpr db tab (cn ': cols) fn r where
--   applyExpr pdb ptab _ fn = applyExpr pdb ptab (Proxy @cols) (fn (col (Proxy @cn)))

-- instance (fn ~ r) => ApplyExpr db tab '[] fn r where
--   applyExpr _ _ _ r = r
  -- , ApplyExpr db tab (PrimaryKey db tab) predicate (Expr (OriginalTableFields tab) Bool)
  -- , UnifyPkPredicate db tab predicate

  
get :: forall tab db driver cfg tpks pks sc.
  ( Table db tab
  , MonadIO (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasQuery driver
  , FromDBRow driver tab
  , ToHList tpks
  , TupleToHList tpks ~ pks
  , pks ~ FromRights (FindFields sc (PrimaryKey db tab))
  , SingCtx db tab
  , SingCtxDb db
  , All EqExpr pks
  , All ConstExpr pks
  , All (HasCol db tab sc) pks
  , sc ~ OriginalTableFields tab
  ) => tpks -> DBM db (Maybe tab)
get tpks = do
  let filtE = getExpr (applyEqs (Proxy @(DBTag db tab ())) (toHList tpks Identity) :: Expr sc Bool)
      cls = clauses { criteria = [filtE] }
  res <- getAll' cls
  pure $ case res of
    []  -> Nothing
    [r] -> Just r
    _   -> error "get: query with primarykey return more than 1 rows"

getBy :: forall tab (uniq :: Symbol) db driver cfg uqKeysM uqKeys tuqs uqs sc.
  ( Table db tab
  , MonadIO (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasQuery driver
  , FromDBRow driver tab
  , uqKeysM ~ (GetUniqBy uniq (Unique db tab))
  , SingCtx db tab
  , SingCtxDb db
  , 'Just uqKeys ~ uqKeysM
  , uqs ~ FromRights (FindFields sc uqKeys)
  , All EqExpr uqs
  , All ConstExpr uqs
  , All (HasCol db tab sc) uqs
  , sc ~ OriginalTableFields tab
  , ToHList tuqs
  , TupleToHList tuqs ~ uqs
  ) => Uq uniq -> tuqs -> DBM db (Maybe tab)
getBy _ tuqs = do
  let filtE = getExpr (applyEqs (Proxy @(DBTag db tab ())) (toHList tuqs Identity) :: Expr sc Bool)
      cls = clauses { criteria = [filtE] }
  res <- getAll' cls
  pure $ case res of
    []  -> Nothing
    [r] -> Just r
    _  -> error "get: query with primarykey return more than 1 rows"

getAll :: forall tab db driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasQuery driver
  , FromDBRow driver tab
  , SingCtx db tab
  , SingCtxDb db
  ) =>    Expr (OriginalTableFields tab) Bool
       -> Order (OriginalTableFields tab)
       -> Maybe Page
       -> DBM db [tab]
getAll filt ord page = do
  let filtE = case filt of
        TRUE -> []
        _    -> [getExpr filt]
      ordE = case ord of
        AnyOrder -> []
        _        -> getOrder ord        
      (off, lmt) = case page of
        Nothing -> (Nothing, Nothing)
        (Just (Offset n)) -> (Just n, Nothing)
        (Just (Limit n)) -> (Nothing, Just n)
        (Just (OffsetLimit o l)) -> (Just o, Just l)
        
      cls = clauses
        { criteria = filtE
        , limit = (ConstExpr . Integer . fromIntegral) <$> lmt
        , offset = (ConstExpr . Integer . fromIntegral) <$> off
        , orderbys = ordE
        }
  getAll' cls

getAll' :: forall db tab driver cfg.
           ( Table db tab
           , SingCtx db tab
           , SingCtxDb db
           , MonadIO (DBM db)
           , MonadReader (driver cfg) (DBM db)
           , HasQuery driver
           , FromDBRow driver tab             
           ) => Clauses -> DBM db [tab]
getAll' cls = do
  let tabId = getTableId (Proxy @db) (Proxy @tab)
      tabFlds = getTableProjections (Proxy @db) (Proxy @tab)
  runQuery tabId (cls { projections = tabFlds })

count :: forall tab db driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasQuery driver
  , FromDBRow driver (Only Int)
  , SingCtx db tab
  , SingCtxDb db
  ) => Expr (OriginalTableFields tab) Bool -> DBM db (ColVal tab Int)
count filt = do
  let tabId = getTableId (Proxy @db) (Proxy @tab)
      filtE = case filt of
        TRUE -> []
        _    -> [getExpr filt]
      cls = clauses { criteria = filtE }
  res <- runQuery tabId cls    
  case res of
    [Only ct] -> pure $ ColVal ct
    _         -> error "Panic: Expecting only a singleton @count"

runQuery :: ( MonadIO (DBM db)
             , MonadReader (driver cfg) (DBM db)
             , HasQuery driver
             , FromDBRow driver tab
             ) => TableId -> Clauses -> DBM db [tab]
runQuery tabId cls = do
  let primQ = BaseTable tabId cls
  driver <- ask
  liftIO $ dbQuery driver primQ

update :: forall tab db keys driver cfg sc rets.
  ( Table db tab
  , MonadIO (DBM db)
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey db tab)))
  , driver ~ Driver (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasUpdateRet driver
  , FromDBRow driver (HListToTuple keys)
  , SingCtx db tab
  , SingCtxDb db
  , sc ~ OriginalTableFields tab
  ) => Expr (OriginalTableFields tab) Bool
  -> (Updated tab (OriginalTableFields tab) -> Updated tab (OriginalTableFields tab))
  -> HList (Expr sc) rets -> DBM db [HListToTuple keys]  
update filt updateFn rets =
  runUpdateRet (Proxy @tab) [getExpr filt] (HM.toList $ getUpdateMap $ updateFn EmptyUpdate) (toPrimExprs rets)
  
update_ :: forall tab db cfg driver.
  ( Table db tab
  , Monad (DBM db)
  , KnownSymbol (Schema db)
  , SingCtx db tab
  , SingCtxDb db
  , MonadIO (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasUpdate driver
  , FromDBRow driver Int  
  ) => Expr (OriginalTableFields tab) Bool
  -> (Updated tab (OriginalTableFields tab) -> Updated tab (OriginalTableFields tab))
  -> DBM db ()
update_ filt updateFn =
  runUpdate (Proxy @tab) [getExpr filt] (HM.toList $ getUpdateMap $ updateFn EmptyUpdate)

runUpdate :: forall tab db cfg driver.
             ( Table db tab
             , SingCtx db tab
             , SingCtxDb db
             , MonadIO (DBM db)
             , MonadReader (driver cfg) (DBM db)
             , HasUpdate driver
             , FromDBRow driver Int
             ) => Proxy tab -> [PrimExpr] -> Assoc -> DBM db ()
runUpdate ptab crit assoc = do
  let updateQ = UpdateQuery tabId crit assoc []
      tabId   = getTableId (Proxy @db) ptab
  driver <- ask
  _ <- liftIO $ dbUpdate driver updateQ
  pure ()

runUpdateRet :: forall tab db cfg driver a.
                ( Table db tab
                , Monad (DBM db)
                , SingCtx db tab
                , SingCtxDb db
                , MonadIO (DBM db)
                , MonadReader (driver cfg) (DBM db)
                , HasUpdateRet driver
                , FromDBRow driver a
                ) => Proxy tab -> [PrimExpr] -> Assoc -> [PrimExpr] -> DBM db [a]
runUpdateRet ptab crit assoc rets = do
  let updateQ = UpdateQuery tabId crit assoc rets
      tabId   = getTableId (Proxy @db) ptab  
  driver <- ask
  liftIO $ dbUpdateRet driver updateQ

delete :: forall tab db driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , KnownSymbol (Schema db)
  , driver ~ Driver (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasDelete driver
  , SingCtx db tab
  , SingCtxDb db
  ) => Expr (OriginalTableFields tab) Bool -> DBM db (ColVal tab Int64)
delete filt = do
  let deleteQ = DeleteQuery (getTableId (Proxy @db) (Proxy @tab)) [getExpr filt]
  driver <- ask
  ColVal <$> (liftIO $ dbDelete driver deleteQ)

-- TODO: is list reversed?
toPrimExprs :: HList (Expr sc) xs ->
               [PrimExpr]
toPrimExprs = toPrimExprs' []

toPrimExprs' :: [PrimExpr] -> HList (Expr sc) xs -> [PrimExpr]
toPrimExprs' exprs (e :& es) = toPrimExprs' (getExpr e : exprs) es
toPrimExprs' exprs Nil       = exprs

-- TODO: is list reversed?
toDBValues :: (All ConstExpr xs) =>
               HList Identity xs ->
               [PrimExpr]
toDBValues = toDBValues' []

toDBValues' :: (All ConstExpr xs) =>
                [PrimExpr]        ->
                HList Identity xs ->
                [PrimExpr]
toDBValues' exprs (Identity v :& vals) = toDBValues' (getExpr (constExpr v) : exprs) vals
toDBValues' acc Nil = acc

insert :: forall tab db row keys defs reqCols driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey db tab)))
  , defs ~ (HasDefault db tab :++ PrimaryKey db tab)
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All ConstExpr reqCols
  , SingCols db reqCols (ColumnNames db tab)
  , driver ~ Driver (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , SingCtx db tab
  , SingCtxDb db
  , FromDBRow driver (HListToTuple keys)
  , HasInsertRet driver
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
    
    -- TODO: Singletonize the output
  ) => Proxy tab -> row -> DBM db [HListToTuple keys]
insert _ row = do
  let
    tabId = getTableId (Proxy @db) (Proxy @tab)
    values = toHList row (\v -> Identity v)
    tabFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    colIs = headColInfos (Proxy @db) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns tabFlds colIs)
    cexprs = toDBValues values
    insertQ = InsertQuery tabId cnames (NE.fromList [cexprs]) []
  driver <- ask
  liftIO $ dbInsertRet driver insertQ

insertRet :: forall tab db row keys rets sc defs reqCols driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey db tab)))
  , defs ~ (HasDefault db tab :++ PrimaryKey db tab)
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All ConstExpr reqCols
  , driver ~ Driver (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , SingCtx db tab
  , SingCtxDb db
  , FromDBRow driver (HListToTuple keys)
  , HasInsertRet driver
  , sc ~ (OriginalTableFields tab)
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)    
  ) => Proxy tab -> row -> HList (Expr sc) rets -> DBM db (Maybe (HListToTuple keys))
insertRet _ row rets = do
  let
    tabId = getTableId (Proxy @db) (Proxy @tab)
    values = toHList row (\v -> Identity v)
    tabFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    colIs = headColInfos (Proxy @db) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns tabFlds colIs)
    cexprs = toDBValues values
    insertQ = InsertQuery tabId cnames (NE.fromList [cexprs]) (toPrimExprs rets)
  driver <- ask
  out <- liftIO $ dbInsertRet driver insertQ
  pure $ case out of
    [] -> Nothing
    [x] -> Just x
    _ ->  error "insertRet: insert query with return more than 1 rows"

insertMany :: forall tab db row defs reqCols driver keys cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey db tab)))    
  , defs ~ (HasDefault db tab :++ PrimaryKey db tab)
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All ConstExpr reqCols
  , SingCols db reqCols (ColumnNames db tab)
  , driver ~ Driver (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , SingCtx db tab
  , SingCtxDb db
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , FromDBRow driver (HListToTuple keys)
  , HasInsertRet driver    
  ) => Proxy tab -> [row] -> DBM db [HListToTuple keys]
insertMany _ rows = do
  let
    tabFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    colIs = headColInfos (Proxy @db) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns tabFlds colIs)
    cexprss = fmap toDBValues values
    
    values = fmap (\row -> toHList row (\v -> Identity v)) rows
    insertQ = InsertQuery (getTableId (Proxy @db) (Proxy @tab)) cnames (NE.fromList cexprss) []
  driver <- ask
  liftIO $ dbInsertRet driver insertQ

insertManyRet :: forall tab db row rets sc defs reqCols driver keys cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey db tab)))    
  , defs ~ (HasDefault db tab :++ PrimaryKey db tab)
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All ConstExpr reqCols
  , SingCols db reqCols (ColumnNames db tab)
  , driver ~ Driver (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasInsert driver
  , SingCtx db tab
  , SingCtxDb db
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , FromDBRow driver (HListToTuple keys)
  , HasInsertRet driver
  , sc ~ OriginalTableFields tab
  ) => Proxy tab -> [row] -> HList (Expr sc) rets -> DBM db [HListToTuple keys]
insertManyRet _ rows rets = do
  let
    tabFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    colIs = headColInfos (Proxy @db) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns tabFlds colIs)
    cexprss = fmap toDBValues values
    values = fmap (\row -> toHList row (\v -> Identity v)) rows
    insertQ = InsertQuery (getTableId (Proxy @db) (Proxy @tab)) cnames (NE.fromList cexprss) (toPrimExprs rets)
  driver <- ask
  liftIO $ dbInsertRet driver insertQ

insert_ :: forall tab db row defs reqCols driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , KnownSymbol (Schema db)
  , defs ~ (HasDefault db tab :++ PrimaryKey db tab)
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All ConstExpr reqCols
  , driver ~ Driver (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasInsert driver
  , SingCtx db tab
  , SingCtxDb db
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  ) => Proxy tab -> row -> DBM db ()
insert_ _ row = do
  let
    tabFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    colIs = headColInfos (Proxy @db) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns tabFlds colIs)
    cexprs = toDBValues values
    values = toHList row (\v -> Identity v)
    insertQ = InsertQuery (getTableId (Proxy @db) (Proxy @tab)) cnames (NE.fromList [cexprs]) []
  driver <- ask
  _ <- liftIO $ dbInsert driver insertQ
  pure ()

insertMany_ :: forall tab db row defs reqCols driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , KnownSymbol (Schema db)
  , defs ~ (HasDefault db tab :++ PrimaryKey db tab)
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All ConstExpr reqCols
  , driver ~ Driver (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasInsert driver
  , SingCtx db tab
  , SingCtxDb db
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)    
  ) => Proxy tab -> [row] -> DBM db ()
insertMany_ _ rows = do
  let
    tabFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    colIs = headColInfos (Proxy @db) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns tabFlds colIs)
    cexprss = fmap toDBValues values    
    values = fmap (\row -> toHList row (\v -> Identity v)) rows
    insertQ = InsertQuery (getTableId (Proxy @db) (Proxy @tab)) cnames (NE.fromList cexprss) []
  driver <- ask
  _ <- liftIO $ dbInsert driver insertQ
  pure ()
  
getTableProjections :: forall db tab. (SingCtx db tab) => Proxy db -> Proxy tab -> [Projection]
getTableProjections pdb ptab = go (headColInfos pdb ptab)
  where go :: [ColumnInfo] -> [Projection]
        go = map mkProj

        mkProj :: ColumnInfo -> Projection
        mkProj ci =
          let dbColN = ci ^. columnNameInfo . dbName
          in  (Sym [] dbColN, BaseTableAttrExpr dbColN)

getTableId :: forall db tab. (SingCtx db tab, SingCtxDb db) => Proxy db -> Proxy tab -> TableId
getTableId pdb ptab =
  let dbTabName    = headTabNameInfo pdb ptab ^. dbName
      dbSchemaName = headDbNameInfo pdb ^. dbName
  in  TableId { PQ.schema    = dbSchemaName
              , PQ.tableName = dbTabName
              }

