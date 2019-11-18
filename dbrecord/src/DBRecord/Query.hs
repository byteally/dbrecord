{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE ExplicitForAll                #-}
{-# LANGUAGE KindSignatures                #-}
{-# LANGUAGE TypeFamilyDependencies        #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE ConstraintKinds               #-}
{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE TypeOperators                 #-}
{-# LANGUAGE UndecidableInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE TypeApplications              #-}
{-# LANGUAGE PatternSynonyms               #-}
{-# LANGUAGE ViewPatterns                  #-}
{-# LANGUAGE GADTs                         #-}
{-# LANGUAGE FlexibleInstances             #-}
{-# LANGUAGE MultiParamTypeClasses         #-}
{-# LANGUAGE FunctionalDependencies        #-}
{-# LANGUAGE RankNTypes                    #-}

-- | 

module DBRecord.Query
       ( module DBRecord.Internal.Order
       , module DBRecord.Internal.Expr
       , module DBRecord.Internal.Window
       , module DBRecord.Internal.Predicate
       , get, getBy, getAll
       , delete
       , insert, insert_, insertMany, insertMany_, insertRet, insertManyRet
       , update, update_
       -- , count
       , (.~) , (%~)
       , DBM
       , Driver
       -- , PGS(..)
       -- , withResource
       , runTransaction
       , runSession         
       , Page(..)
       , ToDBRow
       , HasUpdate (..)
       , HasQuery (..)
       , HasUpdateRet (..)
       , HasDelete (..)
       , HasInsert (..)
       , HasInsertRet (..)
       , Session (..)
       , HasTransaction (..)
       , DBDecoder (..)
       , DBTag
       , TransactionConfig (..)
       , rawClauses
       , runQuery
       , getBaseTable
       , getBaseTableExpr
       ) where

import DBRecord.Schema

import DBRecord.Internal.Order
import DBRecord.Internal.Expr
import DBRecord.Internal.Predicate
import DBRecord.Internal.Common
import DBRecord.Internal.Window
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
import Control.Monad.IO.Class
import Control.Monad.Reader
import DBRecord.Internal.Lens ((^.))
import Control.Exception hiding (TypeError)

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

newtype Updated db tab (sc :: [*]) = Updated {getUpdateMap :: HashMap Attribute PrimExpr}
  deriving (Show)

pattern EmptyUpdate :: Updated db tab sc
pattern EmptyUpdate <- (HM.null . getUpdateMap -> True) where
  EmptyUpdate = Updated HM.empty



(.~) :: forall db tab fn alfn sc val.
        ( UnifyField sc fn val ('Text "Unable to find column " ':<>: 'ShowType fn)
        , sc ~ (OriginalTableFields tab)
        , KnownSymbol fn
        , alfn ~ AliasedCol fn (ColumnNames db tab)
        , KnownSymbol alfn
        ) => Col fn -> Expr sc val -> Updated db tab sc -> Updated db tab sc
(.~) _ expr (Updated updates) = Updated $ HM.insert (T.pack alfn) (getExpr expr) updates
  where alfn = symbolVal (Proxy @alfn)

infixr 4 .~


(%~) :: forall db tab fn alfn sc val.
        ( UnifyField sc fn val ('Text "Unable to find column " ':<>: 'ShowType fn)
        , sc ~ (OriginalTableFields tab)
        , KnownSymbol fn
        , Table db tab
        , SingCtx db tab
        , SingCtxSc db
        , alfn ~ AliasedCol fn (ColumnNames db tab)
        , KnownSymbol alfn          
        ) => Col fn -> (Expr sc val -> Expr sc val) -> Updated db tab sc -> Updated db tab sc
(%~) col' exprFn updates = (.~) col' (exprFn $ col (Proxy :: Proxy (DBTag db tab fn))) updates

infixr 4 %~

  
class DBDecoder (driver :: * -> *) where
  type FromDBRowParser (driver :: * -> *) :: * -> *
  type FromDBRow (driver :: * -> *)  :: * -> Constraint  
  dbDecoder :: ( FromDBRow driver a
              ) => Proxy driver -> Proxy a -> FromDBRowParser driver a

type family ToDBRow (driver :: * -> *) (a :: *) :: Constraint
  
class HasUpdate (driver :: * -> *) where
  dbUpdate :: driver cfg -> UpdateQuery -> IO Int64

class (DBDecoder driver) => HasUpdateRet (driver :: * -> *) where
  dbUpdateRetWith :: FromDBRowParser driver a -> driver cfg -> UpdateQuery -> IO [a]
  dbUpdateRet     :: (FromDBRow driver a) => driver cfg -> UpdateQuery -> IO [a]

  dbUpdateRet = dbUpdateRetWith (dbDecoder (Proxy :: Proxy driver) (Proxy :: Proxy a))
  

class HasDelete (driver :: * -> *) where
  dbDelete :: driver cfg -> DeleteQuery -> IO Int64

class (DBDecoder driver) => HasQuery (driver :: * -> *) where
  dbQueryWith :: FromDBRowParser driver a -> driver cfg -> PrimQuery -> IO [a]

  dbQuery :: (DBDecoder driver, FromDBRow driver a) => driver cfg -> PrimQuery -> IO [a]
  dbQuery = dbQueryWith (dbDecoder (Proxy :: Proxy driver) (Proxy :: Proxy a))

class HasInsert (driver :: * -> *) where
  dbInsert :: driver cfg -> InsertQuery -> IO Int64

class (DBDecoder driver) => HasInsertRet (driver :: * -> *) where
  dbInsertRetWith :: FromDBRowParser driver a -> driver cfg -> InsertQuery -> IO [a]  
  
  dbInsertRet :: (FromDBRow driver a) => driver cfg -> InsertQuery -> IO [a]
  dbInsertRet = dbInsertRetWith (dbDecoder (Proxy :: Proxy driver) (Proxy :: Proxy a))
  

class Session (driver :: * -> *) where
  data SessionConfig (driver :: * -> *) cfg :: *  
  runSession_ :: SessionConfig driver cfg -> ReaderT (driver cfg) IO a -> (driver cfg -> IO a -> IO a) -> IO a

class HasTransaction (driver :: * -> *) where
  withTransaction :: driver cfg -> IO a -> IO a

data TransactionConfig driver cfg a = TransactionConfig
  { sessionConfig     :: SessionConfig driver cfg
  , maxTries          :: Int
  , beforeTransaction :: IO a
  , onRetry           :: forall e . Exception e => e -> a -> IO ()
  , afterTransaction  :: a -> IO ()
  } 

runSession :: (Session driver) => SessionConfig driver cfg -> ReaderT (driver cfg) IO a -> IO a
runSession cfg dbact = runSession_ cfg dbact (\_ io -> io)

runTransaction :: forall driver cfg a. (Session driver, HasTransaction driver) => TransactionConfig driver cfg a -> ReaderT (driver cfg) IO a -> IO a
runTransaction cfg dbact = do
  c <- beforeTransaction cfg
  res <- withRetry c 1
    $ runSession_ (sessionConfig cfg) dbact withTransaction
  afterTransaction cfg c
  return res
  where
    withRetry :: a -> Int -> IO a -> IO a
    withRetry c n act = act `catchRecoverableExceptions` handler c n act
    handler :: a -> Int -> IO a -> SomeException -> IO a
    handler a n act (SomeException e) =
      if n < maxTries cfg
        then onRetry cfg e a >> withRetry a (n + 1) act
        else throwIO e
    catchRecoverableExceptions :: IO a -> (SomeException -> IO a) -> IO a
    catchRecoverableExceptions action h = action `catches`
      [ Handler $ \(e :: AsyncException)            -> throwIO e
      , Handler $ \(e :: BlockedIndefinitelyOnSTM)  -> throwIO e
      , Handler $ \(e :: BlockedIndefinitelyOnMVar) -> throwIO e
      , Handler $ \(e :: Deadlock)                  -> throwIO e
      , Handler $ \(e :: SomeException)             -> h e
      ]

class HasCol db tab sc (t :: *) where
  hasCol :: Proxy (DBTag db tab t) -> Proxy sc -> Expr sc t

instance ( KnownSymbol fld
         , Table db tab
         , UnifyField sc fld t ('Text "Unable to find column " ':<>: 'ShowType fld)
         , All SingE (ColumnNames db tab)
         , All SingE (GetFieldInfo (DB db) (GenTabFields (Rep tab)))
         , SingI (GetFieldInfo (DB db) (GenTabFields (Rep tab)))
         , SingI (ColumnNames db tab)
         , sc ~ OriginalTableFields tab
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
  , SingCtxSc db
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
  , SingCtxSc db
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
  , SingCtxSc db
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
           , SingCtxSc db
           , MonadIO (DBM db)
           , MonadReader (driver cfg) (DBM db)
           , HasQuery driver
           , FromDBRow driver tab             
           ) => Clauses -> DBM db [tab]
getAll' cls =
  rawClauses cls

rawClauses :: forall db tab driver cfg.
             ( Table db tab
             , HasQuery driver
             , FromDBRow driver tab
             , MonadReader (driver cfg) (DBM db)
             , MonadIO (DBM db)
             , SingCtx db tab
             , SingCtxSc db               
             ) => PQ.Clauses -> DBM db [tab]
rawClauses cls = do
  let tabId = getTableId (Proxy @db) (Proxy @tab)
  runQuery (Table (Just (TableName tabId)) cls)

{-
count :: forall tab db driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasQuery driver
  , FromDBRow driver (Only Int)
  , SingCtx db tab
  , SingCtxSc db
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
-}

runQuery :: ( MonadIO (DBM db)
             , MonadReader (driver cfg) (DBM db)
             , HasQuery driver
             , FromDBRow driver tab
             ) => PrimQuery -> DBM db [tab]
runQuery primQ = do
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
  , SingCtxSc db
  , sc ~ OriginalTableFields tab
  ) => Expr (OriginalTableFields tab) Bool
  -> (Updated db tab (OriginalTableFields tab) -> Updated db tab (OriginalTableFields tab))
  -> HList (Expr sc) rets -> DBM db [HListToTuple keys]  
update filt updateFn rets =
  runUpdateRet (Proxy @tab) [getExpr filt] (HM.toList $ getUpdateMap $ updateFn EmptyUpdate) (toPrimExprs rets)
  
update_ :: forall tab sc cfg driver.
  ( Table sc tab
  , Monad (DBM sc)
  , KnownSymbol (SchemaName sc)
  , Schema sc
  , SingCtx sc tab
  , SingCtxSc sc
  , MonadIO (DBM sc)
  , MonadReader (driver cfg) (DBM sc)
  , HasUpdate driver
  , FromDBRow driver Int  
  ) => Expr (OriginalTableFields tab) Bool
  -> (Updated sc tab (OriginalTableFields tab) -> Updated sc tab (OriginalTableFields tab))
  -> DBM sc ()
update_ filt updateFn =
  runUpdate (Proxy @tab) [getExpr filt] (HM.toList $ getUpdateMap $ updateFn EmptyUpdate)

runUpdate :: forall tab db cfg driver.
             ( Table db tab
             , SingCtx db tab
             , SingCtxSc db
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
                , SingCtxSc db
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
  , KnownSymbol (SchemaName db)
  , Schema db
  , driver ~ Driver (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasDelete driver
  , SingCtx db tab
  , SingCtxSc db
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
  , SingCtxSc db
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
  , SingCtxSc db
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
  , SingCtxSc db
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
  , SingCtxSc db
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

insert_ :: forall tab sc row defs reqCols driver cfg.
  ( Table sc tab
  , MonadIO (DBM sc)
  , KnownSymbol (SchemaName sc)
  , Schema sc
  , defs ~ (HasDefault sc tab :++ PrimaryKey sc tab)
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All ConstExpr reqCols
  , driver ~ Driver (DBM sc)
  , MonadReader (driver cfg) (DBM sc)
  , HasInsert driver
  , SingCtx sc tab
  , SingCtxSc sc
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  ) => Proxy tab -> row -> DBM sc ()
insert_ _ row = do
  let
    tabFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns tabFlds colIs)
    cexprs = toDBValues values
    values = toHList row (\v -> Identity v)
    insertQ = InsertQuery (getTableId (Proxy @sc) (Proxy @tab)) cnames (NE.fromList [cexprs]) []
  driver <- ask
  _ <- liftIO $ dbInsert driver insertQ
  pure ()

insertMany_ :: forall tab sc row defs reqCols driver cfg.
  ( Table sc tab
  , MonadIO (DBM sc)
  , KnownSymbol (SchemaName sc)
  , Schema sc
  , defs ~ (HasDefault sc tab :++ PrimaryKey sc tab)
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All ConstExpr reqCols
  , driver ~ Driver (DBM sc)
  , MonadReader (driver cfg) (DBM sc)
  , HasInsert driver
  , SingCtx sc tab
  , SingCtxSc sc
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)    
  ) => Proxy tab -> [row] -> DBM sc ()
insertMany_ _ rows = do
  let
    tabFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns tabFlds colIs)
    cexprss = fmap toDBValues values    
    values = fmap (\row -> toHList row (\v -> Identity v)) rows
    insertQ = InsertQuery (getTableId (Proxy @sc) (Proxy @tab)) cnames (NE.fromList cexprss) []
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
          in  (dbColN, BaseTableAttrExpr dbColN)

getTableId :: forall sc tab. (SingCtx sc tab, SingCtxSc sc) => Proxy sc -> Proxy tab -> TableId
getTableId psc ptab =
  let dbTabName    = headTabNameInfo psc ptab ^. dbName
      dbSchemaName = headSchemaNameInfo psc ^. dbName
  in  TableId { PQ.schema    = dbSchemaName
              , PQ.tableName = dbTabName
              , PQ.database  = error "Panic: unfilled"
              }

getBaseTableExpr :: forall db tab.
                ( SingCtx db tab
                , SingCtxSc db
                ) => Proxy db -> Proxy tab -> TableExpr PrimQuery
getBaseTableExpr _ _ =
  let tabId = getTableId (Proxy @db) (Proxy @tab)
  in  TableName tabId 

getBaseTable :: forall db tab.
                ( SingCtx db tab
                , SingCtxSc db
                ) => Proxy db -> Proxy tab -> PrimQuery
getBaseTable _ _ =
  let tabId = getTableId (Proxy @db) (Proxy @tab)
      tabFlds = getTableProjections (Proxy @db) (Proxy @tab)      
  in  Table (Just (TableName tabId)) (clauses { projections = tabFlds })
  
