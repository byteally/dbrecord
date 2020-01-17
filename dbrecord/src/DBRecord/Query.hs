{-# OPTIONS_GHC -fno-warn-redundant-constraints -Wwarn #-}
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
       , getBy, get, getAll
       , delete
       , insert, insert_, insertMany, insertMany_, insertRet, insertManyRet
       , insertWithConflict_
       , insertManyWithConflict_
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

       , withPrimaryKey
       , withRows
       , withRow

       , query
       
       , Q.column
       , Q.table
       , Q.project
       , Q.aggregate
       , Q.Tab
       , Q.Columns

       , onConflictUpdate
       , onConflictDoNothing
       , conflictingConstraint
       , conflictingColumns
       , conflictingAnon
       ) where

import DBRecord.Schema

import DBRecord.Internal.Order
import DBRecord.Internal.Expr
import DBRecord.Internal.Predicate
import DBRecord.Internal.Common
import DBRecord.Internal.Window
import DBRecord.Internal.Schema hiding (insert, delete)
import DBRecord.Internal.PrimQuery  hiding (insertQ, updateQ, deleteQ)
import DBRecord.Internal.Query (getTableId, getTableProjections)
import qualified DBRecord.Internal.Query as Q
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
import Data.Kind 

import GHC.Generics

type family DBM (db :: *) = (r :: * -> *) | r -> db
type family Driver (dbm :: * -> *) = (r :: * -> *) | r -> dbm

{-
type family MkPredFn (tab :: *) (args :: [Symbol]) (val :: *) (flds :: [*]) :: [Either ErrorMessage *] where
  MkPredFn tab (fn ': fs) chkFun flds = Note (ColNotFoundMsg fn tab) (FMapMaybe (Expr flds) (FindField flds fn)) ': MkPredFn tab fs chkFun flds
  MkPredFn tab '[] r flds = '[ 'Right (Expr flds Bool)]

  
type family UnifyPkPredicate (db :: *) (tab :: *) (pred :: *) :: Constraint where
  UnifyPkPredicate db tab pred = UnifyOrErr (SeqEither (MkPredFn tab (PrimaryKey db tab) pred (OriginalTableFields tab))) pred

type family UnifyUqPredicate (db :: *) (tab :: *) (pred :: *) (uniqs :: Either ErrorMessage [Symbol]) :: Constraint where
  UnifyUqPredicate db tab _ ('Left err) = TypeError err
  UnifyUqPredicate db tab pred ('Right uniqs) = UnifyOrErr (SeqEither (MkPredFn tab uniqs pred (OriginalTableFields tab))) pred
-}

data Page = Offset Int | Limit Int | OffsetLimit Int Int
          deriving (Show, Eq)

-- Newtype used to tag a Table with a value
newtype ColVal tab a = ColVal a
                     deriving (Show, Eq, Num)

newtype Updated sc tab (scopes :: [*]) = Updated {getUpdateMap :: HashMap Attribute PrimExpr}
  deriving (Show)

newtype WithKey sc t = WithKey { getWithKey :: t }

withPrimaryKey :: forall sc t. t -> WithKey sc t
withPrimaryKey = coerce

pattern EmptyUpdate :: Updated sc tab scopes
pattern EmptyUpdate <- (HM.null . getUpdateMap -> True) where
  EmptyUpdate = Updated HM.empty

(.~) :: forall sc tab fn alfn val scopes.
        ( UnifyField (OriginalTableFields tab) fn val ('Text "Unable to find column " ':<>: 'ShowType fn)
        , KnownSymbol fn
        , alfn ~ AliasedCol fn (ColumnNames sc tab)
        , KnownSymbol alfn
        ) => Col fn -> Expr sc scopes val -> Updated sc tab scopes -> Updated sc tab scopes
(.~) _ expr (Updated updates) = Updated $ HM.insert (T.pack alfn) (getExpr expr) updates
  where alfn = symbolVal (Proxy @alfn)

infixr 4 .~

(%~) :: forall sc tab fn alfn val scopes.
        ( UnifyField (OriginalTableFields tab) fn val ('Text "Unable to find column " ':<>: 'ShowType fn)
        , KnownSymbol fn
        , Table sc tab
        , SingCtx sc tab
        , SingCtxSc sc
        , alfn ~ AliasedCol fn (ColumnNames sc tab)
        , KnownSymbol alfn          
        ) => Col fn -> (Expr sc scopes val -> Expr sc scopes val) -> Updated sc tab scopes -> Updated sc tab scopes
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

class HasCol sc tab (t :: *) where
  hasCol :: proxy (DBTag sc tab t) -> Expr sc scopes t

instance ( KnownSymbol fld
         , Table sc tab
         , UnifyField (OriginalTableFields tab) fld t ('Text "Unable to find column " ':<>: 'ShowType fld)
         , All SingE (ColumnNames sc tab)
         , All SingE (GetFieldInfo sc (DB (SchemaDB sc)) (GenTabFields (Rep tab)))
         , SingI (GetFieldInfo sc (DB (SchemaDB sc)) (GenTabFields (Rep tab)))
         , SingI (ColumnNames sc tab)
         ) => HasCol sc tab (fld ::: t) where
  hasCol _ = coerceExpr (col (Proxy @(DBTag sc tab fld)) :: Expr sc scopes t) 

applyEqs :: forall xs scopes tab sc.
             ( All (HasCol sc tab) xs
             , All (ConstExpr sc) xs
             , All (EqExpr sc) xs
             ) =>
             Proxy (DBTag sc tab ()) ->
             HList Identity xs ->
             Expr sc scopes Bool
applyEqs p (Identity ce :& ces) =
    constExpr ce .== hasCol (proxy ce) .&&
    applyEqs p ces

    where proxy :: forall x.
                  ( HasCol sc tab x
                  ) => x -> Proxy (DBTag sc tab x)
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

get :: forall sc tab driver cfg tpks pks (scopes :: [*]).
  ( QueryCtx sc tab driver cfg
  , ToHList tpks
  , TupleToHList tpks ~ pks
  , pks ~ FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab))
  , All (EqExpr sc) pks
  , All (ConstExpr sc) pks
  , All (HasCol sc tab) pks
  ) => WithKey sc tpks -> DBM (SchemaDB sc) (Maybe tab)
get tpks = do
  let filtE = getExpr (applyEqs (Proxy @(DBTag sc tab ())) (toHList (getWithKey tpks) Identity) :: Expr sc scopes Bool)
      cls = clauses { criteria = [filtE] }
  res <- getAll' (Proxy :: Proxy sc) cls
  pure $ case res of
    []  -> Nothing
    [r] -> Just r
    _   -> error "get: query with primarykey return more than 1 rows"

getBy :: forall sc tab (uniq :: Symbol) driver cfg uqKeysM uqKeys tuqs uqs (scopes :: [*]).
  ( QueryCtx sc tab driver cfg
  , uqKeysM ~ (GetUniqBy uniq (Unique sc tab))
  , 'Just uqKeys ~ uqKeysM
  , uqs ~ FromRights (FindFields (OriginalTableFields tab) uqKeys)
  , All (EqExpr sc) uqs
  , All (ConstExpr sc) uqs
  , All (HasCol sc tab) uqs
  , ToHList tuqs
  , TupleToHList tuqs ~ uqs
  ) => Uq sc uniq -> tuqs -> DBM (SchemaDB sc) (Maybe tab)
getBy _ tuqs = do
  let filtE = getExpr (applyEqs (Proxy @(DBTag sc tab ())) (toHList tuqs Identity) :: Expr sc scopes Bool)
      cls = clauses { criteria = [filtE] }
  res <- getAll' (Proxy :: Proxy sc) cls
  pure $ case res of
    []  -> Nothing
    [r] -> Just r
    _  -> error "get: query with primarykey return more than 1 rows"

getAll :: forall sc tab driver cfg scopes scopes2.
  ( QueryCtx sc tab driver cfg
  ) =>    (Q.Columns tab -> Expr sc scopes Bool)
       -> Order sc scopes2
       -> Maybe Page
       -> DBM (SchemaDB sc) [tab]
getAll filt ord page = do
  let filtE = case filt (Q.Columns prjs) of
        TRUE -> []
        e    -> [getExpr e]
      ordE = case ord of
        AnyOrder -> []
        _        -> getOrder ord        
      (off, lmt) = case page of
        Nothing -> (Nothing, Nothing)
        (Just (Offset n)) -> (Just n, Nothing)
        (Just (Limit n)) -> (Nothing, Just n)
        (Just (OffsetLimit o l)) -> (Just o, Just l)
      prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)
      cls = clauses
        { criteria = filtE
        , limit = (ConstExpr . Integer . fromIntegral) <$> lmt
        , offset = (ConstExpr . Integer . fromIntegral) <$> off
        , orderbys = ordE
        }
  getAll' (Proxy :: Proxy sc) cls

type QueryCtx sc tab driver cfg =
  ( Table sc tab
  , HasQuery driver
  , FromDBRow driver tab
  , MonadReader (driver cfg) (DBM (SchemaDB sc))
  , MonadIO (DBM (SchemaDB sc))
  , SingCtx sc tab
  , SingCtxSc sc
  )

query ::
  ( HasQuery driver
  , FromDBRow driver tab
  , MonadReader (driver cfg) (DBM (SchemaDB sc))
  , MonadIO (DBM (SchemaDB sc))
  ) => Q.Tab sc tab -> DBM (SchemaDB sc) [tab]
query = runQuery . Q.convert . Q.getQuery

getAll' :: forall sc tab driver cfg proxy.
            ( QueryCtx sc tab driver cfg
            ) => proxy sc -> Clauses -> DBM (SchemaDB sc) [tab]
getAll' p cl = rawClauses p (cl { projections = getTableProjections (Proxy @sc) (Proxy @tab) })

rawClauses :: forall sc tab driver cfg proxy.
             ( QueryCtx sc tab driver cfg          
             ) => proxy sc -> PQ.Clauses -> DBM (SchemaDB sc) [tab]
rawClauses _ cls = do
  let tabId = getTableId (Proxy @sc) (Proxy @tab)
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

runQuery :: ( MonadIO (DBM sc)
            , MonadReader (driver cfg) (DBM sc)
            , HasQuery driver
            , FromDBRow driver tab
            ) => PrimQuery -> DBM sc [tab]
runQuery primQ = do
  driver <- ask
  liftIO $ print primQ
  liftIO $ dbQuery driver primQ

update :: forall sc tab keys driver cfg scopes rets.
  ( Table sc tab
  , MonadIO (DBM (SchemaDB sc))
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , driver ~ Driver (DBM (SchemaDB sc))
  , MonadReader (driver cfg) (DBM (SchemaDB sc))
  , HasUpdateRet driver
  , FromDBRow driver (HListToTuple keys)
  , SingCtx sc tab
  , SingCtxSc sc
  ) => (Q.Columns tab -> Expr sc scopes Bool)
  -> (Updated sc tab (OriginalTableFields tab) -> Updated sc tab (OriginalTableFields tab))
  -> HList (Expr sc scopes) rets -> DBM (SchemaDB sc) [HListToTuple keys]  
update filt updateFn rets =
  runUpdateRet (Proxy @sc) (Proxy @tab) [getExpr $ filt (Q.Columns prjs)] (HM.toList $ getUpdateMap $ updateFn EmptyUpdate) (toPrimExprs rets)

  where prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)
  
update_ :: forall sc tab cfg driver scopes.
  ( Table sc tab
  , KnownSymbol (SchemaName sc)
  , Schema sc
  , SingCtx sc tab
  , SingCtxSc sc
  , MonadIO (DBM (SchemaDB sc))
  , MonadReader (driver cfg) (DBM (SchemaDB sc))
  , HasUpdate driver
  ) => (Q.Columns tab -> Expr sc scopes Bool)
  -> (Updated sc tab (OriginalTableFields tab) -> Updated sc tab (OriginalTableFields tab))
  -> DBM (SchemaDB sc) ()
update_ filt updateFn =
  runUpdate (Proxy @sc) (Proxy @tab) [getExpr $ filt (Q.Columns prjs)] (HM.toList $ getUpdateMap $ updateFn EmptyUpdate)

  where prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)

runUpdate :: forall sc tab cfg driver.
             ( Table sc tab
             , SingCtx sc tab
             , SingCtxSc sc
             , MonadIO (DBM (SchemaDB sc))
             , MonadReader (driver cfg) (DBM (SchemaDB sc))
             , HasUpdate driver
             ) => Proxy sc -> Proxy tab -> [PrimExpr] -> Assoc -> DBM (SchemaDB sc) ()
runUpdate psc ptab crit assoc = do
  let updateQ = UpdateQuery tabId crit assoc []
      tabId   = getTableId psc ptab
  driver <- ask
  (_ :: Identity Int64) <- Identity <$> (liftIO $ dbUpdate driver updateQ)
  pure ()

runUpdateRet :: forall sc tab cfg driver a.
                ( Table sc tab
                , Monad (DBM (SchemaDB sc))
                , SingCtx sc tab
                , SingCtxSc sc
                , MonadIO (DBM (SchemaDB sc))
                , MonadReader (driver cfg) (DBM (SchemaDB sc))
                , HasUpdateRet driver
                , FromDBRow driver a
                ) => Proxy sc -> Proxy tab -> [PrimExpr] -> Assoc -> [PrimExpr] -> DBM (SchemaDB sc) [a]
runUpdateRet psc ptab crit assoc rets = do
  let updateQ = UpdateQuery tabId crit assoc rets
      tabId   = getTableId psc ptab  
  driver <- ask
  liftIO $ dbUpdateRet driver updateQ

delete :: forall sc tab driver cfg scopes.
  ( Table sc tab
  , MonadIO (DBM (SchemaDB sc))
  , KnownSymbol (SchemaName sc)
  , Schema sc
  , driver ~ Driver (DBM (SchemaDB sc))
  , MonadReader (driver cfg) (DBM (SchemaDB sc))
  , HasDelete driver
  , SingCtx sc tab
  , SingCtxSc sc
  ) => (Q.Columns tab -> Expr sc scopes Bool) -> DBM (SchemaDB sc) (ColVal tab Int64)
delete filt = do
  let deleteQ = DeleteQuery (getTableId (Proxy @sc) (Proxy @tab)) [getExpr $ filt (Q.Columns prjs)]
  driver <- ask
  ColVal <$> (liftIO $ dbDelete driver deleteQ)

    where prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)


-- TODO: is list reversed?
toPrimExprs :: HList (Expr sc scopes) xs ->
               [PrimExpr]
toPrimExprs = toPrimExprs' []

toPrimExprs' :: [PrimExpr] -> HList (Expr sc scopes) xs -> [PrimExpr]
toPrimExprs' exprs (e :& es) = toPrimExprs' (getExpr e : exprs) es
toPrimExprs' exprs Nil       = exprs

toDBValues :: forall sc xs.
               (All (ConstExpr sc) xs) =>
               Proxy sc          -> 
               HList Identity xs ->
               [PrimExpr]
toDBValues p = reverse . toDBValues' p []

toDBValues' :: forall sc xs.
                (All (ConstExpr sc) xs) =>
                Proxy sc          ->
                [PrimExpr]        ->
                HList Identity xs ->
                [PrimExpr]
toDBValues' p exprs (Identity v :& vals) = toDBValues' p (getExpr (constExpr @sc v) : exprs) vals
toDBValues' _ acc Nil = acc

insert :: forall sc tab row keys keyFields defs reqCols driver cfg.
  ( Table sc tab
  , MonadIO (DBM (SchemaDB sc))
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , keyFields ~ FieldsOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , defs ~ HasDefault sc tab 
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  , driver ~ Driver (DBM (SchemaDB sc))
  , MonadReader (driver cfg) (DBM (SchemaDB sc))
  , SingCtx sc tab
  , SingCtxSc sc
  , FromDBRow driver (HListToTuple keys)
  , HasInsertRet driver
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , SingI keyFields
  , SingE keyFields
  ) => Row sc tab row -> DBM (SchemaDB sc) (Maybe (HListToTuple keys))
insert row = do
  let
    tabId = getTableId (Proxy @sc) (Proxy @tab)
    values = toHList (getRow row) (\v -> Identity v)
    reqFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    pkFlds = fromSing (sing :: Sing keyFields)
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns reqFlds colIs)
    crets = map columnExpr (filterColumns pkFlds colIs)
    cexprs = toDBValues (Proxy @sc) values    
    insertQ = InsertQuery tabId cnames (NE.fromList [cexprs]) Nothing crets
  driver <- ask
  out <- liftIO $ dbInsertRet driver insertQ
  pure $ case out of
    [] -> Nothing
    [x] -> Just x
    _ ->  error "insert: insert query with return more than 1 rows"
  

insertRet :: forall sc tab row keys rets scopes defs reqCols driver cfg.
  ( Table sc tab
  , MonadIO (DBM (SchemaDB sc))
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , defs ~ HasDefault sc tab
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  , driver ~ Driver (DBM (SchemaDB sc))
  , MonadReader (driver cfg) (DBM (SchemaDB sc))
  , SingCtx sc tab
  , SingCtxSc sc
  , FromDBRow driver (HListToTuple keys)
  , HasInsertRet driver
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)    
  ) => Row sc tab row -> (Q.Columns tab -> HList (Expr sc scopes) rets) -> DBM (SchemaDB sc) (Maybe (HListToTuple keys))
insertRet row rets = do
  let
    tabId = getTableId (Proxy @sc) (Proxy @tab)
    values = toHList (getRow row) (\v -> Identity v)
    tabFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns tabFlds colIs)
    prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)    
    cexprs = toDBValues (Proxy @sc) values
    insertQ = InsertQuery tabId cnames (NE.fromList [cexprs]) Nothing (toPrimExprs $ rets (Q.Columns prjs))
  driver <- ask
  out <- liftIO $ dbInsertRet driver insertQ
  pure $ case out of
    [] -> Nothing
    [x] -> Just x
    _ ->  error "insertRet: insert query with return more than 1 rows"

onConflictUpdate :: (Q.Columns tab -> Expr sc scopes Bool) ->
                   (Updated sc tab (OriginalTableFields tab) -> Updated sc tab (OriginalTableFields tab)) ->
                   ConflictAction' sc tab scopes
onConflictUpdate = ConflictUpdate'

onConflictDoNothing :: ConflictAction' sc tab scopes
onConflictDoNothing = ConflictDoNothing'
                      
data ConflictAction' sc tab scopes =
    ConflictDoNothing'
  | ConflictUpdate' (Q.Columns tab -> Expr sc scopes Bool) (Updated sc tab (OriginalTableFields tab) -> Updated sc tab (OriginalTableFields tab))

data ConflictTarget' sc tab =
    ConflictTargetConstraint' T.Text
  | ConflictTargetColumns' [T.Text]
  | ConflictTargetAnon'

type family MissingUniqueOrCheck (ctx :: Symbol) (tab :: Type) (hasCheckOrUniq :: Bool) :: Constraint where
  MissingUniqueOrCheck ctx tab 'False = (TypeError ('Text "constraint " ':<>: 'ShowType ctx ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab)))
  MissingUniqueOrCheck ctx tab 'True  = ()

conflictingConstraint :: forall ctx sc tab.
                          ( KnownSymbol ctx
                          , MissingUniqueOrCheck ctx tab (Elem (OriginalCheckNames sc tab :++ OriginalUQNames sc tab) ctx)
                          ) => Proxy (ctx :: Symbol) -> ConflictTarget' sc tab
conflictingConstraint _ = ConflictTargetConstraint' (T.pack (symbolVal (Proxy @ctx)))

type family MissingColumns  (sc :: Type) (tab :: Type) (cols :: [Symbol]) (fields :: [Symbol]) :: Constraint where
  MissingColumns sc tab (col ': cols) fields = (MissingColumn col sc tab (Elem fields col), MissingColumns sc tab cols fields)
  MissingColumns sc tab '[] fields  = ()

type family MissingColumn (col :: Symbol) (sc :: Type) (tab :: Type) (isCol :: Bool) :: Constraint where
  MissingColumn col sc tab 'False = (TypeError ('Text "Column " ':<>: 'ShowType col ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab)))
  MissingColumn _ _ _ 'True  = ()
  
conflictingColumns :: forall cols sc tab.
                       ( MissingColumns sc tab cols (FieldsOf (OriginalTableFields tab))
                       , Table sc tab
                       , SingI cols
                       , SingE cols
                       ) => Proxy (cols :: [Symbol]) -> ConflictTarget' sc tab
conflictingColumns _ = ConflictTargetColumns' (fromSing (sing :: Sing cols))

conflictingAnon :: (Table sc tab) => ConflictTarget' sc tab
conflictingAnon = ConflictTargetAnon'

insertMany :: forall sc tab row defs reqCols driver keys cfg.
  ( Table sc tab
  , MonadIO (DBM (SchemaDB sc))
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))    
  , defs ~ HasDefault sc tab
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  , SingCols sc reqCols (ColumnNames sc tab)
  , driver ~ Driver (DBM (SchemaDB sc))
  , MonadReader (driver cfg) (DBM (SchemaDB sc))
  , HasInsert driver
  , SingCtx sc tab
  , SingCtxSc sc
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , FromDBRow driver (HListToTuple keys)
  , HasInsertRet driver
  ) => Rows sc tab row -> DBM (SchemaDB sc) [HListToTuple keys]
insertMany rows = do
  let
    tabFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns tabFlds colIs)
    cexprss = fmap (toDBValues (Proxy @sc)) values
    
    values = fmap (\row -> toHList row (\v -> Identity v)) (getRows rows)
    insertQ = InsertQuery (getTableId (Proxy @sc) (Proxy @tab)) cnames (NE.fromList cexprss) Nothing []
  driver <- ask
  liftIO $ dbInsertRet driver insertQ

insertManyRet :: forall sc tab row rets defs reqCols driver keys cfg scopes.
  ( Table sc tab
  , MonadIO (DBM (SchemaDB sc))
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))    
  , defs ~ HasDefault sc tab
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  , SingCols sc reqCols (ColumnNames sc tab)
  , driver ~ Driver (DBM (SchemaDB sc))
  , MonadReader (driver cfg) (DBM (SchemaDB sc))
  , HasInsert driver
  , SingCtx sc tab
  , SingCtxSc sc
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , FromDBRow driver (HListToTuple keys)
  , HasInsertRet driver
  ) => Rows sc tab row -> (Q.Columns tab -> HList (Expr sc scopes) rets) -> DBM (SchemaDB sc) [HListToTuple keys]
insertManyRet rows rets = do
  let
    tabFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns tabFlds colIs)
    cexprss = fmap (toDBValues (Proxy @sc)) values
    values = fmap (\row -> toHList row (\v -> Identity v)) (getRows rows)
    insertQ = InsertQuery (getTableId (Proxy @sc) (Proxy @tab)) cnames (NE.fromList cexprss) Nothing (toPrimExprs $ rets (Q.Columns prjs))
    prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)    
  driver <- ask
  liftIO $ dbInsertRet driver insertQ

insert_ :: forall sc tab row defs reqCols driver cfg.
  ( Table sc tab
  , MonadIO (DBM (SchemaDB sc))
  , KnownSymbol (SchemaName sc)
  , Schema sc
  , defs ~ HasDefault sc tab
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  , driver ~ Driver (DBM (SchemaDB sc))
  , MonadReader (driver cfg) (DBM (SchemaDB sc))
  , HasInsert driver
  , SingCtx sc tab
  , SingCtxSc sc
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)    
  ) => Row sc tab row -> DBM (SchemaDB sc) ()
insert_ = insertMany_ . Rows @sc @tab . pure . getRow

insertWithConflict_ :: forall sc tab row keys keyFields defs scopes reqCols driver cfg.
  ( Table sc tab
  , MonadIO (DBM (SchemaDB sc))
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , keyFields ~ FieldsOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , defs ~ HasDefault sc tab 
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  , driver ~ Driver (DBM (SchemaDB sc))
  , MonadReader (driver cfg) (DBM (SchemaDB sc))
  , SingCtx sc tab
  , SingCtxSc sc
  , HasInsert driver
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , SingI keyFields
  , SingE keyFields
  ) => ConflictTarget' sc tab        -> 
      ConflictAction' sc tab scopes ->   
      Row sc tab row ->      
      DBM (SchemaDB sc) ()
insertWithConflict_ ctgt cact row = do
  let
    tabId = getTableId (Proxy @sc) (Proxy @tab)
    values = toHList (getRow row) (\v -> Identity v)
    reqFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    pkFlds = fromSing (sing :: Sing keyFields)
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns reqFlds colIs)
    crets = map columnExpr (filterColumns pkFlds colIs)
    cexprs = toDBValues (Proxy @sc) values
    prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)
    tgt = case ctgt of
      ConflictTargetConstraint' t -> ConflictConstraint t
      ConflictTargetColumns' cols -> ConflictColumn (map symFromText cols)
      ConflictTargetAnon' -> ConflictAnon
    insertQ = case cact of
      ConflictDoNothing' -> InsertQuery tabId cnames (NE.fromList [cexprs]) (Just (Conflict tgt ConflictDoNothing)) crets
      ConflictUpdate' crit updFn ->
        let updq = UpdateQuery tabId [getExpr $ crit (Q.Columns prjs)] (HM.toList $ getUpdateMap $ updFn EmptyUpdate) []
        in InsertQuery tabId cnames (NE.fromList [cexprs]) (Just (Conflict tgt (ConflictUpdate updq))) crets
  driver <- ask
  _ <- liftIO $ dbInsert driver insertQ
  pure ()

insertMany_ :: forall sc tab row defs reqCols driver cfg.
  ( Table sc tab
  , MonadIO (DBM (SchemaDB sc))
  , KnownSymbol (SchemaName sc)
  , Schema sc
  , defs ~ HasDefault sc tab
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  , driver ~ Driver (DBM (SchemaDB sc))
  , MonadReader (driver cfg) (DBM (SchemaDB sc))
  , HasInsert driver
  , SingCtx sc tab
  , SingCtxSc sc
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)    
  ) => Rows sc tab row -> DBM (SchemaDB sc) ()
insertMany_ rows = do
  let
    tabFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns tabFlds colIs)
    cexprss = fmap (toDBValues (Proxy @sc)) values    
    values = fmap (\row -> toHList row (\v -> Identity v)) (getRows rows)
    insertQ = InsertQuery (getTableId (Proxy @sc) (Proxy @tab)) cnames (NE.fromList cexprss) Nothing []
  driver <- ask
  _ <- liftIO $ dbInsert driver insertQ
  pure ()

insertManyWithConflict_ :: forall sc tab row keys keyFields defs scopes reqCols driver cfg.
  ( Table sc tab
  , MonadIO (DBM (SchemaDB sc))
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , keyFields ~ FieldsOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , defs ~ HasDefault sc tab 
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  , driver ~ Driver (DBM (SchemaDB sc))
  , MonadReader (driver cfg) (DBM (SchemaDB sc))
  , SingCtx sc tab
  , SingCtxSc sc
  , HasInsert driver
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , SingI keyFields
  , SingE keyFields
  ) => ConflictTarget' sc tab        -> 
      ConflictAction' sc tab scopes ->   
      Rows sc tab row ->       
      DBM (SchemaDB sc) ()
insertManyWithConflict_ ctgt cact rows = do
  let
    tabId = getTableId (Proxy @sc) (Proxy @tab)
    values = fmap (\row -> toHList row (\v -> Identity v)) (getRows rows)    
    reqFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    pkFlds = fromSing (sing :: Sing keyFields)
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns reqFlds colIs)
    crets = map columnExpr (filterColumns pkFlds colIs)
    prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)
    cexprss = fmap (toDBValues (Proxy @sc)) values    
    tgt = case ctgt of
      ConflictTargetConstraint' t -> ConflictConstraint t
      ConflictTargetColumns' cols -> ConflictColumn (map symFromText cols)
      ConflictTargetAnon' -> ConflictAnon
    insertQ = case cact of
      ConflictDoNothing' -> InsertQuery tabId cnames (NE.fromList cexprss) (Just (Conflict tgt ConflictDoNothing)) crets
      ConflictUpdate' crit updFn ->
        let updq = UpdateQuery tabId [getExpr $ crit (Q.Columns prjs)] (HM.toList $ getUpdateMap $ updFn EmptyUpdate) []
        in InsertQuery tabId cnames (NE.fromList cexprss) (Just (Conflict tgt (ConflictUpdate updq))) crets
  driver <- ask
  _ <- liftIO $ dbInsert driver insertQ
  pure ()

newtype Row sc tab row = Row { getRow :: row }

withRow :: row -> Row sc tab row
withRow = coerce

newtype Rows sc tab row = Rows { getRows :: [row] }

withRows :: [row] -> Rows sc tab row
withRows = coerce

getBaseTableExpr :: forall sc tab.
                ( SingCtx sc tab
                , SingCtxSc sc
                ) => Proxy sc -> Proxy tab -> TableExpr PrimQuery
getBaseTableExpr _ _ =
  let tabId = getTableId (Proxy @sc) (Proxy @tab)
  in  TableName tabId 

getBaseTable :: forall sc tab.
                ( SingCtx sc tab
                , SingCtxSc sc
                ) => Proxy sc -> Proxy tab -> PrimQuery
getBaseTable _ _ =
  let tabId = getTableId (Proxy @sc) (Proxy @tab)
      tabFlds = getTableProjections (Proxy @sc) (Proxy @tab)      
  in  Table (Just (TableName tabId)) (clauses { projections = tabFlds })
  
