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

module DBRecord.Old.Query
       ( module DBRecord.Internal.Order
       , module DBRecord.Internal.Expr
       , module DBRecord.Internal.Window
       , module DBRecord.Internal.Predicate
       , getBy, get, getAll
       , delete
       , insert, insert_, insertMany, insertMany_, insertRet, insertManyRet
       , insertRetWithConflict
       , insertManyRetWithConflict
       , insertManyWithConflict
       , insertWithConflict_
       , update, update_, updateRet
       -- , count
       , (.~) , (%~)
       , Page(..)
       , DBTag
       , Updatable
       , Updated
       , RowCount
       , rawClauses
       , rawTable
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

       , QueryCtx
       ) where

import DBRecord.Old.Schema

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
import qualified UnliftIO as U
import qualified Control.Monad.Trans.Control as U
import Data.Kind 
import GHC.Generics
import DBRecord.Internal.Driver

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

newtype RowCount tab = RowCount { getRowCount :: Int64 }
                     deriving (Show, Eq, Ord, Num)

newtype Updated sc tab = Updated {getUpdateMap :: HashMap Attribute PrimExpr}
  deriving (Show)

newtype WithKey sc t = WithKey { getWithKey :: t }

withPrimaryKey :: forall sc t. t -> WithKey sc t
withPrimaryKey = WithKey

pattern EmptyUpdate :: Updated sc tab
pattern EmptyUpdate <- (HM.null . getUpdateMap -> True) where
  EmptyUpdate = Updated HM.empty

(.~) :: forall sc tab fn alfn val.
        ( UnifyField (OriginalTableFields tab) fn val ('Text "Unable to find column " ':<>: 'ShowType fn)
        , KnownSymbol fn
        , alfn ~ AliasedCol fn (ColumnNames sc tab)
        , KnownSymbol alfn
        ) => Col fn -> Expr sc val -> Updated sc tab -> Updated sc tab
(.~) _ expr (Updated updates) = Updated $ HM.insert (T.pack alfn) (getExpr expr) updates
  where alfn = symbolVal (Proxy @alfn)

infixr 4 .~

(%~) :: forall sc tab fn alfn val.
        ( UnifyField (OriginalTableFields tab) fn val ('Text "Unable to find column " ':<>: 'ShowType fn)
        , KnownSymbol fn
        , Table sc tab
        , SingCtx sc tab
        , SingCtxSc sc
        , alfn ~ AliasedCol fn (ColumnNames sc tab)
        , KnownSymbol alfn          
        ) => Col fn -> (Expr sc val -> Expr sc val) -> Updated sc tab -> Updated sc tab
(%~) col' exprFn updates = (.~) col' (exprFn $ col (Proxy :: Proxy (DBTag db tab fn))) updates

infixr 4 %~


class HasCol sc tab (t :: *) where
  hasCol :: proxy (DBTag sc tab t) -> Expr sc t

instance ( KnownSymbol fld
         , Table sc tab
         , UnifyField (OriginalTableFields tab) fld t ('Text "Unable to find column " ':<>: 'ShowType fld)
         , All SingE (ColumnNames sc tab)
         , All SingE (GetFieldInfo sc (DB (SchemaDB sc)) (GenTabFields (Rep tab)))
         , SingI (GetFieldInfo sc (DB (SchemaDB sc)) (GenTabFields (Rep tab)))
         , SingI (ColumnNames sc tab)
         ) => HasCol sc tab (fld ::: t) where
  hasCol _ = coerceExpr (col (Proxy @(DBTag sc tab fld)) :: Expr sc t) 

applyEqs :: forall xs tab sc.
             ( All (HasCol sc tab) xs
             , All (ConstExpr sc) xs
             , All (EqExpr sc) xs
             ) =>
             Proxy (DBTag sc tab ()) ->
             HList Identity xs ->
             Expr sc Bool
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

get :: forall sc tab m driver tpks pks.
  ( QueryCtx sc tab m driver
  , ToHList tpks
  , TupleToHList tpks ~ pks
  , pks ~ FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab))
  , All (EqExpr sc) pks
  , All (ConstExpr sc) pks
  , All (HasCol sc tab) pks
  ) => WithKey sc tpks -> m (Maybe tab)
get tpks = do
  let filtE = getExpr (applyEqs (Proxy @(DBTag sc tab ())) (toHList (getWithKey tpks) Identity) :: Expr sc Bool)
      cls = clauses { criteria = [filtE] }
  res <- getAll' (Proxy :: Proxy sc) cls
  pure $ case res of
    []  -> Nothing
    [r] -> Just r
    _   -> error "get: query with primarykey return more than 1 rows"

getBy :: forall sc tab m (uniq :: Symbol) driver uqKeysM uqKeys tuqs uqs.
  ( QueryCtx sc tab m driver
  , uqKeysM ~ (GetUniqBy uniq (Unique sc tab))
  , 'Just uqKeys ~ uqKeysM
  , uqs ~ FromRights (FindFields (OriginalTableFields tab) uqKeys)
  , All (EqExpr sc) uqs
  , All (ConstExpr sc) uqs
  , All (HasCol sc tab) uqs
  , ToHList tuqs
  , TupleToHList tuqs ~ uqs
  ) => Uq sc uniq -> tuqs -> m (Maybe tab)
getBy _ tuqs = do
  let filtE = getExpr (applyEqs (Proxy @(DBTag sc tab ())) (toHList tuqs Identity) :: Expr sc Bool)
      cls = clauses { criteria = [filtE] }
  res <- getAll' (Proxy :: Proxy sc) cls
  pure $ case res of
    []  -> Nothing
    [r] -> Just r
    _  -> error "get: query with primarykey return more than 1 rows"

getAll :: forall sc tab m driver.
  ( QueryCtx sc tab m driver
  ) =>    (Q.Columns tab -> Expr sc Bool)
       -> Order sc
       -> Maybe Page
       -> m [tab]
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

type QueryCtx sc tab m driver =
  ( Table sc tab
  , HasQuery driver
  , FromDBRow driver tab
  , MonadReader driver m
  , MonadIO m
  , SingCtx sc tab
  , SingCtxSc sc
  , Break (NoGeneric tab) (Rep tab)
  , Break0 (NoSchema sc) (SchemaDB sc)
  )

query ::
  ( HasQuery driver
  , FromDBRow driver tab
  , MonadReader driver m
  , MonadIO m
  ) => Q.Tab sc tab -> m [tab]
query = runQuery . Q.convert . Q.getQuery

getAll' :: forall sc tab m driver proxy.
            ( QueryCtx sc tab m driver
            ) => proxy sc -> Clauses -> m [tab]
getAll' p cl = rawClauses p (cl { projections = getTableProjections (Proxy @sc) (Proxy @tab) })

rawClauses :: forall sc tab m driver proxy.
             ( QueryCtx sc tab m driver
             ) => proxy sc -> PQ.Clauses -> m [tab]
rawClauses _ = runQuery . rawTable (Proxy @'(sc, tab))

rawTable :: forall sc tab proxy.
  ( SingCtxSc sc 
  , SingCtx sc tab 
  , Table sc tab
  ) => proxy '(sc, tab) -> PQ.Clauses -> PQ.PrimQuery
rawTable _ cls =
  let tabId = getTableId (Proxy @sc) (Proxy @tab)
  in  Table (Just (TableName tabId)) cls
  {-
count :: forall tab db driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasQuery driver
  , FromDBRow driver (Only Int)
  , SingCtx db tab
  , SingCtxSc db
  ) => Expr (OriginalTableFields tab) Bool -> DBM db (RowCount tab)
count filt = do
  let tabId = getTableId (Proxy @db) (Proxy @tab)
      filtE = case filt of
        TRUE -> []
        _    -> [getExpr filt]
      cls = clauses { criteria = filtE }
  res <- runQuery tabId cls    
  case res of
    [Only ct] -> pure $ RowCount ct
    _         -> error "Panic: Expecting only a singleton @count"
-}

runQuery :: ( MonadIO m
            , MonadReader driver m
            , HasQuery driver
            , FromDBRow driver tab
            ) => PrimQuery -> m [tab]
runQuery primQ = do
  driver <- ask
  liftIO $ dbQuery driver primQ

type family Updatable sc tab :: Constraint where
  Updatable sc tab =
    Updatable' sc tab (TableType sc tab)

type family Updatable' sc tab (ttyp :: TableTypes) :: Constraint where
  Updatable' _ _ 'BaseTable = ()
  Updatable' _ _ 'UpdatableView = ()
  Updatable' sc tab 'NonUpdatableView =
    TypeError ('Text "The view " ':<>: 'ShowType tab ':<>: 'Text " in schema " ':<>: 'ShowType sc ':<>: 'Text " cannot be used in update")

update :: forall sc tab m keys driver keyFields.
  ( Table sc tab
  , MonadIO m
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , keyFields ~ FieldsOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))  
  , driver ~ Driver (SchemaDB sc)
  , MonadReader driver m
  , HasUpdateRet driver
  , FromDBRow driver (HListToTuple keys)
  , SingCtx sc tab
  , SingCtxSc sc
  , Updatable sc tab
  , SingI keyFields
  , SingE keyFields
  ) => (Q.Columns tab -> Expr sc Bool)
  -> (Updated sc tab -> Updated sc tab)
  -> m [HListToTuple keys]  
update filt updateFn =
  runUpdateRet (Proxy @sc) (Proxy @tab) [getExpr $ filt (Q.Columns prjs)] (HM.toList $ getUpdateMap $ updateFn EmptyUpdate) keyExprs

  where
    prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)
    keyExprs = map columnExpr (filterColumns pkFlds colIs)
    pkFlds = fromSing (sing :: Sing keyFields)
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    
updateRet :: forall sc tab m {-keys-} driver rets.
  ( Table sc tab
  , MonadIO m
  , driver ~ Driver (SchemaDB sc)
  , MonadReader driver m
  , HasUpdateRet driver
  , FromDBRow driver (HListToTuple rets)
  , SingCtx sc tab
  , SingCtxSc sc
  , Updatable sc tab
  ) => (Q.Columns tab -> Expr sc Bool)
  -> (Updated sc tab -> Updated sc tab)
  -> HList (Expr sc) rets -> m [HListToTuple rets]  
updateRet filt updateFn rets =
  runUpdateRet (Proxy @sc) (Proxy @tab) [getExpr $ filt (Q.Columns prjs)] (HM.toList $ getUpdateMap $ updateFn EmptyUpdate) (toPrimExprs rets)

  where prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)
  
update_ :: forall sc tab m driver.
  ( Table sc tab
  , KnownSymbol (SchemaName sc)
  , Schema sc
  , SingCtx sc tab
  , SingCtxSc sc
  , MonadIO m
  , MonadReader driver m
  , HasUpdate driver
  , Updatable sc tab
  ) => (Q.Columns tab -> Expr sc Bool)
  -> (Updated sc tab -> Updated sc tab)
  -> m ()
update_ filt updateFn =
  runUpdate (Proxy @sc) (Proxy @tab) [getExpr $ filt (Q.Columns prjs)] (HM.toList $ getUpdateMap $ updateFn EmptyUpdate)

  where prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)

runUpdate :: forall sc tab m driver.
             ( Table sc tab
             , SingCtx sc tab
             , SingCtxSc sc
             , MonadIO m
             , MonadReader driver m
             , HasUpdate driver
             ) => Proxy sc -> Proxy tab -> [PrimExpr] -> Assoc -> m ()
runUpdate psc ptab crit assoc = do
  let updateQ = UpdateQuery tabId crit assoc []
      tabId   = getTableId psc ptab
  driver <- ask
  (_ :: Identity Int64) <- Identity <$> (liftIO $ dbUpdate driver updateQ)
  pure ()

runUpdateRet :: forall sc tab m driver a.
                ( Table sc tab
                , SingCtx sc tab
                , SingCtxSc sc
                , MonadIO m
                , MonadReader driver m
                , HasUpdateRet driver
                , FromDBRow driver a
                ) => Proxy sc -> Proxy tab -> [PrimExpr] -> Assoc -> [PrimExpr] -> m [a]
runUpdateRet psc ptab crit assoc rets = do
  let updateQ = UpdateQuery tabId crit assoc rets
      tabId   = getTableId psc ptab  
  driver <- ask
  liftIO $ dbUpdateRet driver updateQ

delete :: forall sc tab m driver.
  ( Table sc tab
  , MonadIO m
  , KnownSymbol (SchemaName sc)
  , Schema sc
  , driver ~ Driver (SchemaDB sc)
  , MonadReader driver m
  , HasDelete driver
  , SingCtx sc tab
  , SingCtxSc sc
  ) => (Q.Columns tab -> Expr sc Bool) -> m (RowCount tab)
delete filt = do
  let deleteQ = DeleteQuery (getTableId (Proxy @sc) (Proxy @tab)) [getExpr $ filt (Q.Columns prjs)]
  driver <- ask
  RowCount <$> (liftIO $ dbDelete driver deleteQ)

    where prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)


-- TODO: is list reversed?
toPrimExprs :: HList (Expr sc) xs ->
               [PrimExpr]
toPrimExprs = toPrimExprs' []

toPrimExprs' :: [PrimExpr] -> HList (Expr sc) xs -> [PrimExpr]
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

type family Insertable sc tab :: Constraint where
  Insertable sc tab =
    Insertable' sc tab (TableType sc tab)

type family Insertable' sc tab (ttyp :: TableTypes) :: Constraint where
  Insertable' _ _ 'BaseTable = ()
  Insertable' _ _ 'UpdatableView = ()
  Insertable' sc tab 'NonUpdatableView =
    TypeError ('Text "The view " ':<>: 'ShowType tab ':<>: 'Text " in schema " ':<>: 'ShowType sc ':<>: 'Text " cannot be used in insert")

insert :: forall sc tab m row keys keyFields defs reqCols driver.
  ( Table sc tab
  , MonadIO m
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , keyFields ~ FieldsOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , defs ~ HasDefault sc tab 
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  , driver ~ Driver (SchemaDB sc)
  , MonadReader driver m
  , SingCtx sc tab
  , SingCtxSc sc
  , FromDBRow driver (HListToTuple keys)
  , HasInsertRet driver
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , SingI keyFields
  , SingE keyFields
  , Insertable sc tab
  ) => Row sc tab row -> m (Maybe (HListToTuple keys))
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
    insertQ = InsertQuery tabId cnames (InsertValues $ pure cexprs) Nothing crets
  driver <- ask
  out <- liftIO $ dbInsertRet driver insertQ
  pure $ case out of
    [] -> Nothing
    [x] -> Just x
    _ ->  error "insert: insert query with return more than 1 rows"
  

insertRet :: forall sc tab m row keys rets defs reqCols driver.
  ( Table sc tab
  , MonadIO m
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , defs ~ HasDefault sc tab
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  , driver ~ Driver (SchemaDB sc)
  , MonadReader driver m
  , SingCtx sc tab
  , SingCtxSc sc
  , FromDBRow driver (HListToTuple keys)
  , HasInsertRet driver
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , Insertable sc tab
  ) => Row sc tab row -> (Q.Columns tab -> HList (Expr sc) rets) -> m (Maybe (HListToTuple keys))
insertRet row rets = do
  let
    tabId = getTableId (Proxy @sc) (Proxy @tab)
    values = toHList (getRow row) (\v -> Identity v)
    tabFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns tabFlds colIs)
    prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)    
    cexprs = toDBValues (Proxy @sc) values
    insertQ = InsertQuery tabId cnames (InsertValues $ pure cexprs) Nothing (toPrimExprs $ rets (Q.Columns prjs))
  driver <- ask
  out <- liftIO $ dbInsertRet driver insertQ
  pure $ case out of
    [] -> Nothing
    [x] -> Just x
    _ ->  error "insertRet: insert query with return more than 1 rows"

onConflictUpdate :: (Q.Columns tab -> Expr sc Bool) ->
                   (Updated sc tab -> Updated sc tab) ->
                   ConflictAction' sc tab
onConflictUpdate = ConflictUpdate'

onConflictDoNothing :: ConflictAction' sc tab
onConflictDoNothing = ConflictDoNothing'
                      
data ConflictAction' sc tab =
    ConflictDoNothing'
  | ConflictUpdate' (Q.Columns tab -> Expr sc Bool) (Updated sc tab -> Updated sc tab)

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

insertMany :: forall sc tab m row defs reqCols driver keys.
  ( Table sc tab
  , MonadIO m
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))    
  , defs ~ HasDefault sc tab
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  -- , SingCols sc reqCols (ColumnNames sc tab)
  , driver ~ Driver (SchemaDB sc)
  , MonadReader driver m
  , HasInsert driver
  , SingCtx sc tab
  , SingCtxSc sc
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , FromDBRow driver (HListToTuple keys)
  , HasInsertRet driver
  , Insertable sc tab
  ) => Rows sc tab row -> m [HListToTuple keys]
insertMany rows = do
  let
    tabFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns tabFlds colIs)
    cexprss = fmap (toDBValues (Proxy @sc)) values
    values = fmap (\row -> toHList row (\v -> Identity v)) (getRows rows)
    insertQ = InsertQuery (getTableId (Proxy @sc) (Proxy @tab)) cnames (InsertValues $ go cexprss) Nothing []
  driver <- ask
  liftIO $ dbInsertRet driver insertQ

  where
    go [] = error "Panic: Zero exprs @insertMany"
    go vs = NE.fromList vs

insertManyRet :: forall sc tab m row rets defs reqCols driver keys.
  ( Table sc tab
  , MonadIO m
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))    
  , defs ~ HasDefault sc tab
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  -- , SingCols sc reqCols (ColumnNames sc tab)
  , driver ~ Driver (SchemaDB sc)
  , MonadReader driver m
  , HasInsert driver
  , SingCtx sc tab
  , SingCtxSc sc
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , FromDBRow driver (HListToTuple keys)
  , HasInsertRet driver
  , Insertable sc tab  
  ) => Rows sc tab row -> (Q.Columns tab -> HList (Expr sc) rets) -> m [HListToTuple keys]
insertManyRet rows rets = do
  let
    tabFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns tabFlds colIs)
    cexprss = fmap (toDBValues (Proxy @sc)) values
    values = fmap (\row -> toHList row (\v -> Identity v)) (getRows rows)
    insertQ = InsertQuery (getTableId (Proxy @sc) (Proxy @tab)) cnames (InsertValues $ go cexprss) Nothing (toPrimExprs $ rets (Q.Columns prjs))
    prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)    
  driver <- ask
  liftIO $ dbInsertRet driver insertQ

  where
    go [] = error "Panic: Zero exprs @insertManyRet"
    go vs = NE.fromList vs

insert_ :: forall sc tab m row defs reqCols driver.
  ( Table sc tab
  , MonadIO m
  , KnownSymbol (SchemaName sc)
  , Schema sc
  , defs ~ HasDefault sc tab
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  , driver ~ Driver (SchemaDB sc)
  , MonadReader driver m
  , HasInsert driver
  , SingCtx sc tab
  , SingCtxSc sc
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , Insertable sc tab  
  ) => Row sc tab row -> m ()
insert_ = insertMany_ . Rows @sc @tab . pure . getRow

insertWithConflict_ :: forall sc tab m row keys keyFields defs reqCols driver.
  ( Table sc tab
  , MonadIO m
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , keyFields ~ FieldsOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , defs ~ HasDefault sc tab 
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  , driver ~ Driver (SchemaDB sc)
  , MonadReader driver m
  , SingCtx sc tab
  , SingCtxSc sc
  , HasInsert driver
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , SingI keyFields
  , SingE keyFields
  , Insertable sc tab  
  ) => ConflictTarget' sc tab        -> 
      ConflictAction' sc tab ->   
      Row sc tab row ->      
      m ()
insertWithConflict_ ctgt cact row = do
  let
    tabId = getTableId (Proxy @sc) (Proxy @tab)
    values = toHList (getRow row) (\v -> Identity v)
    reqFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    pkFlds = fromSing (sing :: Sing keyFields)
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns reqFlds colIs)
    cexprs = toDBValues (Proxy @sc) values
    prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)
    tgt = case ctgt of
      ConflictTargetConstraint' t -> ConflictConstraint t
      ConflictTargetColumns' cols -> ConflictColumn (map symFromText cols)
      ConflictTargetAnon' -> ConflictAnon
    insertQ = case cact of
      ConflictDoNothing' -> InsertQuery tabId cnames (InsertValues $ pure cexprs) (Just (Conflict tgt ConflictDoNothing)) []
      ConflictUpdate' crit updFn ->
        let updq = UpdateQuery tabId [getExpr $ crit (Q.Columns prjs)] (HM.toList $ getUpdateMap $ updFn EmptyUpdate) []
        in InsertQuery tabId cnames (InsertValues $ pure cexprs) (Just (Conflict tgt (ConflictUpdate updq))) []
  driver <- ask
  _ <- liftIO $ dbInsert driver insertQ
  pure ()

insertRetWithConflict :: forall sc tab m row keys keyFields defs reqCols driver.
  ( Table sc tab
  , MonadIO m
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , keyFields ~ FieldsOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , defs ~ HasDefault sc tab
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  , driver ~ Driver (SchemaDB sc)
  , MonadReader driver m
  , SingCtx sc tab
  , SingCtxSc sc
  , HasInsert driver
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , SingI keyFields
  , SingE keyFields
  , Insertable sc tab
  , FromDBRow driver (HListToTuple keys)
  , HasInsertRet driver
  , Show (HListToTuple keys)
  ) =>
  ConflictTarget' sc tab ->
  ConflictAction' sc tab ->
  Row sc tab row ->
  m (HListToTuple keys)
insertRetWithConflict ctgt cact row = do
  let
    tabId = getTableId (Proxy @sc) (Proxy @tab)
    values = toHList (getRow row) (\v -> Identity v)
    reqFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    pkFlds = fromSing (sing :: Sing keyFields)
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns reqFlds colIs)
    crets = map columnExpr (filterColumns pkFlds colIs)
    prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)
    cexprs = toDBValues (Proxy @sc) values
    tgt = case ctgt of
      ConflictTargetConstraint' t -> ConflictConstraint t
      ConflictTargetColumns' cols -> ConflictColumn (map symFromText cols)
      ConflictTargetAnon' -> ConflictAnon
    insertQ = case cact of
      ConflictDoNothing' -> InsertQuery tabId cnames (InsertValues $ pure cexprs) (Just (Conflict tgt ConflictDoNothing)) crets
      ConflictUpdate' crit updFn ->
        let updq = UpdateQuery tabId [getExpr $ crit (Q.Columns prjs)] (HM.toList $ getUpdateMap $ updFn EmptyUpdate) []
        in InsertQuery tabId cnames (InsertValues $ pure cexprs) (Just (Conflict tgt (ConflictUpdate updq))) crets
  driver <- ask
  out <- liftIO $ dbInsertRet driver insertQ
  pure $ case out of
    [x] -> x
    xs ->  error $ "insert: insert query with return more than 1 rows" <> show xs

insertMany_ :: forall sc tab m row defs reqCols driver.
  ( Table sc tab
  , MonadIO m
  , KnownSymbol (SchemaName sc)
  , Schema sc
  , defs ~ HasDefault sc tab
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  , driver ~ Driver (SchemaDB sc)
  , MonadReader driver m
  , HasInsert driver
  , SingCtx sc tab
  , SingCtxSc sc
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , Insertable sc tab
  ) => Rows sc tab row -> m ()
insertMany_ rows = do
  let
    tabFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns tabFlds colIs)
    cexprss = fmap (toDBValues (Proxy @sc)) values
    values = fmap (\row -> toHList row (\v -> Identity v)) (getRows rows)
    insertQ = InsertQuery (getTableId (Proxy @sc) (Proxy @tab)) cnames (InsertValues $ go cexprss) Nothing []
  driver <- ask
  _ <- liftIO $ dbInsert driver insertQ
  pure ()

  where
    go [] = error "Panic: Zero exprs @insertMany_"
    go vs = NE.fromList vs


insertManyRetWithConflict :: forall sc tab m row keys keyFields defs reqCols driver.
  ( Table sc tab
  , MonadIO m
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , keyFields ~ FieldsOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , defs ~ HasDefault sc tab
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  , driver ~ Driver (SchemaDB sc)
  , MonadReader driver m
  , SingCtx sc tab
  , SingCtxSc sc
  , HasInsert driver
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , SingI keyFields
  , SingE keyFields
  , Insertable sc tab
  , FromDBRow driver (HListToTuple keys)
  , HasInsertRet driver
  ) =>
  ConflictTarget' sc tab ->
  ConflictAction' sc tab ->
  Rows sc tab row ->
  m [HListToTuple keys]
insertManyRetWithConflict ctgt cact rows = do
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
      ConflictDoNothing' -> InsertQuery tabId cnames (InsertValues $ go cexprss) (Just (Conflict tgt ConflictDoNothing)) crets
      ConflictUpdate' crit updFn ->
        let updq = UpdateQuery tabId [getExpr $ crit (Q.Columns prjs)] (HM.toList $ getUpdateMap $ updFn EmptyUpdate) []
        in InsertQuery tabId cnames (InsertValues $ go cexprss) (Just (Conflict tgt (ConflictUpdate updq))) crets
  driver <- ask
  liftIO $ dbInsertRet driver insertQ

  where
    go [] = error "Panic: Zero exprs @insertManyRetWithConflict"
    go vs = NE.fromList vs
  

insertManyWithConflict :: forall sc tab m row keys keyFields defs reqCols driver.
  ( Table sc tab
  , MonadIO m
  , keys ~ TypesOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , keyFields ~ FieldsOf (FromRights (FindFields (OriginalTableFields tab) (PrimaryKey sc tab)))
  , defs ~ HasDefault sc tab
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All (ConstExpr sc) reqCols
  , driver ~ Driver (SchemaDB sc)
  , MonadReader driver m
  , SingCtx sc tab
  , SingCtxSc sc
  , HasInsert driver
  , SingI (FieldsOf reqCols)
  , SingE (FieldsOf reqCols)
  , SingI keyFields
  , SingE keyFields
  , Insertable sc tab
  , FromDBRow driver (HListToTuple keys)
  , HasInsertRet driver
  ) =>
  ConflictTarget' sc tab ->
  ConflictAction' sc tab ->
  Rows sc tab row ->
  m ()
insertManyWithConflict ctgt cact rows = do
  let
    tabId = getTableId (Proxy @sc) (Proxy @tab)
    values = fmap (\row -> toHList row (\v -> Identity v)) (getRows rows)
    reqFlds = fromSing (sing :: Sing (FieldsOf reqCols))
    pkFlds = fromSing (sing :: Sing keyFields)
    colIs = headColInfos (Proxy @sc) (Proxy @tab)
    cnames = map (^. columnNameInfo . dbName) (filterColumns reqFlds colIs)
    prjs = Q.getTableProjections_ (Proxy @sc) (Proxy @tab)
    cexprss = fmap (toDBValues (Proxy @sc)) values
    tgt = case ctgt of
      ConflictTargetConstraint' t -> ConflictConstraint t
      ConflictTargetColumns' cols -> ConflictColumn (map symFromText cols)
      ConflictTargetAnon' -> ConflictAnon
    insertQ = case cact of
      ConflictDoNothing' -> InsertQuery tabId cnames (InsertValues $ go cexprss) (Just (Conflict tgt ConflictDoNothing)) []
      ConflictUpdate' crit updFn ->
        let updq = UpdateQuery tabId [getExpr $ crit (Q.Columns prjs)] (HM.toList $ getUpdateMap $ updFn EmptyUpdate) []
        in InsertQuery tabId cnames (InsertValues $ go cexprss) (Just (Conflict tgt (ConflictUpdate updq))) []
  driver <- ask
  liftIO $ dbInsert driver insertQ
  pure ()
  
  where
    go [] = error "Panic: Zero exprs @insertMany"
    go vs = NE.fromList vs

newtype Row sc tab row = Row { getRow :: row }

withRow :: row -> Row sc tab row
withRow = Row

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
  
