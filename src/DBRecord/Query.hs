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

-- | 

module DBRecord.Query
       ( module DBRecord.Internal.Order
       , module DBRecord.Internal.Expr
       , module DBRecord.Internal.Predicate
       , get, getBy, getAll
       , update, update_
       , delete
       , insert, insert_, insertRetAll, insertMany, insertMany_
       , count
       , (.~), (%~)
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
import DBRecord.Internal.Schema
import DBRecord.Internal.PrimQuery
import DBRecord.Internal.Types
import DBRecord.Internal.DBTypeValidation
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Int
import Data.Typeable
import GHC.Exts
import GHC.TypeLits
import Data.Functor.Const
import Data.Functor.Identity
import qualified DBRecord.Internal.Postgres.SqlGen as PG
import qualified DBRecord.Internal.Postgres.Pretty as PG
import qualified DBRecord.Transaction as PG
import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.PostgreSQL.Simple as PGS
import Database.PostgreSQL.Simple.FromRow as PGS
import Data.Pool (withResource)

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
        ( UnifyField sc (fn ::: val) ('Text "Unable to find column " ':<>: 'ShowType fn)
        , sc ~ (OriginalTableFields tab)
        , KnownSymbol fn
        ) => Col fn -> Expr sc val -> Updated tab sc -> Updated tab sc
(.~) col expr (Updated updates) = Updated $ HM.insert (T.pack $ symbolVal col) (getExpr expr) updates

infixr 4 .~

(%~) :: forall fn sc val tab.
        ( UnifyField sc (fn ::: val) ('Text "Unable to find column " ':<>: 'ShowType fn)
        , sc ~ (OriginalTableFields tab)
        , KnownSymbol fn
        ) => Col fn -> (Expr sc val -> Expr sc val) -> Updated tab sc -> Updated tab sc
(%~) col' exprFn updates = (.~) col' (exprFn $ col (Proxy :: Proxy fn)) updates

infixr 4 %~

type family FromDBRow (driver :: * -> *) (a :: *) :: Constraint
type family ToDBRow (driver :: * -> *) (a :: *) :: Constraint
  
type instance FromDBRow (PGS) a = (FromRow a)
type instance ToDBRow (PGS) a = (ToRow a)

data PGS cfg where
  PGS :: PGS.Connection -> PGS PGS.Connection

class HasUpdate (driver :: * -> *) where
  dbUpdate :: (FromDBRow driver a) => driver cfg -> UpdateQuery -> IO [a]

class HasDelete (driver :: * -> *) where
  dbDelete :: driver cfg -> DeleteQuery -> IO Int64

class HasQuery (driver :: * -> *) where
  dbQuery :: (FromDBRow driver a) => driver cfg -> PrimQuery -> IO [a]

class HasInsert (driver :: * -> *) where
  dbInsert :: {-(ToDBRow driver a) =>-} driver cfg -> InsertQuery -> Either a [a] -> IO Int64

class HasInsertReturning (driver :: * -> *) where
  dbInsertReturning :: ( ToDBRow driver a
                       , FromDBRow driver r
                       ) => driver cfg -> InsertQuery -> Either a [a] -> IO (Either r [r])


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
  

instance HasUpdate PGS where
  dbUpdate (PGS conn) updateQ = do
    let updateSQL = PG.renderUpdate $ PG.updateSql $ updateQ
    putStrLn updateSQL
    returningWith fromRow conn (fromString updateSQL) ([]::[()])

instance HasQuery PGS where
  dbQuery (PGS conn) primQ = do
    let sqlQ= PG.renderQuery $ PG.sql primQ
    putStrLn sqlQ
    query_ conn (fromString sqlQ)

instance HasInsert PGS where
  dbInsert (PGS conn) insQ vals = do
    let insSQL = PG.renderInsert $ PG.insertSql insQ
    putStrLn insSQL
    execute_ conn (fromString insSQL)
    

instance HasDelete PGS where
  dbDelete (PGS conn) deleteQ = do
    let delSQL = PG.renderDelete $ PG.deleteSql $ deleteQ
    putStrLn delSQL
    execute_ conn (fromString delSQL)


class ApplyExpr (db :: *) (tab :: *) (cols :: [Symbol]) fn r where
  applyExpr :: Proxy db -> Proxy tab -> Proxy cols -> fn -> r

instance ( fn ~ (Expr (OriginalTableFields tab) a -> res)
         , r ~ res 
         , KnownSymbol cn
         , UnifyField
           (OriginalTableFields tab)
           (cn ::: a)
           ('Text "Unable to find column " ':<>: 'ShowType cn)
         , ApplyExpr db tab cols res r
         ) => ApplyExpr db tab (cn ': cols) fn r where
  applyExpr pdb ptab _ fn = applyExpr pdb ptab (Proxy @cols) (fn (col (Proxy @cn)))

instance (fn ~ r) => ApplyExpr db tab '[] fn r where
  applyExpr _ _ _ r = r
  
  

get :: forall tab db predicate driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , KnownSymbol (Schema db)
  , MonadReader (driver cfg) (DBM db)
  , HasUpdate driver
  , HasQuery driver
  , FromDBRow driver tab
  , ApplyExpr db tab (PrimaryKey db tab) predicate (Expr (OriginalTableFields tab) Bool)
  , UnifyPkPredicate db tab predicate
  ) => predicate -> DBM db (Maybe tab)
get predicate = do
  let tabId = (getTableId (Proxy @db) (Proxy @tab))
      tabFlds = (flip fmap) (getConst (getTableFields @db @tab)) $ \(Column cn _) ->
        (Sym [] cn, BaseTableAttrExpr cn)
      filtE = getExpr ((applyExpr (Proxy @db) (Proxy @tab) (Proxy @(PrimaryKey db tab)) predicate) :: Expr (OriginalTableFields tab) Bool)
      primQ = BaseTable tabId $ Clauses
        { projections = tabFlds
        , criteria = [filtE]
        , windows = []
        , groupbys = []
        , havings = []
        , orderbys = []
        , limit = Nothing
        , offset = Nothing
        , alias = Nothing
        }
  driver <- ask
  res <- liftIO $ dbQuery driver primQ
  pure $ case res of
    []  -> Nothing
    [r] -> Just r
    rs  -> error "get: query with primarykey return more than 1 rows"


getBy :: forall tab (uniq :: Symbol) db predicate driver cfg uqKeysM uqKeys.
  ( Table db tab
  , MonadIO (DBM db)
  , KnownSymbol (Schema db)
  , MonadReader (driver cfg) (DBM db)
  , HasUpdate driver
  , HasQuery driver
  , FromDBRow driver tab
  , uqKeysM ~ (GetUniqBy uniq (Unique db tab))
  , 'Just uqKeys ~ uqKeysM
  , UnifyUqPredicate db tab predicate (Note ('Text "Unable to find unique " ':<>: 'ShowType uniq ':<>: 'Text " in the table " ':<>: 'ShowType tab ':<>: 'Text " of database " ':<>: 'ShowType db) uqKeysM)
  , ApplyExpr db tab uqKeys predicate (Expr (OriginalTableFields tab) Bool)
  ) => Uq uniq -> predicate -> DBM db (Maybe tab)
getBy _ predicate = do
  let tabId = (getTableId (Proxy @db) (Proxy @tab))
      tabFlds = (flip fmap) (getConst (getTableFields @db @tab)) $ \(Column cn _) ->
        (Sym [] cn, BaseTableAttrExpr cn)
      filtE = getExpr ((applyExpr (Proxy @db) (Proxy @tab) (Proxy @uqKeys) predicate) :: Expr (OriginalTableFields tab) Bool)
      primQ = BaseTable tabId $ Clauses
        { projections = tabFlds
        , criteria = [filtE]
        , windows = []
        , groupbys = []
        , havings = []
        , orderbys = []
        , limit = Nothing
        , offset = Nothing
        , alias = Nothing
        }
  driver <- ask
  res <- liftIO $ dbQuery driver primQ
  pure $ case res of
    []  -> Nothing
    [r] -> Just r
    rs  -> error "get: query with primarykey return more than 1 rows"

    
getAll :: forall tab db driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , KnownSymbol (Schema db)
  , MonadReader (driver cfg) (DBM db)
  , HasUpdate driver
  , HasQuery driver
  , FromDBRow driver tab
  ) => Expr (OriginalTableFields tab) Bool
       -> Order (OriginalTableFields tab)
       -> Maybe Page
       -> DBM db [tab]
getAll filt ord page = do
  let tabId = (getTableId (Proxy @db) (Proxy @tab))
      tabFlds = (flip fmap) (getConst (getTableFields @db @tab)) $ \(Column cn _) ->
        (Sym [] cn, BaseTableAttrExpr cn)
      filtE = case filt of
        TRUE -> []
        f    -> [getExpr filt]
      ordE = case ord of
        AnyOrder -> []
        o        -> getOrder ord
      (off, lmt) = case page of
        Nothing -> (Nothing, Nothing)
        (Just (Offset n)) -> (Just n, Nothing)
        (Just (Limit n)) -> (Nothing, Just n)
        (Just (OffsetLimit o l)) -> (Just o, Just l)
        
        
      primQ = BaseTable tabId $ Clauses
        { projections = tabFlds
        , criteria = filtE
        , windows = []
        , groupbys = []
        , havings = []
        , orderbys = ordE
        -- , limit = lmt
        -- , offset = off
        , alias = Nothing
        }
  driver <- ask
  liftIO $ dbQuery driver primQ

count :: forall tab db driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , KnownSymbol (Schema db)
  , MonadReader (driver cfg) (DBM db)
  , HasUpdate driver
  , HasQuery driver
  , FromDBRow driver (Only Int)
  ) => Expr (OriginalTableFields tab) Bool -> DBM db (ColVal tab Int)
count filt = do
  let tabId = (getTableId (Proxy @db) (Proxy @tab))
      filtE = case filt of
        TRUE -> []
        f    -> [getExpr filt]
      primQ = BaseTable tabId $ Clauses
        { projections = []
        , criteria = filtE
        , windows = []
        , groupbys = []
        , havings = []
        , orderbys = []
        , limit = Nothing
        , offset = Nothing
        , alias = Nothing
        }
  driver <- ask
  res <- liftIO $ dbQuery driver primQ
  case res of
    [Only count] -> pure $ ColVal count

update :: forall tab db keys driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , KnownSymbol (Schema db)
  , keys ~ PGS.Only Int -- TODO: Remove this
  , driver ~ Driver (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasUpdate driver
  , FromDBRow driver keys
  ) => Expr (OriginalTableFields tab) Bool
  -> (Updated tab (OriginalTableFields tab) -> Updated tab (OriginalTableFields tab))
  -> DBM db [keys]
update filt updateFn = do
  let updateQ = UpdateQuery (getTableId (Proxy @db) (Proxy @tab)) [getExpr filt] (HM.toList $ getUpdateMap $ updateFn EmptyUpdate)
  driver <- ask
  liftIO $ dbUpdate driver updateQ
  


update_ :: forall tab db.
  ( Table db tab
  , Monad (DBM db)
  , KnownSymbol (Schema db)
  ) => Expr (OriginalTableFields tab) Bool
  -> (Updated tab (OriginalTableFields tab) -> Updated tab (OriginalTableFields tab))
  -> DBM db ()
update_ filt updateFn = do
  let updateQ = UpdateQuery (getTableId (Proxy @db) (Proxy @tab)) [getExpr filt] (HM.toList $ getUpdateMap $ updateFn EmptyUpdate)
  pure ()

delete :: forall tab db driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , KnownSymbol (Schema db)
  , driver ~ Driver (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasDelete driver
  ) => Expr (OriginalTableFields tab) Bool -> DBM db (ColVal tab Int64)
delete filt = do
  let deleteQ = DeleteQuery (getTableId (Proxy @db) (Proxy @tab)) [getExpr filt]
  driver <- ask
  ColVal <$> (liftIO $ dbDelete driver deleteQ)


  
data Override tab (sc :: [*]) = Override (Updated tab sc)


toDBValues :: (All ConstExpr xs) => HList Identity xs
           -> HList (Const Column) xs
           -> ([Attribute], [PrimExpr])
           -> ([Attribute], [PrimExpr])
toDBValues (Identity v :& vals) (Const (Column cn _) :& cns) (names, exprs)
  = toDBValues vals cns (cn : names, getExpr (constExpr v) : exprs)
toDBValues Nil Nil res = res
  

insert :: forall tab db row keys defs reqCols driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , KnownSymbol (Schema db)
  , defs ~ (HasDefault db tab :++ PrimaryKey db tab)
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All ConstExpr reqCols
  , SingCols db reqCols (ColumnNames db tab)
  , driver ~ Driver (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasInsert driver
  ) => Proxy tab -> row -> DBM db keys
insert _ row = do
  let 
    values = toHList row (\v -> Identity v)
    tabFlds = singCols (Proxy @db) (Proxy @reqCols) (Proxy @(ColumnNames db tab))
    (cnames, cexpr) = toDBValues values tabFlds ([], [])
    insertQ = InsertQuery (getTableId (Proxy @db) (Proxy @tab)) cnames (NE.fromList [cexpr])
  driver <- ask
  liftIO $ dbInsert driver insertQ (Left undefined)
  pure undefined

insertMany :: forall tab db row keys defs reqCols driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , KnownSymbol (Schema db)
  , defs ~ (HasDefault db tab :++ PrimaryKey db tab)
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All ConstExpr reqCols
  , SingCols db reqCols (ColumnNames db tab)
  , driver ~ Driver (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasInsert driver
  ) => Proxy tab -> [row] -> DBM db ()
insertMany _ rows@(r:_) = do
  let 
    values = fmap (\row -> toHList row (\v -> Identity v)) rows
    tabFlds = singCols (Proxy @db) (Proxy @reqCols) (Proxy @(ColumnNames db tab))
    cexprs = (flip fmap) values $ \v -> snd $ toDBValues v tabFlds ([], [])
    cnames = fst $ toDBValues (toHList r (\v -> Identity v)) tabFlds ([], [])
    insertQ = InsertQuery (getTableId (Proxy @db) (Proxy @tab)) cnames (NE.fromList cexprs)
  driver <- ask
  liftIO $ dbInsert driver insertQ (Left undefined)
  pure ()
insertMany _ [] = pure ()

insertRetAll :: forall tab db.
  ( Table db tab
  , Monad (DBM db)
  , KnownSymbol (Schema db)
  ) => tab -> DBM db tab
insertRetAll row = pure undefined  


insert_ :: forall tab db row keys defs reqCols driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , KnownSymbol (Schema db)
  , defs ~ (HasDefault db tab :++ PrimaryKey db tab)
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All ConstExpr reqCols
  , SingCols db reqCols (ColumnNames db tab)
  , driver ~ Driver (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasInsert driver
  ) => Proxy tab -> row -> DBM db ()
insert_ _ row = do
  let 
    values = toHList row (\v -> Identity v)
    tabFlds = singCols (Proxy @db) (Proxy @reqCols) (Proxy @(ColumnNames db tab))
    (cnames, cexpr) = toDBValues values tabFlds ([], [])
    insertQ = InsertQuery (getTableId (Proxy @db) (Proxy @tab)) cnames (NE.fromList [cexpr])
  driver <- ask
  liftIO $ dbInsert driver insertQ (Left undefined)
  pure ()

insertMany_ :: forall tab db row keys defs reqCols driver cfg.
  ( Table db tab
  , MonadIO (DBM db)
  , KnownSymbol (Schema db)
  , defs ~ (HasDefault db tab :++ PrimaryKey db tab)
  , reqCols ~ (FilterNonDefaults (OriginalTableFields tab) defs)
  , TupleToHList row ~ reqCols
  , ToHList row
  , All ConstExpr reqCols
  , SingCols db reqCols (ColumnNames db tab)
  , driver ~ Driver (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasInsert driver
  ) => Proxy tab -> [row] -> DBM db ()
insertMany_ _ rows@(r:_) = do
  let 
    values = fmap (\row -> toHList row (\v -> Identity v)) rows
    tabFlds = singCols (Proxy @db) (Proxy @reqCols) (Proxy @(ColumnNames db tab))
    cexprs = (flip fmap) values $ \v -> snd $ toDBValues v tabFlds ([], [])
    cnames = fst $ toDBValues (toHList r (\v -> Identity v)) tabFlds ([], [])
    insertQ = InsertQuery (getTableId (Proxy @db) (Proxy @tab)) cnames (NE.fromList cexprs)
  driver <- ask
  liftIO $ dbInsert driver insertQ (Left undefined)
  pure ()
insertMany_ _ [] = pure ()  
