module DBRecord.Internal.Driver
  ( runSession
  , runTransaction
  , DBM
  , Driver
  , DBDecoder (..)
  , HasQuery (..)
  , HasInsert (..)
  , HasInsertRet (..)
  , HasUpdate (..)
  , HasUpdateRet (..)
  , HasDelete (..)
  , HasSessionConfig (..)
  , Session (..)
  , HasTransaction (..)
  , TransactionConfig (..)
  , ToDBRow
  ) where

import Data.Kind
import qualified UnliftIO as U
import qualified Control.Monad.Trans.Control as U
import Data.Int
import Data.Proxy
import qualified DBRecord.Internal.PrimQuery as PQ
import Control.Monad.Reader
-- TODO: Revisits the all usage of this
import Control.Exception hiding (TypeError)

type family DBM (db :: Type) = (r :: Type -> Type) | r -> db
-- type family Driver (dbm :: Type -> Type) = (r :: Type -> Type) -- | r -> dbm

type family Driver (db :: Type) = (r :: Type)

class DBDecoder (driver :: Type) where
  type FromDBRowParser driver :: Type -> Type
  type FromDBRow driver  :: Type -> Constraint  
  dbDecoder :: ( FromDBRow driver a
              ) => Proxy driver -> Proxy a -> FromDBRowParser driver a

type family ToDBRow (driver :: Type) (a :: Type) :: Constraint
  
class HasUpdate driver where
  dbUpdate :: driver -> PQ.UpdateQuery -> IO Int64

class (DBDecoder driver) => HasUpdateRet driver where
  dbUpdateRetWith :: FromDBRowParser driver a -> driver -> PQ.UpdateQuery -> IO [a]
  dbUpdateRet     :: (FromDBRow driver a) => driver -> PQ.UpdateQuery -> IO [a]

  dbUpdateRet = dbUpdateRetWith (dbDecoder (Proxy :: Proxy driver) (Proxy :: Proxy a))
  

class HasDelete driver where
  dbDelete :: driver -> PQ.DeleteQuery -> IO Int64

class (DBDecoder driver) => HasQuery driver where
  dbQueryWith :: FromDBRowParser driver a -> driver -> PQ.PrimQuery -> IO [a]

  dbQuery :: (DBDecoder driver, FromDBRow driver a) => driver -> PQ.PrimQuery -> IO [a]
  dbQuery = dbQueryWith (dbDecoder (Proxy :: Proxy driver) (Proxy :: Proxy a))

class HasInsert driver where
  dbInsert :: driver -> PQ.InsertQuery -> IO Int64

class (DBDecoder driver) => HasInsertRet driver where
  dbInsertRetWith :: FromDBRowParser driver a -> driver -> PQ.InsertQuery -> IO [a]  
  
  dbInsertRet :: (FromDBRow driver a) => driver -> PQ.InsertQuery -> IO [a]
  dbInsertRet = dbInsertRetWith (dbDecoder (Proxy :: Proxy driver) (Proxy :: Proxy a))

class HasSessionConfig e driver | e -> driver where
  getSessionConfig :: e -> SessionConfig driver

instance HasSessionConfig (SessionConfig driver) driver where
  getSessionConfig e = e

class Session driver where
  data SessionConfig driver :: Type
  runSession_ :: (U.MonadBaseControl IO m, U.MonadUnliftIO m) => SessionConfig driver -> ReaderT driver m a -> (driver -> m a -> m a) -> m a

class HasTransaction driver where
  withTransaction :: (U.MonadUnliftIO m) => driver -> m a -> m a

data TransactionConfig driver m a = TransactionConfig
  { maxTries          :: Int
  , beforeTransaction :: m a
  , onRetry           :: forall e . Exception e => e -> a -> m ()
  , afterTransaction  :: a -> m ()
  } 

runSession ::
  ( Session driver
  , U.MonadUnliftIO m
  , U.MonadBaseControl IO m
  , MonadReader e m
  , HasSessionConfig e driver
  ) => ReaderT driver m a -> m a
runSession dbact = do
  scfg <- getSessionConfig <$> ask
  runSession_ scfg dbact (\_ io -> io)

runTransaction :: forall driver m a e.
  ( Session driver
  , HasTransaction driver
  , MonadReader e m
  , HasSessionConfig e driver
  , U.MonadUnliftIO m
  , U.MonadBaseControl IO m  
  ) => TransactionConfig driver m a -> ReaderT driver m a -> m a
runTransaction cfg dbact = do 
  scfg <- getSessionConfig <$> ask
  c <- beforeTransaction cfg
  res <- withRetry c 1
    $ runSession_ scfg dbact withTransaction
  afterTransaction cfg c
  return res
  where
    withRetry :: (U.MonadUnliftIO m) => a -> Int -> m a -> m a
    withRetry c n act = act `catchRecoverableExceptions` handler c n act

    handler :: (MonadIO m) => a -> Int -> m a -> SomeException -> m a
    handler a n act (SomeException e) =
      if n < maxTries cfg
        then onRetry cfg e a >> withRetry a (n + 1) act
        else U.throwIO e
    catchRecoverableExceptions :: (U.MonadUnliftIO m) => m a -> (SomeException -> m a) -> m a
    catchRecoverableExceptions action h = action `U.catches`
      [ U.Handler $ \(e :: AsyncException)            -> U.throwIO e
      , U.Handler $ \(e :: BlockedIndefinitelyOnSTM)  -> U.throwIO e
      , U.Handler $ \(e :: BlockedIndefinitelyOnMVar) -> U.throwIO e
      , U.Handler $ \(e :: Deadlock)                  -> U.throwIO e
      , U.Handler $ \(e :: SomeException)             -> h e
      ]

