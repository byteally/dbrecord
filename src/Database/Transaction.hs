{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | 

module Database.Transaction where


import           Control.Exception            (AsyncException,
                                               BlockedIndefinitelyOnMVar,
                                               BlockedIndefinitelyOnSTM,
                                               Deadlock, Handler (Handler),
                                               SomeException (SomeException),
                                               catches, throwIO, Exception(..))
import           Control.Monad.Reader         (ReaderT, ask, runReaderT, MonadReader)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.IO.Class       (liftIO, MonadIO)
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Identity (IdentityT)
import           Control.Monad.Trans.Maybe    (MaybeT)
import           Control.Monad.Trans.State    (StateT)
import           Control.Monad.Trans.Writer   (WriterT)
import           Data.Pool                    (withResource, createPool, Pool)
import           Database.PostgreSQL.Simple (ConnectInfo (..), Connection)
import qualified Database.PostgreSQL.Simple   as PG
import qualified Data.Text as T
import           System.IO                  (hPutStrLn, stderr)

newtype PG a = PG { unPG :: ReaderT Connection IO a }
  deriving (Functor, Applicative, Monad, MonadReader Connection, MonadIO)

unsafeIOToPG :: IO a -> PG a
unsafeIOToPG = PG . liftIO

data Config a = Config
  { connectionPool    :: Pool Connection
  , maxTries          :: Int
  , beforeTransaction :: IO a
  , onRetry           :: forall e . Exception e => e -> a -> IO ()
  , afterTransaction  :: a -> IO ()
  }

type Config_ = Config ()

makeConfig :: Pool Connection -> Config_
makeConfig pc = Config
  { connectionPool    = pc
  , maxTries          = 3
  , beforeTransaction = defaultBeforeTransaction
  , onRetry           = defaultOnRetry
  , afterTransaction  = defaultAfterTransaction
  }

defaultConfig :: ConnectInfo -> IO (Config ())
defaultConfig = fmap makeConfig . defaultPool

defaultPool :: ConnectInfo -> IO (Pool Connection)
defaultPool connectInfo = createPool (PG.connect connectInfo) PG.close 10 5 10

defaultBeforeTransaction :: IO ()
defaultBeforeTransaction = return ()

defaultOnRetry :: Exception e => e -> a -> IO ()
defaultOnRetry e _ = hPutStrLn stderr $ "Error: Exception during database action, retrying: " ++ show e

defaultAfterTransaction :: a -> IO ()
defaultAfterTransaction = const (return ())

setCallbacks :: IO a -> (forall e. Exception e => e -> a -> IO ()) -> (a -> IO ()) -> Config b -> Config a
setCallbacks before retry after c = c
  { beforeTransaction = before
  , onRetry           = retry
  , afterTransaction  = after
  }

class (Functor m, Applicative m, Monad m) => MonadPG m where
  liftPG :: PG a -> m a

instance MonadPG PG where
  liftPG = id

instance MonadPG m => MonadPG (ExceptT e m) where
  liftPG = lift . liftPG

instance MonadPG m => MonadPG (ReaderT r m) where
  liftPG = lift . liftPG

instance (Monoid w, MonadPG m) => MonadPG (WriterT w m) where
  liftPG = lift . liftPG

instance MonadPG m => MonadPG (StateT s m) where
  liftPG = lift . liftPG

instance MonadPG m => MonadPG (IdentityT m) where
  liftPG = lift . liftPG

unsafeIOToTransaction :: MonadPG m => IO a -> m a
unsafeIOToTransaction = liftPG . unsafeIOToPG

runTransaction' :: forall c a . PG a -> Config c -> IO a
runTransaction' q cfg = do
  c <- beforeTransaction cfg
  res <- withRetry c 1
    $ withResource (connectionPool cfg)
    $ \conn -> PG.withTransaction conn . flip runReaderT conn . unPG $ q
  afterTransaction cfg c
  return res
  where
    withRetry :: c -> Int -> IO a -> IO a
    withRetry c n act = act `catchRecoverableExceptions` handler c n act
    handler :: c -> Int -> IO a -> SomeException -> IO a
    handler c n act (SomeException e) =
      if n < maxTries cfg
        then onRetry cfg e c >> withRetry c (n + 1) act
        else throwIO e
    catchRecoverableExceptions :: IO a -> (SomeException -> IO a) -> IO a
    catchRecoverableExceptions action h = action `catches`
      [ Handler $ \(e :: AsyncException)            -> throwIO e
      , Handler $ \(e :: BlockedIndefinitelyOnSTM)  -> throwIO e
      , Handler $ \(e :: BlockedIndefinitelyOnMVar) -> throwIO e
      , Handler $ \(e :: Deadlock)                  -> throwIO e
      , Handler $ \(e :: SomeException)             -> h e
      ]

class (Functor m, Applicative m, Monad m) => MonadTransaction m where
  runTransaction :: PG a -> m a

{-
instance {-# OVERLAPPABLE #-} MonadTransaction m => MonadTransaction (ReaderT r m) where
  runTransaction = lift . runTransaction

instance {-# OVERLAPPING #-} MonadTransaction (ReaderT (Config a) IO) where
-}
instance MonadTransaction (ReaderT (Config a) IO) where
  runTransaction t = ask >>= lift . runTransaction' t

instance MonadTransaction m => MonadTransaction (ExceptT e m) where
  runTransaction = lift . runTransaction

instance MonadTransaction m => MonadTransaction (MaybeT m) where
  runTransaction = lift . runTransaction
