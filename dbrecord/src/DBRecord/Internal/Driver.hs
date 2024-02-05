module DBRecord.Internal.Driver
  ( DBM
  , Driver
  , DBDecoder (..)
  , HasQuery (..)
  , HasInsert (..)
  , HasInsertRet (..)
  , HasUpdate (..)
  , HasUpdateRet (..)
  , HasDelete (..)
  , HasDeleteRet (..)
  , HasSessionConfig (..)
  , Session (..)
  , HasTransaction (..)
  , ToDBRow
  ) where

import Data.Kind
import qualified UnliftIO as U
import qualified Control.Monad.Trans.Control as U
import Data.Int
import Data.Proxy
import qualified DBRecord.Internal.PrimQuery as PQ
import Control.Monad.Reader

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

class (DBDecoder driver) => HasDeleteRet driver where
  dbDeleteRetWith :: FromDBRowParser driver a -> driver -> PQ.DeleteQuery -> IO [a]
  dbDeleteRet :: (FromDBRow driver a) => driver -> PQ.DeleteQuery -> IO [a]

  dbDeleteRet = dbDeleteRetWith (dbDecoder (Proxy :: Proxy driver) (Proxy :: Proxy a))

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
