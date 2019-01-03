{-# LANGUAGE TypeFamilies, DataKinds, DeriveGeneric, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module DBRecord.Postgres.Internal.Types where

import qualified DBRecord.Internal.Types as Type
import DBRecord.Internal.DBTypes
import Data.Text
import GHC.TypeLits
import GHC.Generics
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Expr
import Data.String
import qualified Data.Text as T
import Data.ByteString (ByteString)
import DBRecord.Internal.Types (DbK (..))
import Data.Proxy

newtype PostgresDBT m (db :: *) a = PostgresDBT { runPostgresDB :: ReaderT (PGS PGS.Connection) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (PGS PGS.Connection))

type PostgresDB = PostgresDBT IO
