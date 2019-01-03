{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module DBRecord.Sqlite.Internal.Query where

import Database.Sqlite.Simple as SQS
import Database.Sqlite.Simple.FromRow as SQS
import DBRecord.Query


type instance FromDBRow SQS a = FromRow a
type instance ToDBRow   SQS a = ToRow a

data SQS cfg where
  SQS :: SQS.Connection -> PGS PGS.Connection

{-
instance Session SQS where
  data SessionConfig SQS cfg where
    PGSConfig :: PG.Config_ -> SessionConfig PGS PGS.Connection
  runSession (PGSConfig cfg) dbact =
    withResource (PG.connectionPool cfg) $ \conn -> runReaderT dbact $ PGS conn

instance HasTransaction PGS where
  withTransaction (PGS conn) dbact = .withTransaction conn dbact
-}
