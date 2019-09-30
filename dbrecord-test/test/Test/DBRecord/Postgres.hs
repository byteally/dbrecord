{-# OPTIONS_GHC -fdefer-type-errors  #-}

{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}

module Test.DBRecord.Postgres where

import           Test.DBRecord.RoundTrip
import           DBRecord.Postgres hiding (runSession)
import           Test.Tasty
import           Data.Functor.Identity
import           Control.Monad.Reader
import qualified DBRecord.Internal.Types as DBR
import qualified Database.PostgreSQL.Simple as PG
import           DBRecord.Postgres.Internal.Query (pgDefaultPool)
import           DBRecord.Internal.Expr
import           Data.Proxy
import           Hedgehog
import           DBRecord.Query
import           Data.Coerce
import qualified DBRecord.Internal.PrimQuery as PQ

localConnInfo :: PG.ConnectInfo
localConnInfo =
  PG.ConnectInfo { PG.connectHost     = "172.17.0.2"
                 , PG.connectPort     = 5432
                 , PG.connectUser     = "postgres"
                 , PG.connectPassword = "password"
                 , PG.connectDatabase = "postgres"
                 }

test_roundTrip_postgres :: TestTree
test_roundTrip_postgres = 
  withResource (mssqlDefaultPool sqlServerlocalConnInfo)
               (pure . const ())
               act

  where act ioPool = 
          roundTripProps (Proxy :: Proxy 'DBR.MSSQL) (MSSQLConfig <$> ioPool)

instance RoundTrip 'DBR.MSSQL MSSQL where
  roundTrip _ ioConfig gen = do
    property $ do
      val <- forAll gen
      config <- liftIO ioConfig
      r <- liftIO $ runSession config $ do
        dr <- ask
        liftIO $ dbQuery dr (mkQuery val)
      [Identity val] === r

    where mkQuery = rawQ . pp 
          pp      = coerce . 
                    annotateType' (Proxy :: Proxy 'DBR.MSSQL) .
                    constExpr
          rawQ  e = PQ.Table Nothing (PQ.clauses { PQ.projections = [ ("expr", e) ] })
