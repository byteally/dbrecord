{-# OPTIONS_GHC -fdefer-type-errors  #-}

{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}

module Test.DBRecord.MSSQL where

import           Test.DBRecord.RoundTrip
import           Database.MsSQL hiding (runSession)
import           DBRecord.MSSQL hiding (runSession)
import           Test.Tasty
import           Data.Functor.Identity
import           Control.Monad.Reader
import qualified DBRecord.Internal.Types as DBR
import           DBRecord.Internal.Expr
import           Data.Proxy
import           Hedgehog
import           DBRecord.Query
import           Data.Coerce
import qualified DBRecord.Internal.PrimQuery as PQ

sqlServerlocalConnInfo :: ConnectInfo
sqlServerlocalConnInfo =
  connectInfo (ConnectionString "Chinook" "localhost" 1433 "sa"
                                "p@ssw0rd" odbcSQLServer17 defProperties
              )

test_roundTrip_mssql :: TestTree
test_roundTrip_mssql = 
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
