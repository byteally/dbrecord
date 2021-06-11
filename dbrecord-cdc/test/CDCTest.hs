{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module CDCTest where

import DBRecord.CDC
import DBRecord.Schema
import GHC.Generics
import GHC.Records
import qualified GHC.Records as GRec
import Control.Monad
import Data.Text (Text)


data TestDB = TestDB
  deriving (Show, Eq, Generic)

instance Database TestDB where
  type DB TestDB = 'Postgres
  type DatabaseName TestDB = "test_cdc"

instance Schema TestDB where
  type Tables TestDB = '[ TestCdc
                        ]
  type SchemaDB TestDB = TestDB

data TestCdc = TestCdc
  { testCdcId :: Int
  , f1 :: Int
  , f2 :: Text
  } deriving (Show, Eq, Generic)

instance Table TestDB TestCdc where
  type TableName TestDB TestCdc = "test_cdc"
  type PrimaryKey TestDB TestCdc = '["testCdcId"]
  type ColumnNames TestDB TestCdc = '[ '("testCdcId", "id")
                                     ]

instance CDC TestDB where
  type CDCTables TestDB = '[TestCdc]

sampleRegistration mgr = do
  registerHandler @TestDB @TestCdc @["f1", "f2"]
    (Handler $ \vs -> do
        forM_ vs $ \v -> do
          case v of
            CdcInsert a -> do
              print "CDCInsert"
              print (GRec.getField @"f1" a, GRec.getField @"f2" a)
            CdcDelete a -> do
              print "CDCDelete"
              print (GRec.getField @"f1" a, GRec.getField @"f2" a)
            CdcUpdate { cdcDataOld, cdcDataNew } -> do
              print "CDCUpdate"
              print (GRec.getField @"f1" cdcDataOld, GRec.getField @"f2" cdcDataOld)
              print (GRec.getField @"f1" cdcDataNew, GRec.getField @"f2" cdcDataNew)
    ) mgr
