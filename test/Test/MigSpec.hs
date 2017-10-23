{-# LANGUAGE DataKinds, TypeOperators, UndecidableInstances, OverloadedLabels, FlexibleInstances, MultiParamTypeClasses, DuplicateRecordFields, GADTs, TypeApplications, KindSignatures, DeriveGeneric, FlexibleContexts, FunctionalDependencies, ExplicitForAll, TypeFamilies, ScopedTypeVariables, OverloadedStrings, GeneralizedNewtypeDeriving, RankNTypes #-}
module Test.MigSpec {-( spec )-} where

import DBRecord.Schema
import DBRecord.Migration
import DBRecord.Query
import DBRecord.Internal.Types
import DBRecord.Internal.PrimQuery
import Data.Text (Text)
import qualified Data.Text as T
import Data.Proxy
import GHC.Generics
import Data.Function

import Database.PostgreSQL.Simple as PGS
import Database.PostgreSQL.Simple.FromField as PGS
import DBRecord.Internal.Common
import DBRecord.Internal.Schema
import Data.String
import Control.Monad.Reader
import DBRecord.Transaction hiding (runTrasaction)
import Test.Hspec

data TestDB deriving Generic

data User = User
  { id    :: Int
  , name  :: Text
  , age   :: Int
  , email :: Text
  } deriving (Generic)

data Employee = Employee
  { id           :: Int
  , userId       :: Int
  , employeeName :: Text
  , designation  :: Text
  } deriving (Generic)

instance Database TestDB where
  type DB     TestDB = 'Postgres
  type Tables TestDB = '[ User
                        , Employee
                        ]

instance Table TestDB Employee where
  type TableName TestDB Employee = "EMPloyee"
  type ColumnNames TestDB Employee = '[ '("employeeName", "emp_name")
                                      , '("userId"      , "user_id")
                                      ]
  type HasDefault TestDB Employee  = '[ "designation" ]
  type ForeignKey TestDB Employee  = '[ RefBy '[ "userId", "employeeName" ] User '[ "id", "name" ] "id_ref"]
  type TableSequence TestDB Employee = '[Serial "id" "emp_seq"]
  
  defaults = dbDefaults
    (  #designation (text "common")
    :& end
    )
    
instance Table TestDB User where
  type Check TestDB User         = '[ 'CheckOn '["age"] "age_positive"
                                    ]
  type ColumnNames TestDB User   = '[ '("email", "EMail") ]
                                   
  type PrimaryKey TestDB User    = '["id", "name"]
  type Unique TestDB User        = '[ 'UniqueOn '["name", "email"] "User_name_email_key"
                                    ]
  type TableSequence TestDB User = '[Serial "id" "user_seq"]
  checks = dbChecks
    (  #age_positive (\a -> a .> 0)
    :& end
    )

localConnectInfo = ConnectInfo "10.42.0.1" 5432 "sreenidhi" "" "test_dbrecord"

testDB :: Proxy TestDB
testDB = Proxy       
                   
spec = do
   conn <- runIO $ PGS.connect localConnectInfo
   describe "migration" $
     before (validateSchemaInfo testDB conn >> pure ()) $ it "must match the reified information from db" False
       
