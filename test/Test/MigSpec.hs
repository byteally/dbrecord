{-# LANGUAGE DataKinds, TypeOperators, UndecidableInstances, OverloadedLabels, FlexibleInstances, MultiParamTypeClasses, DuplicateRecordFields, GADTs, TypeApplications, KindSignatures, DeriveGeneric, FlexibleContexts, FunctionalDependencies, ExplicitForAll, TypeFamilies, ScopedTypeVariables, OverloadedStrings, GeneralizedNewtypeDeriving, RankNTypes #-}
module Test.MigSpec {-( spec )-} where

import DBRecord.Schema
import DBRecord.Migration
import DBRecord.Query
import DBRecord.Internal.Types
import DBRecord.Internal.Expr
import DBRecord.Internal.PrimQuery
import Data.Text (Text)
import DBRecord.Internal.DBTypes (GetDBTypeRep)
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
  , foo   :: Int
  } deriving (Generic)

data Employee = Employee
  { id           :: Int
  , userId       :: Int
  , employeeName :: Text
  , designation  :: Text
  } deriving (Generic)

instance BaseDatabase TestDB 0 where
  type BaseTables TestDB 0 =
    '[ 'TypeName "dbrecord" "Test.MigSpec" "User"
     ]

instance BaseTable TestDB ('TypeName "dbrecord" "Test.MigSpec" "User") 0 where
  type BaseColumns TestDB ('TypeName "dbrecord" "Test.MigSpec" "User") 0
    = '[ '( "id",    GetDBTypeRep 'Postgres Int)
       , '( "name",  GetDBTypeRep 'Postgres Text)
       , '( "age",   GetDBTypeRep 'Postgres Int)
       , '( "email", GetDBTypeRep 'Postgres Text)
       , '( "foo",   GetDBTypeRep 'Postgres Int)
       ]
  type BaseColumnNames TestDB ('TypeName "dbrecord" "Test.MigSpec" "User") 0 = '[ '("id", "ID")
      , '("name", "NaME")
      , '("age", "AGE")
      ]
  type BasePrimaryKey TestDB ('TypeName "dbrecord" "Test.MigSpec" "User") 0
    = '[ "id", "email"
       ]
  -- type BasePrimaryKeyName TestDB ('TypeName "dbrecord" "Test.MigSpec" "User") 0 = 'Just "user_id_pkey"
  type BaseUnique TestDB ('TypeName "dbrecord" "Test.MigSpec" "User") 0
    = '[ UniqueOn '[ "name", "age" ] "name_age_is_unique" ]
  -- type BaseUniqueNames TestDB ('TypeName "dbrecord" "Test.MigSpec" "User") 0
  --   = '[ '("name_age_is_unique", "uq_name_age!_!") ]
  -- type BaseTableSequence TestDB ('TypeName "dbrecord" "Test.MigSpec" "User") 0 = '[
  --type BaseDefaultedCols TestDB ('TypeName "dbrecord" "Test.MigSpec" "User") 0
  --  = '[ "foo" ]
{-
  baseDefaults = baseDbDefaults
    (  #foo "foo_value"
    :& end
    )
-}

instance Database TestDB where
  type DB     TestDB = 'Postgres
  type Tables TestDB = '[ User
                        , Employee
                        ]
  type Baseline TestDB = 0
  type Version TestDB  = 0

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
  type ColumnNames TestDB User   = '[ '("email", "EMail")
                                    , '("foo"  , "bar_id")
                                    ]
                                   
  type PrimaryKey TestDB User    = '["id", "name"]
  type Unique TestDB User        = '[ 'UniqueOn '["name", "email"] "User_name_email_key"
                                    ]
  type TableSequence TestDB User = '[Serial "id" "user_seq"]
  checks = dbChecks
    (  #age_positive (\a -> a .> 0)
    :& end
    )



localConnectInfo = ConnectInfo "127.0.0.1" 5432 "sreenidhi" "password" "test_dbrecord"

testDB :: Proxy TestDB
testDB = Proxy       

userTab :: Proxy User
userTab = Proxy       

{-
spec = do
   conn <- runIO $ PGS.connect localConnectInfo
   describe "migration" $
     before (validateSchemaInfo testDB conn >> pure ()) $ it "must match the reified information from db" False

migTest :: IO ()
migTest = do
  conn <- PGS.connect localConnectInfo  
  runMigDiff testDB userTab conn
-}

migTest2 :: IO ()
migTest2 = do
  let stms = renderChangeSets $ mkAllMigrations (Proxy :: Proxy TestDB)
  putStrLn stms
