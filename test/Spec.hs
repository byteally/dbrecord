{-# LANGUAGE DataKinds, TypeOperators, UndecidableInstances, OverloadedLabels, FlexibleInstances, MultiParamTypeClasses, DuplicateRecordFields, GADTs, TypeApplications, KindSignatures, DeriveGeneric, FlexibleContexts, FunctionalDependencies, ExplicitForAll, TypeFamilies, ScopedTypeVariables, PolyKinds, OverloadedStrings #-}
module Main where

import DBRecord.Schema
import DBRecord.Migration
import DBRecord.Internal.Migration
import DBRecord.Internal.Types
import DBRecord.Internal.Common

import Data.Text (Text)
import qualified Data.Text as T
import Data.Proxy
import GHC.Generics

data TestDB

data UserRole = Admin | Guest | Nor'mal
  deriving (Show, Generic)

data Sum1 = Con1 {a :: Int, b :: Bool} | Con2 {a :: Int}
  deriving (Show, Generic)

data Prod1 = Prod1 {pa1 :: Text, pb1 :: Double}
  deriving (Show, Generic)

data User = User
  { id    :: Int
  , name  :: Text
  , email :: Text
  , role  :: UserRole
  } deriving (Generic)

data Profile = Profile
  { id         :: Int
  , first_name :: Text
  , last_name  :: Text
  , age        :: Maybe Int
  , address    :: Address
  , sum        :: Sum1
  , prod       :: Prod1
  } deriving (Generic)

data Address = Address
  { id     :: Int
  , doorno :: Text
  , addr1  :: Text
  , addr2  :: Maybe Text
  , city   :: Text
  } deriving (Generic)

instance Database TestDB where
  type DB     TestDB = 'Postgres
  type Tables TestDB = '[ User
                        , Profile
                        ]
  type Types TestDB = '[ UserRole
                       , Sum1
                       , Prod1
                       , Address -- TODO: Didnt throw an error when not added
                       ]

instance Table TestDB Profile where
  type HasDefault Profile   = '["first_name"]
  type TableName Profile    = "user_profile"
  type ColumnNames Profile   = '[ '("first_name", "First Name")
                                ]
  type Check Profile        = '[ 'CheckOn '["first_name"] "notnull"]
  type Unique Profile       = '[ '["first_name"]]

instance Table TestDB User where
  type HasDefault User   = '["name"]
  type Check User        = '[ 'CheckOn '["name"] "notnull"
                            , 'CheckOn '["email"] "emailValidity"
                            ]
  type ForeignKey User   = '[ 'RefBy '["age"] User '["name"]
                            , 'Ref "id" User
                            ]
  type PrimaryKey User   = '["id"]
  type Unique User       = '[ '["name"], '["id"]]
  type TableName User    = "usr"
  defaults = dbDefaults
    (  def @"id"   1
    :& def @"role" Admin
    :& end
    )

  checks = dbChecks
    (  check @"notnull" (\name -> name == name)
    :& check @"emailValidity" (\email -> email == email)
    :& end
    )

{-
DBRead
DBWrite
DBUpdate
DBDelete
DBWriteRet
DBUpdateRet
DBJoin
DBLeftJoin
-}

someFunc :: IO ()
someFunc = do
  let mig = mkMigration (Proxy :: Proxy TestDB)
  mapM_ putStrLn (fmap renderMig mig)

main :: IO ()
main = do
  putStrLn "Not Implemented Yet"
