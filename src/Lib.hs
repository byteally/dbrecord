{-# LANGUAGE DataKinds, TypeOperators, UndecidableInstances, OverloadedLabels, FlexibleInstances, MultiParamTypeClasses, DuplicateRecordFields, GADTs, TypeApplications, KindSignatures, DeriveGeneric, FlexibleContexts, FunctionalDependencies, ExplicitForAll, TypeFamilies, ScopedTypeVariables, PolyKinds, OverloadedStrings #-}
module Lib where

import Database.Migration
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

type User = DBTable "user"
  '[ "id"    ::: Int
   , "name"  ::: Text
   , "email" ::: String
   , "role"  ::: UserRole
   ]

{-
type Profile = DBTable "profile"
  '[ "id"         ::: Int
   , "first_name" ::: Text
   , "last_name"  ::: Text
   , "age"        ::: Maybe Int
   , "address"    ::: Address
   , "sum"        ::: Sum1
   , "prod"       ::: Prod1
   ]
-}

data Profile = Profile
  { id         :: Int
  , first_name :: Text
  , last_name  :: Text
  , age        :: Maybe Int
  , address    :: Address
  , sum        :: Sum1
  , prod       :: Prod1
  } deriving (Generic)

type Address = DBType "address"
  '[ "id"     ::: Int
   , "doorno" ::: Text
   , "addr1"  ::: Text
   , "addr2"  ::: Maybe Text
   , "city"   ::: Text
   ]

instance Database TestDB where
  type Tables TestDB = '[ User
                        , Profile
                        ]
  type Types TestDB = '[ UserRole
                       , Sum1
                       , Prod1
                       , Address -- TODO: Didnt throw an error when not added
                       ]

instance Table TestDB Profile where
  type HasDefault Profile   = '["id"]
  
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
  defaults = dbDefaults
    (  def @"id"   1
    :& def @"role" Admin
    :& end
    )

  checks = dbChecks
    (  check @"notnull" (\name -> name == name)
    :& check @"emailValidity" (\email -> null email)
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
  mapM_ (putStrLn . T.unpack) $ fmap migrationSql mig
