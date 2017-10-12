{-# LANGUAGE DataKinds, TypeOperators, UndecidableInstances, OverloadedLabels, FlexibleInstances, MultiParamTypeClasses, DuplicateRecordFields, GADTs, TypeApplications, KindSignatures, DeriveGeneric, FlexibleContexts, FunctionalDependencies, ExplicitForAll, TypeFamilies, ScopedTypeVariables, OverloadedStrings, GeneralizedNewtypeDeriving, RankNTypes #-}
module Test.QuerySpec ( spec ) where

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

data TestDB

data UserRole = Admin | Guest | Nor'mal
  deriving (Show, Generic, Enum)

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
  type HasDefault TestDB Profile   = '["first_name"]
  type TableName TestDB Profile    = "user_profile"
  type ColumnNames TestDB Profile  = '[ '("first_name", "First Name")
                                      ]
  type Check TestDB Profile        = '[ 'CheckOn '["first_name"] "notnull"]
  type Unique TestDB Profile       = '[ 'UniqueOn '["first_name"] "uq_first_name"]

instance Table TestDB User where
  type HasDefault TestDB User    = '["name"]
  type Check TestDB User         = '[ 'CheckOn '["name"] "notnull"
                                    , 'CheckOn '["email"] "emailValidity"
                                    ]
  type ForeignKey TestDB User    = '[ 'RefBy '["id"] User '["id"]
--                                    , 'Ref "id" User
                                    ]
  type ColumnNames TestDB User   = '[ '("id", "ID") ]
  type PrimaryKey TestDB User    = '["id"]
  type Unique TestDB User        = '[ 'UniqueOn '["name"] "uq_user_name"
                                    ]
  type TableName TestDB User     = "usr"
  type TableSequence TestDB User = '[Serial "id", Owned "id" "sq"]
  defaults = dbDefaults
    (  #role (DBRecord.Query.toEnum Admin)
    -- :& #id   serial
    :& end
    )

  checks = dbChecks
    (  #notnull       (\n -> n .== "foo")
    :& #emailValidity (\em -> em .== em)
    :& end
    )

data TestDBMig

instance BaseDatabase TestDBMig where
  type BaseVersion TestDBMig = ()
  type BaseTables TestDBMig = '["usr"]

instance BaseTable TestDBMig "usr" where
  type BaseColumns TestDBMig "usr" = '[ "id" ::: Int
                                      , "name" ::: Text
                                      , "email" ::: Text
                                      , "role" ::: UserRole
                                      ]
  type BasePrimaryKey TestDBMig "usr" = '["id"]
  type BaseForeignKey TestDBMig "usr" = '[ 'BaseRefBy '["age"] "usr" '["name"]
                                         , 'BaseRef "id" "usr"
                                         ]
  type BaseUnique TestDBMig "usr" = '[ 'UniqueOn '["name"] "uq_user_name"]
  type BaseDefaultedCols TestDBMig "usr" = '["id"]
  type BaseCheck TestDBMig "usr" = '[ 'CheckOn '["email"] "emailValidity"]

instance DBMigration TestDBMig () where
  type CreatedTables TestDBMig () = '["profile"]

newtype MyDBM (db :: *) a = MyDBM {runDB :: ReaderT (PGS PGS.Connection) IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (PGS PGS.Connection))

type instance DBM TestDB = MyDBM TestDB
type instance Driver (MyDBM TestDB) = PGS

{-
withEnv :: Config_ -> ReaderT (PGS PGS.Connection) IO a -> IO a
withEnv cfg dbact
  = withResource (connectionPool cfg) $ \conn -> runReaderT dbact $ PGS conn
-}



runPGMigration :: ( Database db
                   , schema ~ Schema db
                   , tables ~ Tables db
                   , types  ~ Types db
                   , All (Table db) tables
                   , TyCxts db types
                   , SingI tables
                   , SingI types
                   ) => Proxy db -> PGS.Connection -> IO ()
runPGMigration pdb conn = do
  let mig = mkMigration pdb
      migStmtQ = unlines (fmap renderDDL mig)
  withTransaction conn $ do
    execute_ conn (fromString migStmtQ)
  return ()

instance FromRow User where

instance FromField UserRole
  
someFunc :: IO ()
someFunc = do
  let mig = mkMigration (Proxy :: Proxy TestDB)
  mapM_ putStrLn (fmap renderDDL mig)
  conn <- connect $ defaultConnectInfo { connectHost = "172.17.0.2"
                                       , connectPassword = "mysecretpassword"
                                       }
  dbConfig <- defaultConfig $ defaultConnectInfo { connectHost = "172.17.0.2"
                                                 , connectPassword = "mysecretpassword"
                                                 }
  runSession (PGSConfig dbConfig) $ runDB @TestDB $ do
    _ <- getAll @User true AnyOrder (Just (OffsetLimit 10 20))
    _ <- insertMany @User Proxy [ (#email "m@m.com", #role Admin)
                                 , (#email "m1@m.com", #role Admin)
                                 ]
    _ <- insert @User Proxy (#email "m@m.com", #role Admin)
    delete @User (#id .== #id)
    (_ :: Maybe User) <- getBy #uq_user_name (\n -> n .== "foo")
    (_ :: Maybe User) <- get (\uid -> (uid .== 10) .&& (#id .== #id))
    _ <- get @User (\uid -> (uid .== 10) .&& (#id .== #id))
    _ <- getBy @User #uq_user_name (\n -> n .== "foo")
    _cnt <- count @User (#id .== #id)
    update @User (#id .== #id) (\row ->
                                  row & (Col @"id") .~ 10
                                      & (Col @"id") .~ 10
                                      & (Col @"id") %~ (\n -> n + 10)
                               )      
    pure ()

instance ConstExpr UserRole where
  constExpr r = let roleStr = T.pack $ "'"++show r++"'"
                in literalExpr $ Other roleStr

main :: IO ()
main = do 
  conn <- PGS.connect (ConnectInfo "10.42.0.1" 5432 "sreenidhi" "" "test_dbrecord") 
  let mig = mkMigration (Proxy :: Proxy TestDB)
      migStmtQ = unlines (fmap renderDDL mig)
  putStrLn migStmtQ


spec = describe "query" $
  it "is supposed to run queries" pending

{-  
  someFunc
  conn <- connect $ defaultConnectInfo { connectHost = "172.17.0.2"
                                       , connectPassword = "mysecretpassword"
                                       }
  runPGMigration (Proxy :: Proxy TestDB) conn

  pure ()
-}

{- TODO:
Duplicate Fk Check
-}

{-

NOT (\"age\" <= 0)

-}
