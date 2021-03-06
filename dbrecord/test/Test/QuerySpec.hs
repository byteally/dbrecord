{-# LANGUAGE DataKinds, TypeOperators, UndecidableInstances, OverloadedLabels, FlexibleInstances, MultiParamTypeClasses, DuplicateRecordFields, GADTs, TypeApplications, KindSignatures, DeriveGeneric, FlexibleContexts, FunctionalDependencies, ExplicitForAll, TypeFamilies, ScopedTypeVariables, OverloadedStrings, GeneralizedNewtypeDeriving, RankNTypes #-}
module Test.QuerySpec ( spec ) where

import DBRecord.Schema
import DBRecord.Migration
import DBRecord.Query
import DBRecord.Internal.Types
import DBRecord.Internal.PrimQuery
import Data.Text (Text)
import qualified Data.Text as T
import DBRecord.Internal.Schema (SingI, SingCtx)
import Data.Proxy
import GHC.Generics
import Data.Function

import Database.PostgreSQL.Simple as PGS
import Database.PostgreSQL.Simple.FromField as PGS
import Database.PostgreSQL.Simple.FromRow as PGS
import DBRecord.Internal.Common
import Data.String
import Control.Monad.Reader
import DBRecord.Transaction hiding (runTrasaction)
import Test.Hspec
import Data.Functor.Identity (Identity (..))
import DBRecord.Internal.DBTypes
import DBRecord.Internal.Types (HList (..))

--- TODO: Cheating.
instance (FromField a) => FromRow (Identity a) where
  fromRow = Identity <$> field

data TestDB deriving (Generic)

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
                       -- NOTE: Only Enum supported as of now
                       -- , Sum1 
                       -- , Prod1
                       -- , Address -- TODO: Didnt throw an error when not added
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
  type ForeignKey TestDB User    = '[ 'RefBy '["id"] User '["id"] "id_fk"
--                                    , 'Ref "id" User
                                    ]
  type ColumnNames TestDB User   = '[ '("id", "ID") ]
  type PrimaryKey TestDB User    = '["id"]
  type Unique TestDB User        = '[ 'UniqueOn '["name"] "uq_user_name"
                                    ]
  type TableName TestDB User     = "usr"
  type TableSequence TestDB User = '[Serial "id" "sq_serial", Owned "id" "sq"]
  defaults = dbDefaults
    (  #role (DBRecord.Query.toEnum Admin)
    :& #name "foo"
    -- :& #id   serial
    :& end
    )

  checks = dbChecks
    (  #notnull       (\n -> n .== "foo")
    :& #emailValidity (\em -> em .== em)
    :& end
    )

instance UDType TestDB UserRole where
  type TypeMappings TestDB UserRole = 'EnumType "UserRole" '[ "Admin"
                                                            , "Guest"
                                                            , "Normal"
                                                            ]



data TestDBMig

instance BaseDatabase TestDBMig 0 where
  type BaseTables TestDBMig 0 = '[ 'TypeName "test" "QuerySpec" "usr" ]

instance BaseTable TestDBMig ('TypeName "test" "QuerySpec" "usr") 0 where
  type BaseColumns TestDBMig ('TypeName "test" "QuerySpec" "usr") 0
    = '[ '( "id", DBInt4)
       , '( "name", DBText)
       , '( "email", DBText)
       , '("role", DBTypeName "Role")
       ]
  type BasePrimaryKey TestDBMig ('TypeName "test" "QuerySpec" "usr") 0
    = '["id"]
  type BaseForeignKey TestDBMig ('TypeName "test" "QuerySpec" "usr") 0
    = '[ -- 'RefBy '["age"] ('TypeName "test" "QuerySpec" "usr") '["name"]
       -- , 'Ref "id" ('TypeName "test" "QuerySpec" "usr") "usr"
       ]
  type BaseUnique TestDBMig ('TypeName "test" "QuerySpec" "usr") 0
    = '[ 'UniqueOn '["name"] "uq_user_name"]
  type BaseDefaultedCols TestDBMig ('TypeName "test" "QuerySpec" "usr") 0
    = '["id"]
  type BaseCheck TestDBMig ('TypeName "test" "QuerySpec" "usr") 0
    = '[ 'CheckOn '["email"] "emailValidity"]

instance DBMigration TestDBMig 0 where
  type CreatedTables TestDBMig 0 = '[ 'TypeName "test" "QuerySpec" "usr" ]

newtype MyDBM (db :: *) a = MyDBM {runDB :: ReaderT (PGS PGS.Connection) IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (PGS PGS.Connection))

type instance DBM TestDB = MyDBM TestDB
type instance Driver (MyDBM TestDB) = PGS

{-
withEnv :: Config_ -> ReaderT (PGS PGS.Connection) IO a -> IO a
withEnv cfg dbact
  = withResource (connectionPool cfg) $ \conn -> runReaderT dbact $ PGS conn
-}



-- runPGMigration :: ( Database db
--                    , schema ~ Schema db
--                    , tables ~ Tables db
--                    , types  ~ Types db
--                    , All (Table db) tables
--                    , TyCxts db types
--                    , SingI tables
--                    , SingI types
--                    , All (SingCtx db) tables
--                    ) => Proxy db -> PGS.Connection -> IO ()
-- runPGMigration pdb conn = do
--   let mig = mkMigration pdb
--       migStmtQ = unlines (fmap renderDDL mig)
--   withTransaction conn $ do
--     execute_ conn (fromString migStmtQ)
--   return ()

instance FromRow User where

instance FromField UserRole
  
someFunc :: IO ()
someFunc = do
  dbConfig <- defaultConfig $ defaultConnectInfo { connectHost = "172.17.0.2"
                                                 , connectPassword = "mysecretpassword"
                                                 }
  runSession (PGSConfig dbConfig) $ runDB @TestDB $ do
    _ <- getAll @User true AnyOrder (Just (OffsetLimit 10 20))
    _ <- insertMany @User Proxy [ (#email "m@m.com", #role Admin)
                                 , (#email "m1@m.com", #role Admin)
                                 ]
    -- _ <- insert @User Proxy (#email "m@m.com", #role Admin)
    -- delete @User (#id .== #id)
    -- (_ :: Maybe User) <- getBy #uq_user_name (Identity (#name "foo"))
    -- (_ :: Maybe User) <- get (Identity (#id 10))
    -- _ <- get @User (Identity (#id 10))
    -- _ <- getBy @User #uq_user_name (Identity (#name "foo"))
    -- _cnt <- count @User (#id .== #id)
    update @User true          (\row ->
                                  row & (Col @"id") .~ 10
                                      & (Col @"id") .~ 10
                                      & (Col @"id") %~ (\n -> n + 10)
                               ) Nil
    pure ()

instance ConstExpr UserRole where
  constExpr r = let roleStr = T.pack $ "'"++show r++"'"
                in literalExpr $ Other roleStr


{-
main :: IO ()
main = do 
  conn <- PGS.connect (ConnectInfo "10.42.0.1" 5432 "sreenidhi" "" "test_dbrecord") 
  let mig = mkMigration (Proxy :: Proxy TestDB)
      migStmtQ = unlines (fmap renderDDL mig)
  putStrLn migStmtQ
-}

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
