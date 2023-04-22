{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans    #-}
module Test.PGS.SampleDB.DVDRentalTest where

import DBRecord.Query2
import DBRecord.Old.Schema
import DBRecord.Postgres hiding (query)
import DBRecord.Driver
import Test.SampleDB.DVDRental
import Data.Kind

import GHC.Generics
-- import Data.Text (Text)

import Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import Control.Monad.Reader
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control
import Control.Monad.Base
-- import           Test.Tasty
-- import           Test.Tasty.Hedgehog


--deriving newtype instance FromField Year

type PGDVDRentalDB = DVDRentalDB 'Postgres

-- testQ1 :: IO [Category]
-- testQ1 = runReaderT 'a' $ runPostgresDB $ runQueryAsList $ rel @PGDVDRentalDB @Category $ selectAll

testDBConnectInfo :: ConnectInfo
testDBConnectInfo = defaultConnectInfo { connectHost = "localhost"
                                       , connectPassword = ".haskell."
                                       , connectDatabase = "dvdrental"
                                       }

newtype DVDRentalPGM a = DVDRentalPGM {runDVDRentalPGM :: ReaderT (SessionConfig PGS) IO a}
  deriving newtype (Functor, Applicative, Monad, MonadUnliftIO, MonadIO, MonadBaseControl IO, MonadBase IO, MonadReader (SessionConfig PGS))

main :: IO ()
main = do
  dbConfig <- pgDefaultPool $ testDBConnectInfo
  flip runReaderT (PGSConfig dbConfig) $ runDVDRentalPGM $ runSession $ runPostgresDB @PGDVDRentalDB $ do
    cats <- runQueryAsList $ rel @PGDVDRentalDB @Category $ selectAll
    liftIO $ print cats
    pure ()

hprop_additionCommutative :: Property
hprop_additionCommutative = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  reverse (reverse xs) === (xs)

data DVDRentalState (f :: Type -> Type) = DVDRentalState
  {
  }
  
data DVDRentalIn (f :: Type -> Type) = DVDRentalIn
  deriving (Generic, Show)
data DVDRentalOut = DVDRentalOut
  deriving (Show)

instance FunctorB DVDRentalIn where
instance TraversableB DVDRentalIn where

cbReq :: DVDRentalState Symbolic -> DVDRentalIn Symbolic -> Bool
cbReq _ _ = True

cbUpd :: DVDRentalState v -> DVDRentalIn v -> Var DVDRentalOut v -> DVDRentalState v
cbUpd pstate _inp _vout = pstate

cbEns :: DVDRentalState Concrete
      -> DVDRentalState Concrete
      -> DVDRentalIn Concrete
      -> DVDRentalOut
      -> Test ()
cbEns _ _ _ _ = 'a' === 'a'      

cmdCBs :: [Callback DVDRentalIn DVDRentalOut DVDRentalState]
cmdCBs =
  [ Require cbReq
  , Update cbUpd
  , Ensure cbEns
  ]

cmdGen :: DVDRentalState Symbolic
       -> Maybe (gen (DVDRentalIn Symbolic))
cmdGen _ = Nothing

cmdExec :: (MonadTest m, MonadIO m)
        => DVDRentalIn Concrete
        -> m DVDRentalOut
cmdExec _ = do
  liftIO $ print 'a'
  pure DVDRentalOut

cmd :: (MonadTest m, MonadIO m) => Command gen m DVDRentalState
cmd = Command
  { commandGen = cmdGen
  , commandExecute = cmdExec
  , commandCallbacks = cmdCBs
  }
  
hprop_smt :: Property
hprop_smt = property $ do
  actions <- forAll $ Gen.sequential (Range.linear 1 1) DVDRentalState
        [ cmd
        ]
  executeSequential DVDRentalState actions

hprop_test1 :: Property
hprop_test1 = property $ test $ do
  liftIO $ do
    dbConfig <- pgDefaultPool $ testDBConnectInfo
    flip runReaderT (PGSConfig dbConfig) $ runDVDRentalPGM $ runSession $ runPostgresDB @PGDVDRentalDB $ do
      -- cats <- runQueryAsList $ rel @PGDVDRentalDB @Category $ selectAll
      -- liftIO $ print cats
      q1 <- runQueryAsList $ qExcept -- qWithTopOrBottomN
      liftIO $ print q1      
    pure ()
  'a' === 'a'

hprop_test2 :: Property
hprop_test2 = property $ test $ do
  liftIO $ do
    dbConfig <- pgDefaultPool $ testDBConnectInfo
    flip runReaderT (PGSConfig dbConfig) $ runDVDRentalPGM $ runSession $ runPostgresDB @PGDVDRentalDB $ do
      q1 <- runMQueryAsList $ qDeleteEg1
      liftIO $ print q1      
    pure ()
  'a' === 'a'  


-- test_addition :: TestTree
-- test_addition = testProperty "Addition commutes" $ \(a :: Int) (b :: Int) -> a + b == b + a




-- data TestDB deriving (Generic)
-- data TestSC deriving (Generic)
-- data TestSCDup deriving (Generic)

-- type instance DBM TestDB = PostgresDB TestDB
-- type instance Driver (PostgresDB TestDB ()) = PGS

-- data UserRole = Admin | Guest | Nor'mal
--   deriving (Show, Generic, Enum)

-- data Sum1 = Con1 {a :: Int, b :: Bool} | Con2 {a :: Int}
--   deriving (Show, Generic)

-- data Prod1 = Prod1 {pa1 :: Text, pb1 :: Double}
--   deriving (Show, Generic)

-- data User = User
--   { userId :: Int
--   , name  :: Text
--   , email :: Text
--   , role  :: UserRole
--   } deriving (Show, Generic)

-- instance FromRow User
-- instance FromField UserRole where
--   fromField = undefined

{-
data Profile = Profile
  { id         :: Int
  , first_name :: Text
  , last_name  :: Text
  , age        :: Maybe Int
  , address    :: Address
  -- , sum        :: Sum1
  -- , prod       :: Prod1
  } deriving (Generic)

data Address = Address
  { id     :: Int
  , doorno :: Text
  , addr1  :: Text
  , addr2  :: Maybe Text
  , city   :: Text
  } deriving (Generic)
-}

-- instance Database TestDB where
--   type DB TestDB = 'Postgres
--   type DatabaseName TestDB = "dbrecord_test"

-- instance Schema TestSC where
--   type Tables TestSC = '[ User
--                         -- , Profile
--                         ]
--   type Types TestSC = '[ UserRole
--                        -- NOTE: Only Enum supported as of now
--                        -- , Sum1 
--                        -- , Prod1
--                        -- , Address -- TODO: Didnt throw an error when not added
--                        ]
--   type SchemaDB TestSC = TestDB

-- instance Schema TestSCDup where
--   type Tables TestSCDup = '[ User
--                       --     , Test
--                         -- , Profile
--                         ]
--   type Types TestSCDup = '[ UserRole
--                        -- NOTE: Only Enum supported as of now
--                        -- , Sum1 
--                        -- , Prod1
--                        -- , Address -- TODO: Didnt throw an error when not added
--                        ]
--   type SchemaDB TestSCDup = TestDB


{-
instance Table TestDB Profile where
  type HasDefault TestDB Profile   = '["role"]
  type TableName TestDB Profile    = "user_profile"
  type ColumnNames TestDB Profile  = '[ '("first_name", "First Name")
                                      ]
  type Check TestDB Profile        = '[ 'CheckOn '["first_name"] "notnull"]
  type Unique TestDB Profile       = '[ 'UniqueOn '["first_name"] "uq_first_name"]
-}

-- instance Table TestSC User where
--   type HasDefault TestSC User    = '["userId", "role"]
--   type Check TestSC User         = '[ 'CheckOn '["name"] "non_empty_name"
--                                     -- , 'CheckOn '["email"] "emailValidity"
--                                     ]
--   type CheckNames TestSC User    = '[ '("non_empty_name" , "ck_non_empty_name") ]
--   type ColumnNames TestSC User   = '[ '("userId", "id")
--                                     , '("name", "name")
--                                     , '("email", "email")
--                                     , '("role", "role")
--                                     ]
--   type PrimaryKey TestSC User    = '["userId"]
--   type PrimaryKeyName TestSC User = 'Just "pk_user_id"
--   type Unique TestSC User        = '[ 'UniqueOn '["name"] "user_name"
--                                     ]
--   type UniqueNames TestSC User   = '[ '("user_name", "uq_user_name")
--                                     ]
--   type TableName TestSC User     = "users"

  {-
  defaults = dbDefaults
    (  #role (DBRecord.Query.toEnum Admin)
    :& end
    )

  checks = dbChecks
    (  #non_empty_name (\n -> n /= "")
    :& end
    )
  -}
  
-- instance UDType TestSC UserRole where
--   type TypeMappings TestSC UserRole =
--     'EnumType 'Nothing '[ '("Nor'mal", "Normal")
--                         ]

-- instance ConstExpr TestSC UserRole
-- -- instance EqExpr UserRole

-- instance Table TestSCDup User where
--   type HasDefault TestSCDup User    = '["userId", "role"]
--   type Check TestSCDup User         = '[ 'CheckOn '["name"] "non_empty_name"
--                                     -- , 'CheckOn '["email"] "emailValidity"
--                                     ]
--   type CheckNames TestSCDup User    = '[ '("non_empty_name" , "ck_non_empty_name") ]
--   type ColumnNames TestSCDup User   = '[ '("userId", "id")
--                                     , '("name", "user_name")
--                                     , '("email", "user_email")
--                                     , '("role", "user_role")
--                                     ]
--   type PrimaryKey TestSCDup User    = '["userId"]
--   type PrimaryKeyName TestSCDup User = 'Just "pk_user_id"
--   type Unique TestSCDup User        = '[ 'UniqueOn '["name"] "user_name"
--                                     ]
--   type UniqueNames TestSCDup User   = '[ '("user_name", "uq_user_name")
--                                     ]
--   type TableName TestSCDup User     = "users"

-- instance UDType TestSCDup UserRole where

{-
main = do
  dbConfig <- pgDefaultPool $ defaultConnectInfo { connectHost = "localhost"
                                                , connectPassword = "postgres"
                                                , connectDatabase = "dbrecord_test"
                                                }
  runSession (PGSConfig dbConfig) $ runPostgresDB @TestDB $ do
    -- let rows =
    --       [ (#name "person14", #email "fo1aaz@bar.com")
    --       , (#name "person15", #email "ba1aaz@baz.com")
    --       ]
    -- insertMany_ @TestSC @User (withRows rows)
    
    -- us1 <- get @TestSC @User (withPrimaryKey 1)    
    -- liftIO $ print (us1 :: Maybe User)
    -- let row = (#name "person94", #email "kajak@bar.com")
    -- ret <- insertRet @TestSC @User (withRow row) ((column @"name" @TestSC @User @Text Proxy) :&
    --                                               Nil)
    -- liftIO $ print ret
    -- update_ @TestSC @User (column @"name" @TestSC @User Proxy .== text "person14")
    --                       (\row ->
    --                               row & (Col @"name") .~ "person21"
    --                                   & (Col @"email") .~ "up@bong.com"
    --                       )
    -- us2 <- get @TestSCDup @User (withPrimaryKey 1)    
    -- liftIO $ print (us2 :: Maybe User)
   -- us2 <- getAll @TestSC @User (\tab -> column @"name" tab .== text "person15") AnyOrder Nothing
   let t1 = project (table @TestSC @User)
                    (\t -> #name t         `as` #name {-tag @"name" (column @"name" t)-} :&
                          (#userId t + 1) `as` #userId :&
                     Nil)
                    Nothing
       t2 = aggregate t1
                    (\t -> Nil)
                    (\t _ -> (count (#name t) `as` #name1) :& Nil)
       -- t2 = table @TestSC @User & tabular @"user_2"
       -- j = join t1 t2 (\l r -> column @"name" l .== column @"name" r) 
   us2 <- query t2
   liftIO $ print us2
   
myEnumPGFromField :: PG.FieldParser UserRole
myEnumPGFromField f mdata = do
    n <- PG.typename f
    if n /= "UserRole"
    then PG.returnError PG.Incompatible f ""
    else case mdata of
        Nothing -> PG.returnError PG.UnexpectedNull f ""
        Just bs -> case parseMyEnum bs of
            Nothing -> PG.returnError PG.ConversionFailed f (show bs)
            Just x  -> return x

parseMyEnum :: BS.ByteString -> Maybe UserRole
parseMyEnum "Admin" = Just Admin
parseMyEnum _ = Just Nor'mal

---

prettyExpr :: Expr sc a -> String
prettyExpr = show . PG.ppExpr . (Sql.sqlExpr Sql.defaultSqlGenerator) . getExpr 

instance (PG.FromRow a, PG.FromRow b) => PG.FromRow (Nest a b) where
  fromRow = Nest <$> PG.fromRow <*> PG.fromRow


data EnumT = A | B | C
          deriving (Show, Eq, Generic)

data ProdT = Prod { pa :: Int, pb :: Bool }
          deriving (Show, Eq, Generic)

data Test = Test { x :: Int, y :: EnumT, z :: ProdT } deriving (Show, Eq, Generic)

instance Table TestSC Test where

instance UDType TestSC EnumT

instance UDType TestSC ProdT where
  type TypeMappings TestSC ProdT =
    'Flat '[ '("pa", "pa_aa")
           ]

instance ConstExpr TestSC EnumT
instance ConstExpr TestSC ProdT
  

-}
