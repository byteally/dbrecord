{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# OPTIONS_GHC -fno-warn-orphans    #-}
module Test.PGS.SampleDB.DVDRentalTest where

import DBRecord
import DBRecord.Postgres hiding (query)
import DBRecord.Driver
import Test.SampleDB.DVDRental
import Data.Kind
import Data.Int
import Data.Proxy
import qualified Data.Vector as V
import Test.Query
import Test.Schema
import Test.Util

import GHC.Generics
-- import Data.Text (Text)

import Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import Control.Monad.Reader
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control
import Control.Monad.Base
import           Test.Tasty
import           Test.Tasty.Hedgehog
import Data.Typeable
import DBRecord.Internal.DBTypes


--deriving newtype instance FromField Year

type PGDVDRentalDB = DVDRentalDB 'Postgres

-- testQ1 :: IO [Category]
-- testQ1 = runReaderT 'a' $ runPostgresDB $ runQueryAsList $ rel @PGDVDRentalDB @Category $ selectAll

testDBConnectInfo :: ConnectInfo
testDBConnectInfo = defaultConnectInfo { connectHost = "localhost"
                                       , connectPassword = ".haskell."
                                       , connectDatabase = "dvdrental"
                                       , connectPort = 5433
                                       }

newtype DVDRentalPGM a = DVDRentalPGM {runDVDRentalPGM :: ReaderT (SessionConfig PGS) IO a}
  deriving newtype (Functor, Applicative, Monad, MonadUnliftIO, MonadIO, MonadBaseControl IO, MonadBase IO, MonadReader (SessionConfig PGS))

-- main :: IO ()
-- main = do
--   _ <- error "boom"
--   dbConfig <- pgDefaultPool $ testDBConnectInfo
--   let env = PGSConfig dbConfig
--   liftIO $ flip runReaderT env $ runDVDRentalPGM $ runSession $ do
--     cats <- runQuery $ rel @PGDVDRentalDB @Category $ selectAll
--     liftIO $ print cats
--     pure ()

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
hprop_test1 = withTests 1 $ property $ test $ do
  liftIO $ do
    dbConfig <- pgDefaultPool $ testDBConnectInfo
    let env = PGSConfig dbConfig
    liftIO $ flip runReaderT env $ runDVDRentalPGM $ runSession $ do
      cats <- runQuery @PGDVDRentalDB $ rel @_ @Category selectAll
      liftIO $ print cats
      q1 <- runQuery @PGDVDRentalDB $ leftJoinEg1 --selectUsingEg1
      liftIO $ print q1
      pure ()
    pure ()
  'a' === 'a'

hprop_test2 :: Property
hprop_test2 = withTests 1 $ property $ test $ do
  liftIO $ do
    dbConfig <- pgDefaultPool $ testDBConnectInfo
    let env = PGSConfig dbConfig
    liftIO $ flip runReaderT env $ runDVDRentalPGM $ runTransaction $ do
      q1 <- runMQuery $ qDeleteEg1
      liftIO $ print q1
      pure ()
    pure ()
  'a' === 'a'

hprop_test3 :: Property
hprop_test3 = withTests 1 $ property $ test $ do
  res <- liftIO $ do
    dbConfig <- pgDefaultPool $ testDBConnectInfo
    let env = PGSConfig dbConfig
    liftIO $ flip runReaderT env $ runDVDRentalPGM $ runTransaction $ do
      runQuery @PGDVDRentalDB $ (exprAsQ @Int64 (constExpr 1))
  fmap (.col) res === (pure 1)

hprop_test4 :: Property
hprop_test4 = withTests 1 $ property $ test $ do
  ress <- liftIO $ do
    dbConfig <- pgDefaultPool $ testDBConnectInfo
    let env = PGSConfig dbConfig
    liftIO $ flip runReaderT env $ runDVDRentalPGM $ runTransaction $ do
      runQuery @PGDVDRentalDB $ (testSelList)
  let res = V.head ress
  (res.col1, res.col2, res.col3) === ("test", True, 123)

hprop_test5 :: Property
hprop_test5 = withTests 1 $ property $ test $ do
  -- liftIO $ putStrLn $ showQuery $ allPrimOnly
  ress <- liftIO $ do
    dbConfig <- pgDefaultPool $ testDBConnectInfo
    let env = PGSConfig dbConfig
    liftIO $ flip runReaderT env $ runDVDRentalPGM $ runTransaction $ do
      runQuery @TestDB $ allPrimOnly
  liftIO $ print ress
  V.length ress === 1


-- pgExprTripping =
  
-- hprop_test6 :: Property
-- hprop_test6 = withTests 1 $ property $ test $ exprTripping (Proxy @TestDB) (1 :: Int32)

pgsExprTripper :: forall a (sc :: Type).
  ( Typeable a
  , Eq a
  , Show a
  , AutoConstExpr sc a (ToDBType (DB (SchemaDB sc)) a) (AutoCodec (DB (SchemaDB sc)) a)
  , FromRow (AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) () a)
  , DBRepr (DB (SchemaDB sc)) a
  ) => IO (SessionConfig PGS) -> Proxy sc -> a -> PropertyT IO ()
pgsExprTripper env' = exprTripping
  (\act -> do
      env <- liftIO $ env'
      liftIO @(PropertyT IO) $ flip runReaderT env $ runDVDRentalPGM $ runSession act
  )

test_const :: TestTree
test_const = Test.Tasty.withResource
  (fmap PGSConfig $ pgDefaultPool $ testDBConnectInfo)
  (const $ pure ())
  (\e -> testGroup "Const -> SelectExpr -> runQuery -> FromRow"
    [ testProperty "I32 minBound" $ withTests 1 $ property $ pgsExprTripper e (Proxy @TestDB) (minBound :: Int32)
    , testProperty "I32 maxBound" $ withTests 1 $ property $ pgsExprTripper e (Proxy @TestDB) (maxBound :: Int32)
    , testProperty "I64 minBound" $ withTests 1 $ property $ pgsExprTripper e (Proxy @TestDB) (minBound :: Int64)
    , testProperty "I64 maxBound" $ withTests 1 $ property $ pgsExprTripper e (Proxy @TestDB) (maxBound :: Int64)
    , testProperty "Bool" $ withTests 2 $ property $ (forAll Gen.bool) >>= pgsExprTripper e (Proxy @TestDB)
    , testProperty "latin1" $ withTests 100 $ property $ (forAll $ Gen.text (Range.linear 0 10) Gen.latin1) >>= pgsExprTripper e (Proxy @TestDB)
    , testProperty "unicode" $ withTests 100 $ property $ (forAll $ Gen.text (Range.linear 0 10) Gen.unicode) >>= pgsExprTripper e (Proxy @TestDB)
    , testProperty "unicodeAll" $ withTests 100 $ property $ (forAll $ Gen.text (Range.linear 0 10) Gen.unicodeAll) >>= pgsExprTripper e (Proxy @TestDB)
    , testProperty "float" $ withTests 100 $ property $ (forAll $ Gen.float (Range.exponentialFloat 0 10)) >>= pgsExprTripper e (Proxy @TestDB)
    , testProperty "double" $ withTests 100 $ property $ (forAll $ Gen.double (Range.exponentialFloat 0 10)) >>= pgsExprTripper e (Proxy @TestDB)
    ]
  )
  
    
