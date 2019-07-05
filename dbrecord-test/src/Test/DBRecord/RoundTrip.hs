{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts       #-}

module Test.DBRecord.RoundTrip where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Data.Int
import           Data.Word
import           Data.Time
import           Data.UUID.Types
import           Test.Tasty.Hedgehog
import           Data.Proxy
import           DBRecord.Internal.Expr  (Expr, ConstExpr)
import           DBRecord.Internal.Types
import           DBRecord.Internal.DBTypes
import           DBRecord.Query  hiding (timeOfDay)
import           Data.Functor.Identity

roundTripProps ::
  ( RoundTrip db dr
  , DBTypeCtx (GetDBTypeRep db Int)
  , DBTypeCtx (GetDBTypeRep db Int8)
  , DBTypeCtx (GetDBTypeRep db Int16)
  , DBTypeCtx (GetDBTypeRep db Int32)
  , DBTypeCtx (GetDBTypeRep db Int64)
  , DBTypeCtx (GetDBTypeRep db Word)  
  , DBTypeCtx (GetDBTypeRep db Word8)
  , DBTypeCtx (GetDBTypeRep db Word16)
  , DBTypeCtx (GetDBTypeRep db Word32)
  , DBTypeCtx (GetDBTypeRep db Word64)
  , DBTypeCtx (GetDBTypeRep db Bool)
  , DBTypeCtx (GetDBTypeRep db Float)
  , DBTypeCtx (GetDBTypeRep db Double)
  , DBTypeCtx (GetDBTypeRep db Day)
  , DBTypeCtx (GetDBTypeRep db TimeOfDay)
  , DBTypeCtx (GetDBTypeRep db LocalTime)
  
  , SingI (GetDBTypeRep db Int)
  , SingI (GetDBTypeRep db Int8)  
  , SingI (GetDBTypeRep db Int16)
  , SingI (GetDBTypeRep db Int32)
  , SingI (GetDBTypeRep db Int64)
  , SingI (GetDBTypeRep db Word) 
  , SingI (GetDBTypeRep db Word8) 
  , SingI (GetDBTypeRep db Word16)
  , SingI (GetDBTypeRep db Word32)
  , SingI (GetDBTypeRep db Word64)
  , SingI (GetDBTypeRep db Bool)
  , SingI (GetDBTypeRep db Float)
  , SingI (GetDBTypeRep db Double)
  , SingI (GetDBTypeRep db Day)
  , SingI (GetDBTypeRep db TimeOfDay)
  , SingI (GetDBTypeRep db LocalTime)

  , FromDBRow dr (Identity Int)
  , FromDBRow dr (Identity Int8)  
  , FromDBRow dr (Identity Int16)
  , FromDBRow dr (Identity Int32)
  , FromDBRow dr (Identity Int64)
  , FromDBRow dr (Identity Word)
  , FromDBRow dr (Identity Word8)
  , FromDBRow dr (Identity Word16)
  , FromDBRow dr (Identity Word32)
  , FromDBRow dr (Identity Word64)  
  
  , FromDBRow dr (Identity Bool)
  , FromDBRow dr (Identity Float)
  , FromDBRow dr (Identity Double)
  , FromDBRow dr (Identity Day)
  , FromDBRow dr (Identity TimeOfDay)
  , FromDBRow dr (Identity LocalTime)
  ) => Proxy db -> IO (SessionConfig dr cfg) -> TestTree
roundTripProps pdb dr =
  testGroup "round trip tests"
  [ testProperty "maxBound @Int" $ withTests 1 $ roundTrip pdb dr (Gen.int $ Range.singleton $ maxBound @Int)
  , testProperty "minBound @Int" $ withTests 1 $ roundTrip pdb dr (Gen.int $ Range.singleton $ minBound @Int)
  
  , testProperty "maxBound @Int8" $ withTests 1 $ roundTrip pdb dr (Gen.int8 $ Range.singleton $ maxBound @Int8)
  , testProperty "minBound @Int8" $ withTests 1 $ roundTrip pdb dr (Gen.int8 $ Range.singleton $ minBound @Int8)

  , testProperty "maxBound @Int16" $ withTests 1 $ roundTrip pdb dr (Gen.int16 $ Range.singleton $ maxBound @Int16)
  , testProperty "minBound @Int16" $ withTests 1 $ roundTrip pdb dr (Gen.int16 $ Range.singleton $ minBound @Int16)

  , testProperty "maxBound @Int32" $ withTests 1 $ roundTrip pdb dr (Gen.int32 $ Range.singleton $ maxBound @Int32)
  , testProperty "minBound @Int32" $ withTests 1 $ roundTrip pdb dr (Gen.int32 $ Range.singleton $ minBound @Int32)

  , testProperty "maxBound @Int64" $ withTests 1 $ roundTrip pdb dr (Gen.int64 $ Range.singleton $ maxBound @Int64)
  , testProperty "minBound @Int64" $ withTests 1 $ roundTrip pdb dr (Gen.int64 $ Range.singleton $ minBound @Int64)

  , testProperty "maxBound @Word" $ withTests 1 $ roundTrip pdb dr (Gen.word $ Range.singleton $ maxBound @Word)
  , testProperty "minBound @Word" $ withTests 1 $ roundTrip pdb dr (Gen.word $ Range.singleton $ minBound @Word)

  , testProperty "maxBound @Word8" $ withTests 1 $ roundTrip pdb dr (Gen.word8 $ Range.singleton $ maxBound @Word8)
  , testProperty "minBound @Word8" $ withTests 1 $ roundTrip pdb dr (Gen.word8 $ Range.singleton $ minBound @Word8)

  , testProperty "maxBound @Word16" $ withTests 1 $ roundTrip pdb dr (Gen.word16 $ Range.singleton $ maxBound @Word16)
  , testProperty "minBound @Word16" $ withTests 1 $ roundTrip pdb dr (Gen.word16 $ Range.singleton $ minBound @Word16)

  , testProperty "maxBound @Word32" $ withTests 1 $ roundTrip pdb dr (Gen.word32 $ Range.singleton $ maxBound @Word32)
  , testProperty "minBound @Word32" $ withTests 1 $ roundTrip pdb dr (Gen.word32 $ Range.singleton $ minBound @Word32)

  , testProperty "maxBound @Word64" $ withTests 1 $ roundTrip pdb dr (Gen.word64 $ Range.singleton $ maxBound @Word64)
  , testProperty "minBound @Word64" $ withTests 1 $ roundTrip pdb dr (Gen.word64 $ Range.singleton $ minBound @Word64)

  , testProperty "@Bool" $ roundTrip pdb dr Gen.bool
  -- , testProperty "@Day" $ withTests 100 $ roundTrip pdb dr day
  -- , testProperty "@TimeOfDay" $ withTests 100 $ roundTrip pdb dr timeOfDay
  -- , testProperty "@LocalTime" $ withTests 100 $ roundTrip pdb dr localTime
  , testProperty "@Float" $ withTests 100 $ roundTrip pdb dr (Gen.float $ Range.exponentialFloat (-100) 100)
  , testProperty "@Double" $ withTests 100 $ roundTrip pdb dr (Gen.double $ Range.exponentialFloat (-100) 100)

  ]

day :: MonadGen m => m Day
day = Gen.just $ do
  dy <- Gen.enum 1 31
  m <- Gen.enum 1 12
  yr <- Gen.enum 2012 2016
  pure $ fromGregorianValid yr m dy

{- TODO:
* Valid range of second including leap seconds is 0 to 61
  - TIME has problem parsing 60 and 61 as sec
* TIME has problem with parsing fractional sec
-}
timeOfDay :: MonadGen m => m TimeOfDay
timeOfDay = Gen.just $ do
  h <- Gen.enum 0 24
  m <- Gen.enum 0 59
  (s :: Int) <- Gen.enum 0 59 -- TODO: Handle pico
  pure $ makeTimeOfDayValid h m (fromRational $ toRational s)

localTime :: MonadGen m => m LocalTime
localTime = do
  dy <- day
  tod <- timeOfDay
  pure $ LocalTime dy tod

class RoundTrip (db :: DbK) dr | dr -> db where
  roundTrip :: ( ConstExpr a
              , DBTypeCtx (GetDBTypeRep db a)
              , SingI (GetDBTypeRep db a)
              , FromDBRow dr (Identity a)
              , Show a
              , Eq a
              ) => Proxy db -> IO (SessionConfig dr cfg) -> Gen a -> Property
