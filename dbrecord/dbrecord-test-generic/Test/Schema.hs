{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE DerivingVia             #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE UndecidableInstances       #-}
module Test.Schema
  ( module Test.Schema
  ) where

import DBRecord.Prelude
import GHC.Generics
import Data.Text (Text)
import Data.CaseInsensitive  (CI)
import Data.Time
import Data.Kind
import Data.ByteString (ByteString)
import Data.UUID (UUID)
import Data.Scientific
import Data.Aeson (Value)
import Data.Vector (Vector)
import Data.Map.Strict (Map)
import Data.List.NonEmpty (NonEmpty)

data TestDB = TestDB

instance Database TestDB where
  type DB TestDB = 'Postgres
  databaseName = "dbr_testdb"
  
instance Schema TestDB where
  type SchemaDB TestDB = TestDB
  schemaName = "test_schema"


data PrimOnly = PrimOnly
  { i32 :: Int32
  , i64 :: Int64
  , i16 :: Int16
  -- TODO: Needs Enumalation
  -- , i8 :: Int8 
  -- , w32 :: Word32
  -- , w64 :: Word64
  -- , w16 :: Word16
  -- , w8 :: Word8
  , txt :: Text
  , ciTxt :: CI Text
  , bool :: Bool
  , float :: Float
  , double :: Double
  , tod :: TimeOfDay
  , day :: Day
  , local :: LocalTime
  , utc :: UTCTime
  , bytea :: ByteString
  , uuid :: UUID
  , rat :: Rational
  , scientific :: Scientific
  , json :: Value
  } deriving (Generic, Show, Eq)
    deriving anyclass (DBRepr db)

instance Table TestDB PrimOnly where
  type TableId TestDB PrimOnly = 10
  type NewRow TestDB PrimOnly = PrimOnly

allPrimOnly :: Query TestDB PrimOnly
allPrimOnly = rel @_ @PrimOnly selectAll

data EnumTy
  = Con1
  | Con2
  | Con3
  deriving (Show, Eq, Generic)
  deriving (DBRepr db) via AsEnum EnumTy

data EnumTxt
  = Con4
  | Con5
  | Con6
  deriving (Show, Eq, Generic)
  deriving (DBRepr db) via AsEnumText EnumTxt

data EnumI64
  = Con7
  | Con8
  | Con9
  deriving (Show, Eq, Generic)
  deriving (DBRepr db) via AsEnumNum EnumI64

data CompRec1 = CompRec1
  { cr1 :: Maybe Int32
  , cr2 :: Maybe Text
  , cr3 :: Maybe Bool
  } deriving (Show, Eq, Generic)
    deriving (DBRepr 'Postgres) via AsEnum CompRec1 -- AsCompositeRec CompRec1

data FlatRec1 = FlatRec1
  { fr1 :: Int32
  , fr2 :: Text
  , fr3 :: Maybe Bool
  } deriving (Show, Eq, Generic)
    deriving (DBRepr db) via AsFlatRec FlatRec1

data JsonRec1 = JsonRec1
  { fr1 :: Int32
  , fr2 :: Maybe Text
  , fr3 :: Maybe Bool
  } deriving (Show, Eq, Generic)
    deriving (DBRepr 'Postgres) via AsJsonRec JsonRec1

data JBlob1 = JBlob1
  { jbPrim :: Int32
  , jbComplex1 :: Vector (Either Text (Maybe Int64))
  , jbComplex2 :: Map Text (NonEmpty Int32)
  } deriving (Show, Eq, Generic)
    deriving (DBRepr 'Postgres) via AsJsonBlob JBlob1

data TBlob1 = TBlob1
  { tbPrim :: Int32
  , tbComplex1 :: Vector (Either Text (Maybe Int64))
  , tbComplex2 :: Map Text (NonEmpty Int32)
  } deriving (Show, Eq, Generic)
    deriving (DBRepr 'Postgres) via AsTextBlob TBlob1

data XmlBlob1 = XmlBlob1
  { tbPrim :: Int32
  , tbComplex1 :: Vector (Either Text (Maybe Int64))
  , tbComplex2 :: Map Text (NonEmpty Int32)
  } deriving (Show, Eq, Generic)
    deriving (DBRepr 'Postgres) via AsXmlBlob XmlBlob1

data NestCompRec1 = NestCompRec1
  { ncrEty :: Maybe EnumTy
  , ncrEtxt :: Maybe EnumTxt
  , ncrEno :: Maybe EnumI64
  , ncrCrec :: Maybe CompRec1
  , ncrJrec :: Maybe JsonRec1
  , ncrJblob :: Maybe JBlob1
  , ncrTblob :: Maybe TBlob1
  , ncrXblob :: Maybe XmlBlob1
  } deriving (Show, Eq, Generic)
    deriving (DBRepr 'Postgres) via AsCompositeRec NestCompRec1
  
data TaggedSum1
  = Tag0
  | Tag1 CompRec1
  | Tag2 FlatRec1
  | Tag3 JsonRec1
  | Tag4 JBlob1
  | Tag5 TBlob1
  | Tag6 XmlBlob1
  | Tag7 TBlob1
  deriving (Show, Eq, Generic)
  deriving (DBRepr 'Postgres) via AsTaggedSumFlat UTaggedSum1 TaggedSum1

data UTaggedSum1 (sc :: Type)
  = UTag0
  | UTag1 (Expr sc CompRec1)
  | UTag2 (Expr sc FlatRec1)
  | UTag3 (Expr sc JsonRec1)
  | UTag4 (Expr sc JBlob1)
  | UTag5 (Expr sc TBlob1)
  | UTag6 (Expr sc XmlBlob1)
  | UTag7 (Expr sc TBlob1)
  deriving (Generic)

data TaggedSum2
  = Tag20
  | Tag21 CompRec1
  | Tag22 JsonRec1
  | Tag23 JBlob1
  | Tag24 TBlob1
  | Tag25 XmlBlob1
  | Tag26 TBlob1
  deriving (Show, Eq, Generic)
  deriving (DBRepr 'Postgres) via AsTaggedSumComposite UTaggedSum2 TaggedSum2

data UTaggedSum2 (sc :: Type)
  = UTag20
  | UTag21 (Expr sc CompRec1)
  | UTag22 (Expr sc JsonRec1)
  | UTag23 (Expr sc JBlob1)
  | UTag24 (Expr sc TBlob1)
  | UTag25 (Expr sc XmlBlob1)
  | UTag26 (Expr sc TBlob1)
  deriving (Generic)

data TaggedSum3
  = Tag30
  | Tag31 Int64
  | Tag32 Int64
  deriving (Show, Eq, Generic)
  deriving (DBRepr 'Postgres) via AsTaggedSumMonoFlat Int64 UTaggedSum3 TaggedSum3

data UTaggedSum3 (sc :: Type)
  = UTag30
  | UTag31 (Expr sc Int64)
  | UTag32 (Expr sc Int64)
  deriving (Generic)

data TaggedSum4
  = Tag40
  | Tag41 Int64
  | Tag42 Int64
  deriving (Show, Eq, Generic)
  deriving (DBRepr 'Postgres) via AsTaggedSumMonoComposite Int64 UTaggedSum4 TaggedSum4

data UTaggedSum4 (sc :: Type)
  = UTag40
  | UTag41 (Expr sc Int64)
  | UTag42 (Expr sc Int64)
  deriving (Generic)

data SumOfCol1
  = SOC1 Int64
  | SOC2 EnumTy
  | SOC3 EnumTxt
  | SOC4 EnumI64
  | SOC5 CompRec1
  | SOC6 JsonRec1
  | SOC7 JBlob1
  | SOC8 TaggedSum2
  | SOC9 TaggedSum4
  | SOC10 SumOfCol2
  deriving (Show, Eq, Generic)
  deriving (DBRepr 'Postgres) via AsSumOfColFlat USumOfCol1 SumOfCol1

data USumOfCol1 (sc :: Type)
  = USOC1 (Expr sc Int64)
  | USOC2 (Expr sc EnumTy)
  | USOC3 (Expr sc EnumTxt)
  | USOC4 (Expr sc EnumI64)
  | USOC5 (Expr sc CompRec1)
  | USOC6 (Expr sc JsonRec1)
  | USOC7 (Expr sc JBlob1)
  | USOC8 (Expr sc TaggedSum2)
  | USOC9 (Expr sc TaggedSum4)
  | USOC10 (Expr sc SumOfCol2)
  deriving (Generic)

data SumOfCol2
  = SOC21 Int64
  | SOC22 EnumTy
  | SOC23 EnumTxt
  | SOC24 EnumI64
  | SOC25 CompRec1
  | SOC26 JsonRec1
  | SOC27 JBlob1
  | SOC28 TaggedSum2
  | SOC29 TaggedSum4
  deriving (Show, Eq, Generic)
  deriving (DBRepr 'Postgres) via AsSumOfColComposite USumOfCol2 SumOfCol2

data USumOfCol2 (sc :: Type)
  = USOC21 (Expr sc Int64)
  | USOC22 (Expr sc EnumTy)
  | USOC23 (Expr sc EnumTxt)
  | USOC24 (Expr sc EnumI64)
  | USOC25 (Expr sc CompRec1)
  | USOC26 (Expr sc JsonRec1)
  | USOC27 (Expr sc JBlob1)
  | USOC28 (Expr sc TaggedSum2)
  | USOC29 (Expr sc TaggedSum4)
  deriving (Generic)
