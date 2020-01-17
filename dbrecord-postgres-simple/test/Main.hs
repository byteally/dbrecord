{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import DBRecord.Schema
import DBRecord.Query
import DBRecord.Postgres hiding (query)

import GHC.Generics
import Data.Text (Text)

import Data.Functor.Identity
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.FromRow as PG

import Control.Monad.IO.Class
import qualified Data.ByteString as BS

import qualified DBRecord.Internal.Sql.SqlGen as Sql
import qualified DBRecord.Postgres.Internal.Sql.Pretty as PG

import DBRecord.Internal.Types ((:::) (..))
import DBRecord.Internal.Query (Tabular (..), Nest (..))

import Data.Function ((&))
import Data.Proxy

data TestDB deriving (Generic)
data TestSC deriving (Generic)
data TestSCDup deriving (Generic)

type instance DBM TestDB = PostgresDB TestDB
type instance Driver (PostgresDB TestDB) = PGS

data UserRole = Admin | Guest | Nor'mal
  deriving (Show, Generic, Enum)

data Sum1 = Con1 {a :: Int, b :: Bool} | Con2 {a :: Int}
  deriving (Show, Generic)

data Prod1 = Prod1 {pa1 :: Text, pb1 :: Double}
  deriving (Show, Generic)

data User = User
  { userId :: Int
  , name  :: Text
  , email :: Text
  , role  :: UserRole
  } deriving (Show, Generic)

instance FromRow User
instance FromField UserRole where
  fromField = myEnumPGFromField

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

instance Database TestDB where
  type DB TestDB = 'Postgres
  type DatabaseName TestDB = "dbrecord_test"

instance Schema TestSC where
  type Tables TestSC = '[ User
                        -- , Profile
                        ]
  type Types TestSC = '[ UserRole
                       -- NOTE: Only Enum supported as of now
                       -- , Sum1 
                       -- , Prod1
                       -- , Address -- TODO: Didnt throw an error when not added
                       ]
  type SchemaDB TestSC = TestDB

instance Schema TestSCDup where
  type Tables TestSCDup = '[ User
                           , Test
                        -- , Profile
                        ]
  type Types TestSCDup = '[ UserRole
                       -- NOTE: Only Enum supported as of now
                       -- , Sum1 
                       -- , Prod1
                       -- , Address -- TODO: Didnt throw an error when not added
                       ]
  type SchemaDB TestSCDup = TestDB


{-
instance Table TestDB Profile where
  type HasDefault TestDB Profile   = '["role"]
  type TableName TestDB Profile    = "user_profile"
  type ColumnNames TestDB Profile  = '[ '("first_name", "First Name")
                                      ]
  type Check TestDB Profile        = '[ 'CheckOn '["first_name"] "notnull"]
  type Unique TestDB Profile       = '[ 'UniqueOn '["first_name"] "uq_first_name"]
-}

instance Table TestSC User where
  type HasDefault TestSC User    = '["userId", "role"]
  type Check TestSC User         = '[ 'CheckOn '["name"] "non_empty_name"
                                    -- , 'CheckOn '["email"] "emailValidity"
                                    ]
  type CheckNames TestSC User    = '[ '("non_empty_name" , "ck_non_empty_name") ]
  type ColumnNames TestSC User   = '[ '("userId", "id")
                                    , '("name", "name")
                                    , '("email", "email")
                                    , '("role", "role")
                                    ]
  type PrimaryKey TestSC User    = '["userId"]
  type PrimaryKeyName TestSC User = 'Just "pk_user_id"
  type Unique TestSC User        = '[ 'UniqueOn '["name"] "user_name"
                                    ]
  type UniqueNames TestSC User   = '[ '("user_name", "uq_user_name")
                                    ]
  type TableName TestSC User     = "users"

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
  
instance UDType TestSC UserRole where
  type TypeMappings TestSC UserRole =
    'EnumType 'Nothing '[ '("Nor'mal", "Normal")
                        ]

instance ConstExpr TestSC UserRole
-- instance EqExpr UserRole

instance Table TestSCDup User where
  type HasDefault TestSCDup User    = '["userId", "role"]
  type Check TestSCDup User         = '[ 'CheckOn '["name"] "non_empty_name"
                                    -- , 'CheckOn '["email"] "emailValidity"
                                    ]
  type CheckNames TestSCDup User    = '[ '("non_empty_name" , "ck_non_empty_name") ]
  type ColumnNames TestSCDup User   = '[ '("userId", "id")
                                    , '("name", "user_name")
                                    , '("email", "user_email")
                                    , '("role", "user_role")
                                    ]
  type PrimaryKey TestSCDup User    = '["userId"]
  type PrimaryKeyName TestSCDup User = 'Just "pk_user_id"
  type Unique TestSCDup User        = '[ 'UniqueOn '["name"] "user_name"
                                    ]
  type UniqueNames TestSCDup User   = '[ '("user_name", "uq_user_name")
                                    ]
  type TableName TestSCDup User     = "users"

instance UDType TestSCDup UserRole where

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

prettyExpr :: Expr sc scopes a -> String
prettyExpr = show . PG.ppExpr . (Sql.sqlExpr Sql.defaultSqlGenerator) . getExpr 

instance (PG.FromField a) => PG.FromRow (Identity a) where
  fromRow = (Identity . PG.fromOnly) <$> PG.fromRow

instance ( PG.FromField (f x)
         , PG.FromRow (HList f a)
         ) => PG.FromRow (HList f (x ': a)) where
  fromRow = (:&) <$> PG.field <*> PG.fromRow

instance PG.FromRow (HList f '[]) where
  fromRow = pure Nil

instance ( PG.FromRow (HList Identity xs)
         ) => PG.FromRow (Tabular alias xs) where
  fromRow = Tabular <$> PG.fromRow

instance (PG.FromField a) => PG.FromField (fld ::: a) where
  fromField a b = Field <$> PG.fromField a b

instance (PG.FromField a) => PG.FromField (Identity a) where
  fromField a b = Identity <$> PG.fromField a b

instance (PG.FromRow a, PG.FromRow b) => PG.FromRow (Nest a b) where
  fromRow = Nest <$> PG.fromRow <*> PG.fromRow


