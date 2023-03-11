-- Source: https://www.postgresqltutorial.com/postgresql-getting-started/postgresql-sample-database/
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE OverloadedRecordDot     #-}
{-# LANGUAGE OverloadedLabels        #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE DeriveAnyClass          #-}
module Test.SampleDB.DVDRental
  ( module Test.SampleDB.DVDRental
  ) where

import GHC.Generics
import DBRecord
import Data.Text (Text)
import Data.Time

data DVDRentalDB (db :: DbK)

instance Database (DVDRentalDB db) where
  type DB (DVDRentalDB db) = db
  type DatabaseName (DVDRentalDB db) = "dbr_dvdrental"
  
instance Schema (DVDRentalDB db) where
  type SchemaDB (DVDRentalDB db) = DVDRentalDB db
  type Tables (DVDRentalDB db) =
    '[ Category
     , FilmCategory
     , Film
     , Language
     , FilmActor
     , Actor
     , Inventory
     , Rental
     , Payment
     , Staff
     , Customer
     , Address
     , City
     , Country
     , Store
     ]

-- ^ stores film’s categories data
data Category = Category
  { categoryId :: Int32
  , name :: Text
  , lastUpdate :: LocalTime
  } deriving (Show, Generic)
    deriving anyclass (DBRepr sc)

instance (db ~ 'Postgres) => Table (DVDRentalDB db) Category where
instance (db ~ 'Postgres) => Table (DVDRentalDB db) Customer where
instance (db ~ 'Postgres) => Table (DVDRentalDB db) Film where
instance (db ~ 'Postgres) => Table (DVDRentalDB db) Payment where

-- ^ stores the relationships between films and categories
data FilmCategory = FilmCategory
  { film_id :: Int16
  , categoryId :: Int16
  , lastUpdate :: LocalTime
  } deriving (Show, Generic)
    deriving anyclass (DBRepr sc)

newtype Year = Year {getYear :: Int16}
  deriving (Show, Generic)

-- ^ stores film data such as title, release year, length, rating, etc
data Film = Film
  { filmId :: Int32
  , title :: Text
  , description :: Text
  , releaseYear :: Year
  , languageId :: Int16
  , rentalDuration :: Int16
  , rentalRate :: Double -- numeric(4,2) TODO: Use Scentific
  , length :: Int16
  , replacementCost :: Double -- numeric(5,2) TODO: Use Scentific
  , rating :: Text -- public.mpaa_rating
  , lastUpdate :: LocalTime
  , specialFeatures :: [Text]
  , fulltext :: Text -- tsvector TODO: Handle this
  } deriving (Show, Generic)
    deriving anyclass (DBRepr sc)

-- ^ stores language of the film
data Language = Language
  {
  } deriving (Show, Generic)

-- ^ stores the relationships between films and actors
data FilmActor = FilmActor
  {
  } deriving (Show, Generic)

-- ^ stores actors data including first name and last name
data Actor = Actor
  {
  } deriving (Show, Generic)

-- ^ stores inventory data
data Inventory = Inventory
  { inventoryId :: Int32
  , filmId :: Int16
  , storeId :: Int16
  , lastUpdate :: LocalTime
  } deriving (Show, Generic)
    deriving anyclass (DBRepr sc)
instance (db ~ 'Postgres) => Table (DVDRentalDB db) Inventory where

-- ^ stores rental data
data Rental = Rental
  {
  } deriving (Show, Generic)

-- ^ stores customer’s payments
data Payment = Payment
  { paymentId :: Int32
  , customerId :: Int16
  , staffId :: Int16
  , rentalId :: Int32
  , amount :: Double -- numeric(5,2)
  , paymentDate :: LocalTime
  } deriving (Show, Generic)
    deriving anyclass (DBRepr sc)

-- ^ stores staff data
data Staff = Staff
  { staffId :: Int32
  , firstName :: Text
  , lastName :: Text
  , addressId :: Int16
  , email :: Maybe Text
  , storeId :: Int16
  , active :: Bool
  , username :: Text
  , password :: Maybe Text
  , lastUpdate :: LocalTime
  , picture :: Text -- ByteString
  } deriving (Show, Generic)
    deriving anyclass (DBRepr sc)
instance (db ~ 'Postgres) => Table (DVDRentalDB db) Staff where

-- ^ stores customer data
data Customer = Customer
  { customerId :: Int32
  , storeId :: Int16
  , firstName :: Text
  , lastName :: Text
  , email :: Maybe Text
  , addressId :: Int16
  , activebool :: Bool
  , createDate :: Day
  , lastUpdate :: Maybe LocalTime
  , active :: Maybe Int32
  } deriving (Show, Generic)
    deriving anyclass (DBRepr sc)

-- ^ stores address data for staff and customers
data Address = Address
  {
  } deriving (Show, Generic)

-- ^ stores city names
data City = City
  {
  } deriving (Show, Generic)

-- ^ stores country names
data Country = Country
  {
  } deriving (Show, Generic)

-- ^ contains the store data including manager staff and address
data Store = Store
  {
  } deriving (Show, Generic)


-- select
queryOneColumn :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               ])
queryOneColumn = rel @(DVDRentalDB db) @Customer $ select $ \customer ->
  customer.firstName
  .& end

queryMultipleColumn :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("lastName", Text)
                               , '("email", Maybe Text)
                               ])
queryMultipleColumn = rel @(DVDRentalDB db) @Customer $ select $ \customer ->
  customer.firstName
  .& customer.lastName
  .& customer.email
  .& end

queryAllColumn :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) Customer
queryAllColumn = rel @(DVDRentalDB db) @Customer $ selectAll

selectWithExpr :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("fullName", Text)
                               ])
selectWithExpr = rel @(DVDRentalDB db) @Customer $ select $ \customer ->
  #fullName .= val (customer.firstName) .++ " " .++ val (customer.lastName)
  .& end

selectWithColumnAlias :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("surname", Text)
                               ])
selectWithColumnAlias = rel @(DVDRentalDB db) @Customer $ select $ \customer ->
  customer.firstName
  .& #surname .= val customer.lastName
  .& end


-- order by

orderByOneColumn :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("lastName", Text)
                               ])
orderByOneColumn = rel @(DVDRentalDB db) @Customer $ do
  sort $ \customer -> asc customer.firstName
  select $ \customer ->
    customer.firstName
    .& customer.lastName
    .& end

orderByOneColumnByDesc :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("lastName", Text)
                               ])
orderByOneColumnByDesc = rel @(DVDRentalDB db) @Customer $ do
  sort $ \customer -> desc customer.lastName
  select $ \customer ->
    customer.firstName
    .& customer.lastName
    .& end

orderByMultipleColumn :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("lastName", Text)
                               ])
orderByMultipleColumn = rel @(DVDRentalDB db) @Customer $ do
  sort $ \customer -> asc customer.firstName <> desc customer.lastName
  select $ \customer ->
    customer.firstName
    .& customer.lastName
    .& end    

orderByExpr :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("len", Int)
                               ])
orderByExpr = rel @(DVDRentalDB db) @Customer $ do
  res <- select $ \customer ->
    customer.firstName
    .& #len .= len (val customer.firstName)
    .& end
  sort $ \_ -> desc res.len
  pure res

orderByAscNullsFirst :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("email", Maybe Text)
                               ])
orderByAscNullsFirst = rel @(DVDRentalDB db) @Customer $ do
  sort $ \customer -> ascNullsFirst customer.email
  select $ \customer ->
    customer.firstName
    .& customer.email
    .& end

orderByAscNullsLast :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("email", Maybe Text)
                               ])
orderByAscNullsLast = rel @(DVDRentalDB db) @Customer $ do
  sort $ \customer -> ascNullsLast customer.email
  select $ \customer ->
    customer.firstName
    .& customer.email
    .& end    

orderByDescNullsFirst :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("email", Maybe Text)
                               ])
orderByDescNullsFirst = rel @(DVDRentalDB db) @Customer $ do
  sort $ \customer -> descNullsFirst customer.email
  select $ \customer ->
    customer.firstName
    .& customer.email
    .& end
    
orderByDescNullsLast :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("email", Maybe Text)
                               ])
orderByDescNullsLast = rel @(DVDRentalDB db) @Customer $ do
  sort $ \customer -> descNullsLast customer.email
  select $ \customer ->
    customer.firstName
    .& customer.email
    .& end    

-- select distinct
-- where

whereWithEq :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("lastName", Text)
                               ])
whereWithEq = rel @(DVDRentalDB db) @Customer $ do
  restrict $ \customer -> customer.firstName .== "Jamie"
  select $ \customer ->
    customer.firstName
    .& customer.lastName
    .& end

whereWithAnd :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("lastName", Text)
                               ])
whereWithAnd = rel @(DVDRentalDB db) @Customer $ do
  restrict $ \customer -> customer.firstName .== "Jamie" .&& customer.lastName .== "Rice"
  select $ \customer ->
    customer.firstName
    .& customer.lastName
    .& end

whereWithOr :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("lastName", Text)
                               ])
whereWithOr = rel @(DVDRentalDB db) @Customer $ do
  restrict $ \customer -> customer.firstName .== "Rodriguez" .|| customer.lastName .== "Adam"
  select $ \customer ->
    customer.firstName
    .& customer.lastName
    .& end


-- TODO: in_ arg order & usage of native `in`
whereWithIn :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("lastName", Text)
                               ])
whereWithIn = rel @(DVDRentalDB db) @Customer $ do
  restrict $ \customer ->  ["Ann","Anne","Annie"] `in_` customer.firstName
  select $ \customer ->
    customer.firstName
    .& customer.lastName
    .& end    

whereWithLike :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("lastName", Text)
                               ])
whereWithLike = rel @(DVDRentalDB db) @Customer $ do
  restrict $ \customer -> customer.firstName `like` "Ann%"
  select $ \customer ->
    customer.firstName
    .& customer.lastName
    .& end

whereWithBetween :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("nameLength", Int)
                               ])
whereWithBetween = rel @(DVDRentalDB db) @Customer $ do
  restrict $ \customer -> customer.firstName `like` "A%" .&& len customer.firstName `between` (3, 5)
  select $ \customer ->
    customer.firstName
    .& #nameLength .= len (val customer.firstName)
    .& end

whereWithNE :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("firstName", Text)
                               , '("lastName", Text)
                               ])
whereWithNE = rel @(DVDRentalDB db) @Customer $ do
  restrict $ \customer -> customer.firstName `like` "Bra%" .&& customer.lastName ./= "Motley"
  select $ \customer ->
    customer.firstName
    .& customer.lastName
    .& end    

-- limit

qWithLimit :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("filmId", Int32)
                               , '("title", Text)
                               , '("releaseYear", Year)
                               ])
qWithLimit = rel @(DVDRentalDB db) @Film $ do
  limit $ Just 5
  select $ \film ->
    film.filmId
    .& film.title
    .& film.releaseYear
    .& end

qWithLimitOffset :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("filmId", Int32)
                               , '("title", Text)
                               , '("releaseYear", Year)
                               ])
qWithLimitOffset = rel @(DVDRentalDB db) @Film $ do
  limit $ Just 5
  offset $ Just 3
  select $ \film ->
    film.filmId
    .& film.title
    .& film.releaseYear
    .& end    

qWithTopOrBottomN :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("filmId", Int32)
                               , '("title", Text)
                               , '("rentalRate", Double)
                               ])
qWithTopOrBottomN = rel @(DVDRentalDB db) @Film $ do
  limit $ Just 10
  sort $ \film -> desc film.rentalRate
  select $ \film ->
    film.filmId
    .& film.title
    .& film.rentalRate
    .& end

-- fetch
-- in
-- between
-- like
-- isNull
-- tableAliases
-- joins

-- inner join
innerJoin2Tables :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("customer", Customer)
                               , '("payment", Payment)
                               ])
innerJoin2Tables =
  innerJoin
  (#customer .= rel @(DVDRentalDB db) @Customer selectAll)
  (#payment .= rel @(DVDRentalDB db) @Payment selectAll)
  (\r -> r.customer.customerId .== fromIntegralExpr r.payment.customerId) $ do
  sort $ \r -> asc r.payment.paymentDate
  selectAll

innerJoin3Tables :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec [ '("customerPayment", Rec '[ '("customer", Customer)
                                                          , '("payment", Payment)
                                                          ])
                              , '("staff", Staff)
                              ])
innerJoin3Tables =
  innerJoin
  ( #customerPayment .= innerJoin
    (#customer .= rel @(DVDRentalDB db) @Customer selectAll)
    (#payment .= rel @(DVDRentalDB db) @Payment selectAll)
    (\res -> res.customer.customerId .== fromIntegralExpr res.payment.customerId)
    selectAll)
  (#staff .= rel @(DVDRentalDB db) @Staff selectAll)
  (\r -> r.customerPayment.payment.staffId .== fromIntegralExpr r.staff.staffId) $ do
  sort $ \r -> asc r.customerPayment.payment.paymentDate
  selectAll
  
-- left join
leftJoinEg1 :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("film", Film)
                               , '("inventory", Maybe Inventory)
                               ])
leftJoinEg1 =
  leftJoin
  (#film .= rel @(DVDRentalDB db) @Film selectAll)
  (#inventory .= rel @(DVDRentalDB db) @Inventory selectAll)
  (\r -> r.inventory.filmId .== fromIntegralExpr r.film.filmId) $ do
  sort $ \r -> asc r.film.title
  selectAll

leftJoinEg2 :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("film", Film)
                               , '("inventory", Maybe Inventory)
                               ])
leftJoinEg2 =
  leftJoin
  (#film .= rel @(DVDRentalDB db) @Film selectAll)
  (#inventory .= rel @(DVDRentalDB db) @Inventory selectAll)
  (\r -> r.inventory.filmId .== fromIntegralExpr r.film.filmId) $ do
  restrict $ \r -> isNull r.inventory.filmId
  sort $ \r -> asc r.film.title
  selectAll  
-- right join

rightJoinEg1 :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("inventory", Maybe Inventory)
                               , '("film", Film)
                               ])
rightJoinEg1 =
  rightJoin
  (#inventory .= rel @(DVDRentalDB db) @Inventory selectAll)
  (#film .= rel @(DVDRentalDB db) @Film selectAll)
  (\r -> r.inventory.filmId .== fromIntegralExpr r.film.filmId) $ do
  sort $ \r -> asc r.film.title
  selectAll

rightJoinEg2 :: forall db.
  (db ~ 'Postgres) =>
  Query (DVDRentalDB db) (Rec '[ '("inventory", Maybe Inventory)
                               , '("film", Film)
                               ])
rightJoinEg2 =
  rightJoin
  (#inventory .= rel @(DVDRentalDB db) @Inventory selectAll)
  (#film .= rel @(DVDRentalDB db) @Film selectAll)
  (\r -> r.inventory.filmId .== fromIntegralExpr r.film.filmId) $ do
  restrict $ \r -> isNull r.inventory.filmId
  sort $ \r -> asc r.film.title
  selectAll
  
-- self join
-- full outer join
-- cross join
-- natural join
-- group by
-- union
-- intersect
-- having
-- grouping sets
-- cube
-- rollup
-- subquery
-- any
-- all
-- exits

-- MUTATIONS
-- insert
-- insert many
-- update
-- update join
-- delete
-- delete join
-- upsert
-- TODO:

len :: Expr sc Text -> Expr sc Int 
len = undefined
