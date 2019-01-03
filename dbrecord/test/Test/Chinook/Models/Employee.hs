

{-# LANGUAGE DeriveGeneric #-}
module Test.Chinook.Models.Employee where
import Data.Text
import Data.Time.LocalTime
import GHC.Generics

data Employee  = Employee
  {email :: Maybe Text,
   fax :: Maybe Text,
   phone :: Maybe Text,
   postalCode :: Maybe Text,
   country :: Maybe Text,
   state :: Maybe Text,
   city :: Maybe Text,
   address :: Maybe Text,
   hireDate :: Maybe LocalTime,
   birthDate :: Maybe LocalTime,
   reportsTo :: Maybe Int,
   title :: Maybe Text,
   firstName :: Text,
   lastName :: Text,
   employeeId :: Int} deriving (Eq, Show, Generic)
