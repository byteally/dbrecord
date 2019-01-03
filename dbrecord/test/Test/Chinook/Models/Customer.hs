

{-# LANGUAGE DeriveGeneric #-}
module Test.Chinook.Models.Customer where

import Data.Text
import GHC.Generics

data Customer  = Customer
  {supportRepId :: Maybe Int,
   email :: Text,
   fax :: Maybe Text,
   phone :: Maybe Text,
   postalCode :: Maybe Text,
   country :: Maybe Text,
   state :: Maybe Text,
   city :: Maybe Text,
   address :: Maybe Text,
   company :: Maybe Text,
   lastName :: Text,
   firstName :: Text,
   customerId :: Int} deriving (Eq, Show, Generic)
