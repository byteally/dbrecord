{-# LANGUAGE DeriveGeneric #-}
module Chinook.Models.Invoice where
import Data.Time.LocalTime
import Data.Text
import GHC.Generics

data Invoice  = Invoice
  {total :: Rational,
   billingPostalCode :: Maybe Text,
   billingCountry :: Maybe Text,
   billingState :: Maybe Text,
   billingCity :: Maybe Text,
   billingAddress :: Maybe Text,
   invoiceDate :: LocalTime,
   customerId :: Int,
   invoiceId :: Int} deriving (Eq, Show, Generic)
