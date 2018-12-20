

{-# LANGUAGE DataKinds
           , TypeOperators
           , UndecidableInstances
           , OverloadedLabels
           , FlexibleInstances
           , MultiParamTypeClasses
           , DuplicateRecordFields
           , GADTs
           , TypeApplications
           , KindSignatures
           , DeriveGeneric
           , FlexibleContexts
           , FunctionalDependencies
           , ExplicitForAll
           , TypeFamilies
           , ScopedTypeVariables
           , OverloadedStrings
           , GeneralizedNewtypeDeriving
           , RankNTypes #-}
module Models.Invoice where

import GHC.Generics

data Invoice  = Invoice
  {invoiceid :: Int, 
   customerid :: Int, 
   invoicedate :: timestamp without time zone.timestamp without time zone, 
   billingaddress :: Maybe character varying.character varying, 
   billingcity :: Maybe character varying.character varying, 
   billingstate :: Maybe character varying.character varying, 
   billingcountry :: Maybe character varying.character varying, 
   billingpostalcode :: Maybe character varying.character varying, 
   total :: numeric.numeric} deriving (Eq, Show, Generic)
