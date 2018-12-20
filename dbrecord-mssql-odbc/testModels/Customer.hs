

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
module Models.Customer where

import GHC.Generics

data Customer  = Customer
  {customerid :: Int, 
   firstname :: character varying.character varying, 
   lastname :: character varying.character varying, 
   company :: Maybe character varying.character varying, 
   address :: Maybe character varying.character varying, 
   city :: Maybe character varying.character varying, 
   state :: Maybe character varying.character varying, 
   country :: Maybe character varying.character varying, 
   postalcode :: Maybe character varying.character varying, 
   phone :: Maybe character varying.character varying, 
   fax :: Maybe character varying.character varying, 
   email :: character varying.character varying, 
   supportrepid :: Maybe Int} deriving (Eq, Show, Generic)
