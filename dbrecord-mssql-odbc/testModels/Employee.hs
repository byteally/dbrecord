

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
module Models.Employee where

import GHC.Generics

data Employee  = Employee
  {employeeid :: Int, 
   lastname :: character varying.character varying, 
   firstname :: character varying.character varying, 
   title :: Maybe character varying.character varying, 
   reportsto :: Maybe Int, 
   birthdate :: Maybe timestamp without time zone.timestamp without time zone, 
   hiredate :: Maybe timestamp without time zone.timestamp without time zone, 
   address :: Maybe character varying.character varying, 
   city :: Maybe character varying.character varying, 
   state :: Maybe character varying.character varying, 
   country :: Maybe character varying.character varying, 
   postalcode :: Maybe character varying.character varying, 
   phone :: Maybe character varying.character varying, 
   fax :: Maybe character varying.character varying, 
   email :: Maybe character varying.character varying} deriving (Eq, Show, Generic)
