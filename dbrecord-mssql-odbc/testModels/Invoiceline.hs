

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
module Models.Invoiceline where

import GHC.Generics

data Invoiceline  = Invoiceline
  {invoicelineid :: Int, 
   invoiceid :: Int, 
   trackid :: Int, 
   unitprice :: numeric.numeric, 
   quantity :: Int} deriving (Eq, Show, Generic)
