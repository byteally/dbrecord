

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
module Models.Track where

import GHC.Generics

data Track  = Track
  {trackid :: Int, 
   name :: character varying.character varying, 
   albumid :: Maybe Int, 
   mediatypeid :: Int, 
   genreid :: Maybe Int, 
   composer :: Maybe character varying.character varying, 
   milliseconds :: Int, 
   bytes :: Maybe Int, 
   unitprice :: numeric.numeric} deriving (Eq, Show, Generic)
