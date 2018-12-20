

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
module Models.Playlist where

import GHC.Generics

data Playlist  = Playlist
  {playlistid :: Int, 
   name :: Maybe character varying.character varying} deriving (Eq, Show, Generic)
