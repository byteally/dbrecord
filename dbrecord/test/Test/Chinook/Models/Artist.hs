

{-# LANGUAGE DeriveGeneric #-}
module Test.Chinook.Models.Artist where

import Data.Text
import GHC.Generics

data Artist  = Artist
  {name :: Maybe Text,
   artistId :: Int} deriving (Eq, Show, Generic)
