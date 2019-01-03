

{-# LANGUAGE DeriveGeneric #-}
module Test.Chinook.Models.Album where

import Data.Text
import GHC.Generics

data Album  = Album
  {artistId :: Int,
   title :: Text,
   albumId :: Int} deriving (Eq, Show, Generic)
