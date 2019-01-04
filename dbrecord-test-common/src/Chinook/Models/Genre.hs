{-# LANGUAGE DeriveGeneric #-}
module Chinook.Models.Genre where

import Data.Text
import GHC.Generics

data Genre  = Genre
  {name :: Maybe Text,
   genreId :: Int} deriving (Eq, Show, Generic)
