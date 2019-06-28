{-# LANGUAGE DeriveGeneric #-}
module Chinook.Models.MediaType where

import Data.Text
import GHC.Generics

data MediaType  = MediaType
  {name :: Maybe Text,
   mediaTypeId :: Int} deriving (Eq, Show, Generic)
