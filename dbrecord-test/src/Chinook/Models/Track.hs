{-# LANGUAGE DeriveGeneric #-}
module Chinook.Models.Track where

import Data.Text
import GHC.Generics

data Track  = Track
  {unitPrice :: Rational,
   bytes :: Maybe Int,
   milliseconds :: Int,
   composer :: Maybe Text,
   genreId :: Maybe Int,
   mediaTypeId :: Int,
   albumId :: Maybe Int,
   name :: Text,
   trackId :: Int} deriving (Eq, Show, Generic)
