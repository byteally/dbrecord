{-# LANGUAGE DeriveGeneric #-}
module Chinook.Models.Playlist where

import Data.Text
import GHC.Generics

data Playlist  = Playlist
  {name :: Maybe Text,
   playlistId :: Int} deriving (Eq, Show, Generic)
