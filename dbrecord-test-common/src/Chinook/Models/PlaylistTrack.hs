{-# LANGUAGE DeriveGeneric #-}
module Chinook.Models.PlaylistTrack where

import GHC.Generics

data PlaylistTrack  = PlaylistTrack
  {trackId :: Int, 
   playlistId :: Int} deriving (Eq, Show, Generic)
