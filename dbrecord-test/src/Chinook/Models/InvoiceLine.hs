{-# LANGUAGE DeriveGeneric #-}
module Chinook.Models.InvoiceLine where

import GHC.Generics

data InvoiceLine  = InvoiceLine
  {quantity :: Int, 
   unitPrice :: Rational, 
   trackId :: Int, 
   invoiceId :: Int, 
   invoiceLineId :: Int} deriving (Eq, Show, Generic)
