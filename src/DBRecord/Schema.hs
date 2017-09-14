module DBRecord.Schema
       ( DbK (..)
       , Col (..)
--       , col
       , Database (..)
       , Table (..)  
       , module DBRecord.Internal.DBTypeValidation
       , tabName
       , def
       , end
       , dbDefaults
       , check
       , dbChecks
       , GetSchemaName
       , TableFields
       , AliasedCol
       , CheckCT (..)
       , UniqueCT (..)
       , Uq (..)
       , IgnoredCol (..)
       , ForeignRef (..)
       , HList (..)
       ) where

import DBRecord.Internal.Types
import DBRecord.Internal.Schema
import DBRecord.Internal.DBTypeValidation ()

{-
IgnoredCol
ForeignRef
CheckCT

constructing defautls, checks
-}
