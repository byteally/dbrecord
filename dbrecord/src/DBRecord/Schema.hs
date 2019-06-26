module DBRecord.Schema
       ( DbK (..)
       , Col (..)
--       , col
       , Database (..)
       , Table (..)
       , Schema (..)
       , module DBRecord.Internal.DBTypeValidation
       , tabName
       , def
       , end
       , dbDefaults
       , check
       , dbChecks
       , TableFields
       , AliasedCol
       , CheckCT (..)
       , UniqueCT (..)
       , Uq (..)
       , IgnoredCol (..)
       , ForeignRef (..)
       , HList (..)
       , Serial
       , Owned
       , TypeName (..)
       ) where

import DBRecord.Internal.Types 
import DBRecord.Internal.Schema hiding (DBType)
import DBRecord.Internal.DBTypeValidation ()

{-
IgnoredCol
ForeignRef
CheckCT

constructing defautls, checks
-}
