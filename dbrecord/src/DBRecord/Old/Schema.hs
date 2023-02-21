{-# OPTIONS_GHC -Wno-dodgy-exports #-}
module DBRecord.Old.Schema
       ( DbK (..)
       , Col (..)
       , col
       , Database (..)
       , Table (..)
       , Schema (..)
       , UDType (..)
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
       , UDTypeMappings (..)
       , Json (..)
       , TableTypes (..)
       ) where

import DBRecord.Internal.Types 
import DBRecord.Internal.Schema hiding (DBType)
import DBRecord.Internal.DBTypeValidation ()
import DBRecord.Internal.DBTypes
import DBRecord.Types
{-
IgnoredCol
ForeignRef
CheckCT

constructing defautls, checks
-}
