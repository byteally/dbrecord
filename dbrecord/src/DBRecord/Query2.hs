{-# OPTIONS_GHC -Wwarn -Wno-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE ExplicitForAll                #-}
{-# LANGUAGE KindSignatures                #-}
{-# LANGUAGE TypeFamilyDependencies        #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE ConstraintKinds               #-}
{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE TypeOperators                 #-}
{-# LANGUAGE UndecidableInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE TypeApplications              #-}
{-# LANGUAGE PatternSynonyms               #-}
{-# LANGUAGE ViewPatterns                  #-}
{-# LANGUAGE GADTs                         #-}
{-# LANGUAGE FlexibleInstances             #-}
{-# LANGUAGE MultiParamTypeClasses         #-}
{-# LANGUAGE FunctionalDependencies        #-}
{-# LANGUAGE RankNTypes                    #-}
module DBRecord.Query2
  ( module DBRecord.Internal.Order
  , module DBRecord.Internal.Expr
  , module DBRecord.Internal.Window
  , module DBRecord.Internal.Predicate
  , module DBRecord.Query2
  ) where


import DBRecord.Schema

import DBRecord.Internal.Order
import DBRecord.Internal.Expr
import DBRecord.Internal.Predicate
import DBRecord.Internal.Common
import DBRecord.Internal.Window
import DBRecord.Internal.Schema hiding (insert, delete)
import DBRecord.Internal.PrimQuery  hiding (insertQ, updateQ, deleteQ)
import DBRecord.Internal.Query (getTableId, getTableProjections)
import qualified DBRecord.Internal.Query as Q
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Types
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Int
import GHC.Exts
import GHC.TypeLits
import Data.Functor.Identity
import Control.Monad.IO.Class
import Control.Monad.Reader
import DBRecord.Internal.Lens ((^.))
import Control.Exception hiding (TypeError)
import Data.Kind 

import GHC.Generics

-- TODO: Clean up
import DBRecord.Query (DBM)


get :: DBM (SchemaDB ()) ()
get = undefined
