{-# OPTIONS_GHC -fno-warn-redundant-constraints -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE LambdaCase                 #-}

module DBRecord.Internal.Schema
  ( module DBRecord.Internal.Schema
  , Database (..)
  , Schema (..)
  ) where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits
import GHC.Generics
import Data.String
import Data.Kind
-- import DBRecord.Internal.DBTypes
import DBRecord.Internal.Types
import DBRecord.Internal.Common

---
class ( -- Break (NoGeneric db) (Rep db)
      -- TypeCxts db (Types db)
      ) => Database (db :: Type) where
  type DB db :: DbK
  type DB db = TypeError ('Text "DB type is not configured in the Database instance for type " ':<>: 'ShowType db ':$$:
                          'Text "Hint: add following to the Database instance for type "       ':<>: 'ShowType db ':$$:
                          'Text "type DB " ':<>: 'ShowType db ':<>: 'Text " = " ':<>: 'ShowType 'Postgres
                         )

  databaseName :: DatabaseName db
  default databaseName :: (KnownSymbol (GenTyCon (Rep db)), Break (NoGeneric db) (Rep db)) => DatabaseName db
  databaseName = DatabaseName $ defHSNameToDBName $ T.pack (symbolVal (Proxy @(GenTyCon (Rep db))))

newtype DatabaseName db = DatabaseName Text
  deriving newtype (Show, Eq, IsString)

_getDatabaseName :: DatabaseName db -> Text
_getDatabaseName (DatabaseName db) = db

class ( Database (SchemaDB sc)
      ) => Schema (sc :: Type) where
  type Baseline sc :: Nat
  type Baseline sc = 0

  type Version sc :: Nat
  type Version sc = 0

  type SchemaDB sc :: Type

  schemaName :: SchemaName sc
  default schemaName :: (KnownSymbol (GenTyCon (Rep sc)), Break (NoGeneric sc) (Rep sc)) => SchemaName sc
  schemaName = SchemaName $ defHSNameToDBName $ T.pack (symbolVal (Proxy @(GenTyCon (Rep sc))))

newtype SchemaName sc = SchemaName Text
  deriving newtype (Show, Eq, IsString)

_getSchemaName :: SchemaName sc -> Text
_getSchemaName (SchemaName sc) = sc

class DBCatalog (db :: Type) where
  type Schemas db :: [Type]
  type Roles db :: [Type]
  type Extensions db :: [Type]

class SchemaCatalog (sc :: Type) where
  type Tables sc :: [Type]
  type Types sc :: [Type]
  type Views sc :: [Type]
  type MaterializedViews sc :: [Type]
  type Functions sc :: [(Symbol, Type)]
  type AggFunctions sc :: [(Symbol, Type)]
--  type Sequences sc :: [Type]
