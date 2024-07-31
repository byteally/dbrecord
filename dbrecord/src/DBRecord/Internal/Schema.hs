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

class (Database (DatabaseOf dbc)) => DBCatalog (dbc :: Type) where
  type DatabaseOf dbc = (db :: Type) | db -> dbc
  type Schemas dbc :: [Type]
  type Roles dbc :: [Type]
  type Extensions dbc :: [Type]

class (DBCatalog (DatabaseCatalog scc), Schema (SchemaOf scc)) => SchemaCatalog (scc :: Type) where
  type DatabaseCatalog scc :: Type
  type SchemaOf scc = (sc :: Type) | sc -> scc
  type Tables scc :: [Type]
  type Types scc :: [Type]
  type Views scc :: [Type]
  type MaterializedViews scc :: [Type]
  type Functions scc :: [(Symbol, Type)]
  type AggFunctions scc :: [(Symbol, Type)]
--  type Sequences scc :: [Type]
