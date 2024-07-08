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
module DBRecord.Internal.UDType
  ( module DBRecord.Internal.UDType
  ) where

import GHC.TypeLits
import DBRecord.Internal.DBTypes
import DBRecord.Internal.Schema
-- import DBRecord.Internal.Expr
import DBRecord.Internal.Types
import DBRecord.Internal.Common
import qualified DBRecord.Internal.PrimQuery as PQ
import Data.Kind
import Data.Proxy
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import GHC.Records

class ( DBRepr (DB (SchemaDB sc)) ty
      ) => UDType (sc :: Type) (ty :: Type) where
  type TypeId sc ty = (oid :: Nat) | oid -> ty

udtypeToExpr :: forall ty sc.(UDType sc ty, SynTypeToExpr (GetUDTypeKind (DB (SchemaDB sc)) ty (ToDBType (DB (SchemaDB sc)) ty)) sc ty (Fields ty)) => Proxy '(sc, ty) -> Expr sc ty
udtypeToExpr _ = Expr $ PQ.FlatComposite $ synTypeToExpr_ (Proxy @'(GetUDTypeKind (DB (SchemaDB sc)) ty (ToDBType (DB (SchemaDB sc)) ty), sc, ty, (Fields ty)))

type family GetUDTypeKind (dbk :: DbK) (ty :: Type) (dbt :: DBObjK) :: UDTypeK where
  GetUDTypeKind _ _ ('UDTypeObj udt) = udt
  GetUDTypeKind dbk ty _ = TypeError ('ShowType ty ':<>: 'Text " is not a User Defined Type for database " ':<>: 'ShowType ty)

class SynTypeToExpr (udt :: UDTypeK) (sc :: Type) (ty :: Type) (flds :: [(Symbol, Type)]) where
  synTypeToExpr_ :: Proxy '(udt, sc, ty, flds) -> [PQ.Projection]

instance ( HasField fld ty a
         , DBRepr (DB (SchemaDB sc)) ty
         , KnownSymbol fld
         , SynTypeToExpr (GetUDTypeKind (DB (SchemaDB sc)) ty (ToDBType (DB (SchemaDB sc)) ty)) sc ty flds
         ) => SynTypeToExpr ('TaggedSum enk lay) sc ty ('(fld, a) ': flds) where
  synTypeToExpr_ _ = (fname, cexpr) : synTypeToExpr_ (Proxy @'(GetUDTypeKind (DB (SchemaDB sc)) ty (ToDBType (DB (SchemaDB sc)) ty), sc, ty, flds))
    where
      FieldAliases caliases = fieldAliases @(DB (SchemaDB sc)) @ty
      fname = T.pack $ symbolVal (Proxy @fld)
      cname = maybe (defHSNameToDBName fname) id $ HM.lookup fname caliases
      cexpr = PQ.BaseTableAttrExpr $ cname

instance SynTypeToExpr udt sc ty '[] where
  synTypeToExpr_ _ = []
