{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE AllowAmbiguousTypes         #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE TypeApplications            #-}
{-# LANGUAGE DataKinds                   #-}

{-# OPTIONS_GHC -Wno-orphans             #-}

module DBRecord.Postgres.Internal.RegClass
       ( RegClass
       , regclass
       , regtype
       ) where

import           DBRecord.Internal.DBTypes as DBTypes
import           DBRecord.Internal.Expr (ConstExpr (..), unsafeCast, literalExpr)
import           DBRecord.Internal.PrimQuery (Lit(String))
import           DBRecord.Internal.Schema
import           DBRecord.Internal.Types
import           DBRecord.Postgres.Internal.Sql.Pretty (ppPGType, ppPGOIDType)
import           DBRecord.Types ( PGOID, RegClass, RegType, PGOIDType(..), mkUnsafePGOID, getPGOID)
import           Data.ByteString.Char8 as ASCII
import qualified Data.ByteString.Char8 as B
import           Data.Proxy
import qualified Data.Text as T
import           Database.PostgreSQL.Simple.FromField
       ( FromField (fromField) , typeOid, returnError, ResultError (..), Oid (..) )
import           Database.PostgreSQL.Simple.TypeInfo.Static (TypeInfo (..))
import           GHC.TypeLits



regclass :: forall sc rel. (Table sc rel, KnownSymbol (TableName sc rel)) => RegClass
regclass = mkUnsafePGOID $ T.pack $ symbolVal (Proxy :: Proxy (TableName sc rel))

regtype :: forall sc ty.
  ( DBTypeCtx (GetDBTypeRep sc ty)
  , SingI (GetDBTypeRep sc ty)
  ) => RegType
regtype = mkUnsafePGOID $ T.pack $ ppPGType tyRep
  where tyRep = fromSing (sing :: Sing (GetDBTypeRep sc ty))

instance SingPGOIDType t => ConstExpr sc (PGOID t) where
  constExpr oid = unsafeCast (OtherBuiltInType $ DBTypes.DBTypeName (T.pack $ ppPGOIDType $ singPGOIDType @t) []) $ go (getPGOID oid)
    where
      go = literalExpr . String

class SingPGOIDType (t :: PGOIDType) where
  singPGOIDType :: PGOIDType

instance SingPGOIDType 'PGOID where
  singPGOIDType = PGOID

instance SingPGOIDType 'RegProc where
  singPGOIDType = RegProc

instance SingPGOIDType 'RegProcedure where
  singPGOIDType = RegProcedure

instance SingPGOIDType 'RegOper where
  singPGOIDType = RegOper

instance SingPGOIDType 'RegOperator where
  singPGOIDType = RegOperator

instance SingPGOIDType 'RegClass where
  singPGOIDType = RegClass

instance SingPGOIDType 'RegType where
  singPGOIDType = RegType

instance SingPGOIDType 'RegRole where
  singPGOIDType = RegRole

instance SingPGOIDType 'RegNamespace where
  singPGOIDType = RegNamespace

instance SingPGOIDType 'RegConfig where
  singPGOIDType = RegConfig

instance SingPGOIDType 'RegDictionary where
  singPGOIDType = RegDictionary

regClass :: TypeInfo
regClass =  Basic {
    typoid      = regClassOid,
    typcategory = 'N',
    typdelim    = ',',
    typname     = "regclass"
  }

regClassOid :: Oid
regClassOid = Oid 2205
{-# INLINE regClassOid #-}

instance FromField RegClass where
   fromField f mdata =
      if typeOid f /= typoid regClass
        then returnError Incompatible f ""
        else case B.unpack `fmap` mdata of
               Nothing  -> returnError UnexpectedNull f ""
               Just dat -> return (mkUnsafePGOID (T.pack dat))

regtypeInfo :: TypeInfo
regtypeInfo =  Basic {
    typoid      = regtypeOid,
    typcategory = 'N',
    typdelim    = ',',
    typname     = "regtype"
  }

regtypeOid :: Oid
regtypeOid = Oid 2206
{-# INLINE regtypeOid #-}

instance FromField RegType where
   fromField f mdata =
      if typeOid f /= typoid regtypeInfo
        then returnError Incompatible f ""
        else case ASCII.unpack `fmap` mdata of
               Nothing  -> returnError UnexpectedNull f ""
               Just dat -> return (mkUnsafePGOID (T.pack dat))
