{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}

{-# OPTIONS_GHC -Wno-orphans             #-}

module DBRecord.Postgres.Internal.RegClass
       ( RegClass (..)
       , E.regClass
       ) where

import qualified DBRecord.Internal.Expr as E ( regClass )
import           DBRecord.Types ( RegClass (..) )
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import           Database.PostgreSQL.Simple.FromField
       ( FromField (fromField) , typeOid, returnError, ResultError (..), Oid (..) )
import           Database.PostgreSQL.Simple.TypeInfo.Static (TypeInfo (..))

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
               Just dat -> return (RegClass (T.pack dat))
