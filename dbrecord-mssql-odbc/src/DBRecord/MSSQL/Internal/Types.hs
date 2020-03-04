{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies, DataKinds, DeriveGeneric, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, DeriveFunctor, GeneralizedNewtypeDeriving, MultiParamTypeClasses, UndecidableInstances #-}
module DBRecord.MSSQL.Internal.Types where

import qualified DBRecord.Internal.Types as Type
import DBRecord.Internal.DBTypes
import DBRecord.Internal.Schema
import Data.Text
import GHC.TypeLits
import GHC.Generics
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Expr
import Data.String
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Control.Monad.Reader

newtype MSSQLDBM m (db :: *) a = MSSQLDBM { runMSSQLDB :: ReaderT () m a}
  deriving (Functor, Applicative, Monad, MonadIO)


newtype Sized (n :: Nat) a    = Sized { getSized :: a }
                             deriving (Show, Eq, Generic)
newtype Varsized (n :: Nat) a = Varsized { getVarsized :: a }
                             deriving (Show, Eq, Generic)

type instance CustomDBTypeRep sc (Sized n Text)    = 'Type.DBChar n
type instance CustomDBTypeRep sc (Varsized n Text) = 'Type.DBVarchar ('Right n)

instance EqExpr sc ByteString where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc (Type.CustomType (Sized n Text)) where
  a .== b = binOp PQ.OpEq a b

instance EqExpr sc (Type.CustomType (Varsized n Text)) where
  a .== b = binOp PQ.OpEq a b

instance ( DBTypeCtx (GetDBTypeRep sc Text)
         , Type.SingI (GetDBTypeRep sc Text)
         ) => IsString (Expr sc scopes (Sized n Text)) where
  fromString = coerceExpr . annotateType . mssqltext . pack

instance ( DBTypeCtx (GetDBTypeRep sc Text)
         , Type.SingI (GetDBTypeRep sc Text)
         ) => IsString (Expr sc scopes (Varsized n Text)) where
  fromString = coerceExpr . annotateType . mssqltext  . pack

instance (IsString (Expr sc scopes a)
         ) => IsString (Expr sc scopes (Type.CustomType a)) where
  fromString = coerceExpr . (fromString :: String -> Expr sc scopes a)

mssqltext :: T.Text -> Expr sc scopes T.Text
mssqltext = Expr . PQ.ConstExpr . PQ.String

mssqlbyte :: ByteString -> Expr sc scopes ByteString
mssqlbyte = Expr . PQ.ConstExpr . PQ.Byte

