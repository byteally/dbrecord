{-# LANGUAGE TypeFamilies, DataKinds, DeriveGeneric, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module DBRecord.MSSQL.Internal.Types where

import qualified DBRecord.Internal.Types as Type
import DBRecord.Internal.DBTypes
import Data.Text
import GHC.TypeLits
import GHC.Generics
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Expr
import Data.String
import qualified Data.Text as T
import Data.ByteString (ByteString)
import DBRecord.Internal.Types (DbK (..))
import Data.Proxy

newtype Sized (n :: Nat) a    = Sized { getSized :: a }
                             deriving (Show, Eq, Generic)
newtype Varsized (n :: Nat) a = Varsized { getVarsized :: a }
                             deriving (Show, Eq, Generic)

type instance CustomDBTypeRep 'Type.MSSQL (Sized n Text)    = 'Type.DBChar n
type instance CustomDBTypeRep 'Type.MSSQL (Varsized n Text) = 'Type.DBVarchar ('Right n)

instance EqExpr ByteString where
  a .== b = binOp PQ.OpEq a b

instance EqExpr (Type.CustomType (Sized n Text)) where
  a .== b = binOp PQ.OpEq a b

instance EqExpr (Type.CustomType (Varsized n Text)) where
  a .== b = binOp PQ.OpEq a b

instance IsString (Expr sc (Sized n Text)) where
  fromString = coerceExpr . annotateMSSQL . mssqltext . pack

instance IsString (Expr sc (Varsized n Text)) where
  fromString = coerceExpr . annotateMSSQL . mssqltext  . pack

instance (IsString (Expr sc a)
         ) => IsString (Expr sc (Type.CustomType a)) where
  fromString = coerceExpr . (fromString :: String -> Expr sc a)

mssqltext :: T.Text -> Expr sc T.Text
mssqltext = Expr . PQ.ConstExpr . PQ.String

mssqlbyte :: ByteString -> Expr sc ByteString
mssqlbyte = Expr . PQ.ConstExpr . PQ.Byte

annotateMSSQL :: forall a sc.
               ( Type.SingI (GetMSSQLTypeRep a)
               , DBTypeCtx (GetMSSQLTypeRep a)
               ) => Expr sc a -> Expr sc a
annotateMSSQL = annotateType' (Proxy :: Proxy 'MSSQL)
