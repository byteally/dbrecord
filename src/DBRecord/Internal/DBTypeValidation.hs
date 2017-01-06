{-# OPTIONS_GHC -fno-warn-orphans -Wno-redundant-constraints #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables, DataKinds, KindSignatures, TypeOperators, TypeFamilies, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module DBRecord.Internal.DBTypeValidation where

import DBRecord.Internal.DBTypes
import DBRecord.Internal.Schema
import DBRecord.Internal.Common
import Data.Text (Text)
import qualified Data.Text as T
import DBRecord.Internal.Types
import Data.Proxy
import GHC.TypeLits
import GHC.Exts
import Data.Aeson
import Data.UUID.Types
import Data.Functor.Const
import Data.Time.LocalTime
import Data.ByteString (ByteString)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.CaseInsensitive  (CI)
import Data.Int
import Data.Vector (Vector)
import GHC.Generics

type family InvalidPGType (db :: *) a :: Constraint where
  InvalidPGType _ Int           = ()
  InvalidPGType _ Double        = ()
  InvalidPGType _ Int64         = ()
  InvalidPGType _ Int32         = ()
  InvalidPGType _ Bool          = ()
  InvalidPGType _ String        = ()
  InvalidPGType _ Text          = ()
  InvalidPGType _ (CI Text)     = ()
  InvalidPGType _ ByteString    = ()
  InvalidPGType _ Value         = ()
  InvalidPGType _ UTCTime       = ()
  InvalidPGType _ LocalTime     = ()
  InvalidPGType _ TimeOfDay     = ()
  InvalidPGType _ Day           = ()
  InvalidPGType _ UUID          = ()
  InvalidPGType db (Maybe a)     = InvalidPGType db a
  InvalidPGType db (Vector a)    = InvalidPGType db a
  InvalidPGType db (Json a)      = InvalidPGType db a
  InvalidPGType db (JsonStr a)   = InvalidPGType db a
  InvalidPGType db (CustomType a) = ()
  InvalidPGType db a              = ValidateCustTy db (IsNewTy (Rep a)) a

type family InvalidDBType (db :: *) a :: Constraint where
  InvalidDBType db a  = InvalidDBType' db (DB db) a

type family InvalidDBType' (db :: *) (dbK :: DbK) a :: Constraint where
  InvalidDBType' db 'Postgres a = InvalidPGType db a

type family ValidateCustTy (db :: *) (isNewTy :: Bool) (t :: *) :: Constraint where
  ValidateCustTy db 'True t = InvalidDBType db (InnerTy t)
  ValidateCustTy db 'False t = AssertCxt (Elem (Types db) t)
    ('Text "Invalid postgres type: " ':<>: 'ShowType t
     ':$$: 'Text "Hint: Add " ':<>: 'ShowType t ':<>: ('Text " to Types field in Database instance of ") ':<>: ('ShowType db)
    )
type family UnWrapNT (isNewTy :: Bool) (t :: *) where
  UnWrapNT 'True t  = InnerTy t
  UnWrapNT 'False t = t

instance ( SingCols db cols colMap
         , KnownSymbol cn
         , InvalidDBType db ct
         , ShowDBType (DB db) (GetDBTypeRep (DB db) ct)
         , aliasedCol ~ AliasedCol cn colMap
         , KnownSymbol aliasedCol
         ) => SingCols db ((cn ::: ct) ': cols) colMap where
  singCols _ _ _ = let pgTy = showDBType (Proxy :: Proxy (DB db)) (Proxy :: Proxy (GetDBTypeRep (DB db) ct))
                       colN = T.pack $ symbolVal (Proxy @aliasedCol)
                   in (Const $ Column colN pgTy) :& singCols (Proxy @db) (Proxy :: Proxy cols) (Proxy @colMap)

instance SingCols db '[] colMap where
  singCols _ _ _ = Nil

getSchemaName :: forall db.
               ( KnownSymbol (GetSchemaName db)
               ) => Const Text db
getSchemaName = Const $ T.pack $ symbolVal (Proxy @(GetSchemaName db))

getTableName :: forall db tab.
               ( KnownSymbol (TableName db tab)
               ) => Const Text (db,tab)
getTableName = Const $ T.pack $ symbolVal (Proxy @(TableName db tab))

getTableFields :: forall db tab.
                 ( SingCols db (OriginalTableFields tab) (ColumnNames db tab)
                 ) => Const [Column] (db, tab)
getTableFields = Const $ recordToList $ singCols (Proxy @db) (Proxy @(OriginalTableFields tab)) (Proxy @(ColumnNames db tab))

getTableHFields ::  forall db tab.
                   ( SingCols db (OriginalTableFields tab) (ColumnNames db tab)
                   ) => Proxy db -> Proxy tab -> HList (Const Column) (OriginalTableFields tab)
getTableHFields _ _ = singCols (Proxy @db) (Proxy @(OriginalTableFields tab)) (Proxy @(ColumnNames db tab))

