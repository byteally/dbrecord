{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
module DBRecord.Internal.DDL where

import qualified Data.Text as T
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Types (DBType (..), DBTypeName(..))
import Data.Kind
import Data.Proxy
import Data.Functor.Identity

newtype ColName = ColName T.Text
  deriving newtype (Show, Eq)

newtype ColType = ColType DBType
  deriving newtype (Show, Eq)

data Column = Column ColName ColType
  deriving (Show, Eq)

newtype CheckExpr = CheckExpr PQ.PrimExpr
  deriving newtype (Show, Eq)

newtype DefExpr = DefExpr PQ.PrimExpr
  deriving newtype (Show, Eq)

newtype EnumVal = EnumVal T.Text
  deriving newtype (Show, Eq)

newtype ConstraintName = ConstraintName T.Text
  deriving newtype (Show, Eq)

newtype SeqName = SeqName T.Text
  deriving newtype (Show, Eq)

type PrimDDL = PrimDDLF Identity
type BaseLinePrimDDL = PrimDDLF Proxy

data OidK
  = TableOid
  | TypeOid
  | SchemaOid
  | OwnerOid
  | AttrOid
  | EnumOid

data Oid :: OidK -> Type where
  TableOid_ :: Int -> Oid 'TableOid
  TypeOid_ :: Int -> Oid 'TypeOid
  SchemaOid_ :: Int -> Oid 'SchemaOid
  OwnerOid_ :: Int -> Oid 'OwnerOid
  AttrOid_ :: Oid ownerClass -> Int -> Oid 'TypeOid -> Oid ownerClass
  EnumOid_ :: Oid 'TypeOid -> Double -> Oid 'EnumOid

deriving instance Show (Oid oid)
deriving instance Eq (Oid oid)
deriving instance Ord (Oid oid)

data InsSetK
  = CreateTypeIS
  | CreateEnumIS
  | CreateSeqIS
  | CreateTableIS
  | AlterTypeIS AlterTypeInsSetK
  | AlterSeqIS AlterSeqInsSetK
  | AlterTableIS AlterTableInsSetK
  | DropTableIS
  | DropTypeIS
  | DropSeqIS

data AlterTypeInsSetK
  = RenameTypeIS
  | AddAttributeIS
  | AlterAttributeIS AlterAttributeInsSetK
  | AddEnumValIS
  | AddAfterEnumValIS
  | AddBeforeEnumValIS
  | DropAttributeIS

data AlterTableInsSetK
  = AddColumnIS
  | RenameColumnIS
  | AlterColumnIS AlterColumnInsSetK
  | RenameTableIS
  | AddConstraintIS AddConstraintInsSetK
  | DropConstraintIS DropConstraintInsSetK
  | DropColumnIS

data AlterSeqInsSetK
  = AddOwnerIS

data AlterAttributeInsSetK
  = ChangeAttrTypeIS

data AlterColumnInsSetK
  = SetNotNullIS
  | DropNotNullIS
  | ChangeTypeIS
  | AddDefaultIS
  | DropDefaultIS

data AddConstraintInsSetK
  = AddPrimaryKeyIS
  | AddUniqueIS
  | AddCheckIS
  | AddForeignKeyIS

data DropConstraintInsSetK
  = DropPrimaryKeyIS
  | DropUniqueIS
  | DropCheckIS
  | DropForeignKeyIS

data DDLInsSetId :: InsSetK -> Type where
  CreateTypeISId :: Oid 'TypeOid -> DDLInsSetId 'CreateTypeIS
  CreateEnumISId :: Oid 'TypeOid -> DDLInsSetId 'CreateEnumIS
  CreateSeqISId :: Oid 'TableOid -> DDLInsSetId 'CreateSeqIS
  CreateTableISId :: Oid 'TableOid -> DDLInsSetId 'CreateTableIS
  AlterTypeISId :: Oid 'TypeOid -> AlterTypeInsSetId alterTypeISId -> DDLInsSetId ('AlterTypeIS alterTypeISId)
  AlterSeqISIs :: AlterSeqInsSetId alterSeqISId -> DDLInsSetId ('AlterSeqIS alterSeqISId)
  AlterTableISId :: Oid 'TableOid -> AlterTableInsSetId alterTableISId -> DDLInsSetId ('AlterTableIS alterTableISId)
  DropTableISId :: Oid 'TableOid -> DDLInsSetId 'DropTableIS
  DropTypeISId :: Oid 'TypeOid -> DDLInsSetId 'DropTypeIS
  DropSeqISId :: DDLInsSetId 'DropSeqIS

deriving instance Show (DDLInsSetId is)
deriving instance Eq (DDLInsSetId is)
deriving instance Ord (DDLInsSetId is)

data AlterTableInsSetId :: AlterTableInsSetK -> Type where
  AddColumnISId :: Int -> Word -> Oid 'TypeOid -> AlterTableInsSetId 'AddColumnIS

deriving instance Show (AlterTableInsSetId is)
deriving instance Eq (AlterTableInsSetId is)
deriving instance Ord (AlterTableInsSetId is)

data AlterTypeInsSetId :: AlterTypeInsSetK -> Type where

deriving instance Show (AlterTypeInsSetId is)
deriving instance Eq (AlterTypeInsSetId is)
deriving instance Ord (AlterTypeInsSetId is)

data AlterSeqInsSetId :: AlterSeqInsSetK -> Type where

deriving instance Show (AlterSeqInsSetId is)
deriving instance Eq (AlterSeqInsSetId is)
deriving instance Ord (AlterSeqInsSetId is)

data SomeDDLInsSetId where
  SomeDDLInsSetId :: DDLInsSetId is -> SomeDDLInsSetId

deriving instance Show SomeDDLInsSetId

instance Eq SomeDDLInsSetId where
  (==) sis1@(SomeDDLInsSetId (CreateTypeISId {})) = \case
    sis2@(SomeDDLInsSetId (CreateTypeISId {})) -> sis1 == sis2
    _ -> False
  (==) sis1@(SomeDDLInsSetId (CreateEnumISId {})) = \case
    sis2@(SomeDDLInsSetId (CreateEnumISId {})) -> sis1 == sis2
    _ -> False
  (==) sis1@(SomeDDLInsSetId (CreateSeqISId {})) = \case
    sis2@(SomeDDLInsSetId (CreateSeqISId {})) -> sis1 == sis2
    _ -> False
  (==) sis1@(SomeDDLInsSetId (CreateTableISId {})) = \case
    sis2@(SomeDDLInsSetId (CreateTableISId {})) -> sis1 == sis2
    _ -> False
  (==) sis1@(SomeDDLInsSetId (AlterTypeISId {})) = \case
    sis2@(SomeDDLInsSetId (AlterTypeISId {})) -> sis1 == sis2
    _ -> False
  (==) sis1@(SomeDDLInsSetId (AlterSeqISIs {})) = \case
    sis2@(SomeDDLInsSetId (AlterSeqISIs {})) -> sis1 == sis2
    _ -> False
  (==) sis1@(SomeDDLInsSetId (AlterTableISId {})) = \case
    sis2@(SomeDDLInsSetId (AlterTableISId {})) -> sis1 == sis2
    _ -> False
  (==) sis1@(SomeDDLInsSetId (DropTableISId {})) = \case
    sis2@(SomeDDLInsSetId (DropTableISId {})) -> sis1 == sis2
    _ -> False
  (==) sis1@(SomeDDLInsSetId (DropTypeISId {})) = \case
    sis2@(SomeDDLInsSetId (DropTypeISId {})) -> sis1 == sis2
    _ -> False
  (==) sis1@(SomeDDLInsSetId (DropSeqISId {})) = \case
    sis2@(SomeDDLInsSetId (DropSeqISId {})) -> sis1 == sis2
    _ -> False

instance Ord SomeDDLInsSetId where
  compare sis1@(SomeDDLInsSetId (CreateTypeISId {})) = \case
    sis2@(SomeDDLInsSetId (CreateTypeISId {})) -> sis1 `compare` sis2
    _ -> undefined
  compare sis1@(SomeDDLInsSetId (CreateEnumISId {})) = \case
    sis2@(SomeDDLInsSetId (CreateEnumISId {})) -> sis1 `compare` sis2
    _ -> undefined
  compare sis1@(SomeDDLInsSetId (CreateSeqISId {})) = \case
    sis2@(SomeDDLInsSetId (CreateSeqISId {})) -> sis1 `compare` sis2
    _ -> undefined
  compare sis1@(SomeDDLInsSetId (CreateTableISId {})) = \case
    sis2@(SomeDDLInsSetId (CreateTableISId {})) -> sis1 `compare` sis2
    _ -> undefined
  compare sis1@(SomeDDLInsSetId (AlterTypeISId {})) = \case
    sis2@(SomeDDLInsSetId (AlterTypeISId {})) -> sis1 `compare` sis2
    _ -> undefined
  compare sis1@(SomeDDLInsSetId (AlterSeqISIs {})) = \case
    sis2@(SomeDDLInsSetId (AlterSeqISIs {})) -> sis1 `compare` sis2
    _ -> undefined
  compare sis1@(SomeDDLInsSetId (AlterTableISId {})) = \case
    sis2@(SomeDDLInsSetId (AlterTableISId {})) -> sis1 `compare` sis2
    _ -> undefined
  compare sis1@(SomeDDLInsSetId (DropTableISId {})) = \case
    sis2@(SomeDDLInsSetId (DropTableISId {})) -> sis1 `compare` sis2
    _ -> undefined
  compare sis1@(SomeDDLInsSetId (DropTypeISId {})) = \case
    sis2@(SomeDDLInsSetId (DropTypeISId {})) -> sis1 `compare` sis2
    _ -> undefined
  compare sis1@(SomeDDLInsSetId (DropSeqISId {})) = \case
    sis2@(SomeDDLInsSetId (DropSeqISId {})) -> sis1 `compare` sis2
    _ -> undefined    


data DataSafety
  = Lossy
  | Lossless
  deriving (Show, Eq)

instance Semigroup DataSafety where
  Lossless <> Lossless = Lossless
  Lossy <> _ = Lossy
  _ <> Lossy = Lossy

data MigOpType
  = BackwardCompatible DataSafety
  | BackwardInCompatible DataSafety
  deriving (Show, Eq)

getMigDataSafety :: MigOpType -> DataSafety
getMigDataSafety = \case
  BackwardCompatible s -> s
  BackwardInCompatible s -> s

class HasMigOpType (is :: InsSetK) where
  getMigOpType :: Proxy is -> MigOpType

instance HasMigOpType 'CreateTypeIS where
  getMigOpType _ = BackwardCompatible Lossless

instance HasMigOpType 'CreateEnumIS where
  getMigOpType _ = BackwardCompatible Lossless

instance HasMigOpType 'CreateSeqIS where
  getMigOpType _ = BackwardCompatible Lossless

instance HasMigOpType 'CreateTableIS where
  getMigOpType _ = BackwardCompatible Lossless

instance HasMigOpType ('AlterTypeIS is) where
  getMigOpType _ = BackwardCompatible Lossless

instance HasMigOpType ('AlterSeqIS is) where
  getMigOpType _ = BackwardCompatible Lossless

instance HasMigOpType ('AlterTableIS 'AddColumnIS) where
  getMigOpType _ = BackwardCompatible Lossless

instance HasMigOpType ('AlterTableIS 'RenameColumnIS) where
  getMigOpType _ = BackwardInCompatible Lossless

instance HasMigOpType ('AlterTableIS ('AlterColumnIS is)) where
  getMigOpType _ = BackwardInCompatible Lossy

instance HasMigOpType ('AlterTableIS 'RenameTableIS) where
  getMigOpType _ = BackwardInCompatible Lossless

instance HasMigOpType ('AlterTableIS ('AddConstraintIS is)) where
  getMigOpType _ = BackwardInCompatible Lossless

instance HasMigOpType ('AlterTableIS ('DropConstraintIS is)) where
  getMigOpType _ = BackwardCompatible Lossless

instance HasMigOpType ('AlterTableIS 'DropColumnIS) where
  getMigOpType _ = BackwardInCompatible Lossy

instance HasMigOpType 'DropTableIS where
  getMigOpType _ = BackwardInCompatible Lossy

instance HasMigOpType 'DropTypeIS where
  getMigOpType _ = BackwardCompatible Lossless

instance HasMigOpType 'DropSeqIS where
  getMigOpType _ = BackwardCompatible Lossy

instance Semigroup MigOpType where
  BackwardCompatible safety1 <> BackwardCompatible safety2 = BackwardCompatible (safety1 <> safety2)
  BackwardInCompatible s <> m = BackwardInCompatible (s <> getMigDataSafety m)
  m <> BackwardInCompatible s = BackwardInCompatible (getMigDataSafety m <> s)

data PrimDDLF (f :: Type -> Type)
  = CreateType  DBTypeName (f [Column])
  | CreateEnum  DBTypeName (f [EnumVal])
  | CreateSeq   SeqName
  | CreateTable PQ.TableId  (f [Column])
  | AlterType   DBTypeName AlterType
  | AlterSeq    SeqName  AlterSeq
  | AlterTable  PQ.TableId  AlterTable
  | DropTable   PQ.TableId
  | DropType    DBTypeName
  | DropSeq     SeqName


deriving instance (Show (f [Column]), Show (f [EnumVal])) => Show (PrimDDLF f)
deriving instance (Eq (f [Column]), Eq (f [EnumVal])) => Eq (PrimDDLF f)

data AlterTable
  = AddColumn      Column
  | RenameColumn   ColName        ColName
  | AlterColumn    ColName        AlterColumn
  | RenameTable    PQ.TableId
  | AddConstraint  ConstraintName AddConstraint
  | DropConstraint DropConstraint
  | DropColumn     ColName
  deriving (Show, Eq)

data DropConstraint
  = DropPrimaryKey ConstraintName
  | DropUnique     ConstraintName
  | DropCheck      ConstraintName
  | DropForeignKey ConstraintName
  deriving (Show, Eq)

data AlterSeq
  = AddOwner PQ.TableId ColName
  deriving (Show, Eq)

data AddConstraint
  = AddPrimaryKey [ColName]
  | AddUnique     [ColName]
  | AddCheck      CheckExpr
  | AddForeignKey [ColName] PQ.TableId [ColName]
  deriving (Show, Eq)

data AlterColumn
  = SetNotNull
  | DropNotNull
  | ChangeType ColType
  | AddDefault DefExpr
  | DropDefault
  deriving (Show, Eq)

data AlterType
  = RenameType       DBTypeName
  | AddAttribute     Column
  | AlterAttribute   ColName     AlterAttribute
  | AddEnumVal       EnumVal
  | AddAfterEnumVal  EnumVal     EnumVal
  | AddBeforeEnumVal EnumVal     EnumVal
  | DropAttribute    ColName
  deriving (Show, Eq)

data AlterAttribute
  = ChangeAttrType ColType
  deriving (Show, Eq)
