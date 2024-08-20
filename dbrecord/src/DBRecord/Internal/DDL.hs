{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
module DBRecord.Internal.DDL where

import qualified Data.Text as T
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Types (DBType (..), DBTypeName(..), Sing, SingE(..))
import DBRecord.Internal.Schema
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
  | AttrOid OidK
  | EnumOid
  deriving (Show, Eq, Ord)

data TypeOid
  = UDTyOid Int
  | PrimTyOid Int
data Oid :: OidK -> Type where
  TableOid_ :: Int -> Oid 'TableOid
  TypeOid_ :: Int -> Oid 'TypeOid
  SchemaOid_ :: T.Text -> Oid 'SchemaOid
  OwnerOid_ :: Int -> Oid 'OwnerOid
  AttrOid_ :: Oid ownerClass -> Int -> Oid 'TypeOid -> Oid ('AttrOid ownerClass)
  EnumOid_ :: Oid 'TypeOid -> Double -> Oid 'EnumOid

deriving instance Show (Oid oid)
deriving instance Eq (Oid oid)
deriving instance Ord (Oid oid)

data SomeOid where
  SomeOid :: Oid oidk -> SomeOid

instance Eq SomeOid where
  (==) (SomeOid o1@(TableOid_ {})) = \case
    (SomeOid o2@(TableOid_ {})) -> o1 == o2
    _ -> False
  (==) (SomeOid o1@(TypeOid_ {})) = \case
    (SomeOid o2@(TypeOid_ {})) -> o1 == o2
    _ -> False
  (==) (SomeOid o1@(SchemaOid_ {})) = \case
    (SomeOid o2@(SchemaOid_ {})) -> o1 == o2
    _ -> False
  (==) (SomeOid o1@(OwnerOid_ {})) = \case
    (SomeOid o2@(OwnerOid_ {})) -> o1 == o2
    _ -> False
  (==) (SomeOid (AttrOid_ s1 pos1 t1)) = \case
    (SomeOid (AttrOid_ s2 pos2 t2)) -> (SomeOid s1) == (SomeOid s2) &&
                                       pos1 == pos2 &&
                                       (SomeOid t1) == (SomeOid t2)
    _ -> False
  (==) (SomeOid o1@(EnumOid_ {})) = \case
    (SomeOid o2@(EnumOid_ {})) -> o1 == o2
    _ -> False

instance Ord SomeOid where
  compare (SomeOid o1@(TableOid_ {})) = \case
    (SomeOid o2@(TableOid_ {})) -> o1 `compare` o2
    SomeOid o2 -> fromSing (SingOid o1) `compare` fromSing (SingOid o2)
  compare (SomeOid o1@(TypeOid_ {})) = \case
    (SomeOid o2@(TypeOid_ {})) -> o1 `compare` o2
    SomeOid o2 -> fromSing (SingOid o1) `compare` fromSing (SingOid o2)
  compare (SomeOid o1@(SchemaOid_ {})) = \case
    (SomeOid o2@(SchemaOid_ {})) -> o1 `compare` o2
    SomeOid o2 -> fromSing (SingOid o1) `compare` fromSing (SingOid o2)
  compare (SomeOid o1@(OwnerOid_ {})) = \case
    (SomeOid o2@(OwnerOid_ {})) -> o1 `compare` o2
    SomeOid o2 -> fromSing (SingOid o1) `compare` fromSing (SingOid o2)
  compare (SomeOid o1@(AttrOid_ s1 pos1 t1)) = \case
    (SomeOid (AttrOid_ s2 pos2 t2)) -> ((SomeOid s1) `compare` (SomeOid s2)) `compare`
                                       (pos1 `compare` pos2) `compare`
                                       ((SomeOid t1) `compare` (SomeOid t2))
    SomeOid o2 -> fromSing (SingOid o1) `compare` fromSing (SingOid o2)
  compare (SomeOid o1@(EnumOid_ {})) = \case
    (SomeOid o2@(EnumOid_ {})) -> o1 `compare` o2
    SomeOid o2 -> fromSing (SingOid o1) `compare` fromSing (SingOid o2)

data InsSetK
  = RootIS OidK
  | CreateTypeIS
  | CreateEnumIS
  | CreateSeqIS
  | CreateTableIS
  | AlterTypeIS AlterTypeInsSetK
  | AlterSeqIS AlterSeqInsSetK
  | AlterTableIS AlterTableInsSetK
  | DropTableIS
  | DropTypeIS
  | DropSeqIS
  deriving (Show, Eq, Ord)

data AlterTypeInsSetK
  = RenameTypeIS
  | AddAttributeIS
  | AlterAttributeIS AlterAttributeInsSetK
  | AddEnumValIS
  | AddAfterEnumValIS
  | AddBeforeEnumValIS
  | DropAttributeIS
  deriving (Show, Eq, Ord)

data AlterTableInsSetK
  = AddColumnIS
  | RenameColumnIS
  | AlterColumnIS AlterColumnInsSetK
  | RenameTableIS
  | AddConstraintIS AddConstraintInsSetK
  | DropConstraintIS DropConstraintInsSetK
  | DropColumnIS
  deriving (Show, Eq, Ord)

data AlterSeqInsSetK
  = AddOwnerIS
  deriving (Show, Eq, Ord)

data AlterAttributeInsSetK
  = ChangeAttrTypeIS
  deriving (Show, Eq, Ord)

data AlterColumnInsSetK
  = SetNotNullIS
  | DropNotNullIS
  | ChangeTypeIS
  | AddDefaultIS
  | DropDefaultIS
  deriving (Show, Eq, Ord)

data AddConstraintInsSetK
  = AddPrimaryKeyIS
  | AddUniqueIS
  | AddCheckIS
  | AddForeignKeyIS
  deriving (Show, Eq, Ord)

data DropConstraintInsSetK
  = DropPrimaryKeyIS
  | DropUniqueIS
  | DropCheckIS
  | DropForeignKeyIS
  deriving (Show, Eq, Ord)

data DDLInsSetId :: InsSetK -> Type where
  RootISId :: Oid oidk -> T.Text -> DDLInsSetId ('RootIS oidk)
  CreateTypeISId :: Oid 'TypeOid -> DDLInsSetId 'CreateTypeIS
  CreateEnumISId :: Oid 'TypeOid -> DDLInsSetId 'CreateEnumIS
  CreateSeqISId :: Oid 'TableOid -> DDLInsSetId 'CreateSeqIS
  CreateTableISId :: Oid 'TableOid -> DDLInsSetId 'CreateTableIS
  AlterTypeISId :: Oid 'TypeOid -> AlterTypeInsSetId alterTypeISId -> DDLInsSetId ('AlterTypeIS alterTypeISId)
  AlterSeqISId :: AlterSeqInsSetId alterSeqISId -> DDLInsSetId ('AlterSeqIS alterSeqISId)
  AlterTableISId :: Oid 'TableOid -> AlterTableInsSetId alterTableISId -> DDLInsSetId ('AlterTableIS alterTableISId)
  DropTableISId :: Oid 'TableOid -> DDLInsSetId 'DropTableIS
  DropTypeISId :: Oid 'TypeOid -> DDLInsSetId 'DropTypeIS
  DropSeqISId :: DDLInsSetId 'DropSeqIS

deriving instance Show (DDLInsSetId is)
deriving instance Eq (DDLInsSetId is)
deriving instance Ord (DDLInsSetId is)

data AlterTableInsSetId :: AlterTableInsSetK -> Type where
  AddColumnISId :: Oid ('AttrOid 'TableOid) -> AlterTableInsSetId 'AddColumnIS
  RenameColumnISId :: Oid ('AttrOid 'TableOid) -> AlterTableInsSetId 'RenameColumnIS
  AlterColumnISId :: Oid ('AttrOid 'TableOid) -> AlterColumnInsSetId altColIS -> AlterTableInsSetId ('AlterColumnIS altColIS)
  RenameTableISId :: AlterTableInsSetId 'RenameTableIS
  AddConstraintISId :: AddConstraintInsSetId addConstIS -> AlterTableInsSetId ('AddConstraintIS addConstIS)
  DropConstraintISId :: DropConstraintInsSetId dropConstIS -> AlterTableInsSetId ('DropConstraintIS dropConstIS)
  DropColumnISId :: AlterTableInsSetId 'DropColumnIS

data AlterColumnInsSetId :: AlterColumnInsSetK -> Type where
  SetNotNullISId :: AlterColumnInsSetId 'SetNotNullIS
  DropNotNullISId :: AlterColumnInsSetId 'DropNotNullIS
  ChangeTypeISId :: AlterColumnInsSetId 'ChangeTypeIS
  AddDefaultISId :: AlterColumnInsSetId 'AddDefaultIS
  DropDefaultISId :: AlterColumnInsSetId 'DropDefaultIS

deriving instance Show (AlterColumnInsSetId is)
deriving instance Eq (AlterColumnInsSetId is)
deriving instance Ord (AlterColumnInsSetId is)

data AddConstraintInsSetId :: AddConstraintInsSetK -> Type where
  AddPrimaryKeyISId :: AddConstraintInsSetId 'AddPrimaryKeyIS
  AddUniqueISId :: AddConstraintInsSetId 'AddUniqueIS
  AddCheckISId :: AddConstraintInsSetId 'AddCheckIS
  AddForeignKeyISId :: AddConstraintInsSetId 'AddForeignKeyIS

deriving instance Show (AddConstraintInsSetId is)
deriving instance Eq (AddConstraintInsSetId is)
deriving instance Ord (AddConstraintInsSetId is)

data DropConstraintInsSetId :: DropConstraintInsSetK -> Type where
  DropPrimaryKeyISId :: DropConstraintInsSetId 'DropPrimaryKeyIS
  DropUniqueISId :: DropConstraintInsSetId 'DropUniqueIS
  DropCheckISId :: DropConstraintInsSetId 'DropCheckIS
  DropForeignKeyISId :: DropConstraintInsSetId 'DropForeignKeyIS

deriving instance Show (DropConstraintInsSetId is)
deriving instance Eq (DropConstraintInsSetId is)
deriving instance Ord (DropConstraintInsSetId is)

deriving instance Show (AlterTableInsSetId is)
deriving instance Eq (AlterTableInsSetId is)
deriving instance Ord (AlterTableInsSetId is)

data AlterTypeInsSetId :: AlterTypeInsSetK -> Type where
  RenameTypeISId :: Oid ('AttrOid 'TypeOid) -> AlterTypeInsSetId 'RenameTypeIS
  AddAttributeISId :: Oid ('AttrOid 'TypeOid) -> AlterTypeInsSetId 'AddAttributeIS
  AlterAttributeISId :: Oid ('AttrOid 'TypeOid) -> AlterAttributeInsSetId alterAttrIS -> AlterTypeInsSetId ('AlterAttributeIS alterAttrIS)
  AddEnumValISId :: Oid ('AttrOid 'TypeOid) -> AlterTypeInsSetId 'AddEnumValIS
  AddAfterEnumValISId :: Oid ('AttrOid 'TypeOid) -> AlterTypeInsSetId 'AddAfterEnumValIS
  AddBeforeEnumValISId :: Oid ('AttrOid 'TypeOid) -> AlterTypeInsSetId 'AddBeforeEnumValIS
  DropAttributeISId :: Oid ('AttrOid 'TypeOid) -> AlterTypeInsSetId 'DropAttributeIS

data AlterAttributeInsSetId :: AlterAttributeInsSetK -> Type where
  ChangeAttrTypeISId :: AlterAttributeInsSetId 'ChangeAttrTypeIS

deriving instance Show (AlterTypeInsSetId is)
deriving instance Eq (AlterTypeInsSetId is)
deriving instance Ord (AlterTypeInsSetId is)

deriving instance Show (AlterAttributeInsSetId is)
deriving instance Eq (AlterAttributeInsSetId is)
deriving instance Ord (AlterAttributeInsSetId is)

data AlterSeqInsSetId :: AlterSeqInsSetK -> Type where
  AddOwnerISId :: AlterSeqInsSetId 'AddOwnerIS

deriving instance Show (AlterSeqInsSetId is)
deriving instance Eq (AlterSeqInsSetId is)
deriving instance Ord (AlterSeqInsSetId is)

data SomeDDLInsSetId where
  SomeDDLInsSetId :: DDLInsSetId is -> SomeDDLInsSetId

deriving instance Show SomeDDLInsSetId

mkRootNode :: SomeDDLInsSetId -> (BaseLinePrimDDL, SomeDDLInsSetId, [SomeDDLInsSetId])
mkRootNode root = (NoOp, root, [])

mkSomeRootISId :: forall sc oidk. (Schema sc) => Proxy sc -> Oid oidk -> SomeDDLInsSetId
mkSomeRootISId _ oid = SomeDDLInsSetId $ RootISId oid (_getSchemaName $ schemaName @sc)

instance Eq SomeDDLInsSetId where
  (==) (SomeDDLInsSetId (RootISId oid1 sn1)) = \case
    (SomeDDLInsSetId (RootISId oid2 sn2)) -> (SomeOid oid1 == SomeOid oid2) && (sn1 == sn2)
    _ -> False
  (==) (SomeDDLInsSetId isid1@(CreateTypeISId {})) = \case
    (SomeDDLInsSetId isid2@(CreateTypeISId {})) -> isid1 == isid2
    _ -> False
  (==) (SomeDDLInsSetId isid1@(CreateEnumISId {})) = \case
    (SomeDDLInsSetId isid2@(CreateEnumISId {})) -> isid1 == isid2
    _ -> False
  (==) (SomeDDLInsSetId isid1@(CreateSeqISId {})) = \case
    (SomeDDLInsSetId isid2@(CreateSeqISId {})) -> isid1 == isid2
    _ -> False
  (==) (SomeDDLInsSetId isid1@(CreateTableISId {})) = \case
    (SomeDDLInsSetId isid2@(CreateTableISId {})) -> isid1 == isid2
    _ -> False
  (==) (SomeDDLInsSetId (AlterTypeISId oid1 altTy1)) = \case
    (SomeDDLInsSetId (AlterTypeISId oid2 altTy2)) -> (SomeOid oid1 == SomeOid oid2) && (fromSing (SingAltTyId altTy1) == fromSing (SingAltTyId altTy2))
    _ -> False
  (==) (SomeDDLInsSetId (AlterSeqISId altSeq1)) = \case
    (SomeDDLInsSetId (AlterSeqISId altSeq2)) -> (fromSing (SingAltSeqId altSeq1) == fromSing (SingAltSeqId altSeq2))
    _ -> False
  (==) (SomeDDLInsSetId (AlterTableISId oid1 altTab1)) = \case
    (SomeDDLInsSetId (AlterTableISId oid2 altTab2)) -> (SomeOid oid1 == SomeOid oid2) && (fromSing (SingAltTabId altTab1) == fromSing (SingAltTabId altTab2))
    _ -> False
  (==) (SomeDDLInsSetId isid1@(DropTableISId {})) = \case
    (SomeDDLInsSetId isid2@(DropTableISId {})) -> isid1 == isid2
    _ -> False
  (==) (SomeDDLInsSetId isid1@(DropTypeISId {})) = \case
    (SomeDDLInsSetId isid2@(DropTypeISId {})) -> isid1 == isid2
    _ -> False
  (==) (SomeDDLInsSetId isid1@(DropSeqISId {})) = \case
    (SomeDDLInsSetId isid2@(DropSeqISId {})) -> isid1 == isid2
    _ -> False

instance Ord SomeDDLInsSetId where
  compare (SomeDDLInsSetId isid1@(RootISId oid1 sn1)) = \case
    SomeDDLInsSetId (RootISId oid2 sn2) -> (sn1 `compare` sn2) `compare` (SomeOid oid1 `compare` SomeOid oid2)
    SomeDDLInsSetId isid2 -> fromSing (SingIS isid1) `compare` fromSing (SingIS isid2)
  compare (SomeDDLInsSetId isid1@(CreateTypeISId {})) = \case
    (SomeDDLInsSetId isid2@(CreateTypeISId {})) -> isid1 `compare` isid2
    SomeDDLInsSetId isid2 -> fromSing (SingIS isid1) `compare` fromSing (SingIS isid2)
  compare (SomeDDLInsSetId isid1@(CreateEnumISId {})) = \case
    (SomeDDLInsSetId isid2@(CreateEnumISId {})) -> isid1 `compare` isid2
    SomeDDLInsSetId isid2 -> fromSing (SingIS isid1) `compare` fromSing (SingIS isid2)
  compare (SomeDDLInsSetId isid1@(CreateSeqISId {})) = \case
    (SomeDDLInsSetId isid2@(CreateSeqISId {})) -> isid1 `compare` isid2
    SomeDDLInsSetId isid2 -> fromSing (SingIS isid1) `compare` fromSing (SingIS isid2)
  compare (SomeDDLInsSetId isid1@(CreateTableISId {})) = \case
    (SomeDDLInsSetId isid2@(CreateTableISId {})) -> isid1 `compare` isid2
    SomeDDLInsSetId isid2 -> fromSing (SingIS isid1) `compare` fromSing (SingIS isid2)
  compare (SomeDDLInsSetId isid1@(AlterTypeISId oid1 altTy1)) = \case
    (SomeDDLInsSetId (AlterTypeISId oid2 altTy2)) -> (SomeOid oid1 `compare` SomeOid oid2) `compare` (fromSing (SingAltTyId altTy1) `compare` fromSing (SingAltTyId altTy2))
    SomeDDLInsSetId isid2 -> fromSing (SingIS isid1) `compare` fromSing (SingIS isid2)
  compare (SomeDDLInsSetId isid1@(AlterSeqISId altSeq1)) = \case
    (SomeDDLInsSetId (AlterSeqISId altSeq2)) -> (fromSing (SingAltSeqId altSeq1) `compare` fromSing (SingAltSeqId altSeq2))
    SomeDDLInsSetId isid2 -> fromSing (SingIS isid1) `compare` fromSing (SingIS isid2)
  compare (SomeDDLInsSetId isid1@(AlterTableISId oid1 altTab1)) = \case
    (SomeDDLInsSetId (AlterTableISId oid2 altTab2)) -> (SomeOid oid1 `compare` SomeOid oid2) `compare` (fromSing (SingAltTabId altTab1) `compare` fromSing (SingAltTabId altTab2))
    SomeDDLInsSetId isid2 -> fromSing (SingIS isid1) `compare` fromSing (SingIS isid2)
  compare (SomeDDLInsSetId isid1@(DropTableISId {})) = \case
    (SomeDDLInsSetId isid2@(DropTableISId {})) -> isid1 `compare` isid2
    SomeDDLInsSetId isid2 -> fromSing (SingIS isid1) `compare` fromSing (SingIS isid2)
  compare (SomeDDLInsSetId isid1@(DropTypeISId {})) = \case
    (SomeDDLInsSetId isid2@(DropTypeISId {})) -> isid1 `compare` isid2
    SomeDDLInsSetId isid2 -> fromSing (SingIS isid1) `compare` fromSing (SingIS isid2)
  compare (SomeDDLInsSetId isid1@(DropSeqISId {})) = \case
    (SomeDDLInsSetId isid2@(DropSeqISId {})) -> isid1 `compare` isid2
    SomeDDLInsSetId isid2 -> fromSing (SingIS isid1) `compare` fromSing (SingIS isid2)

newtype instance Sing (is :: InsSetK) where
  SingIS :: DDLInsSetId is -> Sing is

newtype instance Sing (altTyk :: AlterTypeInsSetK) where
  SingAltTyId :: AlterTypeInsSetId altTyk -> Sing altTyk

newtype instance Sing (altTabk :: AlterTableInsSetK) where
  SingAltTabId :: AlterTableInsSetId altTabk -> Sing altTabk

newtype instance Sing (altSeqk :: AlterSeqInsSetK) where
  SingAltSeqId :: AlterSeqInsSetId altSeqk -> Sing altSeqk

newtype instance Sing (altAttrk :: AlterAttributeInsSetK) where
  SingAltAttributeId :: AlterAttributeInsSetId altAttrk -> Sing altAttrk

newtype instance Sing (altColk :: AlterColumnInsSetK) where
  SingAltColumnId :: AlterColumnInsSetId altColk -> Sing altColk

newtype instance Sing (addColk :: AddConstraintInsSetK) where
  SingAddConstId :: AddConstraintInsSetId addColk -> Sing addColk

newtype instance Sing (dropColk :: DropConstraintInsSetK) where
  SingDropConstId :: DropConstraintInsSetId dropColk -> Sing dropColk


newtype instance Sing (oidk :: OidK) where
  SingOid :: Oid oidk -> Sing oidk

instance SingE (is :: InsSetK) where
  type Demote is = InsSetK
  fromSing = \case
    (SingIS (RootISId oid _)) -> RootIS (fromSing $ SingOid oid)
    (SingIS (CreateTypeISId {})) -> CreateTypeIS
    (SingIS (CreateEnumISId {})) -> CreateEnumIS
    (SingIS (CreateSeqISId {})) -> CreateSeqIS
    (SingIS (CreateTableISId {})) -> CreateTableIS

    (SingIS (AlterTypeISId _ altTy)) -> AlterTypeIS $ fromSing $ SingAltTyId altTy
    (SingIS (AlterSeqISId altSeq)) -> AlterSeqIS $ fromSing $ SingAltSeqId altSeq
    (SingIS (AlterTableISId _ altTab)) -> AlterTableIS $ fromSing $ SingAltTabId altTab

    (SingIS (DropTableISId {})) -> DropTableIS
    (SingIS (DropTypeISId {})) -> DropTypeIS
    (SingIS (DropSeqISId {})) -> DropSeqIS

instance SingE (is :: AlterTypeInsSetK) where
  type Demote is = AlterTypeInsSetK
  fromSing = \case
    SingAltTyId (RenameTypeISId {}) -> RenameTypeIS
    SingAltTyId (AddAttributeISId {}) -> AddAttributeIS
    SingAltTyId (AlterAttributeISId _ altAttr) -> AlterAttributeIS $ fromSing $ SingAltAttributeId altAttr
    SingAltTyId (AddEnumValISId {}) -> AddEnumValIS
    SingAltTyId (AddAfterEnumValISId {}) -> AddAfterEnumValIS
    SingAltTyId (AddBeforeEnumValISId {}) -> AddBeforeEnumValIS
    SingAltTyId (DropAttributeISId {}) -> DropAttributeIS

instance SingE (is :: AlterAttributeInsSetK) where
  type Demote is = AlterAttributeInsSetK
  fromSing = \case
    SingAltAttributeId (ChangeAttrTypeISId {}) -> ChangeAttrTypeIS

instance SingE (is :: AlterTableInsSetK) where
  type Demote is = AlterTableInsSetK
  fromSing = \case
    SingAltTabId (AddColumnISId {}) -> AddColumnIS
    SingAltTabId (RenameColumnISId {}) -> RenameColumnIS
    SingAltTabId (AlterColumnISId _ altCol) -> AlterColumnIS $ fromSing $ SingAltColumnId altCol
    SingAltTabId (RenameTableISId {}) -> RenameTableIS
    SingAltTabId (AddConstraintISId addConst) -> AddConstraintIS $ fromSing $ SingAddConstId addConst
    SingAltTabId (DropConstraintISId dropConst) -> DropConstraintIS $ fromSing $ SingDropConstId dropConst
    SingAltTabId (DropColumnISId {}) -> DropColumnIS

instance SingE (is :: AlterColumnInsSetK) where
  type Demote is = AlterColumnInsSetK
  fromSing = \case
    SingAltColumnId (SetNotNullISId {}) -> SetNotNullIS
    SingAltColumnId (DropNotNullISId {}) -> DropNotNullIS
    SingAltColumnId (ChangeTypeISId {}) -> ChangeTypeIS
    SingAltColumnId (AddDefaultISId {}) -> AddDefaultIS
    SingAltColumnId (DropDefaultISId {}) -> DropDefaultIS

instance SingE (is :: AddConstraintInsSetK) where
  type Demote is = AddConstraintInsSetK
  fromSing = \case
    SingAddConstId (AddPrimaryKeyISId {}) -> AddPrimaryKeyIS
    SingAddConstId (AddUniqueISId {}) -> AddUniqueIS
    SingAddConstId (AddCheckISId {}) -> AddCheckIS
    SingAddConstId (AddForeignKeyISId {}) -> AddForeignKeyIS

instance SingE (is :: DropConstraintInsSetK) where
  type Demote is = DropConstraintInsSetK
  fromSing = \case
    SingDropConstId (DropPrimaryKeyISId {}) -> DropPrimaryKeyIS
    SingDropConstId (DropUniqueISId {}) -> DropUniqueIS
    SingDropConstId (DropCheckISId {}) -> DropCheckIS
    SingDropConstId (DropForeignKeyISId {}) -> DropForeignKeyIS

instance SingE (is :: AlterSeqInsSetK) where
  type Demote is = AlterSeqInsSetK
  fromSing = \case
    SingAltSeqId (AddOwnerISId {}) -> AddOwnerIS

instance SingE (oidk :: OidK) where
  type Demote oidk = OidK
  fromSing = \case
    SingOid (TableOid_ {}) -> TableOid
    SingOid (TypeOid_ {}) -> TypeOid
    SingOid (SchemaOid_ {}) -> SchemaOid
    SingOid (OwnerOid_ {}) -> OwnerOid
    SingOid (AttrOid_ own _ _) -> AttrOid (fromSing (SingOid own))
    SingOid (EnumOid_ {}) -> EnumOid

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
  | NoOp


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
