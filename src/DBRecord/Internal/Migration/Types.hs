module DBRecord.Internal.Migration.Types where

import qualified Data.Text as T
import qualified DBRecord.Internal.PrimQuery as PQ

newtype TabName  = TabName T.Text
                 deriving Show
newtype TypeName = TypeName T.Text
                 deriving Show
newtype ColName = ColName T.Text
                deriving Show
newtype ColType = ColType TypeName
                deriving Show
data Column = Column ColName ColType
            deriving Show
newtype CheckExpr = CheckExpr PQ.PrimExpr
                  deriving Show
newtype DefExpr = DefExpr PQ.PrimExpr
                deriving Show
newtype EnumVal = EnumVal T.Text
                deriving Show
newtype ConstraintName = ConstraintName T.Text
                deriving Show
newtype SeqName = SeqName T.Text
                deriving Show


data PrimDDL
  = CreateTable TabName [Column]
  | CreateType TypeName [Column]
  | CreateSeq  SeqName
  | DropSeq SeqName
  | CreateEnum TypeName [EnumVal]
  | DropTable TabName
  | DropType TypeName
  | AlterTable TabName AlterTable
  | AlterType TypeName AlterType
  | AlterSeq  SeqName  AlterSeq
  deriving (Show)

data AlterTable
  = AddColumn Column
  | DropColumn ColName
  | RenameColumn ColName ColName
  | AlterColumn ColName AlterColumn
  | RenameTable TabName
  | AddConstraint ConstraintName AddConstraint
  | DropConstraint DropConstraint
  deriving (Show)

data DropConstraint
  = DropPrimaryKey ConstraintName
  | DropUnique     ConstraintName
  | DropCheck      ConstraintName
  | DropForeignKey ConstraintName
  deriving (Show)

data AlterSeq
  = AddOwner TabName ColName
  deriving (Show)

data AddConstraint
  = AddPrimaryKey [ColName]
  | AddUnique [ColName]
  | AddCheck CheckExpr
  | AddForeignKey [ColName] TabName [ColName]
  deriving (Show)

data AlterColumn
  = SetNotNull
  | DropNotNull
  | ChangeType ColType
  | AddDefault DefExpr
  | DropDefault
  deriving (Show)

data AlterType
  = RenameType TypeName
  | AddAttribute Column
  | DropAttribute ColName
  | AlterAttribute ColName AlterAttribute
  | AddAfterEnumVal EnumVal EnumVal
  | AddBeforeEnumVal EnumVal EnumVal
  | AddEnumVal EnumVal
  deriving (Show)

data AlterAttribute
  = ChangeAttrType ColType
  deriving (Show)

data TypeAttr
  = SumAttr [(ColName, [Column])]
  | ProdAttr [Column]
  | EnumAttr [ColName]

alterAddEnumBefore :: TypeName -> EnumVal -> EnumVal -> PrimDDL
alterAddEnumBefore tn eVal eValBef =
  AlterType tn (AddBeforeEnumVal eVal eValBef)

alterAddEnumAfter :: TypeName -> EnumVal -> EnumVal -> PrimDDL
alterAddEnumAfter tn eVal eValAft =
  AlterType tn (AddAfterEnumVal eVal eValAft)

alterAddEnum :: TypeName -> EnumVal -> PrimDDL
alterAddEnum tn eVal =
  AlterType tn (AddEnumVal eVal)

createEnum :: TypeName -> [EnumVal] -> PrimDDL
createEnum tn cons =
  CreateEnum tn cons

dropType :: TypeName -> PrimDDL
dropType =
  DropType

column :: ColName -> ColType -> Column
column =
  Column

createTable :: TabName -> [Column] -> PrimDDL
createTable =
  CreateTable

addPrimaryKey :: ConstraintName -> [ColName] -> AlterTable
addPrimaryKey cn cols =
  AddConstraint cn (AddPrimaryKey cols)

addUnique :: ConstraintName -> [ColName] -> AlterTable
addUnique cn cols =
  AddConstraint cn (AddUnique cols)

addForeignKey :: ConstraintName -> [ColName] -> TabName -> [ColName] -> AlterTable
addForeignKey cn cols reft refcols =
  AddConstraint cn (AddForeignKey cols reft refcols)

addCheck :: ConstraintName -> CheckExpr -> AlterTable
addCheck cn ce =
  AddConstraint cn (AddCheck ce)

addDefault :: ColName -> DefExpr -> AlterTable
addDefault col de =
  AlterColumn col (AddDefault de)

dropDefault :: ColName -> AlterTable
dropDefault col =
  AlterColumn col DropDefault

setNotNull :: ColName -> AlterTable
setNotNull col =
  AlterColumn col SetNotNull

dropNotNull :: ColName -> AlterTable
dropNotNull col =
  AlterColumn col DropNotNull

renameTab :: TabName -> AlterTable
renameTab new =
  RenameTable new

renameColumn :: ColName -> ColName -> AlterTable
renameColumn old new =
  RenameColumn old new

dropColumn :: ColName -> AlterTable
dropColumn cn =
  DropColumn cn

addColumn :: Column -> AlterTable
addColumn =
  AddColumn 

dropPrimaryKey :: ConstraintName -> AlterTable
dropPrimaryKey cn =
  DropConstraint (DropPrimaryKey cn)

dropUnique :: ConstraintName -> AlterTable
dropUnique cn =
  DropConstraint (DropUnique cn)

dropCheck :: ConstraintName -> AlterTable
dropCheck cn =
  DropConstraint (DropCheck cn)

dropForeignKey :: ConstraintName -> AlterTable
dropForeignKey cn =
  DropConstraint (DropForeignKey cn)

changeType :: ColName -> ColType -> AlterTable
changeType cn ct =
  AlterColumn cn (ChangeType ct)

altering :: TabName -> [AlterTable] -> [PrimDDL]
altering = map . AlterTable

createSequence :: SeqName -> PrimDDL
createSequence = CreateSeq

addOwnerToSequence :: TabName -> ColName -> SeqName -> PrimDDL
addOwnerToSequence tn cn sn =
  AlterSeq sn (AddOwner tn cn)

dropSequence :: SeqName -> PrimDDL
dropSequence sn =
  DropSeq sn

dropTable :: TabName -> PrimDDL
dropTable =
  DropTable

single :: a -> [a]
single a = [a]
