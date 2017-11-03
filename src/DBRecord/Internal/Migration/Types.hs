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
  | DropConstraint ConstraintName
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
  deriving (Show)

data AlterAttribute
  = ChangeAttrType ColType
  deriving (Show)

data TypeAttr
  = SumAttr [(ColName, [Column])]
  | ProdAttr [Column]
  | EnumAttr [ColName]

column :: ColName -> ColType -> Column
column = Column

createTable :: TabName -> [Column] -> PrimDDL
createTable = CreateTable

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

addColumn :: ColName -> ColType -> AlterTable
addColumn cn ct =
  AddColumn (Column cn ct)

changeType :: ColName -> ColType -> AlterTable
changeType cn ct =
  AlterColumn cn (ChangeType ct)

altering :: TabName -> [AlterTable] -> [PrimDDL]
altering = map . AlterTable

single :: a -> [a]
single a = [a]
