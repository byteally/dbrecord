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

addPrimaryKey :: TabName -> ConstraintName -> [ColName] -> PrimDDL
addPrimaryKey tn cn cols =
  AlterTable tn (AddConstraint cn (AddPrimaryKey cols))

addUnique :: TabName -> ConstraintName -> [ColName] -> PrimDDL
addUnique tn cn cols =
  AlterTable tn (AddConstraint cn (AddUnique cols))

addForeignKey :: TabName -> ConstraintName -> [ColName] -> TabName -> [ColName] -> PrimDDL
addForeignKey tn cn cols reft refcols =
  AlterTable tn (AddConstraint cn (AddForeignKey cols reft refcols))

addCheckExpr :: TabName -> ConstraintName -> CheckExpr -> PrimDDL
addCheckExpr tn cn ce =
  AlterTable tn (AddConstraint cn (AddCheck ce))

addDefaultExpr :: TabName -> ColName -> DefExpr -> PrimDDL
addDefaultExpr tn col de =
  AlterTable tn (AlterColumn col (AddDefault de))

addNotNull :: TabName -> ColName -> PrimDDL
addNotNull tn col =
  AlterTable tn (AlterColumn col SetNotNull)

single :: a -> [a]
single a = [a]
