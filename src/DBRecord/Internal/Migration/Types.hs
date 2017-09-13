module DBRecord.Internal.Migration.Types where

import qualified Data.Text as T

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
newtype CheckExpr = CheckExpr T.Text
                  deriving Show
newtype DefExpr = DefExpr T.Text
                deriving Show
newtype EnumVal = EnumVal T.Text
                deriving Show
newtype ConstraintName = ConstraintName T.Text
                deriving Show

data Migration
  = CreateTable TabName [Column]
  | CreateType TypeName [Column]
  | CreateEnum TypeName [EnumVal]
  | DropTable TabName
  | DropType TypeName
  | AlterTable TabName AlterTable
  | AlterType TypeName AlterType
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
