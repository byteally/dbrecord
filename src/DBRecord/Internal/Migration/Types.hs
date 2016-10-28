module DBRecord.Internal.Migration.Types where

import DBRecord.Internal.Schema
import qualified Data.Text as T

type TabName  = T.Text
type TypeName = T.Text

type ConstraintName = T.Text
type PrimaryKeys    = [T.Text]

data Migration
  = CreateTable !TabName [Column]
  | CreateType !TypeName [Column]
  | CreateEnum !TypeName [T.Text]
  | DropTable !TabName
  | DropType !TypeName
  | AlterTable !TabName AlterTable
  | AlterType !TypeName AlterType
  deriving (Show)

data AlterTable
  = AddColumn Column
  | DropColumn !ColName
  | RenameColumn !ColName !ColName
  | AlterColumn !ColName AlterColumn
  | RenameTable !TabName
  | AddConstraint !ConstraintName AddConstraint
  | DropConstraint !ConstraintName
  deriving (Show)

data AddConstraint
  = AddPrimaryKey [ColName]
  | AddUnique [ColName]
  | AddCheck CheckExpr
  | AddForeignKey [ColName] !TabName [ColName]
  deriving (Show)

data AlterColumn
  = SetNotNull
  | DropNotNull
  | ChangeType !ColType
  | AddDefault DefExpr
  | DropDefault
  deriving (Show)

data AlterType
  = RenameType !TypeName
  | AddAttribute Column
  | DropAttribute !ColName
  | AlterAttribute !ColName AlterAttribute
  | AddAfterEnumVal !T.Text !T.Text
  deriving (Show)

data AlterAttribute
  = ChangeAttrType !ColType
  deriving (Show)

data TypeAttr
  = SumAttr [(ColName, [Column])]
  | ProdAttr [Column]
  | EnumAttr [ColName]
