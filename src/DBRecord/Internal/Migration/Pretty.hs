{-# LANGUAGE OverloadedStrings #-}
module DBRecord.Internal.Migration.Pretty where

import DBRecord.Internal.Migration.Types
import DBRecord.Internal.Schema (Column (..))

import Data.Foldable (toList)
import Data.Monoid ((<>))
import Data.List (intersperse)
import Data.Text (Text, unpack)
import Text.PrettyPrint.HughesPJ (Doc, ($$), (<+>), text, empty,
                                  parens, comma, punctuate, quotes,
                                  hcat, vcat, brackets, doubleQuotes,
                                  hsep, equals, char, semi, render)

text_ :: Text -> Doc
text_ = text . unpack

ppMigration :: Migration -> Doc
ppMigration (CreateTable tab cols) =
      text "CREATE TABLE"
  <+> doubleQuotes (text_ tab)
  <+> parens (hsep (punctuate comma (map ppColumn cols)))
  <+> semi
ppMigration (CreateType ty cols) =
      text "CREATE TYPE"
  <+> text_ ty
  <+> text "AS"
  <+> parens (hsep (punctuate comma (map ppColumn cols)))
  <+> semi
ppMigration (CreateEnum ty cols) =
      text "CREATE TYPE"
  <+> text_ ty
  <+> text "AS ENUM"
  <+> parens (hsep (punctuate comma (map (quotes . text_) cols)))
  <+> semi
ppMigration (DropTable tab) =
      text "DROP TABLE"
  <+> doubleQuotes (text_ tab)
  <+> semi
ppMigration (DropType ty) =
      text "DROP TYPE"
  <+> text_ ty
  <+> semi
ppMigration (AlterTable tab alter) =
      text "ALTER TABLE"
  <+> doubleQuotes (text_ tab)
  <+> ppAlterTable alter
  <+> semi

ppColumn :: Column -> Doc
ppColumn (Column name ty) =
      doubleQuotes (text_ name)
  <+> text_ ty 

ppAlterTable :: AlterTable -> Doc
ppAlterTable (AddColumn coln) =
      text "ADD COLUMN"
  <+> ppColumn coln
ppAlterTable (DropColumn coln) =
      text "DROP COLUMN"
  <+> text_ coln
ppAlterTable (RenameColumn oldn newn) =
      text "RENAME COLUMN"
  <+> text_ oldn
  <+> text "TO"
  <+> text_ newn
ppAlterTable (AlterColumn coln alter) =
      text "ALTER COLUMN"
  <+> text_ coln
  <+> ppAlterColumn alter
ppAlterTable (RenameTable newn) =
      text "RENAME TO"
  <+> text_ newn 
ppAlterTable (AddConstraint cname con) =
      text "ADD CONSTRAINT"
  <+> text_ cname
  <+> ppAddConstraint con
ppAlterTable (DropConstraint cname) =
      text "DROP CONSTRAINT"
  <+> text_ cname

ppAddConstraint :: AddConstraint -> Doc
ppAddConstraint (AddPrimaryKey cols) =
      text "PRIMARY KEY"
  <+> parens (hsep (punctuate comma (map text_ cols)))
ppAddConstraint (AddUnique cols) =
      text "UNIQUE"
  <+> parens (hsep (punctuate comma (map text_ cols)))
ppAddConstraint (AddCheck chkExpr) =
      text "CHECK"
  <+> parens (text_ chkExpr)
ppAddConstraint (AddForeignKey fcols rtab rcols) =
      text "FOREIGN KEY"
  <+> parens (hsep (punctuate comma (map text_ fcols)))
  <+> text "REFERENCES"
  <+> doubleQuotes (text_ rtab)
  <+> parens (hsep (punctuate comma (map text_ rcols)))

ppAlterColumn :: AlterColumn -> Doc
ppAlterColumn SetNotNull  = text "SET NOT NULL"
ppAlterColumn DropNotNull = text "DROP NOT NULL"
ppAlterColumn (ChangeType ctype) =
      text "TYPE"
  <+> text_ ctype
ppAlterColumn (AddDefault defV) =
      text "SET DEFAULT"
  <+> text_ defV
ppAlterColumn DropDefault =
     text "DROP DEFAULT"

ppAlterType :: AlterType -> Doc
ppAlterType (RenameType newTy) =
      text "RENAME TO"
  <+> doubleQuotes (text_ newTy)
ppAlterType (AddAttribute colN) =
      text "ADD ATTRIBUTE"
  <+> ppColumn colN
ppAlterType (DropAttribute cname) =
     text "DROP ATTRIBUTE"
  <+> text_ cname
ppAlterType (AddAfterEnumVal newVal oldVal) =
     text "ADD VALUE"
  <+> text_ newVal
  <+> text "AFTER"
  <+> text_ oldVal
ppAlterType (AlterAttribute cname alter) =
     text "ALTER ATTRIBUTE"
  <+> text_ cname
  <+> ppAlterAttr alter

ppAlterAttr :: AlterAttribute -> Doc
ppAlterAttr (ChangeAttrType ty) =
     text "SET DATA TYPE"
  <+> text_ ty   

renderMig :: Migration -> String
renderMig = render . ppMigration
