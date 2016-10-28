{-# LANGUAGE OverloadedStrings #-}
module DBRecord.Internal.Migration.Pretty where

import DBRecord.Internal.Migration.Types
import DBRecord.Internal.Schema (Column (..))

import Data.Text (Text, unpack, pack)
import qualified Text.PrettyPrint.HughesPJ as Pretty
import Text.PrettyPrint.HughesPJ (Doc, (<+>), text, 
                                  parens, comma, punctuate,
                                  hsep, semi, render)

escQuote :: Text -> Text
escQuote = escapeBy (Just '\'')

escDoubleQuote :: Text -> Text
escDoubleQuote = escapeBy (Just '"')

escapeBy :: Maybe Char -> Text -> Text
escapeBy esc s = pack $ go esc (unpack s)
  where
    go Nothing s'           = s'
    go (Just _) ""          = ""
    go (Just esch) (ch':xs)
      | ch' == esch          = esch : ch': go esc xs
    go esc' (x:xs)          = x : go esc' xs

text_ :: Text -> Doc
text_ = text . unpack

quotes :: Text -> Doc
quotes = Pretty.quotes . text_ . escQuote

doubleQuotes :: Text -> Doc
doubleQuotes = Pretty.doubleQuotes . text_ . escDoubleQuote

ppMigration :: Migration -> Doc
ppMigration (CreateTable tab cols) =
      text "CREATE TABLE"
  <+> doubleQuotes tab
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
  <+> parens (hsep (punctuate comma (map quotes cols)))
  <+> semi
ppMigration (DropTable tab) =
      text "DROP TABLE"
  <+> doubleQuotes tab
  <+> semi
ppMigration (DropType ty) =
      text "DROP TYPE"
  <+> text_ ty
  <+> semi
ppMigration (AlterTable tab alter) =
      text "ALTER TABLE"
  <+> doubleQuotes tab
  <+> ppAlterTable alter
  <+> semi
ppMigration (AlterType typ alter) =
      text "ALTER TYPE"
  <+> doubleQuotes typ
  <+> ppAlterType alter
  <+> semi  

ppColumn :: Column -> Doc
ppColumn (Column name ty) =
      doubleQuotes name
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
  <+> doubleQuotes rtab
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
ppAlterType (RenameType typ) =
      text "RENAME TO"
  <+> doubleQuotes typ
  <+> semi
ppAlterType (AddAttribute col) =
      text "ADD ATTRIBUTE"
  <+> ppColumn col
  <+> semi
ppAlterType (DropAttribute col) =
      text "DROP ATTRIBUTE"
  <+> doubleQuotes col
  <+> semi
ppAlterType (AlterAttribute col altAttr) =
      text "ALTER ATTRIBUTE"
  <+> doubleQuotes col
  <+> ppAlterAttr altAttr
  <+> semi
ppAlterType (AddAfterEnumVal newEnum prevEnum) =
      text "ADD VALUE"
  <+> text_ newEnum
  <+> text "AFTER"
  <+> text_ prevEnum

ppAlterAttr :: AlterAttribute -> Doc
ppAlterAttr (ChangeAttrType ty) =
     text "SET DATA TYPE"
  <+> text_ ty   

renderMig :: Migration -> String
renderMig = render . ppMigration
