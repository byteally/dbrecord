{-# LANGUAGE OverloadedStrings #-}
module DBRecord.Internal.Migration.Pretty where

import DBRecord.Internal.Migration.Types
import DBRecord.Internal.Postgres (ppPGExpr)
import Data.Text (Text, unpack, pack)
import qualified Text.PrettyPrint.HughesPJ as Pretty
import Text.PrettyPrint.HughesPJ (Doc, (<+>), text, 
                                  parens, comma, punctuate,
                                  hsep, semi, render, char,
                                  (<>))

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

ppColumnName :: ColName -> Doc
ppColumnName (ColName colN) = doubleQuotes colN

ppTableName :: TabName -> Doc
ppTableName (TabName tabN) = doubleQuotes tabN

ppTypeName :: TypeName -> Doc
ppTypeName (TypeName typeN) = text_ typeN

ppColumnType :: ColType -> Doc
ppColumnType (ColType tn) = ppTypeName tn

ppCheckExpr :: CheckExpr -> Doc
ppCheckExpr (CheckExpr e) = parens (ppPGExpr e)

ppDefaultExpr :: DefExpr -> Doc
ppDefaultExpr (DefExpr e) = parens (ppPGExpr e)

ppEnumVal :: EnumVal -> Doc
ppEnumVal (EnumVal e) = quotes e

ppSeqName :: SeqName -> Doc
ppSeqName (SeqName seqN) = doubleQuotes seqN

ppColumn :: Column -> Doc
ppColumn (Column name ty) =
      ppColumnName name
  <+> ppColumnType ty

ppConstraintName :: ConstraintName -> Doc
ppConstraintName (ConstraintName c) = doubleQuotes c

ppMigration :: Migration -> Doc
ppMigration (CreateTable tab cols) =
      text "CREATE TABLE"
  <+> ppTableName tab
  <+> parens (hsep (punctuate comma (map ppColumn cols)))
  <+> semi
ppMigration (CreateType ty cols) =
      text "CREATE TYPE"
  <+> ppTypeName ty
  <+> text "AS"
  <+> parens (hsep (punctuate comma (map ppColumn cols)))
  <+> semi
ppMigration (CreateSeq seqN) =
      text "CREATE SEQUENCE"
  <+> ppSeqName seqN
  <+> semi  
ppMigration (CreateEnum ty cols) =
      text "CREATE TYPE"
  <+> ppTypeName ty
  <+> text "AS ENUM"
  <+> parens (hsep (punctuate comma (map ppEnumVal cols)))
  <+> semi
ppMigration (DropTable tab) =
      text "DROP TABLE"
  <+> ppTableName tab
  <+> semi
ppMigration (DropType ty) =
      text "DROP TYPE"
  <+> ppTypeName ty
  <+> semi
ppMigration (AlterTable tab alter) =
      text "ALTER TABLE"
  <+> ppTableName tab
  <+> ppAlterTable alter
  <+> semi
ppMigration (AlterType typ alter) =
      text "ALTER TYPE"
  <+> ppTypeName typ
  <+> ppAlterType alter
  <+> semi  
ppMigration (AlterSeq seqN alter) =
      text "ALTER SEQUENCE"
  <+> ppSeqName seqN
  <+> ppAlterSeqType alter
  <+> semi  

ppAlterSeqType :: AlterSeq -> Doc
ppAlterSeqType (AddOwner tabn coln) =
      text "OWNED BY"
  <+> ppTableName tabn
  <>  char '.'
  <>  ppColumnName coln
  
ppAlterTable :: AlterTable -> Doc
ppAlterTable (AddColumn coln) =
      text "ADD COLUMN"
  <+> ppColumn coln
ppAlterTable (DropColumn coln) =
      text "DROP COLUMN"
  <+> ppColumnName coln
ppAlterTable (RenameColumn oldn newn) =
      text "RENAME COLUMN"
  <+> ppColumnName oldn
  <+> text "TO"
  <+> ppColumnName newn
ppAlterTable (AlterColumn coln alter) =
      text "ALTER COLUMN"
  <+> ppColumnName coln
  <+> ppAlterColumn alter
ppAlterTable (RenameTable newn) =
      text "RENAME TO"
  <+> ppTableName newn
ppAlterTable (AddConstraint cname con) =
      text "ADD CONSTRAINT"
  <+> ppConstraintName cname
  <+> ppAddConstraint con
ppAlterTable (DropConstraint cname) =
      text "DROP CONSTRAINT"
  <+> ppConstraintName cname

ppAddConstraint :: AddConstraint -> Doc
ppAddConstraint (AddPrimaryKey cols) =
      text "PRIMARY KEY"
  <+> parens (hsep (punctuate comma (map ppColumnName cols)))
ppAddConstraint (AddUnique cols) =
      text "UNIQUE"
  <+> parens (hsep (punctuate comma (map ppColumnName cols)))
ppAddConstraint (AddCheck chkExpr) =
      text "CHECK"
  <+> ppCheckExpr chkExpr
ppAddConstraint (AddForeignKey fcols rtab rcols) =
      text "FOREIGN KEY"
  <+> parens (hsep (punctuate comma (map ppColumnName fcols)))
  <+> text "REFERENCES"
  <+> ppTableName rtab
  <+> parens (hsep (punctuate comma (map ppColumnName rcols)))

ppAlterColumn :: AlterColumn -> Doc
ppAlterColumn SetNotNull  = text "SET NOT NULL"
ppAlterColumn DropNotNull = text "DROP NOT NULL"
ppAlterColumn (ChangeType ctype) =
      text "TYPE"
  <+> ppColumnType ctype
ppAlterColumn (AddDefault defV) =
      text "SET DEFAULT"
  <+> ppDefaultExpr defV
ppAlterColumn DropDefault =
     text "DROP DEFAULT"

ppAlterType :: AlterType -> Doc
ppAlterType (RenameType typ) =
      text "RENAME TO"
  <+> ppTypeName typ
  <+> semi
ppAlterType (AddAttribute col) =
      text "ADD ATTRIBUTE"
  <+> ppColumn col
  <+> semi
ppAlterType (DropAttribute col) =
      text "DROP ATTRIBUTE"
  <+> ppColumnName col
  <+> semi
ppAlterType (AlterAttribute col altAttr) =
      text "ALTER ATTRIBUTE"
  <+> ppColumnName col
  <+> ppAlterAttr altAttr
  <+> semi
ppAlterType (AddAfterEnumVal newEnum prevEnum) =
      text "ADD VALUE"
  <+> ppEnumVal newEnum
  <+> text "AFTER"
  <+> ppEnumVal prevEnum

ppAlterAttr :: AlterAttribute -> Doc
ppAlterAttr (ChangeAttrType ty) =
     text "SET DATA TYPE"
  <+> ppColumnType ty  

renderMig :: Migration -> String
renderMig = render . ppMigration
