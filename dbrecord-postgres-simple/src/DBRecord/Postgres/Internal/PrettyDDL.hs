{-# LANGUAGE OverloadedStrings #-}
module DBRecord.Postgres.Internal.PrettyDDL where

import DBRecord.Internal.DDL
import Data.Text (Text, unpack, pack)
import qualified Prettyprinter as PP
import Data.Monoid ( (<>) )
import Prettyprinter ( (<+>) , squotes
                     , parens, comma, punctuate
                     , hsep, semi, dquotes, pretty
                     )

import Prelude hiding ((<>))
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Postgres.Internal.Sql.Pretty
import DBRecord.Internal.Sql.SqlGen
import Data.Functor.Identity

type Doc = PP.Doc ()

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

ppColumnName :: ColName -> Doc
ppColumnName (ColName colN) = dquotes (pretty colN)

ppTableName :: PQ.TableId -> Doc
ppTableName tabId =
  dquotes (pretty (PQ.schema tabId))
  <> pretty '.'
  <> dquotes (pretty (PQ.tableName tabId))

ppColumnType :: ColType -> Doc
ppColumnType (ColType tn) = ppPGType tn

ppCheckExpr :: CheckExpr -> Doc
ppCheckExpr (CheckExpr e) = parens (ppExpr (genSqlExpr e))

ppDefaultExpr :: DefExpr -> Doc
ppDefaultExpr (DefExpr e) = parens (ppExpr (genSqlExpr e))

ppEnumVal :: EnumVal -> Doc
ppEnumVal (EnumVal e) = squotes (pretty e)

ppSeqName :: SeqName -> Doc
ppSeqName (SeqName seqN) = pretty seqN

ppColumn :: Column -> Doc
ppColumn (Column name ty) =
      ppColumnName name
  <+> ppColumnType ty

ppConstraintName :: ConstraintName -> Doc
ppConstraintName (ConstraintName c) = dquotes (pretty c)

ppPrimDDL :: PrimDDL -> Doc
ppPrimDDL (CreateTable tab (Identity cols)) =
      "CREATE TABLE"
  <+> ppTableName tab
  <+> parens (hsep (punctuate comma (map ppColumn cols)))
  <+> semi
ppPrimDDL (CreateType ty (Identity cols)) =
      "CREATE TYPE"
  <+> ppDBTypeName ty
  <+> "AS"
  <+> parens (hsep (punctuate comma (map ppColumn cols)))
  <+> semi
ppPrimDDL (CreateSeq seqN) =
      "CREATE SEQUENCE"
  <+> ppSeqName seqN
  <+> semi
ppPrimDDL (DropSeq seqN) =
      "DROP SEQUENCE"
  <+> ppSeqName seqN
  <+> semi    
ppPrimDDL (CreateEnum ty (Identity cols)) =
      "CREATE TYPE"
  <+> ppDBTypeName ty
  <+> "AS ENUM"
  <+> parens (hsep (punctuate comma (map ppEnumVal cols)))
  <+> semi
ppPrimDDL (DropTable tab) =
      "DROP TABLE"
  <+> ppTableName tab
  <+> semi
ppPrimDDL (DropType ty) =
      "DROP TYPE"
  <+> ppDBTypeName ty
  <+> semi
ppPrimDDL (AlterTable tab alter) =
      "ALTER TABLE"
  <+> ppTableName tab
  <+> ppAlterTable alter
  <+> semi
ppPrimDDL (AlterType typ alter) =
      "ALTER TYPE"
  <+> ppDBTypeName typ
  <+> ppAlterType alter
  <+> semi  
ppPrimDDL (AlterSeq seqN alter) =
      "ALTER SEQUENCE"
  <+> ppSeqName seqN
  <+> ppAlterSeqType alter
  <+> semi
ppPrimDDL NoOp = mempty  

ppAlterSeqType :: AlterSeq -> Doc
ppAlterSeqType (AddOwner tabn coln) =
      "OWNED BY"
  <+> ppTableName tabn
  <>  pretty '.'
  <>  ppColumnName coln
  
ppAlterTable :: AlterTable -> Doc
ppAlterTable (AddColumn coln) =
      "ADD COLUMN"
  <+> ppColumn coln
ppAlterTable (DropColumn coln) =
      "DROP COLUMN"
  <+> ppColumnName coln
ppAlterTable (RenameColumn oldn newn) =
      "RENAME COLUMN"
  <+> ppColumnName oldn
  <+> "TO"
  <+> ppColumnName newn
ppAlterTable (AlterColumn coln alter) =
      "ALTER COLUMN"
  <+> ppColumnName coln
  <+> ppAlterColumn alter
ppAlterTable (RenameTable newn) =
      "RENAME TO"
  <+> ppTableName newn
ppAlterTable (AddConstraint cname con) =
      "ADD CONSTRAINT"
  <+> ppConstraintName cname
  <+> ppAddConstraint con
ppAlterTable (DropConstraint (DropPrimaryKey cname)) =
      "DROP CONSTRAINT"
  <+> ppConstraintName cname
ppAlterTable (DropConstraint (DropUnique cname)) =
      "DROP CONSTRAINT"
  <+> ppConstraintName cname
ppAlterTable (DropConstraint (DropCheck cname)) =
      "DROP CONSTRAINT"
  <+> ppConstraintName cname
ppAlterTable (DropConstraint (DropForeignKey cname)) =
      "DROP CONSTRAINT"
  <+> ppConstraintName cname

ppAddConstraint :: AddConstraint -> Doc
ppAddConstraint (AddPrimaryKey cols) =
      "PRIMARY KEY"
  <+> parens (hsep (punctuate comma (map ppColumnName cols)))
ppAddConstraint (AddUnique cols) =
      "UNIQUE"
  <+> parens (hsep (punctuate comma (map ppColumnName cols)))
ppAddConstraint (AddCheck chkExpr) =
      "CHECK"
  <+> ppCheckExpr chkExpr
ppAddConstraint (AddForeignKey fcols rtab rcols) =
      "FOREIGN KEY"
  <+> parens (hsep (punctuate comma (map ppColumnName fcols)))
  <+> "REFERENCES"
  <+> ppTableName rtab
  <+> parens (hsep (punctuate comma (map ppColumnName rcols)))

ppAlterColumn :: AlterColumn -> Doc
ppAlterColumn SetNotNull  = "SET NOT NULL"
ppAlterColumn DropNotNull = "DROP NOT NULL"
ppAlterColumn (ChangeType ctype) =
      "TYPE"
  <+> ppColumnType ctype
ppAlterColumn (AddDefault defV) =
      "SET DEFAULT"
  <+> ppDefaultExpr defV
ppAlterColumn DropDefault =
     "DROP DEFAULT"

ppAlterType :: AlterType -> Doc
ppAlterType (RenameType typ) =
      "RENAME TO"
  <+> ppDBTypeName typ
  <+> semi
ppAlterType (AddAttribute col) =
      "ADD ATTRIBUTE"
  <+> ppColumn col
  <+> semi
ppAlterType (DropAttribute col) =
      "DROP ATTRIBUTE"
  <+> ppColumnName col
  <+> semi
ppAlterType (AlterAttribute col altAttr) =
      "ALTER ATTRIBUTE"
  <+> ppColumnName col
  <+> ppAlterAttr altAttr
  <+> semi
ppAlterType (AddAfterEnumVal newEnum prevEnum) =
      "ADD VALUE"
  <+> ppEnumVal newEnum
  <+> "AFTER"
  <+> ppEnumVal prevEnum
ppAlterType e =
  error ("Panic: not implemented @ppAlterType: " ++ show e)

ppAlterAttr :: AlterAttribute -> Doc
ppAlterAttr (ChangeAttrType ty) =
     "SET DATA TYPE"
  <+> ppColumnType ty  

renderDDL :: PrimDDL -> Text
renderDDL = render . ppPrimDDL

{-
renderChangeSets :: [ChangeSet] -> String
renderChangeSets =
  unlines . map renderChangeSet 

renderChangeSet :: ChangeSet -> String
renderChangeSet =
  unlines . map renderDDL . statements
-}
