{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}

module DBRecord.Migration
       ( mkMigration
       , renderDDL
       , diffMigration
       , BaseTable (..)
       , validateSchemaInfo
       , module DBRecord.Migration
       ) where

import qualified Database.PostgreSQL.Simple  as PGS
import qualified DBRecord.Internal.Migration as M
import qualified DBRecord.Internal.Schema    as S
import Data.Maybe

import DBRecord.Internal.Migration   hiding (Column)
import DBRecord.Internal.Schema      hiding (Column)
import DBRecord.Internal.Types
import DBRecord.Internal.Common
import DBRecord.Internal.DBTypes
import qualified DBRecord.Internal.PrimQuery as PQ
import qualified Data.Text as T
import Data.Proxy
import GHC.Exts
import GHC.TypeLits
import Data.Kind
import Data.Functor.Const
import Data.Coerce (coerce)
import DBRecord.Internal.Migration.Validation (getSchemaInfo)
import Data.Monoid ((<>))

class BaseDatabase (base :: *) where
  type BaseVersion base :: *
  type BaseSchema base :: Symbol
  type BaseSchema base = "public"
  type BaseTables base :: [Symbol]
  type BaseTypes base :: [Symbol]
  type BaseTypes base = '[]

instance BaseDatabase () where
  type BaseVersion () = ()
  type BaseTables () = '[]

class BaseTable (base :: *) (tab :: Symbol) where
  type BaseTableVersion base tab :: *
  type BaseColumns base tab :: [*]
  type BasePrimaryKey base tab :: [Symbol]
  type BaseForeignKey base tab :: [BaseForeignRef]
  type BaseForeignKey base tab = '[]
  type BaseUnique base tab :: [UniqueCT]
  type BaseUnique base tab = '[]
  type BaseDefaultedCols base tab :: [Symbol]
  type BaseDefaultedCols base tab = '[]
  type BaseCheck base tab :: [CheckCT]
  type BaseCheck base tab = '[]

data MigratedEntityK
  = AddedTable Type [Type]
  | DropedTable Type
  | AlteredTable Type
  | RenamedTable  Type

data BaseForeignRef
  = BaseRefBy [Symbol] Symbol [Symbol]
  | BaseRef Symbol Symbol  


class DBMigration (base :: *) (v :: *) where
  type RenameSchema base v :: Maybe Symbol
  type RenameSchema base v = 'Nothing
  
  type CreatedTables base v :: [Symbol]
  type CreatedTables base v = '[]
  
  type DropedTables base v :: [Symbol]
  type DropedTables base v = '[]
  
  type RenamedTables base v :: [(Symbol, Symbol)]
  type RenamedTables base v = '[]
  
  type AlteredTables base v :: [Symbol]
  type AlteredTables base v = '[]

  type CreatedTypes base v :: [Symbol]
  type CreatedTypes base v = '[]
  
  type DropedTypes base v :: [Symbol]
  type DropedTypes base v = '[]
  
  type RenamedTypes base v :: [(Symbol, Symbol)]
  type RenamedTypes base v = '[]
  
  type AlteredTypes base v :: [Symbol]
  type AlteredTypes base v = '[]

class TableMigration (base :: *) (tab :: Symbol) (v :: *) where
  type AddedColumn base tab v :: [*]
  type DropedColumn base tab v :: [Symbol]
  type RenamedColumn base tab v :: [(Symbol, Symbol)]
  type AlteredColumn base tab v :: [*]
  type AddedConstraint base tab v :: [*]
  type DropedConstraint base tab v :: [Symbol]


data DBDiff = DBDiff
data TableDiff
data TypeDiff
data ColumnDiff
data ConstraintDiff


type family DiffDB (db :: *) (bl :: *) (currver :: *) where
  DiffDB db bl currver = DiffDB' db bl currver (BaseVersion bl)

type family DiffDB' (db :: *) (bl :: *) (currver :: *) (basever :: *) where
  DiffDB' db bl currver curver = ()
  
  
toTypeAttr :: HList (Const DConAttr) xs -> TypeAttr
toTypeAttr hlist =
  let consAttrs = recordToList hlist
      isUnary (DConAttr (_cn, [])) = True
      isUnary _                    = False
  in case consAttrs of
    [DConAttr (_cn, cols)]   -> ProdAttr (map toMColumn cols)
    [] -> error "@toTypeAttr: DB Type cannot be of Void type"
    cons | all isUnary cons -> EnumAttr $ fmap (\(DConAttr cattr) -> coerce (fst cattr)) cons
         | otherwise        -> SumAttr $ fmap (\(DConAttr (cn, cns)) -> (coerce cn, map toMColumn cns)) cons

toMColumn :: S.Column -> M.Column
toMColumn (S.Column n t) = M.Column (ColName n) (ColType (coerce t))

mkMigration :: forall db schema tables types.
  ( Database db
  , schema ~ Schema db
  , tables ~ Tables db
  , types  ~ Types db
  , All (Table db) tables
  , TyCxts db types
  , SingI tables
  , SingI types
  ) => Proxy db -> [PrimDDL]
mkMigration pxyDB = mkMigrationTypes pxyDB (sing :: Sing types)
  ++ mkMigrationTables pxyDB (sing :: Sing tables)

mkMigrationTables :: forall db tabs.
                    ( All (Table db) tabs
                    ) => Proxy (db :: *) -> Sing (tabs :: [*]) -> [PrimDDL]
mkMigrationTables _ SNil             = []
mkMigrationTables pxyDB (SCons tab tabs) = mkMigrationTable pxyDB tab ++ mkMigrationTables pxyDB tabs

mkMigrationTable :: forall db tab pks fks chks uqs defs seqs nonNulls colMap.
                   ( Table db tab
                   , pks ~ PrimaryKey db tab
                   , fks ~ ForeignKey db tab
                   , chks ~ Check db tab
                   , uqs ~ Unique db tab
                   , defs ~ HasDefault db tab
                   , seqs ~ TableSequence db tab
                   , colMap ~ (ColumnNames db tab)
                   , nonNulls ~ GetNonNulls (DB db) (OriginalTableFields tab) colMap
                   ) => Proxy (db :: *) -> Sing (tab :: *) -> [PrimDDL]
mkMigrationTable _ _
  = let addPks = case fromSing (sing :: Sing (MapAliasedCol pks colMap)) of
                      [] -> []
                      xs  ->
                        let keyCols = fmap T.pack $ xs
                        in single (addPrimaryKey (coerce tabN) (coerce (genKeyName $ PkNameGen tabN keyCols)) (coerce keyCols))
        addUqs = let addUq fs =
                       let keyCols = fmap T.pack fs
                       in addUnique (coerce tabN) (coerce (genKeyName $ UqNameGen tabN keyCols)) (coerce keyCols)
                 in fmap addUq $ fromSing (sing :: Sing (GetAllUniqs uqs colMap))
        addFks = let addFk (DemotedDBTagFk fcols reft rcols) = addForeignKey (coerce tabN) (coerce (genKeyName $ FkNameGen tabN fcols reft)) (coerce fcols) (coerce reft) (coerce rcols)
                 in fmap addFk  $ fromSing (sing :: Sing (TagEachFks db tab fks))
        (cTabSeqs, defSeqs, oTabSeqs)
                   = let addCreateSeq (DemotedDBTagSeq tab col seqn) = case seqn of
                           Nothing  -> createSeq tab col Nothing
                           (Just n) -> createSeq tab col (Just n)
                         createSeq tab col (Just seqn) = CreateSeq (coerce $ doubleQuoteT (genKeyName $ SeqNameGen tab col (Just seqn)))
                         createSeq tab col Nothing     = CreateSeq (coerce $ doubleQuoteT (genKeyName $ SeqNameGen tab col Nothing))
                         addAlterSeq (DemotedDBTagSeq tab col seqn) = case seqn of
                           Nothing -> Just (addOwnSeq tab col)
                           Just _  -> Nothing 
                         addOwnSeq tab col             = AlterSeq (coerce $ doubleQuoteT (genKeyName $ SeqNameGen tab col Nothing))
                                                                  (AddOwner (coerce tab) (coerce col))                         
                         tagSeqs = fromSing (sing :: Sing (TagEachSeqs db tab seqs))
                         createDefSeqs (DemotedDBTagSeq tab col (Just seqn)) = Nothing
                         createDefSeqs (DemotedDBTagSeq tab col Nothing)     =
                           let seqn = quoteT $ doubleQuoteT (genKeyName $ SeqNameGen tab col Nothing)
                               seqnE = PQ.ConstExpr (PQ.Other seqn)
                               nextValE = PQ.FunExpr "nextVal" [seqnE]
                           in  Just (AlterTable (coerce tab) $ AlterColumn (ColName col) $ AddDefault (DefExpr nextValE))
                     in (fmap addCreateSeq tagSeqs, catMaybes (fmap createDefSeqs tagSeqs), catMaybes (fmap addAlterSeq tagSeqs))
        addChks = let addChk (cname, chExpr) = addCheckExpr (coerce tabN) (coerce (genKeyName $ CkNameGen tabN cname)) (CheckExpr chExpr)
                  in fmap addChk $ case (checks :: DBChecks db tab) of
                                     DBChecks hl -> happlyChkCtx hl
        addNotNullChks = let addNonNullCtx colN = addNotNull (coerce tabN) (coerce (T.pack colN))
                         in fmap addNonNullCtx $ fromSing (sing :: Sing nonNulls)                                           
        addDefs = let addDef (cname, dfExpr) = addDefaultExpr (coerce tabN) (coerce (cname :: T.Text)) (DefExpr dfExpr)
                      defs = case (defaults :: DBDefaults db tab) of
                                     DBDefaults hl -> (happlyDefExprs (Proxy @db) hl)
                  in  fmap addDef defs
        tabColHList = singCols (Proxy @db) (Proxy :: Proxy (OriginalTableFields tab)) (Proxy @(ColumnNames db tab))
        createTab = [CreateTable (coerce tabN) (map toMColumn $ recordToList tabColHList)]
        tabN = T.pack $ fromSing (sing :: Sing (TableName db tab))
    in  concat [ cTabSeqs
               , createTab
               , oTabSeqs
               , addPks  
               , addUqs
               , addFks
               , addChks
               , defSeqs
               , addDefs
               , addNotNullChks
               ]

data KeyNameGen
  = PkNameGen T.Text [T.Text]
  | FkNameGen T.Text [T.Text] T.Text
  | UqNameGen T.Text [T.Text]
  | CkNameGen T.Text T.Text
  | SeqNameGen T.Text T.Text (Maybe T.Text)
  deriving (Show, Eq)

genKeyName :: KeyNameGen -> T.Text
genKeyName (PkNameGen tab cols)           = T.intercalate "_" ("pk":tab:cols)
genKeyName (FkNameGen tab cols reft)      = T.intercalate "_" (("fk":tab:cols) ++ [reft])
genKeyName (UqNameGen tab cols)           = T.intercalate "_" ("uq":tab:cols)
genKeyName (CkNameGen tab cn)             = T.intercalate "_" ["ck",tab,cn]
genKeyName (SeqNameGen tab cn Nothing)    = T.intercalate "_" ["seq",tab,cn]
genKeyName (SeqNameGen tab cn (Just n))   = T.intercalate "_" ["seq",tab, cn, n]



  

type family TyCxts (db :: *) (tys :: [*]) :: Constraint where
  TyCxts db (ty ': ts) = ( ShowDBType (DB db) (GetDBTypeRep (DB db) ty)
                         , SingAttrs db (GetTypeFields ty)
                         , TyCxts db ts
                         )
  TyCxts db '[]        = ()
mkMigrationTypes :: forall db tys.
                    ( Database db
                    , TyCxts db tys
                    ) => Proxy (db :: *) -> Sing (tys :: [*]) -> [PrimDDL]
mkMigrationTypes _ SNil             = []
mkMigrationTypes pxyDB (SCons ty tys) = mkMigrationType pxyDB ty ++ mkMigrationTypes pxyDB tys

mkMigrationType :: forall db ty.
                  ( Database db
                  , ShowDBType (DB db) (GetDBTypeRep (DB db) ty)
                  , SingAttrs db (GetTypeFields ty)
                  ) => Proxy (db :: *) -> Sing (ty :: *) -> [PrimDDL]
mkMigrationType _ _
  = let tyName = showDBType (Proxy @(DB db)) (Proxy @(GetDBTypeRep (DB db) ty))
        tyAttrHList = singAttrs (Proxy @db) (Proxy :: Proxy (GetTypeFields ty))
        migTypes = case toTypeAttr tyAttrHList of
          EnumAttr cnames  -> [CreateEnum (coerce tyName) (coerce cnames)]
          ProdAttr cols    -> [CreateType (coerce tyName) (coerce cols)]
          SumAttr conAttrs ->
            let tyTag = (if T.last tyName == '"' then T.init tyName else tyName) `T.append` "_tags\""
                createSumTags cons = CreateEnum (coerce tyTag) $ (coerce $ fmap fst cons)
                sumTagTys = createSumTags conAttrs
                conTyName cn = TypeName ("\"" `T.append` cn `T.append` "_con\"")
                createConTy (cn, flds) = CreateType (conTyName cn) flds
                sumTy = CreateType (TypeName tyName)
                                   ( M.Column (coerce ("tag" :: T.Text)) (coerce tyTag)
                                          : fmap (\(ColName cn,_) -> M.Column (coerce cn) (coerce $ conTyName cn)) conAttrs
                                          )
            in (sumTagTys : fmap createConTy (coerce conAttrs)) ++ [sumTy]
    in concat [ migTypes
              ]

diffMigration :: [PrimDDL] -> [PrimDDL] -> [PrimDDL] -> [PrimDDL]
diffMigration _current _previous _reified = []

validateSchemaInfo :: ( Database db
                        , AllF (Table db) (Tables db)
                        , TyCxts db (Types db)
                        , SingI (Tables db)
                        , SingI (Types db)
                        ) => Proxy (db :: *) -> PGS.Connection -> IO Bool
validateSchemaInfo pdb conn = do
  let mig = mkMigration pdb
      migStmtQ = unlines (fmap renderDDL mig)
  rMigs <- getSchemaInfo pdb conn
  let rmigStmtQ = unlines (fmap renderDDL rMigs)
  putStrLn "+++++++++++++++++++++++++++++++++++++++++"
  putStrLn migStmtQ
  putStrLn "========================================="
  putStrLn rmigStmtQ
  return False
  

doubleQuoteT :: T.Text -> T.Text
doubleQuoteT t = "\"" <> t <> "\""

quoteT :: T.Text -> T.Text
quoteT t = "\'" <> t <> "\'"
