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
       , renderMig
       , diffMigration
       , BaseTable (..)
       , module DBRecord.Migration
       ) where

import DBRecord.Internal.Migration
import DBRecord.Internal.Schema
import DBRecord.Internal.Types
import DBRecord.Internal.Common
import DBRecord.Internal.DBTypes

import qualified Data.Text as T
import Data.Proxy
import GHC.Exts
import GHC.TypeLits
import Data.Kind
import Data.Functor.Const

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
    [DConAttr (_cn, cols)]   -> ProdAttr cols
    [] -> error "@toTypeAttr: DB Type cannot be of Void type"
    cons | all isUnary cons -> EnumAttr $ fmap (\(DConAttr cattr) -> fst cattr) cons
         | otherwise        -> SumAttr $ fmap (\(DConAttr cattr) -> cattr) cons

mkMigration :: forall db schema tables types.
  ( Database db
  , schema ~ Schema db
  , tables ~ Tables db
  , types  ~ Types db
  , All (Table db) tables
  , TyCxts db types
  , SingI tables
  , SingI types
  ) => Proxy db -> [Migration]
mkMigration pxyDB = mkMigrationTypes pxyDB (sing :: Sing types)
  ++ mkMigrationTables pxyDB (sing :: Sing tables)


mkMigrationTables :: forall db tabs.
                    ( All (Table db) tabs
                    ) => Proxy (db :: *) -> Sing (tabs :: [*]) -> [Migration]
mkMigrationTables _ SNil             = []
mkMigrationTables pxyDB (SCons tab tabs) = mkMigrationTable pxyDB tab ++ mkMigrationTables pxyDB tabs

mkMigrationTable :: forall db tab pks fks chks uqs defs.
                   ( Table db tab
                   , pks ~ PrimaryKey db tab
                   , fks ~ ForeignKey db tab
                   , chks ~ Check db tab
                   , uqs ~ Unique db tab
                   , defs ~ HasDefault db tab
                   ) => Proxy (db :: *) -> Sing (tab :: *) -> [Migration]
mkMigrationTable _ _
  = let addPks = case fromSing (sing :: Sing pks) of
                      [] -> []
                      xs  ->
                        let keyCols = fmap T.pack $ xs
                        in [AlterTable tabN $ AddConstraint (genKeyName $ PkNameGen tabN keyCols) $ AddPrimaryKey keyCols]
        addUqs = let addUq fs =
                       let keyCols = fmap T.pack fs
                       in AlterTable tabN $ AddConstraint (genKeyName $ UqNameGen tabN keyCols) $ AddUnique keyCols
                 in fmap addUq $ fromSing (sing :: Sing (GetAllUniqs uqs))
        addFks = let addFk (DemotedDBTagFk fcols reft rcols) = AlterTable tabN $ AddConstraint (genKeyName $ FkNameGen tabN fcols reft) $ AddForeignKey fcols reft rcols
                 in fmap addFk  $ fromSing (sing :: Sing (TagEachFks db fks))
        addChks = let addChk (cname, chExpr) = AlterTable tabN $ AddConstraint (genKeyName $ CkNameGen tabN cname) $ AddCheck chExpr
                  in fmap addChk $ fromSing (sing :: Sing chks)
        chkExpr = let singChks :: HList (Chk db tab) chks1 -> [()]
                      singChks Nil = []
                      singChks (c :& cs) = () : singChks cs
                  in case (checks :: DBChecks db tab) of
                       DBChecks chks -> singChks chks
        addDefs = let addDef dfExpr = AlterTable tabN $ AlterColumn "col" $ AddDefault dfExpr
                  in fmap addDef $ fromSing (sing :: Sing ('DefSyms defs))
        tabColHList = singCols (Proxy @db) (Proxy :: Proxy (OriginalTableFields tab)) (Proxy @(ColumnNames db tab))
        createTab = [CreateTable tabN $ recordToList tabColHList]
        tabN = T.pack $ fromSing (sing :: Sing (TableName db tab))
    in  concat [ createTab
               , addPks  
               , addUqs
               , addFks
               , addChks
               , addDefs
               ]

data KeyNameGen
  = PkNameGen T.Text [T.Text]
  | FkNameGen T.Text [T.Text] T.Text
  | UqNameGen T.Text [T.Text]
  | CkNameGen T.Text T.Text
  deriving (Show, Eq)

genKeyName :: KeyNameGen -> T.Text
genKeyName (PkNameGen tab cols) = T.intercalate "_" ("pk":tab:cols)
genKeyName (FkNameGen tab cols reft) = T.intercalate "_" (("fk":tab:cols) ++ [reft])
genKeyName (UqNameGen tab cols) = T.intercalate "_" ("uq":tab:cols)
genKeyName (CkNameGen tab cn) = T.intercalate "_" ["ck",tab,cn]



  

type family TyCxts (db :: *) (tys :: [*]) :: Constraint where
  TyCxts db (ty ': ts) = ( ShowDBType (DB db) (GetDBTypeRep (DB db) ty)
                         , SingAttrs db (GetTypeFields ty)
                         , TyCxts db ts
                         )
  TyCxts db '[]        = ()
mkMigrationTypes :: forall db tys.
                    ( Database db
                    , TyCxts db tys
                    ) => Proxy (db :: *) -> Sing (tys :: [*]) -> [Migration]
mkMigrationTypes _ SNil             = []
mkMigrationTypes pxyDB (SCons ty tys) = mkMigrationType pxyDB ty ++ mkMigrationTypes pxyDB tys

mkMigrationType :: forall db ty.
                  ( Database db
                  , ShowDBType (DB db) (GetDBTypeRep (DB db) ty)
                  , SingAttrs db (GetTypeFields ty)
                  ) => Proxy (db :: *) -> Sing (ty :: *) -> [Migration]
mkMigrationType _ _
  = let tyName = showDBType (Proxy @(DB db)) (Proxy @(GetDBTypeRep (DB db) ty))
        tyAttrHList = singAttrs (Proxy @db) (Proxy :: Proxy (GetTypeFields ty))
        migTypes = case toTypeAttr tyAttrHList of
          EnumAttr cnames  -> [CreateEnum tyName cnames]
          ProdAttr cols    -> [CreateType tyName cols]
          SumAttr conAttrs ->
            let tyTag = (if T.last tyName == '"' then T.init tyName else tyName) `T.append` "_tags\""
                createSumTags cons = CreateEnum tyTag $ fmap fst cons
                sumTagTys = createSumTags conAttrs
                conTyName cn = "\"" `T.append` cn `T.append` "_con\""
                createConTy (cn, flds) = CreateType (conTyName cn) flds
                sumTy = CreateType tyName ( Column "tag" tyTag
                                          : fmap (\(cn,_) -> Column cn $ conTyName cn) conAttrs
                                          )
            in (sumTagTys : fmap createConTy conAttrs) ++ [sumTy]
    in concat [ migTypes
              ]

diffMigration :: [Migration] -> [Migration] -> [Migration] -> [Migration]
diffMigration _current _previous _reified = []
