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
       ) where

import DBRecord.Internal.Migration
import DBRecord.Internal.Schema
import DBRecord.Internal.Types
import DBRecord.Internal.Common
import DBRecord.Internal.DBTypes

import qualified Data.Text as T
import Data.Proxy
import GHC.Exts
import Data.Functor.Const


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
mkMigration pxyDB = mkMigrationTables pxyDB (sing :: Sing tables)
  ++ mkMigrationTypes pxyDB (sing :: Sing types)


mkMigrationTables :: forall db tabs.
                    ( All (Table db) tabs
                    ) => Proxy (db :: *) -> Sing (tabs :: [*]) -> [Migration]
mkMigrationTables _ SNil             = []
mkMigrationTables pxyDB (SCons tab tabs) = mkMigrationTable pxyDB tab ++ mkMigrationTables pxyDB tabs

mkMigrationTable :: forall db tab pks fks chks uqs defs.
                   ( Table db tab
                   , pks ~ PrimaryKey tab
                   , fks ~ ForeignKey tab
                   , chks ~ Check tab
                   , uqs ~ Unique tab
                   , defs ~ HasDefault tab
                   ) => Proxy (db :: *) -> Sing (tab :: *) -> [Migration]
mkMigrationTable _ _
  = let addPks = case fromSing (sing :: Sing pks) of
                      [] -> []
                      xs  -> [AlterTable tabN $ AddConstraint "pk_" $ AddPrimaryKey $ fmap T.pack $ xs]
        addUqs = let addUq fs = AlterTable tabN $ AddConstraint "uq_" $ AddUnique $ fmap T.pack fs
                 in fmap addUq $ fromSing (sing :: Sing uqs)
        addFks = let addFk (fcols, reft, rcols) = AlterTable tabN $ AddConstraint "fk_" $ AddForeignKey fcols reft rcols
                 in fmap addFk $ fromSing (sing :: Sing fks)
        addChks = let addChk chExpr = AlterTable tabN $ AddConstraint "ch_" $ AddCheck chExpr
                  in fmap addChk $ fromSing (sing :: Sing chks)
        addDefs = let addDef dfExpr = AlterTable tabN $ AlterColumn "col" $ AddDefault dfExpr
                  in fmap addDef $ fromSing (sing :: Sing ('DefSyms defs))
        tabColHList = singCols (Proxy @db) (Proxy :: Proxy (OriginalTableFields tab)) (Proxy @(ColumnNames tab))
        createTab = [CreateTable tabN $ recordToList tabColHList]
        tabN = T.pack $ fromSing (sing :: Sing (TableName tab))
    in  concat [ createTab
               , addPks  
               , addUqs
               , addFks
               , addChks
               , addDefs
               ]

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
