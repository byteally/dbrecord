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
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableSuperClasses #-}


module DBRecord.Migration
       ( -- mkMigration
         mkAllMigrations
       -- , diffMigration
       , BaseTable (..)
       , module DBRecord.Migration
       ) where

-- import qualified Database.PostgreSQL.Simple  as PGS
import qualified DBRecord.Internal.DDL       as M
import qualified DBRecord.Internal.Schema    as S
import Data.Maybe

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
import Data.Monoid ((<>))
import qualified Data.List as L
import qualified Control.Monad.State as M
import qualified Control.Monad.Reader as M
import qualified Data.Functor.Identity as I
import DBRecord.Internal.Lens
import GHC.Generics
import Data.Typeable (Typeable)
import GHC.OverloadedLabels

class BaseDatabase (db :: *) (ver :: Nat) where
  type BaseDatabaseName db ver :: Symbol
  type BaseDatabaseName db ver = "public"

class BaseSchema (sc :: *) (ver :: Nat) where
  type BaseSchemaName sc ver :: Symbol
  type BaseSchemaName sc ver = "public"
  
  type BaseTables sc ver :: [TypeName Symbol]
  type BaseTables sc ver = '[]
  
  type BaseTypes sc ver :: [TypeName Symbol]
  type BaseTypes sc ver = '[]


class BaseTable (sc :: *) (tab :: TypeName Symbol) (ver :: Nat) where
  type BaseColumns sc tab ver :: [(Symbol, DBTypeK)]
  type BaseColumns sc tab ver = '[]

  type BaseColumnNames sc tab ver :: [(Symbol, Symbol)]
  type BaseColumnNames sc tab ver = '[]

  type BasePrimaryKeyName sc tab ver :: Maybe Symbol
  type BasePrimaryKeyName sc tab ver = 'Nothing
  
  type BasePrimaryKey sc tab ver :: [Symbol]
  type BasePrimaryKey sc tab ver = '[]
  
  type BaseForeignKey sc tab ver :: [ForeignRef (TypeName Symbol)]
  type BaseForeignKey sc tab ver = '[]

  type BaseForeignKeyNames sc tab ver :: [(Symbol, Symbol)]
  type BaseForeignKeyNames sc tab ver = '[]
  
  type BaseUnique sc tab ver :: [UniqueCT]
  type BaseUnique sc tab ver = '[]

  type BaseUniqueNames sc tab ver :: [(Symbol, Symbol)]
  type BaseUniqueNames sc tab ver = '[]
  
  type BaseDefaultedCols sc tab ver :: [Symbol]
  type BaseDefaultedCols sc tab ver = '[]
  
  type BaseCheck sc tab ver :: [CheckCT]
  type BaseCheck sc tab ver = '[]

  type BaseCheckNames sc tab ver :: [(Symbol, Symbol)]
  type BaseCheckNames sc tab ver = '[]
  
  type BaseTableSequence sc tab ver :: [Sequence]
  type BaseTableSequence sc tab ver = '[]

  type BaseSequenceNames sc tab ver :: [(Symbol, Symbol)]
  type BaseSequenceNames sc tab ver = '[]

  baseDefaults :: BaseDBDefaults sc tab ver
  baseChecks   :: BaseDBChecks sc tab ver

  baseDefaults = BaseDBDefaults Nil
  baseChecks   = BaseDBChecks Nil

class BaseUDType (sc :: *) (ver :: Nat) (ty :: TypeName Symbol) where
  type BaseTypeMappings sc ver ty :: UDTypeMappings
  -- type TypeMappings sc ty = 'Flat '[]

data BaseDBDefaults (sc :: *) (tab :: TypeName Symbol) (ver :: Nat) =
  forall xs. (All KnownSymbol xs) => BaseDBDefaults (HList (Def sc tab) xs)

baseDbDefaults :: forall tab sc ver xs.
              ( All KnownSymbol xs
              ) => HList (Def sc tab) xs -> BaseDBDefaults sc tab ver
baseDbDefaults = BaseDBDefaults

data BaseDBChecks (sc :: *) (tab :: TypeName Symbol) (ver :: Nat) =
  forall chks. (All CheckExpr chks) => BaseDBChecks (HList (Chk sc tab) chks)

baseDbChecks :: forall tab (sc :: *) ver chks.
            ( All CheckExpr chks
            ) => HList (Chk sc tab) chks -> BaseDBChecks sc tab ver
baseDbChecks = BaseDBChecks

class SchemaMigration (base :: *) (v :: Nat) where
  type CreatedTables base v :: [TypeName Symbol]
  type CreatedTables base v = '[]
  
  type DropedTables base v :: [TypeName Symbol]
  type DropedTables base v = '[]
  
  type AlteredTables base v :: [TypeName Symbol]
  type AlteredTables base v = '[]

  type CreatedTypes base v :: [TypeName Symbol]
  type CreatedTypes base v = '[]
  
  type DropedTypes base v :: [TypeName Symbol]
  type DropedTypes base v = '[]
  
  type RenamedTypes base v :: [(TypeName Symbol, Symbol)]
  type RenamedTypes base v = '[]
  
  type AlteredTypes base v :: [TypeName Symbol]
  type AlteredTypes base v = '[]

data UDTypeOP = AddEnumValAfter Symbol -- Add enum val
                                Symbol -- After this enum
              | AddEnumValBefore Symbol -- Add enum val
                                 Symbol -- Before this enum
              | AddEnumVal Symbol -- Add enum val
                             
class TypeMigration (sc :: *) (ver :: Nat) (ty :: TypeName Symbol) where
  type TypeMigrations sc ver ty :: [UDTypeOP]
  
class TableMigration (base :: *) (tab :: TypeName Symbol) (v :: Nat) where
  type RenamedTable base tab v :: Maybe Symbol
  type RenamedTable base tab v = 'Nothing
  
  type AddedColumn base tab v :: [AddColumn]
  type AddedColumn base tab v = '[]
  
  type DropedColumn base tab v :: [Symbol]
  type DropedColumn base tab v = '[]
  
  type RenamedColumn base tab v :: [(Symbol, Symbol)]
  type RenamedColumn base tab v = '[]
  
  type AlteredColumn base tab v :: [AlterColumn]
  type AlteredColumn base tab v = '[]
  
  type AddedConstraint base tab v :: [AddConstraint]
  type AddedConstraint base tab v = '[]
  
  type DropedConstraint base tab v :: [DropConstraint]
  type DropedConstraint base tab v = '[]
  
  migDefaults :: MigDBDefaults base tab v
  migChecks   :: MigDBChecks base tab v

  migDefaults = MigDBDefaults Nil
  migChecks   = MigDBChecks Nil

data MigDBDefaults (sc :: *) (tab :: TypeName Symbol) (ver :: Nat) =
  forall xs. (All KnownSymbol xs) => MigDBDefaults (HList (Def sc tab) xs)

migDbDefaults :: forall tab sc ver xs.
              ( All KnownSymbol xs
              ) => HList (Def sc tab) xs -> MigDBDefaults sc tab ver
migDbDefaults = MigDBDefaults

data MigDBChecks (sc :: *) (tab :: TypeName Symbol) (ver :: Nat) =
  forall chks. (All CheckExpr chks) => MigDBChecks (HList (Chk sc tab) chks)

migDbChecks :: forall tab (sc :: *) ver chks.
            ( All CheckExpr chks
            ) => HList (Chk sc tab) chks -> MigDBChecks sc tab ver
migDbChecks = MigDBChecks

{-
instance ( ValidateBaseDBFld db tab un a
         , un ~ fn
         , v ~ Expr '[] a
         ) => IsLabel un (v -> Def (db :: *) (tab :: (TypeName Symbol, Nat)) fn) where
#if __GLASGOW_HASKELL__ > 800
  fromLabel v = def @un @tab v
#else
  fromLabel _ v = def @un @tab v
#endif

type family ValidateBaseDBFld (db :: *) (tab :: (TypeName Symbol, Nat)) (fn :: Symbol) a where
  ValidateBaseDBFld db '(tab, ver) fn t =
    UnifyField (BaseColumns db tab ver) fn ft ('Text "err message ")

type family UnifyDBField (flds :: [(Symbol, DBTypeK)]) (fn :: Symbol) (match :: *) (nfMsg :: ErrorMessage) :: Constraint where
  UnifyDBField ('(fn, ft') ': fs) fn ft nfMsg  = (ft ~ ft')
  UnifyDBField ('(fn', ft') ': fs) fn ft nfMsg = UnifyDBField fs fn ft nfMsg
  UnifyDBField '[] fn ft nfMsg                 = TypeError nfMsg
-}

data instance Sing (t :: CreateType) where
  SCreateType :: Sing 'CreateType

data instance Sing (t :: DropType) where
  SDropType :: Sing 'DropType

data instance Sing (t :: AlterType) where
  SAlterType :: Sing 'AlterType

data instance Sing (t :: RenameTable) where
  SRenameTable :: Sing (dcn :: Maybe Symbol) -> Sing ('RenameTable dcn)

data instance Sing (t :: AddColumn) where
  SAddColumn :: Sing (cn :: Symbol) -> Sing (dbTyp :: TagHK DbK DBTypeK) -> Sing ('AddColumn cn dbTyp)

data instance Sing (t :: DropColumn) where
  SDropColumn :: Sing (cn :: Symbol) -> Sing ('DropColumn cn)

data instance Sing (t :: RenameColumn) where
  SRenameColumn :: Sing (cn :: Symbol) -> Sing (dcn :: Symbol) -> Sing ('RenameColumn cn dcn)

data instance Sing (t :: AlterColumn) where
  SSetNotNull  :: Sing (cn :: Symbol) -> Sing ('SetNotNull cn)
  SDropNotNull :: Sing (cn :: Symbol) -> Sing ('DropNotNull cn)
  SChangeType  :: Sing (cn :: Symbol) -> Sing (dbTyp :: TagHK DbK DBTypeK) ->
                 Sing ('ChangeType cn dbTyp)
  SAddDefault :: Sing (cn :: Symbol) -> Sing ('AddDefault cn)
  SDropDefault :: Sing (cn :: Symbol) -> Sing ('DropDefault cn)

data instance Sing (t :: AddConstraint) where
  SAddPrimaryKey :: Sing (cns :: [Symbol]) -> Sing (ctxn :: Symbol) -> Sing ('AddPrimaryKey cns ctxn)
  SAddUnique     :: Sing (uq :: UniqueCT) -> Sing ('AddUnique uq)
  SAddCheck      :: Sing (chk :: CheckCT) -> Sing ('AddCheck chk)
  SAddForeignKey :: Sing (fkref :: ForeignRef (TypeName Symbol)) -> Sing ('AddForeignKey fkref)

data instance Sing (t :: DropConstraint) where
  SDropPrimaryKey  :: Sing 'DropPrimaryKey
  SDropUnique      :: Sing (cn :: Symbol) -> Sing ('DropUnique cn)
  SDropForeignKey  :: Sing (cn :: Symbol) -> Sing ('DropForeignKey cn)
  SDropCheck       :: Sing (cn :: Symbol) -> Sing ('DropCheck cn)

data instance Sing (t :: UDTypeOP) where
  SAddEnumValAfter :: Sing (enVal :: Symbol) -> Sing (afterEn :: Symbol) -> Sing ('AddEnumValAfter enVal afterEn)
  SAddEnumValBefore :: Sing (enVal :: Symbol) -> Sing (beforeEn :: Symbol) -> Sing ('AddEnumValBefore enVal beforeEn)
  SAddEnumVal :: Sing (enVal :: Symbol) -> Sing ('AddEnumVal enVal)

instance ( SingI enVal
         , SingI afterEn
         ) => SingI ('AddEnumValAfter enVal afterEn) where
  sing = SAddEnumValAfter sing sing

instance ( SingI enVal
         , SingI beforeEn
         ) => SingI ('AddEnumValBefore enVal beforeEn) where
  sing = SAddEnumValBefore sing sing

instance ( SingI enVal
         ) => SingI ('AddEnumVal enVal) where
  sing = SAddEnumVal sing

instance ( SingI dcn
         ) => SingI ('RenameTable dcn) where
  sing = SRenameTable sing

instance ( SingI cn
         , SingI dbTyp
         ) => SingI ('AddColumn cn dbTyp) where
  sing = SAddColumn sing sing

instance ( SingI cn
         ) => SingI ('DropColumn cn) where
  sing = SDropColumn sing

instance ( SingI cn
         , SingI dcn
         ) => SingI ('RenameColumn cn dcn) where
  sing = SRenameColumn sing sing

instance (SingI cn) => SingI ('SetNotNull cn) where
  sing = SSetNotNull sing

instance (SingI cn) => SingI ('DropNotNull cn) where
  sing = SDropNotNull sing

instance ( SingI cn
         , SingI dbTyp
         ) => SingI ('ChangeType cn dbTyp) where
  sing = SChangeType sing sing

instance (SingI cn) => SingI ('AddDefault cn) where
  sing = SAddDefault sing

instance (SingI cn) => SingI ('DropDefault cn) where
  sing = SDropDefault sing

instance ( SingI cns
         , SingI ctxn
         ) => SingI ('AddPrimaryKey cns ctxn) where
  sing = SAddPrimaryKey sing sing

instance (SingI uct) => SingI ('AddUnique uct) where
  sing = SAddUnique sing

instance (SingI cct) => SingI ('AddCheck cct) where
  sing = SAddCheck sing

instance (SingI fkref) => SingI ('AddForeignKey fkref) where
  sing = SAddForeignKey sing

instance SingI 'DropPrimaryKey where
  sing = SDropPrimaryKey

instance (SingI cn) => SingI ('DropUnique cn) where
  sing = SDropUnique sing

instance (SingI cn) => SingI ('DropForeignKey cn) where
  sing = SDropForeignKey sing

instance (SingI cn) => SingI ('DropCheck cn) where
  sing = SDropCheck sing

type family RenameTableCtx (t :: RenameTable) :: Constraint where
  RenameTableCtx ('RenameTable t) = (MaybeCtx SingE t)

instance (RenameTableCtx t) => SingE (t :: RenameTable) where
  type Demote t = ChangeSetM (Maybe M.AlterTable)
  fromSing (SRenameTable smName) = do
    case fromSing smName of
      Nothing    -> pure Nothing
      (Just dbN) -> do
        curTab <- use currentTable
        scInfo . tableInfoAt curTab . tableName . dbName .= dbN
        pure (Just $ M.renameTab (M.tableName (curTab ^. typeName) dbN))

type family AddColumnCtx (t :: AddColumn) :: Constraint where
  AddColumnCtx ('AddColumn cn dbTyp) =
    (SingE cn, SingE dbTyp, SingI (IsNullable (UnTag dbTyp)), SingE (IsNullable (UnTag dbTyp)))
  
instance (AddColumnCtx t) => SingE (t :: AddColumn) where
  type Demote t = ChangeSetM M.AlterTable
  fromSing (SAddColumn scoln scolt) = do
    let colnHs = fromSing scoln
        colnDb = mkDbColumnName colnHs
        colt   = fromSing scolt
        cni    = mkEntityName colnHs colnDb
    curTab <- use currentTable
    scInfo . tableInfoAt curTab . columnInfo %=
         insert (mkColumnInfo cni (coerce colt))
    pure $ M.addColumn (M.column (M.columnName colnHs colnDb) (coerce colt))

      where isNullableSing :: forall dbt db. (SingI (IsNullable (UnTag dbt))) => Sing dbt -> Sing (IsNullable (UnTag dbt))
            isNullableSing _ = sing

type family DropColumnCtx (t :: DropColumn) :: Constraint where
  DropColumnCtx ('DropColumn cn) = ()

instance SingE (t :: DropColumn) where
  type Demote t = ChangeSetM M.AlterTable
  fromSing (SDropColumn scoln) = do
    let coln = fromSing scoln
    curTab <- use currentTable
    mdbN   <- preuse (scInfo . tableInfoAt curTab . columnInfoAt coln . columnNameInfo . dbName)
    let dbN = fromJust mdbN
    scInfo . tableInfoAt curTab . columnInfo %=
         delete coln (\ci -> ci ^. columnNameInfo . hsName)
    pure $ M.dropColumn (M.columnName coln dbN)

instance SingE (t :: RenameColumn) where
  type Demote t = ChangeSetM M.AlterTable
  fromSing (SRenameColumn scoln sdcoln) = do
    let hsColN = fromSing scoln
        dbColN = fromSing sdcoln
    curTab <- use currentTable
    mprevDbColN <- preuse (scInfo . tableInfoAt curTab . columnInfoAt hsColN . columnNameInfo . dbName)   
    scInfo . tableInfoAt curTab . columnInfoAt hsColN . columnNameInfo . dbName .= dbColN
    let prevDbColN = fromJust mprevDbColN
    pure $ M.renameColumn (M.columnName hsColN prevDbColN) (M.columnName hsColN dbColN)

type family AlterColumnCtx (t :: AlterColumn) :: Constraint where
  AlterColumnCtx ('SetNotNull cn)  = (SingE cn)
  AlterColumnCtx ('DropNotNull cn) = (SingE cn)  
  AlterColumnCtx ('ChangeType cn tdbTyp) = (SingE cn, SingE tdbTyp)
  AlterColumnCtx ('AddDefault cn) = (SingE cn)
  AlterColumnCtx ('DropDefault cn) = (SingE cn)

instance (AlterColumnCtx t) => SingE (t :: AlterColumn) where
  type Demote t = ChangeSetM M.AlterTable
  fromSing (SSetNotNull scoln) = do
    let hsColn = fromSing scoln
    curTab <- use currentTable    
    mdbColn <- preuse (scInfo . tableInfoAt curTab . columnInfoAt hsColn . columnNameInfo . dbName)
    let dbColn = fromJust mdbColn
    scInfo . tableInfoAt curTab . columnInfoAt hsColn . columnTypeName . dbType %= removeNullable
    pure $ M.setNotNull (M.columnName hsColn dbColn)
  fromSing (SDropNotNull scoln) = do
    let hsColn = fromSing scoln
    curTab <- use currentTable 
    mdbColn <- preuse (scInfo . tableInfoAt curTab . columnInfoAt hsColn . columnNameInfo . dbName)
    let dbColn = fromJust mdbColn
    scInfo . tableInfoAt curTab . columnInfoAt hsColn . columnTypeName . dbType %= toNullable
    pure $ M.dropNotNull (M.columnName hsColn dbColn)
  fromSing (SChangeType scoln scolt) = do
    let hsColn = fromSing scoln
        colt   = fromSing scolt
    curTab <- use currentTable    
    mdbColn <- preuse (scInfo . tableInfoAt curTab . columnInfoAt hsColn . columnNameInfo . dbName)
    let dbColn = fromJust mdbColn
    scInfo . tableInfoAt curTab . columnInfoAt hsColn . columnTypeName .= (coerce colt)
    pure $ M.changeType (M.columnName hsColn dbColn) (coerce colt)
  fromSing (SAddDefault scoln) = do
    let hsColn = (fromSing scoln)
    curTab <- use currentTable
    cksAndDefs <- use currentChecksAndDefs
    mtabInfo <- preuse (scInfo . tableInfoAt curTab)
    let mdbColn    = tabInfo ^? columnInfoAt hsColn . columnNameInfo . dbName
        cols       = tabInfo ^. columnInfo
        curTabName = tabInfo ^. tableName
        tabInfo    = fromJust mtabInfo
        dbColn     = fromJust mdbColn
        defVal = migDefInfo cksAndDefs hsColn curTabName cols
    scInfo . tableInfoAt curTab . defaultInfo %=
              insert defVal
    pure $ M.addDefault (M.columnName hsColn dbColn) (coerce (defVal ^. defaultExp))
  fromSing (SDropDefault scoln) = do
    let hsColn = fromSing scoln
    curTab <- use currentTable    
    mdbColn <- preuse (scInfo . tableInfoAt curTab . columnInfoAt hsColn . columnNameInfo . dbName)
    let dbColn = fromJust mdbColn
    scInfo . tableInfoAt curTab . defaultInfo %=
              delete hsColn (\di -> di ^. defaultOn)
    pure $ M.dropDefault (M.columnName hsColn dbColn)

type family AddConstraintCtx (t :: AddConstraint) :: Constraint where
  AddConstraintCtx ('AddPrimaryKey cols ctxn) =
    (SingE cols, SingE ctxn)
  AddConstraintCtx ('AddUnique uct) = SingE uct
  AddConstraintCtx ('AddCheck cct)  = SingE cct
  AddConstraintCtx ('AddForeignKey fkref) = SingE fkref
    
instance (AddConstraintCtx t) => SingE (t :: AddConstraint) where
  type Demote t = ChangeSetM M.AlterTable
  fromSing (SAddPrimaryKey scols sctxn) = do
    curTab <- use currentTable    
    mTabInfo <- preuse (scInfo . tableInfoAt curTab)
    let cols = coerce $ fromSing scols
        mcols = map (\hsCol -> case mTabInfo ^? traverse . columnInfoAt hsCol . columnNameInfo . dbName of
                                Just dbCol -> M.columnName hsCol dbCol
                                Nothing    -> error "Panic: impossible case"
                    ) cols
        dbCtxn = {-mkDbKeyName (PkName curTab cols)-} (fromSing sctxn)
        pki  = mkPrimaryKeyInfo dbCtxn (coerce cols)
    scInfo . tableInfoAt curTab . primaryKeyInfo .= Just pki
    pure (M.addPrimaryKey (M.constraintName dbCtxn dbCtxn) mcols)
  fromSing (SAddUnique suq) = do
    curTab <- use currentTable
    mTabInfo <- preuse (scInfo . tableInfoAt curTab)
    
    let (cols, hsUqN) = fromSing suq
        dbUqN = mkDbKeyName (UqName (curTab ^. typeName) cols)
        uniqEt = mkEntityName hsUqN dbUqN
        mcols = map (\hsCol -> case mTabInfo ^? traverse . columnInfoAt hsCol . columnNameInfo . dbName of
                      Just dbCol -> M.columnName hsCol dbCol
                      Nothing    -> error "Panic: impossible case"
                    ) cols        
    scInfo . tableInfoAt curTab . uniqueInfo %=
         insert (mkUniqueInfo uniqEt cols)
    pure (M.addUnique (M.constraintName hsUqN dbUqN) mcols)
  fromSing (SAddCheck sck)  = do
    let (scols, hsCname) = fromSing sck
        chkEt = mkEntityName hsCname hsCname
    curTab <- use currentTable
    mtabInfo <- preuse (scInfo . tableInfoAt curTab)
    let cols       = tabInfo ^. columnInfo
        curTabName = tabInfo ^. tableName
        tabInfo    = fromJust mtabInfo
    cksAndDefs <- use currentChecksAndDefs    
    let ckInfo = migCksInfo cksAndDefs hsCname curTabName cols
    scInfo . tableInfoAt curTab . checkInfo %=
         insert ckInfo
    pure (M.addCheck (M.constraintName hsCname (coerce (ckInfo ^. checkName . dbName)))
                     (coerce (ckInfo ^. checkExp)))
  fromSing (SAddForeignKey (sfkref :: Sing (fkref :: ForeignRef (TypeName Symbol)))) = do
    let fkref = fromSing sfkref
    curTab <- use currentTable
    mTabInfo <- preuse (scInfo . tableInfoAt curTab)
    case fkref of
      RefByD fkN cols reft refcols -> do
         let fk = mkForeignKeyInfo fkEt cols reft refcols
             dbFkN = mkDbKeyName (FkName (curTab ^. typeName) cols (reft ^. typeName))
             hsFkN = fkN
             fkEt = mkEntityName hsFkN dbFkN
             mcols = map (\hsCol -> case mTabInfo ^? traverse . columnInfoAt hsCol . columnNameInfo . dbName of
                                Just dbCol -> M.columnName hsCol dbCol
                                Nothing    -> error "Panic: impossible case"
                    ) cols
         scInfo . tableInfoAt curTab . foreignKeyInfo %= insert fk
         mrefDbTabN <- preuse (scInfo . tableInfoAt reft . tableName . dbName)
         mrefAllCols <- preuse (scInfo . tableInfoAt reft . columnInfo)
         let refDbTabN  = fromJust mrefDbTabN
             refAllCols = fromJust mrefAllCols
             mrefcols = map (\hsCol -> case refAllCols ^? ixBy hsCol (_hsName . _columnNameInfo) . columnNameInfo . dbName of
                                (Just dbCol) -> M.columnName hsCol dbCol
                                _            -> error "Panic: impossible case"
                       ) refcols             
         pure $ M.addForeignKey (M.constraintName hsFkN dbFkN)
                                mcols
                                (M.tableName (reft ^. typeName) refDbTabN)
                                mrefcols
      _ -> error "No sing for RefD"

instance SingE (t :: DropConstraint) where
  type Demote t = ChangeSetM M.AlterTable
  fromSing SDropPrimaryKey = do
    curTab <- use currentTable    
    mpki <- preuse (scInfo . tableInfoAt curTab . primaryKeyInfo . traverse)
    scInfo . tableInfoAt curTab . primaryKeyInfo .=
      Nothing
    pure $ case mpki of
      Just pki -> M.dropPrimaryKey (M.constraintName (pki ^. pkeyName) (pki ^. pkeyName))
      Nothing  -> error "Panic: non existant primary key"
  fromSing (SDropCheck schkn) = do
    curTab <- use currentTable    
    let hsChkn = (fromSing schkn)
    mdbChkn <- preuse (scInfo . tableInfoAt curTab . checkInfoAt hsChkn . checkName . dbName)
    let dbChkn = fromJust mdbChkn
    scInfo . tableInfoAt curTab . checkInfo %=
      delete hsChkn (\ck -> ck ^. checkName . hsName)
    pure $ M.dropCheck (M.constraintName hsChkn dbChkn)
  fromSing (SDropUnique suqn) = do
    curTab <- use currentTable    
    let hsUqn = (fromSing suqn)
    mdbUqn <- preuse (scInfo . tableInfoAt curTab . uniqueInfoAt hsUqn . uqName . dbName)
    let dbUqn = fromJust mdbUqn
    scInfo . tableInfoAt curTab . uniqueInfo %=
         delete hsUqn (\uq -> uq ^. uqName . hsName)
    pure $ M.dropUnique (M.constraintName hsUqn dbUqn)
  fromSing (SDropForeignKey sfkn) = do
    curTab <- use currentTable    
    let hsFkn = (fromSing sfkn)
    mdbFkn <- preuse (scInfo . tableInfoAt curTab . foreignKeyInfoAt hsFkn . fkeyName . dbName)
    let dbFkn = fromJust mdbFkn
    scInfo . tableInfoAt curTab . foreignKeyInfo %=
         delete hsFkn (\fk -> fk ^. fkeyName . hsName)
    pure $ M.dropForeignKey (M.constraintName hsFkn dbFkn)

type family UDTOpCtx (t :: TagHK (TypeName Symbol) UDTypeOP) where
  UDTOpCtx ('Tag tn ('AddEnumValAfter ev after))   = (SingE tn, SingE ev, SingE after)
  UDTOpCtx ('Tag tn ('AddEnumValBefore ev before)) = (SingE tn, SingE ev, SingE before)
  UDTOpCtx ('Tag tn ('AddEnumVal ev))              = (SingE tn, SingE ev)  

instance (UDTOpCtx t) => SingE (t :: TagHK (TypeName Symbol) UDTypeOP) where
  type Demote t = ChangeSetM M.PrimDDL
  fromSing (STag stypN (SAddEnumValAfter sEnVal sEnAfter)) = do
    let typN   = fromSing stypN
        pgt    = enumType (typN ^. typeName)
        enVal  = fromSing sEnVal
        enAft  = fromSing sEnAfter
    scInfo . typeNameInfoAt pgt . typeNameMap %=
      addEnumValAfter enVal enAft
    pure (M.alterAddEnumAfter (M.customTypeName $ mkDbTypeName (typN ^. typeName)) (coerce enVal) (coerce enAft))
  fromSing (STag stypN (SAddEnumValBefore sEnVal sEnBef)) = do
    let typN   = fromSing stypN
        pgt    = enumType (typN ^. typeName)
        enVal  = fromSing sEnVal
        enBef  = fromSing sEnBef
    scInfo . typeNameInfoAt pgt . typeNameMap %=
      addEnumValBefore enVal enBef
    pure (M.alterAddEnumBefore (M.customTypeName $ mkDbTypeName (typN ^. typeName)) (coerce enVal) (coerce enBef))
  fromSing (STag stypN (SAddEnumVal sEnVal)) = do
    let typN   = fromSing stypN
        enVal  = fromSing sEnVal
        pgt    = enumType (typN ^. typeName)
    scInfo . typeNameInfoAt pgt . typeNameMap %=
      addEnumVal enVal
    pure (M.alterAddEnum (M.customTypeName $ mkDbTypeName (typN ^. typeName)) (coerce enVal))

data CreateType = CreateType
data DropType   = DropType
data AlterType  = AlterType
  
data AddTable = AddTable
data RenameTable = RenameTable (Maybe Symbol)      -- ^ To tablename
data DropTable = DropTable


data AddColumn = AddColumn Symbol                  --  Column ref
                           (TagHK DbK DBTypeK)     --  type in DB with tag

data DropColumn = DropColumn Symbol                -- Column ref

data RenameColumn = RenameColumn Symbol            -- hask columnName
                                 Symbol            -- To tablename

data AlterColumn = SetNotNull Symbol               -- Column ref
                 | DropNotNull Symbol              -- Column ref
                 | ChangeType Symbol               -- Column ref
                              (TagHK DbK DBTypeK)  -- type in DB with tag
                 | AddDefault Symbol               -- Column ref
                 | DropDefault Symbol              -- Column ref

data AddConstraint = AddPrimaryKey [Symbol] -- Column refs
                                   Symbol   -- Constraint name
                   | AddUnique UniqueCT     -- ^ Unique constraint 
                   | AddCheck CheckCT       -- ^ Check constraint
                   | AddForeignKey (ForeignRef (TypeName Symbol))
                        -- ^ Foreignkey referencing a table with packagename, modulename and typename

data DropConstraint = DropPrimaryKey
                    | DropUnique Symbol     -- ^ Unique constraint name
                    | DropCheck Symbol      -- ^ Check constraint name
                    | DropForeignKey Symbol -- ^ Foreign key name

-- NOTE: A ChangeSet is supposed to be consistent
--       as long as it is run in a all or nothing fashion.
newtype ChangeSet = ChangeSet { statements :: [M.PrimDDL]
                              } deriving (Show)

mkHeadMigration :: (SingCtxSc sc) => Proxy sc -> ChangeSet
mkHeadMigration = scInfoChangeSet . headSchemaInfo

mkAllMigrations :: forall sc.
                   ( Schema sc
                   , BaseDatabase sc (Baseline sc)
                   , SingI (TagEach sc (Range (Baseline sc) (Version sc)))
                   , AllMigDbCtx (TagEach sc (Range (Baseline sc) (Version sc)))
                   , SingCtxBaseSc sc (Baseline sc)
                   ) => Proxy sc -> [ChangeSet]
mkAllMigrations pdb =
  let css = mkChangeSetState bsc
      bsc  = baseSchemaInfo pdb (Proxy :: Proxy (Baseline sc))
      bdCs = scInfoChangeSet bsc
  in  bdCs : M.evalState (runChangeSet (mkMigrations (sing :: Sing (TagEach sc (Range (Baseline sc) (Version sc)))))) css

{-
mkDatabaseInfoTill :: forall sc till.
                        ( Schema sc
                        , BaseDatabase db (Baseline db)
                        , SingI (TagEach db (Range (Baseline db) till))
                        , AllMigDbCtx (TagEach db (Range (Baseline db) till))
                        , SingCtxBaseSc db (Baseline db)
                        ) => Proxy db -> Proxy (till :: Nat) -> DatabaseInfo
mkDatabaseInfoTill pdb _ =
  let css = mkChangeSetState bdi
      bdi  = baseDatabaseInfo pdb (Proxy :: Proxy (Baseline db))
      newCss = M.execState (runChangeSet (mkMigrations (sing :: Sing (TagEach db (Range (Baseline db) till))))) css
  in  newCss ^. scInfo

mkMigrationsFrom :: forall db from.
                    ( Database db
                    , BaseDatabase db (Baseline db)
                    , SingI (TagEach db (Range from (Version db)))
                    , AllMigDbCtx (TagEach db (Range from (Version db)))
                    , SingCtxBaseSc db from
                    ) => Proxy db -> Proxy (from :: Nat) -> DatabaseInfo -> [ChangeSet]
mkMigrationsFrom pdb _ bdi =
  let css = mkChangeSetState bdi
  in  M.evalState (runChangeSet (mkMigrations (sing :: Sing (TagEach db (Range from (Version db)))))) css
-}      
-- Proxy db -> Step -> Maybe Step -> (DatabaseInfo, [ChangeSet]) -- DatabaseInfo including Step, changeset from Step { dirtyness check@ Step }
                                                                 -- + changeset till Step or Head.
type family AllMigDbCtx (tagHks :: [TagHK * Nat]) :: Constraint where
  AllMigDbCtx ('Tag db ver ': tagHks) = (MigDbCtx db ver , AllMigDbCtx tagHks)
  AllMigDbCtx '[]                     = ()

mkMigrations :: (AllMigDbCtx tagHks) => Sing (tagHks :: [TagHK * Nat]) -> ChangeSetM [ChangeSet]
mkMigrations (SCons tagHK@(STag {}) tagHKs) = do
  (:) <$> mkMigrationDb tagHK <*> mkMigrations tagHKs
mkMigrations SNil = pure []

type MigDbCtx sc ver = ( SchemaMigration sc ver
                       , SingI (TagEach '(sc, ver) (AlteredTables sc ver))
                       , SingI (TagEach '(sc, ver) (DropedTables sc ver))
                       , SingI (TagEach '(sc, ver) (CreatedTables sc ver))
                       , AllMigTableCtx (TagEach '(sc, ver) (AlteredTables sc ver))
                       , AllMigTableCtx (TagEach '(sc, ver) (DropedTables sc ver))
                       , AllMigTableCtx (TagEach '(sc, ver) (CreatedTables sc ver))                         
                       , AllMigTypeCtx (TagEach '(sc, ver) (CreatedTypes sc ver))
                       , AllMigTypeCtx (TagEach '(sc, ver) (AlteredTypes sc ver))

                       , SingI (TagEach '(sc, ver) (CreatedTypes sc ver))
                       , SingI (TagEach '(sc, ver) (DropedTypes sc ver))
                       , SingI (TagEach '(sc, ver) (AlteredTypes sc ver))
                       )

mkMigrationDb :: forall sc ver.
                  ( MigDbCtx sc ver
                  ) => Sing ('Tag sc ver) -> ChangeSetM ChangeSet
mkMigrationDb _ = do
  -- crtys <- mkMigrationTypes (sing :: Sing (TagEach '(sc, ver) (CreatedTypes sc ver)))
  -- drtys <- dropMigrationTypes (sing :: Sing (TagEach '(sc, ver) (DropedTypes sc ver)))  
  altys <- mkMigrationTypes (sing :: Sing (TagEach '(sc, ver) (AlteredTypes sc ver)))  
  crs <- mkMigrationTables (sing :: Sing (TagEach '(sc, ver) (CreatedTables sc ver)))
  dls <- mkMigrationTables (sing :: Sing (TagEach '(sc, ver) (DropedTables sc ver)))
  alts <- mkMigrationTables (sing :: Sing (TagEach '(sc, ver) (AlteredTables sc ver)))    
  pure $ coerce ({-crtys ++ drtys ++-} altys ++ crs ++ dls ++ alts)

type family AllMigTypeCtx (tagHks :: [TagHK (*, Nat) (TypeName Symbol)]) :: Constraint where
  AllMigTypeCtx ('Tag '(sc, ver) tab ': tagHks) = (MigTypeCtx sc ver tab, AllMigTypeCtx tagHks)
  AllMigTypeCtx '[]                             = ()

type MigTypeCtx sc ver ty = ( SingI (TagEach ty (TypeMigrations sc ver ty))
                            , SingE (TagEach ty (TypeMigrations sc ver ty))
                            )

-- dropMigrationTypes :: forall tagHks. (AllMigTypeCtx tagHks) => Sing (tagHks :: [TagHK (*, Nat) (TypeName Symbol)]) -> ChangeSetM [M.PrimDDL]
-- dropMigrationTypes (SCons tagHK@(STag (STuple {}) (STypeName {})) tagHKs) = do
--   (++) <$> dropMigrationType tagHK <*> dropMigrationTypes tagHKs
-- dropMigrationTypes SNil = pure []

-- dropMigrationType :: forall db ver ty.
--                      ( 
--                      ) => Sing ('Tag '(db, ver) ty) -> ChangeSetM [M.PrimDDL]
-- dropMigrationType _ =
--   -- crts <- sequence $ fromSing (sing :: Sing (TagEach ty (AddedColumn db ver ty)))
--   sequence $ fromSing (sing :: Sing (TagEach ty (TypeMigrations db ver ty)))

mkMigrationTypes :: forall tagHks. (AllMigTypeCtx tagHks) => Sing (tagHks :: [TagHK (*, Nat) (TypeName Symbol)]) -> ChangeSetM [M.PrimDDL]
mkMigrationTypes (SCons tagHK@(STag (STuple {}) (STypeName {})) tagHKs) = do
  (++) <$> mkMigrationType tagHK <*> mkMigrationTypes tagHKs
mkMigrationTypes SNil = pure []

mkMigrationType :: forall db ver ty.
                     ( MigTypeCtx db ver ty
                     ) => Sing ('Tag '(db, ver) ty) -> ChangeSetM [M.PrimDDL]
mkMigrationType _ =
  -- crts <- sequence $ fromSing (sing :: Sing (TagEach ty (AddedColumn db ver ty)))
  sequence $ fromSing (sing :: Sing (TagEach ty (TypeMigrations db ver ty)))


type family AllMigTableCtx (tagHks :: [TagHK (*, Nat) (TypeName Symbol)]) :: Constraint where
  AllMigTableCtx ('Tag '(db, ver) tab ': tagHks) = (MigTableCtx db ver tab, AllMigTableCtx tagHks)
  AllMigTableCtx '[]                             = ()

mkMigrationTables :: forall tagHks. (AllMigTableCtx tagHks) => Sing (tagHks :: [TagHK (*, Nat) (TypeName Symbol)]) -> ChangeSetM [M.PrimDDL]
mkMigrationTables (SCons tagHK@(STag (STuple {}) (STypeName {})) tagHKs) = do
  (++) <$> mkMigrationTable tagHK <*> mkMigrationTables tagHKs
mkMigrationTables SNil = pure []

type MigTableCtx db ver tab = ( TableMigration db tab ver
                              , SingI (AddedColumn db tab ver)
                              , SingE (AddedColumn db tab ver)
                              , SingI (MkDroppedColumn (DropedColumn db tab ver))
                              , SingE (MkDroppedColumn (DropedColumn db tab ver))
                              , SingI (MkRenamedColumn (RenamedColumn db tab ver)) 
                              , SingE (MkRenamedColumn (RenamedColumn db tab ver))                     
                              , SingE (AlteredColumn db tab ver)
                              , SingI (AlteredColumn db tab ver)
                              , SingE (AddedConstraint db tab ver)
                              , SingI (AddedConstraint db tab ver)
                              , SingE (DropedConstraint db tab ver)
                              , SingI (DropedConstraint db tab ver)
                              , SingE tab
                              , SingI tab
                              )

mkMigrationTable :: forall db ver tab.
                     ( MigTableCtx db ver tab
                     ) => Sing ('Tag '(db, ver) tab) -> ChangeSetM [M.PrimDDL]
mkMigrationTable _ = do
  setTabCtx (Proxy :: Proxy db) (Proxy :: Proxy tab) (Proxy :: Proxy ver)
  let curTab = fromSing (sing :: Sing (tab :: TypeName Symbol))
  acs <- sequence $ fromSing (sing :: Sing (AddedColumn db tab ver))
  dcs <- sequence $ fromSing (sing :: Sing (MkDroppedColumn (DropedColumn db tab ver)))
  rcs <- sequence $ fromSing (sing :: Sing (MkRenamedColumn (RenamedColumn db tab ver)))
  alcs <- sequence $ fromSing (sing :: Sing (AlteredColumn db tab ver))
  acts <- sequence $ fromSing (sing :: Sing (AddedConstraint db tab ver))
  dcts <- sequence $ fromSing (sing :: Sing (DropedConstraint db tab ver))
  mdbTabN <- preuse (scInfo . tableInfoAt curTab . tableName . dbName)
  let dbTabN = fromJust mdbTabN
  pure $ M.altering (M.tableName (curTab ^. typeName) dbTabN) (acs ++ dcs ++ rcs ++ alcs ++ acts ++  dcts)

type family MkDroppedColumn (dcs :: [Symbol]) where
  MkDroppedColumn (dc ': dcs) = 'DropColumn dc ': MkDroppedColumn dcs
  MkDroppedColumn '[]         = '[]

type family MkRenamedColumn (rns :: [(Symbol, Symbol)]) :: [RenameColumn] where
  MkRenamedColumn ('(cn, dcn) ': rns) = 'RenameColumn cn dcn ': MkRenamedColumn rns
  MkRenamedColumn '[]                 = '[]

data ChangeSetState = ChangeSetState { _scInfo               :: SchemaInfo
                                     , _currentTable         :: Maybe (TypeName T.Text)
                                     , _currentChecksAndDefs :: Maybe ChecksAndDefs
                                     } 

data ChecksAndDefs = forall db tab ver. ChecksAndDefs { _curChecks :: MigDBChecks db tab ver
                                                      , _curDefs   :: MigDBDefaults db tab ver
                                                      } 

mkChangeSetState :: SchemaInfo -> ChangeSetState
mkChangeSetState sci =
  ChangeSetState { _scInfo = sci
                 , _currentTable = Nothing
                 , _currentChecksAndDefs = Nothing
                 }

mkCurrentChecksAndDefs :: MigDBDefaults base tab ver -> MigDBChecks base tab ver -> ChecksAndDefs
mkCurrentChecksAndDefs mDefs mCks =
  ChecksAndDefs { _curChecks = mCks
                , _curDefs   = mDefs
                }
  
scInfo :: Functor f => (SchemaInfo -> f SchemaInfo) -> ChangeSetState -> f ChangeSetState
scInfo k t = fmap (\a -> t { _scInfo = a }) (k (_scInfo t))

currentTable :: Functor f => (TypeName T.Text -> f (TypeName T.Text)) -> ChangeSetState -> f ChangeSetState
currentTable k t = fmap (\a -> t { _currentTable = Just a }) (k (fromJust (_currentTable t)))

currentChecksAndDefs :: Functor f => (ChecksAndDefs -> f ChecksAndDefs) -> ChangeSetState -> f ChangeSetState
currentChecksAndDefs k t = fmap (\a -> t { _currentChecksAndDefs = Just a }) (k (fromJust (_currentChecksAndDefs t)))

newtype ChangeSetM a = ChangeSetM { runChangeSet :: M.StateT ChangeSetState I.Identity a }
                     deriving (Functor, Applicative, Monad, M.MonadState ChangeSetState)

type family AllTableMig (db :: *) (ver :: Nat) (tabs :: [TypeName Symbol]) :: Constraint where
  AllTableMig db ver (tab ': tabs) = (TableMigration db tab ver, AllTableMig db ver tabs)
  AllTableMig db ver '[]           = ()

data DBDiff = DBDiff
data TableDiff
data TypeDiff
data ColumnDiff
data ConstraintDiff


type family DiffDB' (db :: *) (bl :: *) (currver :: *) (basever :: *) where
  DiffDB' db bl currver curver = ()

scInfoChangeSet :: SchemaInfo -> ChangeSet
scInfoChangeSet sci = coerce (go sci :: [M.PrimDDL])
  where go scs = let tis = (scs ^. tableInfos . coerceL)
                 in map createType (scs ^. typeNameInfos) ++
                    concatMap (createTable sci) (tis :: [TableInfo])

createPKey :: TableInfo -> PrimaryKeyInfo -> M.AlterTable
createPKey tabInfo pkInfo = 
  let pkNameDb = pkInfo ^. pkeyName
      pkNameHs = ""
      pkColsDb = getBothColumnNames (tabInfo ^. columnInfo) (pkInfo ^. pkeyColumns)
  in  M.addPrimaryKey (M.constraintName pkNameDb pkNameHs) (map (uncurry M.columnName) pkColsDb)

createType :: TypeNameInfo -> M.PrimDDL
createType tni =
  let dbTypN = S.dbTypeName (tni ^. typeNameMap)
      cons   = S.dbConstructors (tni ^. typeNameMap)
  in M.createEnum (M.customTypeName dbTypN) (map coerce cons)

dropType :: TypeNameInfo -> M.PrimDDL
dropType tni =
  M.dropType (M.customTypeName (S.dbTypeName (tni ^. typeNameMap)))

createUnique :: TableInfo -> [UniqueInfo] -> [M.AlterTable]
createUnique tabInfo uqInfos = 
  let uqColsDb uqInfo = getBothColumnNames (tabInfo ^. columnInfo) (uqInfo ^. uqColumns)
      uqHsN = view (uqName . hsName)
      uqDbN = view (uqName . dbName)
      addUniq uqInfo  = M.addUnique (M.constraintName (uqHsN uqInfo) (uqDbN uqInfo))
                                    (map (uncurry M.columnName) (uqColsDb uqInfo))
  in  map addUniq uqInfos

dropUnique :: UniqueInfo -> M.AlterTable
dropUnique uqInfo =
  let uqHsN = uqInfo ^. (uqName . hsName)
      uqDbN = uqInfo ^. (uqName . dbName)
  in  M.dropUnique (M.constraintName uqHsN uqDbN)

createForeignKey :: SchemaInfo -> TableInfo -> [ForeignKeyInfo] -> [M.AlterTable]
createForeignKey sci tabInfo fkInfos = 
  let fkColsDb fkInfo = getBothColumnNames (tabInfo ^. columnInfo) (fkInfo ^. fkeyColumns)
      fkRefColsDb fkInfo = getBothColumnNames (fromJust (sci ^? tableInfoAt (fkInfo ^. fkeyRefTable) . columnInfo))
                                              (fkInfo ^. fkeyRefColumns)
      refTabDbN fki = fromJust $ sci ^? tableInfoAt (fki ^. fkeyRefTable) . tableName . dbName
      refTabHsN fki = fromJust $ sci ^? tableInfoAt (fki ^. fkeyRefTable) . tableName . hsName . typeName
      fkDbN = view (fkeyName . dbName)
      fkHsN = view (fkeyName . hsName)
      addFk fkInfo = M.addForeignKey (M.constraintName (fkHsN fkInfo) (fkDbN fkInfo))
                                     (map (uncurry M.columnName) (fkColsDb fkInfo))                            
                                     (M.tableName (refTabHsN fkInfo) (refTabDbN fkInfo))
                                     (map (uncurry M.columnName) (fkRefColsDb fkInfo))
  in  map addFk fkInfos

dropForeignKey :: ForeignKeyInfo -> M.AlterTable
dropForeignKey fkInfo =
  let fkDbN = fkInfo ^. (fkeyName . dbName)
      fkHsN = fkInfo ^. (fkeyName . hsName)
  in M.dropForeignKey (M.constraintName fkHsN fkDbN)

createCheck :: [CheckInfo] -> [M.AlterTable]
createCheck ckInfos = 
  let addCk ckInfo = M.addCheck (M.constraintName (ckHsN ckInfo) (ckDbN ckInfo))
                                (coerce (ckInfo ^. checkExp))
      ckHsN = view (checkName . hsName)
      ckDbN = view (checkName . dbName)
  in  map addCk ckInfos

dropCheck :: CheckInfo -> M.AlterTable
dropCheck ckInfo =
  let ckHsN = ckInfo ^. (checkName . hsName)
      ckDbN = ckInfo ^. (checkName . dbName)
  in M.dropCheck (M.constraintName ckHsN ckDbN)

createDefault ::  TableInfo -> [DefaultInfo] -> [M.AlterTable]
createDefault tabInfo defInfos =
  let addDef defInfo = M.addDefault (uncurry M.columnName (getBothColumnName (tabInfo ^. columnInfo) (defInfo ^. defaultOn)))
                                    (coerce (defInfo ^. defaultExp))
  in map addDef defInfos

dropDefault :: TableInfo -> DefaultInfo -> M.AlterTable
dropDefault ti dfi =
  M.dropDefault (uncurry M.columnName (getBothColumnName (ti ^. columnInfo) (dfi ^. defaultOn)))

createSequence :: TableInfo -> [SequenceInfo] -> [M.PrimDDL]
createSequence ti seqInfos =
  let createSeq (_ , dbColN) seqn = M.createSequence (M.sequenceName (seqn ^. hsName) (seqn ^. dbName))
      dbTabN = ti ^. tableName . dbName
      hsTabN = ti ^. tableName . hsName . typeName   
      ownSeq (hsColN, dbColN) seqn seqT = case seqT of
        SeqOwned -> Just $ M.addOwnerToSequence (M.tableName hsTabN dbTabN)
                                               (M.columnName hsColN dbColN)
                                               (M.sequenceName (seqn ^. hsName) (seqn ^. dbName))
                                                    
        _        -> Nothing
      seqNameGen = quoteT . doubleQuoteT
      seqnE dbSeqN = PQ.ConstExpr (PQ.Other (seqNameGen dbSeqN))
      nextValE dbSeqN = PQ.FunExpr "nextVal" [seqnE dbSeqN]
      defSeq (hsColN, dbColN) seqn seqT = case seqT of
        SeqOwned  -> Nothing
        SeqSerial ->
          let tN   = M.tableName hsTabN dbTabN
              colN = M.columnName hsColN dbColN
          in  Just (M.altering tN $ M.single $ M.addDefault colN (coerce (nextValE dbColN)))
  in  map (\si -> createSeq (getBothColumnName (ti ^. columnInfo) (si ^. seqOn)) (si ^. seqName)) seqInfos ++                         
      (catMaybes $ map (\si -> ownSeq (getBothColumnName (ti ^. columnInfo) (si ^. seqOn)) (si ^. seqName) (si ^. seqType)) seqInfos) ++
      concat (catMaybes $ map (\si -> defSeq (getBothColumnName (ti ^. columnInfo) (si ^. seqOn)) (si ^. seqName) (si ^. seqType)) seqInfos)
                    
dropSequence :: SequenceInfo -> M.PrimDDL
dropSequence sq =
  M.dropSequence (M.sequenceName (sq ^. seqName . hsName) (sq ^. seqName . dbName))
  
addNotNull :: [ColumnInfo] -> [M.AlterTable]
addNotNull notNullCols =
  let addNotNullCtx notNullCol = M.setNotNull (M.columnName (notNullCol ^. columnNameInfo . hsName) (notNullCol ^. columnNameInfo . dbName))
  in map addNotNullCtx notNullCols

columnName :: ColumnInfo -> M.ColName
columnName colInfo =
  M.columnName (colInfo ^. columnNameInfo . hsName) (colInfo ^. columnNameInfo . dbName)

columnType :: ColumnInfo -> M.ColType
columnType colInfo =
  M.columnType (colInfo ^. columnTypeName . dbType)

toMColumn :: ColumnInfo -> M.Column
toMColumn colInfo =
  let colN = columnName colInfo
      colT = columnType colInfo
  in  M.column colN colT

dropTable :: TableInfo -> [M.PrimDDL]
dropTable tabInfo =
  let dpk   = maybe [] M.single $ fmap (\pki -> M.dropPrimaryKey (M.constraintName (pki ^. pkeyName) (pki ^. pkeyName))) (tabInfo ^. primaryKeyInfo)
      duqs  = map dropUnique (tabInfo ^. uniqueInfo)
      dfks  = map dropForeignKey (tabInfo ^. foreignKeyInfo)
      dcks  = map dropCheck (tabInfo ^. checkInfo)
      ddefs = map (dropDefault tabInfo) (tabInfo ^. defaultInfo)
      dseqs = map dropSequence (tabInfo ^. sequenceInfo)
      tabn  = M.tableName (tabInfo ^. tableName . hsName . typeName) (tabInfo ^. tableName . dbName)
  in  (M.altering tabn $
        dpk ++ duqs ++ dfks ++ dcks ++ ddefs)            ++
      [M.dropTable (coerce tabn)]                        ++
      dseqs
      
createTable :: SchemaInfo -> TableInfo -> [M.PrimDDL]
createTable sci tabInfo =          
  let tabNDb  = tabInfo ^. tableName . dbName
      tabNHs  = tabInfo ^. tableName . hsName . typeName
      createTab = M.createTable (M.tableName tabNHs tabNDb) (coerce (map toMColumn (tabInfo ^. columnInfo)))
      addPks  = let mpkInfo  = tabInfo ^. primaryKeyInfo                             
                in  maybe [] M.single (fmap (createPKey tabInfo) mpkInfo)
      addUqs  = createUnique tabInfo (tabInfo ^. uniqueInfo)
      addFks  = createForeignKey sci tabInfo (tabInfo ^. foreignKeyInfo)
      addChks = createCheck (tabInfo ^. checkInfo)
      addDefs = createDefault tabInfo (tabInfo ^. defaultInfo)                        
      addNotNullChks = addNotNull (getNonNullableColumns (tabInfo ^. columnInfo))
      tN   = M.tableName tabNHs tabNDb
  in (:) createTab $
         createSequence tabInfo (tabInfo ^. sequenceInfo) ++
         (M.altering tN $ concat [ addPks  
                                 , addUqs
                                 , addFks
                                 , addChks
                                 , addDefs
                                 , addNotNullChks
                                 ]
          )
        
type family TyCxts (db :: *) (tys :: [*]) :: Constraint where
  TyCxts db (ty ': ts) = ( -- ShowDBType (DB db) (GetDBTypeRep (DB db) ty)
                           SingAttrs db (GetTypeFields ty)
                         , TyCxts db ts
                         )
  TyCxts db '[]        = ()

{-  
mkMigrationTypes :: forall db tys.
                    ( Database db
                    , TyCxts db tys
                    ) => Proxy (db :: *) -> Sing (tys :: [*]) -> [M.PrimDDL]
mkMigrationTypes _ SNil             = []
mkMigrationTypes pxyDB (SCons ty tys) = mkMigrationType pxyDB ty ++ mkMigrationTypes pxyDB tys

mkMigrationType :: forall db ty.
                  ( Database db
                  , ShowDBType (DB db) (GetDBTypeRep (DB db) ty)
                  , SingAttrs db (GetTypeFields ty)
                  ) => Proxy (db :: *) -> Sing (ty :: *) -> [M.PrimDDL]
mkMigrationType _ _
  = let tyName = showDBType (Proxy @(DB db)) (Proxy @(GetDBTypeRep (DB db) ty))
        tyAttrHList = singAttrs (Proxy @db) (Proxy :: Proxy (GetTypeFields ty))
        migTypes = case toTypeAttr tyAttrHList of
          M.EnumAttr cnames  -> [M.CreateEnum (coerce tyName) (coerce cnames)]
          M.ProdAttr cols    -> [M.CreateType (coerce tyName) (coerce cols)]
          M.SumAttr conAttrs ->
            let tyTag = (if T.last tyName == '"' then T.init tyName else tyName) `T.append` "_tags\""
                createSumTags cons = M.CreateEnum (coerce tyTag) $ (coerce $ fmap fst cons)
                sumTagTys = createSumTags conAttrs
                conTyName cn = M.TypeName ("\"" `T.append` cn `T.append` "_con\"")
                createConTy (cn, flds) = M.CreateType (conTyName cn) flds
                sumTy = M.CreateType (M.TypeName tyName)
                                     (M.Column (coerce ("tag" :: T.Text)) (coerce tyTag)
                                          : fmap (\(M.ColName cn,_) -> M.Column (coerce cn) (coerce $ conTyName cn)) conAttrs
                                          )
            in (sumTagTys : fmap createConTy (coerce conAttrs)) ++ [sumTy]
    in concat [ migTypes
              ]
-}

diffPKInfo :: TableInfo -> TableInfo -> Maybe PrimaryKeyInfo -> Maybe PrimaryKeyInfo -> [M.AlterTable]
diffPKInfo oldTi newTi mOldPki mNewPki =
  case (mOldPki, mNewPki) of
    (Just oldPki, Just newPki) -> case oldPki `isSamePkAs` newPki of
                                True  -> []
                                False -> [ M.dropPrimaryKey (M.constraintName (oldPki ^. pkeyName) (oldPki ^. pkeyName))
                                         , createPKey newTi newPki
                                         ]
    (Nothing, Just newPki) -> M.single $ createPKey newTi newPki
    (Just oldPki, Nothing) -> M.single $ M.dropPrimaryKey (M.constraintName (oldPki ^. pkeyName) (oldPki ^. pkeyName))
    (Nothing, Nothing)   -> []

  where isSamePkAs oldPki newPki =
          (oldPki ^. pkeyName) == (newPki ^. pkeyName) &&
          getDbColumnNames (oldTi ^. columnInfo) (oldPki ^. pkeyColumns) ==
          getDbColumnNames (newTi ^. columnInfo) (newPki ^. pkeyColumns)

diffUqInfo :: TableInfo -> TableInfo -> UniqueInfo -> UniqueInfo -> [M.AlterTable]
diffUqInfo oldTi newTi oldUq newUq =
  case oldUq `isSameUqAs` newUq of
    True  -> []
    False ->   dropUnique oldUq
             : createUnique newTi [newUq]
  
  where isSameUqAs oldUq newUq =
          getDbColumnNames (oldTi ^. columnInfo) (oldUq ^. uqColumns) ==
          getDbColumnNames (newTi ^. columnInfo) (newUq ^. uqColumns) 

diffFkInfo :: SchemaInfo -> SchemaInfo -> TableInfo -> TableInfo -> ForeignKeyInfo -> ForeignKeyInfo -> [M.AlterTable]
diffFkInfo oldSi newSi oldTi newTi oldFk newFk =
  case oldFk `isSameFkAs` newFk of
    True  -> []
    False -> dropForeignKey oldFk : createForeignKey newSi newTi [newFk]
  
  where isSameFkAs oldFk newFk =
          getDbColumnNames (oldTi ^. columnInfo) (oldFk ^. fkeyColumns) ==
          getDbColumnNames (oldTi ^. columnInfo) (newFk ^. fkeyColumns) &&
          getDbColumnNames (oldSi ^. tableInfoAt (oldFk ^. fkeyRefTable) . columnInfo) (oldFk ^. fkeyRefColumns) ==
          getDbColumnNames (newSi ^. tableInfoAt (newFk ^. fkeyRefTable) . columnInfo) (newFk ^. fkeyRefColumns)

diffCheck :: CheckInfo -> CheckInfo -> [M.AlterTable]
diffCheck oldCki newCki =
  case oldCki `isSameCheckAs` newCki of
    True -> []
    False -> dropCheck oldCki
           : createCheck [newCki]
  where isSameCheckAs oldCki newCki =
          (oldCki ^. checkExp) == (newCki ^. checkExp)
          
diffDefault :: TableInfo -> TableInfo -> DefaultInfo -> DefaultInfo -> [M.AlterTable]
diffDefault oldTi newTi oldDfi newDfi =
  case oldDfi `isSameDefAs` newDfi of
     True  -> []
     False -> dropDefault oldTi oldDfi
            : createDefault newTi [newDfi]

  where isSameDefAs oldDfi newDfi =
          (oldDfi ^. defaultExp) == (newDfi ^. defaultExp)

-- TODO: 
diffSequence :: TableInfo -> TableInfo -> SequenceInfo -> SequenceInfo -> [M.PrimDDL]
diffSequence = undefined

diffTypeInfo :: TypeNameInfo -> TypeNameInfo -> [M.PrimDDL]
diffTypeInfo oldTni newTni =
  case oldTni `isSameTypeNameAs` newTni of
    True  -> []
    False -> [ dropType oldTni
             , createType newTni
             ]

  where isSameTypeNameAs o n =
          (o ^. typeNameMap) == (n ^. typeNameMap)

diffColumnInfo :: ColumnInfo -> ColumnInfo -> [M.AlterTable]
diffColumnInfo oldCi newCi =
  diffNulls oldCi newCi ++
  diffType  oldCi newCi

  where diffNulls oldCi newCi =
          case isNullable (oldCi ^. columnTypeName . dbType) == isNullable (newCi ^. columnTypeName . dbType) of
            False -> case isNullable (oldCi ^. columnTypeName . dbType) of
              False -> M.single $ M.dropNotNull (M.columnName (newCi ^. columnNameInfo . hsName)  (newCi ^. columnNameInfo . dbName))
              True  -> M.single $ M.setNotNull (M.columnName (newCi ^. columnNameInfo . hsName)  (newCi ^. columnNameInfo . dbName))
            _     -> []
        diffType oldCi newCi =
          case (oldCi ^. columnTypeName) == (newCi ^. columnTypeName) of
            True  -> []
            False -> M.single $ M.changeType (M.columnName (newCi ^. columnNameInfo . hsName) (newCi ^. columnNameInfo . dbName))
                                            (coerce (newCi ^. columnTypeName . dbType))
          
diffTableInfo :: SchemaInfo -> SchemaInfo -> TableInfo -> TableInfo -> [M.PrimDDL]
diffTableInfo oldSi newSi oldTi newTi =
  let (oldCols, newCols, pairedCols) = pairBy (eqBy (columnNameInfo . dbName))
                                              (oldTi ^. columnInfo)
                                              (newTi ^. columnInfo)
      colDiff = concatMap (uncurry diffColumnInfo) pairedCols
      pkDiff  = diffPKInfo oldTi newTi (oldTi ^. primaryKeyInfo) (newTi ^. primaryKeyInfo)
      (oldUqs, newUqs, pairedUqs) = pairBy (eqBy (uqName . dbName))
                                           (oldTi ^. uniqueInfo)
                                           (newTi ^. uniqueInfo)
      uqDiff  = concatMap (uncurry (diffUqInfo oldTi newTi)) pairedUqs                                         
      (oldFks, newFks, pairedFks) = pairBy (eqBy (fkeyName . dbName))
                                           (oldTi ^. foreignKeyInfo)
                                           (newTi ^. foreignKeyInfo)
                                         
      fkDiff  = concatMap (uncurry (diffFkInfo oldSi newSi oldTi newTi)) pairedFks
      (oldDefs, newDefs, pairedDefs) = pairBy (\di1 di2 -> getDbColumnName (oldTi ^. columnInfo) (di1 ^. defaultOn) ==
                                                           getDbColumnName (newTi ^. columnInfo) (di2 ^. defaultOn)
                                              )
                                              (oldTi ^. defaultInfo)
                                              (newTi ^. defaultInfo)                                         
      defDiff  = concatMap (uncurry (diffDefault oldTi newTi)) pairedDefs
      (oldCks, newCks, pairedCks) = pairBy (eqBy (checkName . dbName))
                                           (oldTi ^. checkInfo)
                                           (newTi ^. checkInfo)                                         
      cksDiff  = concatMap (uncurry diffCheck) pairedCks
      (oldSqs, newSqs, pairedSqs) = pairBy (eqBy (seqName . dbName))
                                           (oldTi ^. sequenceInfo)
                                           (newTi ^. sequenceInfo)                                         
      seqDiff = concatMap (uncurry (diffSequence oldTi newTi)) pairedSqs
      
  in  (M.altering (M.tableName (oldTi ^. tableName . hsName . typeName) (oldTi ^. tableName . dbName)) $
        map M.dropColumn (map columnName oldCols) ++
        map (M.addColumn . toMColumn) newCols ++
        colDiff                               ++
        pkDiff                                ++
        map dropUnique oldUqs                 ++
        createUnique newTi newUqs             ++
        uqDiff                                ++
        map dropForeignKey oldFks             ++
        createForeignKey newSi newTi newFks   ++
        fkDiff                                ++
        map (dropDefault oldTi) oldDefs       ++
        createDefault newTi newDefs           ++
        defDiff                               ++
        map dropCheck oldCks                  ++
        createCheck newCks                    ++
        cksDiff)                              ++
      map dropSequence oldSqs                 ++
      createSequence newTi newSqs             ++
      seqDiff
      
diffSchemaInfo :: SchemaInfo -> SchemaInfo -> ChangeSet
diffSchemaInfo oldSi newSi = coerce $ 
  let (old, new, paired) = pairBy (eqBy (tableName . dbName))
                                  (oldSi ^. tableInfos . coerceL)
                                  (newSi ^. tableInfos . coerceL)
      tabDiff = map (uncurry (diffTableInfo oldSi newSi)) paired
      (oldT, newT, pairedT) = pairBy (\s1 s2 -> dbTypeName (s1 ^. typeNameMap) ==
                                                dbTypeName (s2 ^. typeNameMap)
                                     )
                                     (oldSi ^. typeNameInfos)
                                     (newSi ^. typeNameInfos)
      typeDiff = map (uncurry diffTypeInfo) pairedT
  in  map createType newT               ++
      map dropType   oldT               ++
      concat typeDiff                   ++
      concatMap (createTable newSi) new ++
      concatMap dropTable old           ++
      concat tabDiff
            
pairBy :: (a -> a -> Bool) -> [a] -> [a] -> ([a], [a], [(a, a)])
pairBy eqBy old new =  go ([], new, []) old
  where go (old', new', paired) (x : xs) =
          case L.find (eqBy x) new of
            Just e  -> go (old', L.deleteBy eqBy x new', (x, e) : paired) xs
            Nothing -> go (x : old', new', paired) xs
        go (old', new', paired) [] = (reverse old', new', reverse paired)

doubleQuoteT :: T.Text -> T.Text
doubleQuoteT t = "\"" <> t <> "\""

quoteT :: T.Text -> T.Text
quoteT t = "\'" <> t <> "\'"

--
class ( BaseTable sc tab ver
      , Schema sc
      , Database (SchemaDB sc)
        
      , SingI (BasePrimaryKeyName sc tab ver)
      , SingE (BasePrimaryKeyName sc tab ver)
       
      , SingE (GetBaseColumnInfo (DB (SchemaDB sc))
               (BaseColumns sc tab ver))
      , SingI (GetBaseColumnInfo (DB (SchemaDB sc))
               (BaseColumns sc tab ver))

      , SingE (BasePrimaryKey sc tab ver)
      , SingI (BasePrimaryKey sc tab ver)

      , SingE (BaseUnique sc tab ver)
      , SingI (BaseUnique sc tab ver)

      , SingE (BaseUniqueNames sc tab ver)
      , SingI (BaseUniqueNames sc tab ver)
        
      , SingE (BaseForeignKey sc tab ver)
      , SingI (BaseForeignKey sc tab ver)

      , SingE (BaseForeignKeyNames sc tab ver)
      , SingI (BaseForeignKeyNames sc tab ver)
        
      , SingI (BaseTableSequence sc tab ver)
      , SingE (BaseTableSequence sc tab ver)

      , SingI (BaseCheckNames sc tab ver)
      , SingE (BaseCheckNames sc tab ver)

      , SingI (BaseSequenceNames sc tab ver)
      , SingE (BaseSequenceNames sc tab ver)

      , SingI (BaseColumnNames sc tab ver)
      , SingE (BaseColumnNames sc tab ver)
        
      , SingI tab
      , SingE tab
      ) => SingCtxBase sc ver tab where

instance ( BaseTable sc tab ver
         , Schema sc
         , Database (SchemaDB sc)
           
         , SingE (GetBaseColumnInfo (DB (SchemaDB sc))
                  (BaseColumns sc tab ver))
         , SingI (GetBaseColumnInfo (DB (SchemaDB sc))
                  (BaseColumns sc tab ver))
           
         , SingI (BasePrimaryKeyName sc tab ver)
         , SingE (BasePrimaryKeyName sc tab ver)
           
         , SingE (BasePrimaryKey sc tab ver)
         , SingI (BasePrimaryKey sc tab ver)

         , SingE (BaseUnique sc tab ver)
         , SingI (BaseUnique sc tab ver)

         , SingE (BaseUniqueNames sc tab ver)
         , SingI (BaseUniqueNames sc tab ver)
                      
         , SingE (BaseForeignKey sc tab ver)
         , SingI (BaseForeignKey sc tab ver)

         , SingE (BaseForeignKeyNames sc tab ver)
         , SingI (BaseForeignKeyNames sc tab ver)
                      
         , SingI (BaseTableSequence sc tab ver)
         , SingE (BaseTableSequence sc tab ver)

         , SingI (BaseCheckNames sc tab ver)
         , SingE (BaseCheckNames sc tab ver)

         , SingI (BaseSequenceNames sc tab ver)
         , SingE (BaseSequenceNames sc tab ver)

         , SingI (BaseColumnNames sc tab ver)
         , SingE (BaseColumnNames sc tab ver)
           
         , SingI tab
         , SingE tab           
         ) => SingCtxBase sc ver tab

class ( BaseSchema sc ver
      , SingE (BaseSchema sc ver)
      , SingI (BaseSchema sc ver)
      , SingI (BaseTables sc ver)        
      , Schema sc
      , SingI (GetPMT (Rep sc))
      , SingE (GetPMT (Rep sc))
      , All (SingCtxBase sc ver) (BaseTables sc ver)
      , AllBaseUDCtx sc ver (BaseTypes sc ver)
      , SingI (BaseTypes sc ver)
      , KnownSymbol (BaseSchemaName sc ver)
      ) => SingCtxBaseSc sc ver where

instance ( BaseSchema sc ver
         , SingE (BaseSchema sc ver)
         , SingI (BaseSchema sc ver)
         , SingI (BaseTables sc ver)
         , Schema sc
         , SingI (GetPMT (Rep sc))
         , SingE (GetPMT (Rep sc))
         , All (SingCtxBase sc ver) (BaseTables sc ver)
         , AllBaseUDCtx sc ver (BaseTypes sc ver)
         , SingI (BaseTypes sc ver)
         , KnownSymbol (BaseSchemaName sc ver)
         ) => SingCtxBaseSc sc ver where  

type family AllBaseUDCtx sc ver tys :: Constraint where
  AllBaseUDCtx sc ver (ty ': tys) = ( BaseUDType sc ver ty
                                    , SingI (BaseTypeMappings sc ver ty)
                                    , UDTCtx (BaseTypeMappings sc ver ty)
                                    , AllBaseUDCtx sc ver tys
                                    )
  AllBaseUDCtx sc ver '[]         = ()                                  
  
baseTypeInfo :: forall sc ver.
                 ( AllBaseUDCtx sc ver (BaseTypes sc ver)
                 , SingI (BaseTypes sc ver)
                 ) => Proxy sc -> Proxy ver -> [TypeNameInfo]
baseTypeInfo pdb pver = baseTypeNameInfos pdb pver (sing :: Sing (BaseTypes sc ver))

baseTypeNameInfos :: (AllBaseUDCtx sc ver xs) => Proxy sc -> Proxy ver -> Sing (xs :: [TypeName Symbol]) -> [TypeNameInfo]
baseTypeNameInfos pdb pver (SCons x xs) =
  baseTypeNameInfo pdb pver x : baseTypeNameInfos pdb pver xs
baseTypeNameInfos _ _ SNil =
  []

baseTypeNameInfo :: forall db ver ty.
                      ( BaseUDType db ver ty
                      , SingI (BaseTypeMappings db ver ty)
                      , UDTCtx (BaseTypeMappings db ver ty)
                      ) => Proxy db -> Proxy ver -> Sing (ty :: TypeName Symbol) -> TypeNameInfo
baseTypeNameInfo _ _ sty =
  let tnm = fromSing (sing :: Sing (BaseTypeMappings db ver ty))
      pgt = enumType (fromSing sty ^. typeName)
  in  mkTypeNameInfo pgt tnm

{-  
baseTypeNameInfo :: forall db ver.
  ( SingI (TypeNamesWithPMT (BaseTypeNames db))
  , SingE (TypeNamesWithPMT (BaseTypeNames db))
  , SingI (GetTypeMappings db)
  , SingE (GetTypeMappings db)
  , SingI (BaseTypes db ver)
  , SingE (BaseTypes db ver)
  , All (UDType db) (Types db)
  , All Generic     (Types db)
  ) => Proxy db -> [TypeNameInfo]
baseTypeNameInfo pdb =
  let typNMap     = fromSing (sing :: Sing (TypeNamesWithPMT (TypeNames db)))
      typMappings = fromSing (sing :: Sing (GetTypeMappings db))
      typNs       = fromSing (sing :: Sing (GetPMTs (Types db)))
  in  map (typeMapOne typNMap typMappings) typNs
-}
  
baseSchemaInfo :: forall sc ver.
                ( SingCtxBaseSc sc ver
                ) => Proxy (sc :: *) -> Proxy ver -> SchemaInfo
baseSchemaInfo psc pver =
  let btys = baseTypeInfo psc pver
      schn = mkEntityName (coerce (fromSing (sing :: Sing (GetPMT (Rep sc)))))
                          (fromSing (sing :: Sing (BaseSchemaName sc ver)))             
  in mkSchemaInfo schn btys 0 0 (coerce (baseTableInfos psc pver (sing :: Sing (BaseTables sc ver))))

baseTableInfos :: (All (SingCtxBase db ver) xs) => Proxy (db :: *) -> Proxy (ver :: Nat) -> Sing (xs :: [TypeName Symbol]) -> [TableInfo]
baseTableInfos pdb pver (SCons st@(STypeName {}) sxs) =
  baseTableInfo pdb st pver : baseTableInfos pdb pver sxs
baseTableInfos _ _ _ = []  
                           
baseTableInfo :: forall db tab ver.
             ( SingCtxBase db ver tab
             ) => Proxy db -> Sing tab -> Proxy ver -> TableInfo
baseTableInfo db stab ver =
  let btn = baseTabNameInfo db tab ver
      bcn = baseColInfos db tab ver
      tab = Proxy :: Proxy tab
  in mkTableInfo (basePkInfo db tab ver btn)
                 (baseFkInfo db tab ver btn)
                 (baseDefInfo db tab ver btn bcn)
                 (baseCksInfo db tab ver btn bcn)
                 (baseUqInfo db tab ver btn)
                 (baseSeqsInfo db tab ver btn)
                 btn
                 bcn

basePkInfo :: forall db tab ver.
          ( BaseTable db tab ver
          , SingE (BasePrimaryKeyName db tab ver)
          , SingI (BasePrimaryKeyName db tab ver)
          , SingE (BasePrimaryKey db tab ver)
          , SingI (BasePrimaryKey db tab ver)
          ) => Proxy db -> Proxy tab -> Proxy ver -> EntityNameWithType -> Maybe PrimaryKeyInfo
basePkInfo _ _ _ et =
  let pkCols = fromSing (sing :: Sing (BasePrimaryKey db tab ver))
      pkDefN = let hsn = et ^. hsName . typeName
               in  mkDbKeyName (PkName hsn pkCols)      
  in  case pkCols of
    [] -> Nothing
    _  -> let dbn = maybe pkDefN id (fromSing (sing :: Sing (BasePrimaryKeyName db tab ver)))
          in Just $ mkPrimaryKeyInfo dbn pkCols

baseFkInfo :: forall db tab ver.
          ( BaseTable db tab ver
          , SingE (BaseForeignKey db tab ver)
          , SingI (BaseForeignKey db tab ver)

          , SingE (BaseForeignKeyNames db tab ver)
          , SingI (BaseForeignKeyNames db tab ver)
          ) => Proxy db -> Proxy tab -> Proxy ver -> EntityNameWithType -> [ForeignKeyInfo]
baseFkInfo _ _ _ et = 
  let fkds = fromSing (sing :: Sing (BaseForeignKey db tab ver))
      fkNameMappings = fromSing (sing :: Sing (BaseForeignKeyNames db tab ver))
  in map (fkInfoOne et fkNameMappings) fkds     
        
baseUqInfo :: forall db tab ver.
          ( BaseTable db tab ver
          , SingE (BaseUnique db tab ver)
          , SingI (BaseUnique db tab ver)
          , SingE (BaseUniqueNames db tab ver)
          , SingI (BaseUniqueNames db tab ver)
          ) => Proxy db -> Proxy tab -> Proxy ver -> EntityNameWithType -> [UniqueInfo]
baseUqInfo _ _ _ et =
  let uniqs = fromSing (sing :: Sing (BaseUnique db tab ver))
      uniqNameMappings = fromSing (sing :: Sing (BaseUniqueNames db tab ver))      
  in  map (uniqWithMapping et uniqNameMappings) uniqs

baseDefInfo :: forall db tab ver.
           ( BaseTable db tab ver
           ) => Proxy db -> Proxy tab -> Proxy ver -> EntityNameWithType -> [ColumnInfo] -> [DefaultInfo]
baseDefInfo _ _ _ _et cis = case (baseDefaults :: BaseDBDefaults db tab ver) of
  BaseDBDefaults hl -> map mkDefInfo (happlyDefExprs cis hl)

  where mkDefInfo (n, expr) = mkDefaultInfo n expr

baseCksInfo :: forall db tab ver.
           ( BaseTable db tab ver
           , SingI (BaseCheckNames db tab ver)
           , SingE (BaseCheckNames db tab ver)
           ) => Proxy db -> Proxy tab -> Proxy ver -> EntityNameWithType -> [ColumnInfo] -> [CheckInfo]
baseCksInfo _ _ _ et cis = case (baseChecks :: BaseDBChecks db tab ver) of
  BaseDBChecks hls -> map (checkInfoOne et chkNameMaps) (happlyChkExpr cis chkNameMaps hls)
  where chkNameMaps = fromSing (sing :: Sing (BaseCheckNames db tab ver))

baseSeqsInfo :: forall db tab ver.
            ( BaseTable db tab ver
            , SingI (BaseTableSequence db tab ver)
            , SingE (BaseTableSequence db tab ver)

            , SingI (BaseSequenceNames db tab ver)
            , SingE (BaseSequenceNames db tab ver)
            ) => Proxy db -> Proxy tab -> Proxy ver -> EntityNameWithType -> [SequenceInfo]
baseSeqsInfo _ _ _ et =
  let seqs = fromSing (sing :: Sing (BaseTableSequence db tab ver))
      seqNameMappings = fromSing (sing :: Sing (BaseSequenceNames db tab ver))
  in  map (mkSeqInfoOne et seqNameMappings) seqs

baseTabNameInfo :: forall tab db ver.
               ( BaseTable db tab ver
               , SingI tab
               , SingE tab
               ) => Proxy db -> Proxy tab -> Proxy ver -> EntityNameWithType
baseTabNameInfo _ _ _ =
  let tabTN = fromSing (sing :: Sing tab)
      dbTabN = mkDbTabName tabTN
  in mkEntityName tabTN dbTabN
               
baseColInfos :: forall tab sc ver.
            ( BaseTable sc tab ver
            , Schema sc
            , Database (SchemaDB sc)
            , SingE (GetBaseColumnInfo (DB (SchemaDB sc)) (BaseColumns sc tab ver))
            , SingI (GetBaseColumnInfo (DB (SchemaDB sc)) (BaseColumns sc tab ver))
            , SingI (BaseColumnNames sc tab ver)
            , SingE (BaseColumnNames sc tab ver)
            ) => Proxy sc -> Proxy tab -> Proxy ver -> [ColumnInfo]
baseColInfos _ _ _ =
  let hsns = fromSing (sing :: Sing (GetBaseColumnInfo (DB (SchemaDB sc)) (BaseColumns sc tab ver)))
      colMap = fromSing (sing :: Sing (BaseColumnNames sc tab ver))      
  in  map (colInfoOne colMap) hsns

type family GetBaseColumnInfo (db :: DbK) (xs :: [(Symbol, DBTypeK)]) where
  GetBaseColumnInfo db ('(fld, x) ': xs) = 
    '(TagTypeInfo db x, fld) ': GetBaseColumnInfo db xs
  GetBaseColumnInfo db '[] =
    '[]

setTabCtx :: forall db tab ver.
             ( TableMigration db tab ver
             , SingI tab
             ) => Proxy db -> Proxy (tab :: TypeName Symbol) -> Proxy ver -> ChangeSetM ()
setTabCtx _ _ _ = do
  let curTab = fromSing (sing :: Sing (tab :: TypeName Symbol))
  currentTable .= curTab
  currentChecksAndDefs .=
    mkCurrentChecksAndDefs (migDefaults :: MigDBDefaults db tab ver)
                           (migChecks :: MigDBChecks db tab ver)  
  pure ()

migDefInfo :: ChecksAndDefs -> HaskName -> EntityNameWithType -> [ColumnInfo] -> DefaultInfo
migDefInfo (ChecksAndDefs _ defs) hsColn _et cis = case defs of
  MigDBDefaults hl -> mkDefInfo (fromJust (L.find (\x -> fst x == hsColn) (happlyDefExprs cis hl)))

  where mkDefInfo (n, expr) = mkDefaultInfo n expr

migCksInfo :: ChecksAndDefs -> HaskName -> EntityNameWithType -> [ColumnInfo] -> CheckInfo
migCksInfo (ChecksAndDefs cks _) hsCkn et cis = case cks of
  MigDBChecks hls -> checkInfoOne et [] (fromJust (L.find (\x -> fst x == hsCkn) (happlyChkExpr cis [] hls)))
  where chkNameMaps = []

