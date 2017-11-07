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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableSuperClasses #-}


module DBRecord.Migration
       ( -- mkMigration
         renderChangeSets
       , mkAllMigrations
       -- , diffMigration
       , BaseTable (..)
       , module DBRecord.Migration
       ) where

import qualified Database.PostgreSQL.Simple  as PGS
import qualified DBRecord.Internal.Migration as M
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
import DBRecord.Internal.Migration.Validation ()-- (getSchemaInfo)
import Data.Monoid ((<>))
import qualified Data.List as L
import qualified Control.Monad.State as M
import qualified Control.Monad.Reader as M
import qualified Data.Functor.Identity as I
import DBRecord.Internal.Lens
import GHC.Generics
import Data.Typeable (Typeable)

class BaseDatabase (db :: *) (ver :: Nat) where
  type BaseSchema db ver :: Symbol
  type BaseSchema db ver = "public"
  
  type BaseTables db ver :: [TypeName Symbol]
  type BaseTables db ver = '[]
  
  type BaseTypes db ver :: [TypeName Symbol]
  type BaseTypes db ver = '[]

class BaseTable (db :: *) (tab :: TypeName Symbol) (ver :: Nat) where
  type BaseColumns db tab ver :: [(Symbol, DBTypeK)]
  type BaseColumns db tab ver = '[]

  type BaseColumnNames db tab ver :: [(Symbol, Symbol)]
  type BaseColumnNames db tab ver = '[]

  type BasePrimaryKeyName db tab ver :: Maybe Symbol
  type BasePrimaryKeyName db tab ver = 'Nothing
  
  type BasePrimaryKey db tab ver :: [Symbol]
  type BasePrimaryKey db tab ver = '[]
  
  type BaseForeignKey db tab ver :: [ForeignRef (TypeName Symbol)]
  type BaseForeignKey db tab ver = '[]

  type BaseForeignKeyNames db tab ver :: [(Symbol, Symbol)]
  type BaseForeignKeyNames db tab ver = '[]
  
  type BaseUnique db tab ver :: [UniqueCT]
  type BaseUnique db tab ver = '[]

  type BaseUniqueNames db tab ver :: [(Symbol, Symbol)]
  type BaseUniqueNames db tab ver = '[]
  
  type BaseDefaultedCols db tab ver :: [Symbol]
  type BaseDefaultedCols db tab ver = '[]
  
  type BaseCheck db tab ver :: [CheckCT]
  type BaseCheck db tab ver = '[]

  type BaseCheckNames db tab ver :: [(Symbol, Symbol)]
  type BaseCheckNames db tab ver = '[]
  
  type BaseTableSequence db tab ver :: [Sequence]
  type BaseTableSequence db tab ver = '[]

  type BaseSequenceNames db tab ver :: [(Symbol, Symbol)]
  type BaseSequenceNames db tab ver = '[]

  baseDefaults :: BaseDBDefaults db tab ver
  baseChecks   :: BaseDBChecks db tab ver

  baseDefaults = BaseDBDefaults Nil
  baseChecks   = BaseDBChecks Nil

data BaseDBDefaults (db :: *) (tab :: TypeName Symbol) (ver :: Nat) =
  forall xs. (All KnownSymbol xs) => BaseDBDefaults (HList (Def db tab) xs)

baseDbDefaults :: forall tab db ver xs.
              ( All KnownSymbol xs
              ) => HList (Def db tab) xs -> BaseDBDefaults db tab ver
baseDbDefaults = BaseDBDefaults

data BaseDBChecks (db :: *) (tab :: TypeName Symbol) (ver :: Nat) =
  forall chks. (All CheckExpr chks) => BaseDBChecks (HList (Chk db tab) chks)

baseDbChecks :: forall tab (db :: *) ver chks.
            ( All CheckExpr chks
            ) => HList (Chk db tab) chks -> BaseDBChecks db tab ver
baseDbChecks = BaseDBChecks

class DBMigration (base :: *) (v :: Nat) where
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
  migChecks    = MigDBChecks Nil

data MigDBDefaults (db :: *) (tab :: TypeName Symbol) (ver :: Nat) =
  forall xs. (All KnownSymbol xs) => MigDBDefaults (HList (Def db tab) xs)

migDbDefaults :: forall tab db ver xs.
              ( All KnownSymbol xs
              ) => HList (Def db tab) xs -> MigDBDefaults db tab ver
migDbDefaults = MigDBDefaults

data MigDBChecks (db :: *) (tab :: TypeName Symbol) (ver :: Nat) =
  forall chks. (All CheckExpr chks) => MigDBChecks (HList (Chk db tab) chks)

migDbChecks :: forall tab (db :: *) ver chks.
            ( All CheckExpr chks
            ) => HList (Chk db tab) chks -> MigDBChecks db tab ver
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
    UnifyField (BaseColumns db tab ver) (fn ::: t) 
-}

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
        curTab <- view currentTable
        dbInfo . tableInfoAt curTab . tableName . dbName .= dbN
        pure (Just $ M.renameTab (coerce dbN))

type family AddColumnCtx (t :: AddColumn) :: Constraint where
  AddColumnCtx ('AddColumn cn dbTyp) =
    (SingE cn, SingE dbTyp, SingI (IsNullable (UnTag dbTyp)), SingE (IsNullable (UnTag dbTyp)))
  
instance (AddColumnCtx t) => SingE (t :: AddColumn) where
  type Demote t = ChangeSetM M.AlterTable
  fromSing (SAddColumn scoln scolt) = do
    let coln = fromSing scoln
        colt = fromSing scolt
        isn  = fromSing (isNullableSing scolt)
        cni  = mkEntityName coln coln 
    curTab <- view currentTable
    dbInfo . tableInfoAt curTab . columnInfo %=
         insert (mkColumnInfo isn cni (coerce colt))
    pure $ M.addColumn (coerce coln) (coerce colt)

      where isNullableSing :: forall dbt db. (SingI (IsNullable (UnTag dbt))) => Sing dbt -> Sing (IsNullable (UnTag dbt))
            isNullableSing _ = sing

type family DropColumnCtx (t :: DropColumn) :: Constraint where
  DropColumnCtx ('DropColumn cn) = ()

instance SingE (t :: DropColumn) where
  type Demote t = ChangeSetM M.AlterTable
  fromSing (SDropColumn scoln) = do
    let coln = fromSing scoln
    curTab <- view currentTable
    dbInfo . tableInfoAt curTab . columnInfo %=
         delete coln (\ci -> ci ^. columnNameInfo . hsName)
    pure $ M.dropColumn (coerce coln)

instance SingE (t :: RenameColumn) where
  type Demote t = ChangeSetM M.AlterTable
  fromSing (SRenameColumn scoln sdcoln) = do
    let coln   = fromSing scoln
        dbColn = fromSing sdcoln
    curTab <- view currentTable
    prevDbColn <- view (dbInfo . tableInfoAt curTab . columnInfoAt coln . columnNameInfo . dbName)   
    dbInfo . tableInfoAt curTab . columnInfoAt coln . columnNameInfo . dbName .= dbColn
    pure $ M.renameColumn (coerce prevDbColn) (coerce dbColn)

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
    curTab <- view currentTable    
    dbColn <- view (dbInfo . tableInfoAt curTab . columnInfoAt hsColn . columnNameInfo . dbName)
    dbInfo . tableInfoAt curTab . columnInfoAt hsColn . isNullable .= False
    pure $ M.setNotNull (coerce dbColn)
  fromSing (SDropNotNull scoln) = do
    let hsColn = fromSing scoln
    curTab <- view currentTable    
    dbColn <- view (dbInfo . tableInfoAt curTab . columnInfoAt hsColn . columnNameInfo . dbName)
    dbInfo . tableInfoAt curTab . columnInfoAt hsColn . isNullable .= True    
    pure $ M.dropNotNull (coerce dbColn)
  fromSing (SChangeType scoln scolt) = do
    let hsColn = fromSing scoln
        colt   = fromSing scolt
    curTab <- view currentTable    
    dbColn <- view (dbInfo . tableInfoAt curTab . columnInfoAt hsColn . columnNameInfo . dbName)
    dbInfo . tableInfoAt curTab . columnInfoAt hsColn . columnTypeName .= (coerce colt)
    pure $ M.addColumn (coerce dbColn) (coerce colt)
  fromSing (SAddDefault scoln) =
    undefined
    {-
    let hsColn = (fromSing scoln)
    curTab <- view currentTable
    dbColn <- view (dbInfo . tableInfoAt curTab . columnInfoAt hsColn . columnNameInfo . dbName)    
    pure $ M.addDefault (coerce (fromSing scoln)) undefined
    -}
  fromSing (SDropDefault scoln) = do
    let hsColn = fromSing scoln
    curTab <- view currentTable    
    dbColn <- view (dbInfo . tableInfoAt curTab . columnInfoAt hsColn . columnNameInfo . dbName)
    dbInfo . tableInfoAt curTab . defaultInfo %=
              delete (fromSing scoln) (\di -> di ^. defaultOn)
    pure $ M.dropDefault (coerce (fromSing scoln))

type family AddConstraintCtx (t :: AddConstraint) :: Constraint where
  AddConstraintCtx ('AddPrimaryKey cols ctxn) =
    (SingE cols, SingE ctxn)
  AddConstraintCtx ('AddUnique uct) = SingE uct
  AddConstraintCtx ('AddCheck cct)  = SingE cct
  AddConstraintCtx ('AddForeignKey fkref) = SingE fkref
    
instance (AddConstraintCtx t) => SingE (t :: AddConstraint) where
  type Demote t = ChangeSetM M.AlterTable
  fromSing (SAddPrimaryKey scols sctxn) = do
    let cols = coerce $ fromSing scols
        ctxn = fromSing sctxn
        pki  = mkPrimaryKeyInfo ctxn (coerce cols)
    curTab <- view currentTable
    dbInfo . tableInfoAt curTab . primaryKeyInfo .= Just pki
    pure (M.addPrimaryKey (coerce ctxn) cols)
  fromSing (SAddUnique suq) = do
    let (cols, ctxn) = fromSing suq
        ctxn' = ctxn
        uniqEt = mkEntityName ctxn' ctxn'
    curTab <- view currentTable
    dbInfo . tableInfoAt curTab . uniqueInfo %=
         insert (mkUniqueInfo uniqEt cols)
    pure (M.addUnique (coerce ctxn') (coerce cols))
  fromSing (SAddCheck sck)  = undefined
  fromSing (SAddForeignKey (sfkref :: Sing (fkref :: ForeignRef (TypeName Symbol)))) = do
    let fkref = fromSing sfkref
    curTab <- view currentTable
    case fkref of
      RefByD ctxn cols reft refcols -> do
         let fk = mkForeignKeyInfo fkEt cols reft refcols
             ctxn' = ctxn
             fkEt = mkEntityName ctxn' ctxn'
         dbInfo . tableInfoAt curTab . foreignKeyInfo %= insert fk
         dbTabN <- view (dbInfo . tableInfoAt reft . tableName . dbName)
         pure $ M.addForeignKey (coerce ctxn') (coerce cols) (coerce dbTabN) (coerce refcols)
      RefD ctxn col reft -> do
        let fk = mkForeignKeyInfo fkEt ([col]) reft ([col])
            ctxn' = ctxn
            fkEt = mkEntityName ctxn' ctxn'
        dbInfo . tableInfoAt curTab . foreignKeyInfo %= insert fk
        dbTabN <- view (dbInfo . tableInfoAt reft . tableName . dbName)        
        pure $ M.addForeignKey (coerce ctxn') (coerce [col]) (coerce dbTabN) (coerce [col])

instance SingE (t :: DropConstraint) where
  type Demote t = ChangeSetM M.AlterTable
  fromSing SDropPrimaryKey = do
    curTab <- view currentTable    
    mpki <- view $ dbInfo . tableInfoAt curTab . primaryKeyInfo
    dbInfo . tableInfoAt curTab . primaryKeyInfo .=
      Nothing
    pure $ case mpki of
      Just pki -> M.dropConstraint (coerce (pki ^. pkeyName))
      Nothing  -> error "Panic: non existant primary key"
  fromSing (SDropCheck schkn) = do
    curTab <- view currentTable    
    let hsChkn = (fromSing schkn)
    dbChkn <- view (dbInfo . tableInfoAt curTab . checkInfoAt hsChkn . checkName . dbName)
    dbInfo . tableInfoAt curTab . checkInfo %=
      delete hsChkn (\ck -> ck ^. checkName . hsName)
    pure $ M.dropConstraint (coerce dbChkn)
  fromSing (SDropUnique suqn) = do
    curTab <- view currentTable    
    let hsUqn = (fromSing suqn)
    dbUqn <- view (dbInfo . tableInfoAt curTab . uniqueInfoAt hsUqn . uqName . dbName)    
    dbInfo . tableInfoAt curTab . uniqueInfo %=
         delete hsUqn (\uq -> uq ^. uqName . hsName)
    pure $ M.dropConstraint (coerce dbUqn)
  fromSing (SDropForeignKey sfkn) = do
    curTab <- view currentTable    
    let hsFkn = (fromSing sfkn)
    dbFkn <- view (dbInfo . tableInfoAt curTab . foreignKeyInfoAt hsFkn . fkeyName . dbName)    
    dbInfo . tableInfoAt curTab . foreignKeyInfo %=
         delete hsFkn (\fk -> fk ^. fkeyName . hsName)
    pure $ M.dropConstraint (coerce dbFkn)
    

data AddTable = AddTable (TypeName Symbol)         -- ^ Table name
data RenameTable = RenameTable (Maybe Symbol)      -- ^ To tablename
data DropTable = DropTable (TypeName Symbol)       -- ^ Table name


data AddColumn = AddColumn Symbol                  -- ^ Column ref
                           (TagHK DbK DBTypeK)     -- ^ type in DB with tag

data DropColumn = DropColumn Symbol                -- ^ Column ref

data RenameColumn = RenameColumn Symbol            -- ^ hask columnName
                                 Symbol            -- ^ To tablename

data AlterColumn = SetNotNull Symbol               -- ^ Column ref
                 | DropNotNull Symbol              -- ^ Column ref
                 | ChangeType Symbol               -- ^ Column ref
                              (TagHK DbK DBTypeK)  -- ^ type in DB with tag
                 | AddDefault Symbol               -- ^ Column ref
                 | DropDefault Symbol              -- ^ Column ref

data AddConstraint = AddPrimaryKey [Symbol] -- ^ Column refs
                                   Symbol   -- ^ Constraint name
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

mkHeadMigration :: (SingCtxDb db) => Proxy db -> ChangeSet
mkHeadMigration = dbInfoChangeSet . headDatabaseInfo

mkAllMigrations :: forall db.
                   ( Database db
                   , BaseDatabase db (Baseline db)
                   , SingI (TagEach db (Range (Baseline db) (Version db)))
                   , AllMigDbCtx (TagEach db (Range (Baseline db) (Version db)))
                   , SingCtxBaseDb db (Baseline db)
                   ) => Proxy db -> [ChangeSet]
mkAllMigrations pdb =
  let css = mkChangeSetState bd
      bd  = baseDatabaseInfo pdb (Proxy :: Proxy (Baseline db))
      bdCs = dbInfoChangeSet bd
  in  bdCs : M.evalState (runChangeSet (mkMigrations (sing :: Sing (TagEach db (Range (Baseline db) (Version db)))))) css

-- Proxy db -> Step -> Maybe Step -> (DatabaseInfo, [ChangeSet]) -- DatabaseInfo including Step, changeset from Step { dirtyness check@ Step }
                                                                 -- + changeset till Step or Head.
-- DatabaseInfo @Step -> DatabaseInfo @DB -> [ChangeSet]         -- diff between DB and DatabaseInfo @Step

type family AllMigDbCtx (tagHks :: [TagHK * Nat]) :: Constraint where
  AllMigDbCtx ('Tag db ver ': tagHks) = (MigDbCtx db ver , AllMigDbCtx tagHks)
  AllMigDbCtx '[]                     = ()

mkMigrations :: (AllMigDbCtx tagHks) => Sing (tagHks :: [TagHK * Nat]) -> ChangeSetM [ChangeSet]
mkMigrations (SCons tagHK@(STag {}) tagHKs) = do
  (:) <$> mkMigrationDb tagHK <*> mkMigrations tagHKs
mkMigrations SNil = pure []

type MigDbCtx db ver = ( DBMigration db ver
                       , SingI (TagEach '(db, ver) (AlteredTables db ver))
                       , SingI (TagEach '(db, ver) (DropedTables db ver))
                       , SingI (TagEach '(db, ver) (CreatedTables db ver))
                       , AllMigTableCtx (TagEach '(db, ver) (AlteredTables db ver))
                       , AllMigTableCtx (TagEach '(db, ver) (DropedTables db ver))
                       , AllMigTableCtx (TagEach '(db, ver) (CreatedTables db ver))
                       )

mkMigrationDb :: forall db ver.
                  ( MigDbCtx db ver
                  ) => Sing ('Tag db ver) -> ChangeSetM ChangeSet
mkMigrationDb _ = do
  crs <- mkMigrationTables (sing :: Sing (TagEach '(db, ver) (CreatedTables db ver)))
  dls <- mkMigrationTables (sing :: Sing (TagEach '(db, ver) (DropedTables db ver)))
  alts <- mkMigrationTables (sing :: Sing (TagEach '(db, ver) (AlteredTables db ver)))    
  pure $ coerce (crs ++ dls ++ alts)

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
  let curTab = fromSing (sing :: Sing (tab :: TypeName Symbol))
  currentTable .= curTab
  acs <- sequence $ fromSing (sing :: Sing (AddedColumn db tab ver))
  dcs <- sequence $ fromSing (sing :: Sing (MkDroppedColumn (DropedColumn db tab ver)))
  rcs <- sequence $ fromSing (sing :: Sing (MkRenamedColumn (RenamedColumn db tab ver)))
  alcs <- sequence $ fromSing (sing :: Sing (AlteredColumn db tab ver))
  acts <- sequence $ fromSing (sing :: Sing (AddedConstraint db tab ver))
  dcts <- sequence $ fromSing (sing :: Sing (DropedConstraint db tab ver))
  dbTabN <- view (dbInfo . tableInfoAt curTab . tableName . dbName) 
  pure $ M.altering (coerce dbTabN) (acs ++ dcs ++ rcs ++ alcs ++ acts ++  dcts)

type family MkDroppedColumn (dcs :: [Symbol]) where
  MkDroppedColumn (dc ': dcs) = 'DropColumn dc ': MkDroppedColumn dcs
  MkDroppedColumn '[]         = '[]

type family MkRenamedColumn (rns :: [(Symbol, Symbol)]) :: [RenameColumn] where
  MkRenamedColumn ('(cn, dcn) ': rns) = 'RenameColumn cn dcn ': MkRenamedColumn rns
  MkRenamedColumn '[]                 = '[]

data ChangeSetState = ChangeSetState { _dbInfo       :: DatabaseInfo
                                     , _currentTable :: Maybe (TypeName T.Text)
                                     } deriving (Show, Eq)

mkChangeSetState :: DatabaseInfo -> ChangeSetState
mkChangeSetState dbi =
  ChangeSetState { _dbInfo = dbi
                 , _currentTable = Nothing
                 }

dbInfo :: Functor f => (DatabaseInfo -> f DatabaseInfo) -> ChangeSetState -> f ChangeSetState
dbInfo k t = fmap (\a -> t { _dbInfo = a }) (k (_dbInfo t))

currentTable :: Functor f => (TypeName T.Text -> f (TypeName T.Text)) -> ChangeSetState -> f ChangeSetState
currentTable k t = fmap (\a -> t { _currentTable = Just a }) (k (fromJust (_currentTable t)))

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
  
toTypeAttr :: HList (Const DConAttr) xs -> M.TypeAttr
toTypeAttr hlist =
  let consAttrs = recordToList hlist
      isUnary (DConAttr (_cn, [])) = True
      isUnary _                    = False
  in case consAttrs of
    [DConAttr (_cn, cols)]   -> M.ProdAttr (map toMColumn cols)
    [] -> error "@toTypeAttr: DB Type cannot be of Void type"
    cons | all isUnary cons -> M.EnumAttr $ fmap (\(DConAttr cattr) -> coerce (fst cattr)) cons
         | otherwise        -> M.SumAttr $ fmap (\(DConAttr (cn, cns)) -> (coerce cn, map toMColumn cns)) cons

toMColumn :: S.Column -> M.Column
toMColumn (S.Column n t) = M.Column (M.ColName n) (M.ColType (coerce t))

dbInfoChangeSet :: DatabaseInfo -> ChangeSet
dbInfoChangeSet di = coerce (go di :: [M.PrimDDL])
  where go dbInfo = let tis = (dbInfo ^. tableInfos)
                    in concatMap (tabInfoChangeSet dbInfo) tis

pkeyInfoChangeSet :: TableInfo -> PrimaryKeyInfo -> M.AlterTable
pkeyInfoChangeSet tabInfo pkInfo = 
  let pkNameDb = pkInfo ^. pkeyName
      pkColsDb = getDbColumnNames (tabInfo ^. columnInfo) (pkInfo ^. pkeyColumns)
  in  M.addPrimaryKey (coerce pkNameDb) (coerce pkColsDb)

uniqueInfoChangeSet :: TableInfo -> [UniqueInfo] -> [M.AlterTable]
uniqueInfoChangeSet tabInfo uqInfos = 
  let uqColsDb uqInfo = getDbColumnNames (tabInfo ^. columnInfo) (uqInfo ^. uqColumns)                    
      addUniq uqInfo  = M.addUnique (coerce (uqInfo ^. uqName . dbName))
                                    (coerce (uqColsDb uqInfo))
  in  map addUniq uqInfos

fkeyInfoChangeSet :: DatabaseInfo -> TableInfo -> [ForeignKeyInfo] -> [M.AlterTable]
fkeyInfoChangeSet dbInfo tabInfo fkInfos = 
  let fkColsDb fkInfo = getDbColumnNames (tabInfo ^. columnInfo) (fkInfo ^. fkeyColumns)
      fkRefColsDb fkInfo = getDbColumnNames (dbInfo ^. tableInfoAt (fkInfo ^. fkeyRefTable) . columnInfo)
                                            (fkInfo ^. fkeyRefColumns)
      addFk fkInfo = M.addForeignKey (coerce (fkInfo ^. fkeyName . dbName))
                                     (coerce (fkColsDb fkInfo))                            
                                     (coerce (dbInfo ^. tableInfoAt (fkInfo ^. fkeyRefTable) . tableName . dbName))
                                     (coerce (fkRefColsDb fkInfo))
  in  map addFk fkInfos

checkInfoChangeSet :: [CheckInfo] -> [M.AlterTable]
checkInfoChangeSet ckInfos = 
  let addCk ckInfo = M.addCheck (coerce (ckInfo ^. checkName . dbName))
                                (coerce (ckInfo ^. checkExp))
  in  map addCk ckInfos

defaultInfoChangeSet ::  TableInfo -> [DefaultInfo] -> [M.AlterTable]
defaultInfoChangeSet tabInfo defInfos =
  let addDef defInfo = M.addDefault (coerce (getDbColumnName (tabInfo ^. columnInfo) (defInfo ^. defaultOn)))
                                    (coerce (defInfo ^. defaultExp))
  in map addDef defInfos

notNullChangeSet :: [ColumnInfo] -> [M.AlterTable]
notNullChangeSet notNullCols =
  let addNotNullCtx notNullCol = M.setNotNull (coerce (notNullCol ^. columnNameInfo . dbName))
  in map addNotNullCtx notNullCols

tabInfoChangeSet :: DatabaseInfo -> TableInfo -> [M.PrimDDL]
tabInfoChangeSet dbi tabInfo =          
  let tabNDb  = tabInfo ^. tableName . dbName
      toMColumn colInfo = M.Column (coerce (colInfo ^. columnNameInfo . dbName))
                                   (coerce (colInfo ^. columnTypeName))
      createTab = M.CreateTable (coerce tabNDb) (coerce (map toMColumn (tabInfo ^. columnInfo)))
      addPks  = let mpkInfo  = tabInfo ^. primaryKeyInfo                             
                in  maybe [] M.single (fmap (pkeyInfoChangeSet tabInfo) mpkInfo)
      addUqs  = uniqueInfoChangeSet tabInfo (tabInfo ^. uniqueInfo)
      addFks  = fkeyInfoChangeSet dbi tabInfo (tabInfo ^. foreignKeyInfo)
      addChks = checkInfoChangeSet (tabInfo ^. checkInfo)
      addDefs = defaultInfoChangeSet tabInfo (tabInfo ^. defaultInfo)                        
      addNotNullChks = notNullChangeSet (getNonNullableColumns (tabInfo ^. columnInfo))
{-                                  
              cTabSeqs = let createSeq colInfo seqn = let coln = colInfo ^. columnNameInfo . dbName
                                                          seqKey = 
                                                      in CreateSeq (coerce $ doubleQuoteT (genKeyName $ SeqNameGen tabNDb coln (Just seqn)))
                         in  map (\si -> createSeq (seqOn si) (seqName si)) (sequenceInfo tabInfo)                             
              oTabSeqs = let ownSeq colInfo seqn seqT = case seqT of
                               SeqOwned -> let coln = dbColumnName (columnNameInfo colInfo)
                                           in Just $ AlterSeq (coerce $ doubleQuoteT (genKeyName $ SeqNameGen tabNDb coln Nothing))
                                                    (AddOwner (coerce tabNDb) (coerce coln))
                               _        -> Nothing
                         in  catMaybes $ map (\si -> ownSeq (seqOn si) (seqName si) (seqType si)) (sequenceInfo tabInfo)                     
              defSeqs = let seqn coln = quoteT $ doubleQuoteT (genKeyName $ SeqNameGen tabNDb coln Nothing)
                            seqnE coln = PQ.ConstExpr (PQ.Other (seqn coln))
                            nextValE coln = PQ.FunExpr "nextVal" [seqnE coln]
                            defSeq colInfo seqn seqT =
                              case seqT of
                                SeqOwned  -> Nothing
                                SeqSerial -> let coln = dbColumnName (columnNameInfo colInfo)
                                             in  Just (AlterTable (coerce tabNDb) $ AlterColumn (ColName coln) $ AddDefault (DefExpr $ nextValE coln))
                        in catMaybes $ map (\si -> defSeq (seqOn si) (seqName si) (seqType si)) (sequenceInfo tabInfo)                     
-}
  in (:) createTab $
         M.altering (coerce tabNDb) $ concat [ -- cTabSeqs
                                               -- , oTabSeqs
                                                 addPks  
                                               , addUqs
                                               , addFks
                                               , addChks
                                               -- , defSeqs
                                               , addDefs
                                               , addNotNullChks
                                               ]
        
type family TyCxts (db :: *) (tys :: [*]) :: Constraint where
  TyCxts db (ty ': ts) = ( ShowDBType (DB db) (GetDBTypeRep (DB db) ty)
                         , SingAttrs db (GetTypeFields ty)
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

{-
runMigDiff :: ( Table db tab
               , Database db
               , SingCtx db tab
               , SingCtxDb db
               ) => Proxy (db :: *) -> Proxy (tab :: *) -> PGS.Connection -> IO ()
runMigDiff pdb ptab conn = undefined

data MigDirection = HsToDb | DbToHs
                  deriving (Show, Eq)
-}

{-
migDiffHsToDb :: Step -> [ChangeSet] -> [TableInfo] -> [TableInfo] -> ChangeSet
migDiffHsToDb step csets hsTabInfos dbTabInfos =
  let (paired, missed, extra) = pairTables hsTabInfos dbTabInfos
      (dropDepFKs, newPaired) = dropDependentFKs extra paired
      dropExtras              = dropStatementsTabInfo extra
                              = migDiffHasToDb 
  
  in [ dropDependentFKs extra paired
     , dropStatementsTabInfo extra
     , createStatementsTabInfo missed
     ] 
        -- TODO: Verify order of DDL generation
  where csetsToApp = L.drop step csets

pairTables :: [TableInfo] -> [TableInfo] -> ([(TableInfo, TableInfo)], [TableInfo], [TableInfo])
pairTables hsTabs dbTabs =
   foldr (\hsTab (paired, missed, extra) ->
          let mdbTabMatch = L.find (\dbTab -> eqByDbName hsTab dbTab) extra
          in case mdbTabMatch of
               Just dbTabMatch -> ((hsTab, dbTabMatch) : paired, missed, L.delete dbTabMatch extra)
               Nothing         -> (paired, hsTab : missed, extra)
        ) ([], [], dbTabs) hsTabs

  where eqByDbName hsTab dbTab = tableName hsTab `eqByDb` tableName dbTab
-}

diffDatabaseInfo :: DatabaseInfo -> DatabaseInfo -> ChangeSet
diffDatabaseInfo di1 di2 =
  let (missed, extra, paired) = pairTables (di1 ^. tableInfos)
                                           (di2 ^. tableInfos)
  in  undefined

pairTables :: [TableInfo] -> [TableInfo] -> ([TableInfo], [TableInfo], [(TableInfo, TableInfo)])
pairTables = undefined

doubleQuoteT :: T.Text -> T.Text
doubleQuoteT t = "\"" <> t <> "\""

quoteT :: T.Text -> T.Text
quoteT t = "\'" <> t <> "\'"


--
class ( BaseTable db tab ver
      , Database db
        
      , SingI (BasePrimaryKeyName db tab ver)
      , SingE (BasePrimaryKeyName db tab ver)
       
      , SingE (GetBaseColumnInfo (DB db) (BaseColumns db tab ver))
      , SingI (GetBaseColumnInfo (DB db) (BaseColumns db tab ver))

      , SingE (BasePrimaryKey db tab ver)
      , SingI (BasePrimaryKey db tab ver)

      , SingE (BaseUnique db tab ver)
      , SingI (BaseUnique db tab ver)

      , SingE (BaseUniqueNames db tab ver)
      , SingI (BaseUniqueNames db tab ver)
        
      , SingE (BaseForeignKey db tab ver)
      , SingI (BaseForeignKey db tab ver)

      , SingE (BaseForeignKeyNames db tab ver)
      , SingI (BaseForeignKeyNames db tab ver)
        
      , SingI (BaseTableSequence db tab ver)
      , SingE (BaseTableSequence db tab ver)

      , SingI (BaseCheckNames db tab ver)
      , SingE (BaseCheckNames db tab ver)

      , SingI (BaseSequenceNames db tab ver)
      , SingE (BaseSequenceNames db tab ver)

      , SingI (BaseColumnNames db tab ver)
      , SingE (BaseColumnNames db tab ver)
        
      , SingI tab
      , SingE tab
      ) => SingCtxBase db ver tab where

instance ( BaseTable db tab ver
         , Database db
           
         , SingE (GetBaseColumnInfo (DB db) (BaseColumns db tab ver))
         , SingI (GetBaseColumnInfo (DB db) (BaseColumns db tab ver))
           
         , SingI (BasePrimaryKeyName db tab ver)
         , SingE (BasePrimaryKeyName db tab ver)
           
         , SingE (BasePrimaryKey db tab ver)
         , SingI (BasePrimaryKey db tab ver)

         , SingE (BaseUnique db tab ver)
         , SingI (BaseUnique db tab ver)

         , SingE (BaseUniqueNames db tab ver)
         , SingI (BaseUniqueNames db tab ver)
                      
         , SingE (BaseForeignKey db tab ver)
         , SingI (BaseForeignKey db tab ver)

         , SingE (BaseForeignKeyNames db tab ver)
         , SingI (BaseForeignKeyNames db tab ver)
                      
         , SingI (BaseTableSequence db tab ver)
         , SingE (BaseTableSequence db tab ver)

         , SingI (BaseCheckNames db tab ver)
         , SingE (BaseCheckNames db tab ver)

         , SingI (BaseSequenceNames db tab ver)
         , SingE (BaseSequenceNames db tab ver)

         , SingI (BaseColumnNames db tab ver)
         , SingE (BaseColumnNames db tab ver)
           
         , SingI tab
         , SingE tab           
         ) => SingCtxBase db ver tab

class ( BaseDatabase db ver
      , SingE (BaseSchema db ver)
      , SingI (BaseSchema db ver)
      , SingI (BaseTables db ver)        
      , Database db

      , SingI (GetPMT (Rep db))
      , SingE (GetPMT (Rep db))
      , All (SingCtxBase db ver) (BaseTables db ver)
      ) => SingCtxBaseDb db ver where

instance ( BaseDatabase db ver
         , SingE (BaseSchema db ver)
         , SingI (BaseSchema db ver)
         , SingI (BaseTables db ver)
           
           
         , Database db
           
         , SingI (GetPMT (Rep db))
         , SingE (GetPMT (Rep db))
         , All (SingCtxBase db ver) (BaseTables db ver)           
         ) => SingCtxBaseDb db ver where  

baseDatabaseInfo :: forall db ver.
                ( SingCtxBaseDb db ver
                , All (SingCtxBase db ver) (BaseTables db ver)
                ) => Proxy (db :: *) -> Proxy ver -> DatabaseInfo
baseDatabaseInfo pdb pver =
  mkDatabaseInfo (mkEntityName (coerce (fromSing (sing :: Sing (GetPMT (Rep db)))))
                                       (fromSing (sing :: Sing (BaseSchema db ver)))
                 ) [] 0 0 (baseTableInfos pdb pver (sing :: Sing (BaseTables db ver)))

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
      pkDefN = let hsn    = genTabName (et ^. hsName)
               in  genKeyName (PkNameGen hsn pkCols)      
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
      dbTabN = genTabName tabTN
  in mkEntityName (coerce tabTN) (coerce dbTabN)
               
baseColInfos :: forall tab db ver.
            ( BaseTable db tab ver
            , Database db
            , SingE (GetBaseColumnInfo (DB db) (BaseColumns db tab ver))
            , SingI (GetBaseColumnInfo (DB db) (BaseColumns db tab ver))
            , SingI (BaseColumnNames db tab ver)
            , SingE (BaseColumnNames db tab ver)
            ) => Proxy db -> Proxy tab -> Proxy ver -> [ColumnInfo]
baseColInfos _ _ _ =
  let hsns = fromSing (sing :: Sing (GetBaseColumnInfo (DB db) (BaseColumns db tab ver)))
      colMap = fromSing (sing :: Sing (BaseColumnNames db tab ver))      
  in  map (colInfoOne colMap) hsns

type family GetBaseColumnInfo (db :: DbK) (xs :: [(Symbol, DBTypeK)]) where
  GetBaseColumnInfo db ('(fld, x) ': xs) = 
    '(TagTypeInfo db x, fld) ': GetBaseColumnInfo db xs
  GetBaseColumnInfo db '[] =
    '[]

renderChangeSets :: [ChangeSet] -> String
renderChangeSets =
  unlines . map renderChangeSet 

renderChangeSet :: ChangeSet -> String
renderChangeSet =
  unlines . map M.renderDDL . statements
