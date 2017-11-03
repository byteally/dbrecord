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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module DBRecord.Migration
       ( mkMigration
       , M.renderDDL
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
import DBRecord.Internal.Migration.Validation -- (getSchemaInfo)
import qualified DBRecord.Internal.Migration.Validation as DB
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

  type BasePrimaryKeyName db tab ver :: Maybe Symbol
  type BasePrimaryKeyName db tab ver = 'Nothing
  
  type BasePrimaryKey db tab ver :: [Symbol]
  type BasePrimaryKey db tab ver = '[]
  
  type BaseForeignKey db tab ver :: [ForeignRef (TypeName Symbol)]
  type BaseForeignKey db tab ver = '[]
  
  type BaseUnique db tab ver :: [UniqueCT]
  type BaseUnique db tab ver = '[]
  
  type BaseDefaultedCols db tab ver :: [Symbol]
  type BaseDefaultedCols db tab ver = '[]
  
  type BaseCheck db tab ver :: [CheckCT]
  type BaseCheck db tab ver = '[]
  
  type BaseTableSequence db tab ver :: [Sequence]
  type BaseTableSequence db tab ver = '[]

  -- baseDefaults :: () DBDefaults db tab ver 
  -- baseChecks   :: () DBChecks db tab ver

class DBMigration (base :: *) (v :: Nat) where
  type RenameSchema base v :: Maybe Symbol
  type RenameSchema base v = 'Nothing
  
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
  type AddedColumn base tab v :: [AddColumn]
  type DropedColumn base tab v :: [Symbol]
  type RenamedColumn base tab v :: [(Symbol, Symbol)]
  type AlteredColumn base tab v :: [AlterColumn]
  type AddedConstraint base tab v :: [AddConstraint]
  type DropedConstraint base tab v :: [Symbol]

  -- migDefaults :: ()
  -- migChecks   :: ()
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
  SAddPrimaryKey :: Sing (cns :: [Symbol]) -> Sing (ctxn :: Symbol) ->
                    Sing ('AddPrimaryKey cns ctxn)
  SAddUnique     :: Sing (uq :: UniqueCT) -> Sing ('AddUnique uq)
  SAddCheck      :: Sing (chk :: CheckCT) -> Sing ('AddCheck chk)
  SAddForeignKey :: Sing (fkref :: ForeignRef (TypeName Symbol)) ->
                    Sing ('AddForeignKey fkref)

data instance Sing (t :: DropConstraint) where
  SDropConstraint :: Sing (cn :: Symbol) -> Sing ('DropConstraint cn)

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

instance (SingI cn) => SingI ('DropConstraint cn) where
  sing = SDropConstraint sing

type family RenameTableCtx (t :: RenameTable) :: Constraint where
  RenameTableCtx ('RenameTable t) = (MaybeCtx SingE t)

instance (RenameTableCtx t) => SingE (t :: RenameTable) where
  type Demote t = ChangeSetM (Maybe M.AlterTable)
  fromSing (SRenameTable smName) = do
    case fromSing smName of
      Nothing    -> pure Nothing
      (Just dbN) -> do
        curTab <- view currentTable
        dbInfo . tableInfoAt curTab . tableName . dbName .= (T.pack dbN)
        pure (Just $ M.renameTab (coerce (T.pack dbN)))

type family AddColumnCtx (t :: AddColumn) :: Constraint where
  AddColumnCtx ('AddColumn cn dbTyp) =
    (SingE cn, SingE dbTyp, SingI (IsNullable (UnTag dbTyp)), SingE (IsNullable (UnTag dbTyp)))
  
instance (AddColumnCtx t) => SingE (t :: AddColumn) where
  type Demote t = ChangeSetM M.AlterTable
  fromSing (SAddColumn scoln scolt) = do
    let coln = T.pack (fromSing scoln)
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
    let coln = T.pack (fromSing scoln)
    curTab <- view currentTable
    dbInfo . tableInfoAt curTab . columnInfo %=
         delete coln (\ci -> ci ^. columnNameInfo . hsName)
    pure $ M.dropColumn (coerce coln)

instance SingE (t :: RenameColumn) where
  type Demote t = ChangeSetM M.AlterTable
  fromSing (SRenameColumn scoln sdcoln) = do
    let coln   = T.pack (fromSing scoln)
        dbColn = T.pack (fromSing sdcoln)
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
    let hsColn = T.pack (fromSing scoln)
    curTab <- view currentTable    
    dbColn <- view (dbInfo . tableInfoAt curTab . columnInfoAt hsColn . columnNameInfo . dbName)
    dbInfo . tableInfoAt curTab . columnInfoAt hsColn . isNullable .= False
    pure $ M.setNotNull (coerce dbColn)
  fromSing (SDropNotNull scoln) = do
    let hsColn = T.pack (fromSing scoln)
    curTab <- view currentTable    
    dbColn <- view (dbInfo . tableInfoAt curTab . columnInfoAt hsColn . columnNameInfo . dbName)
    dbInfo . tableInfoAt curTab . columnInfoAt hsColn . isNullable .= True    
    pure $ M.dropNotNull (coerce dbColn)
  fromSing (SChangeType scoln scolt) = do
    let hsColn = T.pack (fromSing scoln)
        colt   = fromSing scolt
    curTab <- view currentTable    
    dbColn <- view (dbInfo . tableInfoAt curTab . columnInfoAt hsColn . columnNameInfo . dbName)
    dbInfo . tableInfoAt curTab . columnInfoAt hsColn . columnTypeName .= (coerce colt)
    pure $ M.addColumn (coerce dbColn) (coerce colt)
  fromSing (SAddDefault scoln) =
    undefined
    {-
    let hsColn = T.pack (fromSing scoln)
    curTab <- view currentTable
    dbColn <- view (dbInfo . tableInfoAt curTab . columnInfoAt hsColn . columnNameInfo . dbName)    
    pure $ M.addDefault (coerce (T.pack (fromSing scoln))) undefined
    -}
  fromSing (SDropDefault scoln) = do
    let hsColn = T.pack (fromSing scoln)    
    curTab <- view currentTable    
    dbColn <- view (dbInfo . tableInfoAt curTab . columnInfoAt hsColn . columnNameInfo . dbName)
    dbInfo . tableInfoAt curTab . defaultInfo %=
              delete (T.pack (fromSing scoln)) (\di -> di ^. defaultOn)
    pure $ M.dropDefault (coerce (T.pack (fromSing scoln)))

type family AddConstraintCtx (t :: AddConstraint) :: Constraint where
  AddConstraintCtx ('AddPrimaryKey cols ctxn) =
    (SingE cols, SingE ctxn)
  AddConstraintCtx ('AddUnique uct) = SingE uct
  AddConstraintCtx ('AddCheck cct)  = SingE cct
  AddConstraintCtx ('AddForeignKey fkref) = SingE fkref
    
instance (AddConstraintCtx t) => SingE (t :: AddConstraint) where
  type Demote t = ChangeSetM M.AlterTable
  fromSing (SAddPrimaryKey scols sctxn) = do
    let cols = coerce $ map T.pack (fromSing scols)
        ctxn = T.pack (fromSing sctxn)
        pki  = mkPrimaryKeyInfo (Just ctxn) (coerce cols)
    curTab <- view currentTable
    dbInfo . tableInfoAt curTab . primaryKeyInfo .= Just pki
    pure (M.addPrimaryKey (coerce ctxn) cols)
  fromSing (SAddUnique suq) = do
    let (cols, ctxn) = fromSing suq
    curTab <- view currentTable
    dbInfo . tableInfoAt curTab . uniqueInfo %=
         insert (mkUniqueInfo undefined (map T.pack cols))
    pure (M.addUnique (coerce (T.pack ctxn)) (coerce (map T.pack cols)))
  fromSing (SAddCheck sck)  = undefined
  fromSing (SAddForeignKey (sfkref :: Sing (fkref :: ForeignRef (TypeName Symbol)))) = do
    let fkref = fromSing sfkref
    curTab <- view currentTable
    case fkref of
      RefByD ctxn cols reft refcols -> do
         let fk = mkForeignKeyInfo undefined (map T.pack cols) reft (map T.pack refcols)
         dbInfo . tableInfoAt curTab . foreignKeyInfo %= insert fk           
         pure $ M.addForeignKey (coerce (T.pack ctxn)) (coerce (map T.pack cols)) undefined (coerce (map T.pack refcols))
      RefD ctxn col reft -> do
        let fk = mkForeignKeyInfo undefined (map T.pack [col]) reft (map T.pack [col])        
        dbInfo . tableInfoAt curTab . foreignKeyInfo %= insert fk
        pure $ M.addForeignKey (coerce (T.pack ctxn)) (coerce (map T.pack [col])) undefined (coerce (map T.pack [col]))

instance SingE (t :: DropConstraint) where
  type Demote t = ChangeSetM M.AlterTable
  fromSing (SDropConstraint scoln) = do
    let hsColn = T.pack (fromSing scoln)
    undefined

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

data DropConstraint = DropConstraint Symbol -- ^ Constraint name

-- NOTE: A ChangeSet is supposed to be consistent
--       as long as it is run in a all or nothing fashion.
newtype ChangeSet = ChangeSet { statements :: [M.PrimDDL]
                              } deriving (Show)

mkHead :: Proxy db -> ChangeSet
mkHead = undefined

mkAllMigrations :: forall db.
                   ( Database db
                   , BaseDatabase db (Version db)
                   , SingI (TagEach db (Range (Baseline db) (Version db)))
                   , KnownSymbol (BaseSchema db (Version db))
                   , Typeable db
                   , SingE db
                   , SingI db
                   , SingI (GetPMT (Rep db))
                   , SingE (GetPMT (Rep db))
                   ) => Proxy db -> [ChangeSet]
mkAllMigrations pdb =
  let css = mkChangeSetState (baseDatabaseInfo pdb (Proxy :: Proxy (Version db))) undefined
  in  M.evalState (runChangeSet (mkMigrations (sing :: Sing (TagEach db (Range (Baseline db) (Version db)))))) css

-- Proxy db -> Step -> Maybe Step -> (DatabaseInfo, [ChangeSet]) -- DatabaseInfo including Step, changeset from Step { dirtyness check@ Step }
                                                                 -- + changeset till Step or Head.
-- DatabaseInfo @Step -> DatabaseInfo @DB -> [ChangeSet]         -- diff between DB and DatabaseInfo @Step

mkMigrations :: Sing (tagHks :: [TagHK * Nat]) -> ChangeSetM [ChangeSet]
mkMigrations (SCons tagHK@(STag {}) tagHKs) = do
  (:) <$> mkMigrationDb tagHK <*> mkMigrations tagHKs
mkMigrations SNil = pure []

mkMigrationDb :: Sing ('Tag db ver) -> ChangeSetM ChangeSet
mkMigrationDb = do
  crs <- mkMigrationTables (sing :: Sing (TagEach '(db, ver) (CreatedTables db ver)))
  dls <- mkMigrationTables (sing :: Sing (TagEach '(db, ver) (DroppedTables db ver)))
  alts <- mkMigrationTables (sing :: Sing (TagEach '(db, ver) (AlteredTables db ver)))    
  pure $ coerce (crs ++ dls ++ alts)

mkMigrationTables :: Sing (tagHks :: [Tag (*, Nat) (TypeName Symbol)]) -> ChangeSetM [PrimDDL]
mkMigrationTables (SCons tagHK@(STag {}) tagHKs) = do
  (:) <$> mkMigrationTable tagHK <*> mkMigrationTables tagHKs
mkMigrationTables SNil = pure []

mkMigrationTable :: Sing ('Tag '(db, ver) tab) -> ChangeSetM [PrimDDL]
mkMigrationTable _ = do
  -- setCurrentTab .= .
  acs <- fromSing (sing :: AddedColumn db v tab)
  dcs <- fromSing (sing :: DropedColumn db v tab)
  rcs <- fromSing (sing :: RenamedColumn db v tab)
  alcs <- fromSing (sing :: AlteredColumn db v tab)
  acts <- fromSing (sing :: AddedConstraint db v tab)
  dcts <- fromSing (sing :: DropedConstraint db v tab)   
  pure (acs ++ dcs ++ rcs ++ alcs ++ acts ++ dcs)
  
    
{-
mkMigrations :: Proxy db -> (DatabaseInfo, [ChangeSet])
mkMigrations =
  foldl' (\(dbi, cs) -> let (dbi', cs') = undefined -- mkDbMigration ...
                        in  (dbi', cs ++ [cs'])
         ) (baseDatabaseInfo undefined) (undefined :: Int) -- Sing '[versions between baseline and final]
-}

data ChangeSetState = ChangeSetState { _dbInfo       :: DatabaseInfo
                                     , _currentTable :: TypeName T.Text
                                     } deriving (Show, Eq)

mkChangeSetState :: DatabaseInfo -> TypeName T.Text -> ChangeSetState
mkChangeSetState dbi ctab =
  ChangeSetState { _dbInfo = dbi
                 , _currentTable = ctab
                 }

dbInfo :: Functor f => (DatabaseInfo -> f DatabaseInfo) -> ChangeSetState -> f ChangeSetState
dbInfo k t = fmap (\a -> t { _dbInfo = a }) (k (_dbInfo t))

currentTable :: Functor f => (TypeName T.Text -> f (TypeName T.Text)) -> ChangeSetState -> f ChangeSetState
currentTable k t = fmap (\a -> t { _currentTable = a }) (k (_currentTable t))

newtype ChangeSetM a = ChangeSetM { runChangeSet :: M.StateT ChangeSetState I.Identity a }
                     deriving (Functor, Applicative, Monad, M.MonadState ChangeSetState)

mkDbMigration :: ( DBMigration db ver
                  , AllTableMig db ver (AlteredTables db ver)
                  , AllTableMig db ver (CreatedTables db ver)
                  ) => Sing (db :: *) -> Sing (ver :: Nat) -> DatabaseInfo -> (DatabaseInfo, ChangeSet)
mkDbMigration = undefined

mkTableMigration :: Sing (db :: TypeName Symbol) -> Sing (tab :: TypeName Symbol) -> Sing (ver :: Nat) -> ChangeSetM [M.PrimDDL]
mkTableMigration sdb stab sver =
  undefined

mkAddedColumn :: Sing (db :: TypeName Symbol) -> Sing (tab :: TypeName Symbol) -> Sing (ver :: Nat) -> Sing (addedCol :: AddColumn) ->  ChangeSetM [M.PrimDDL]
mkAddedColumn = undefined


{-
mkTableMigrations :: Sing (db :: TypeName Symbol) -> Sing (tabs :: [TypeName Symbol]) -> Sing (ver :: Nat) -> DatabaseInfo -> (DatabaseInfo, [M.PrimDDL])
mkTableMigrations sdb (SCons stab stabs) sver di =
  let (di', ddls)   = mkTableMigration sdb stab sver di
      (di'', ddlss) = mkTableMigrations sdb stabs sver di'
  in  (di'', ddls ++ ddlss)
-}

type family AllTableMig (db :: *) (ver :: Nat) (tabs :: [TypeName Symbol]) :: Constraint where
  AllTableMig db ver (tab ': tabs) = (TableMigration db tab ver, AllTableMig db ver tabs)
  AllTableMig db ver '[]           = ()

data DBDiff = DBDiff
data TableDiff
data TypeDiff
data ColumnDiff
data ConstraintDiff


-- type family DiffDB (db :: *) (bl :: *) (currver :: *) where
--   DiffDB db bl currver = DiffDB' db bl currver (BaseVersion bl)

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
dbInfoChangeSet = undefined -- coerce . go
{-
  where go dbInfo = let tis = (dbInfo ^. tableInfos)
                    in map (flip tabInfoChangeSet tis) tis
        tabInfoChangeSet tabInfo allTis =          
          let tabNDb  = dbTableName (tableName tabInfo)
              toMColumn colInfo = M.Column (coerce (dbColumnName (columnNameInfo colInfo)))
                                           (coerce (columnTypeName colInfo))
              createTab = [CreateTable (coerce tabNDb) (coerce (map toMColumn (columnInfo tabInfo)))]
              addPks  = let pkInfo   = primaryKeyInfo tabInfo
                            pkNameDb = maybe (genKeyName $ PkNameGen tabNDb pkColsDb) id (pkeyName pkInfo)
                            pkColsDb = getDbColumnNames (pkeyColumns pkInfo)
                        in  [addPrimaryKey (coerce tabNDb) (coerce pkNameDb) (coerce pkColsDb)]
              addUqs  = let uqInfos         = uniqueInfo tabInfo
                            uqNameDb uqInfo = genKeyName $ UqNameGen tabNDb (uqColsDb uqInfo)
                            uqColsDb uqInfo = getDbColumnNames (uqColumns uqInfo)                    
                            addUniq uqInfo  = addUnique (coerce tabNDb)
                                                        (coerce (uqNameDb uqInfo))
                                                        (coerce (uqColsDb uqInfo))
                        in  map addUniq uqInfos
              addFks  = let fkInfos = foreignKeyInfo tabInfo
                            fkNameDb fkInfo = genKeyName $ FkNameGen tabNDb (fkColsDb fkInfo) (fkRefTabDb fkInfo)
                            fkRefTabDb fkInfo = dbTableName (getTabInfo (fkeyType fkInfo))
                            getTabInfo (ForeignKeyRefBy _ reft _) = reft
                            getTabInfo (ForeignKeyRef _ reft _)   = reft
                            fkColsDb fkInfo = getDbColumnNames (getFkColsDb (fkeyType fkInfo))
                            fkRefColsDb fkInfo = getDbColumnNames (getRefColsDb (fkeyType fkInfo))                    
                            getFkColsDb (ForeignKeyRefBy fkcols _ _) = fkcols
                            getFkColsDb (ForeignKeyRef   fkcol _ _)  = [fkcol]
                            getRefColsDb (ForeignKeyRefBy _ _ refcols) = refcols
                            getRefColsDb (ForeignKeyRef   _ _ refcol)  = [refcol]
                    
                            addFk fkInfo = addForeignKey (coerce tabNDb)
                                                         (coerce (fkNameDb fkInfo))
                                                         (coerce (fkColsDb fkInfo))
                                                         (coerce (fkRefTabDb fkInfo))
                                                         (coerce (fkRefColsDb fkInfo))
                                   
                        in  map addFk fkInfos
              addChks  = let ckInfos = checkInfo tabInfo
                             ckName ckInfo = genKeyName $ CkNameGen tabNDb (checkName ckInfo)
                             addCk ckInfo = addCheckExpr (coerce tabNDb)
                                                         (coerce (ckName ckInfo))
                                                         (coerce (checkExp ckInfo))
                         in  map addCk ckInfos
              addDefs = let defInfos = defaultInfo tabInfo
                            addDef defInfo = addDefaultExpr (coerce tabNDb)
                                                            (coerce (dbColumnName (columnNameInfo (defaultOn defInfo))))
                                                            (coerce (defaultExp  defInfo))
                        in map addDef defInfos
              addNotNullChks = let notNullCols              = getNonNullableColumns (columnInfo tabInfo)
                                   addNotNullCtx notNullCol = addNotNull (coerce tabNDb) (coerce (dbColumnName (columnNameInfo notNullCol)))
                               in map addNotNullCtx notNullCols
              cTabSeqs = let createSeq colInfo seqn = let coln = dbColumnName (columnNameInfo colInfo)
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
-}

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

-- camelToSnake, PascalToSnake 
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

runMigDiff :: ( Table db tab
               , Database db
               , SingCtx db tab
               , SingCtxDb db
               ) => Proxy (db :: *) -> Proxy (tab :: *) -> PGS.Connection -> IO ()
runMigDiff pdb ptab conn = undefined

data MigDirection = HsToDb | DbToHs
                  deriving (Show, Eq)

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

      , SingE (BaseForeignKey db tab ver)
      , SingI (BaseForeignKey db tab ver)

      , SingI (BaseTableSequence db tab ver)
      , SingE (BaseTableSequence db tab ver)

      , SingI tab
      , SingE tab
      ) => SingCtxBase db tab ver where

instance ( BaseTable db tab ver
         , Database db
           
         , SingE (GetBaseColumnInfo (DB db) (BaseColumns db tab ver))
         , SingI (GetBaseColumnInfo (DB db) (BaseColumns db tab ver))
           
         , SingI (BasePrimaryKeyName db tab ver)
         , SingE (BasePrimaryKeyName db tab ver)
           
         , SingE (BaseColumns db tab ver)
         , SingI (BaseColumns db tab ver)

         , SingE (BasePrimaryKey db tab ver)
         , SingI (BasePrimaryKey db tab ver)

         , SingE (BaseUnique db tab ver)
         , SingI (BaseUnique db tab ver)

         , SingE (BaseForeignKey db tab ver)
         , SingI (BaseForeignKey db tab ver)

         , SingI (BaseTableSequence db tab ver)
         , SingE (BaseTableSequence db tab ver)

         , SingI tab
         , SingE tab           
         ) => SingCtxBase db tab ver

class ( BaseDatabase db ver
      , SingE (BaseSchema db ver)
      , SingI (BaseSchema db ver)
        
      , SingI db
      , SingE db
      , Database db

      , SingI (GetPMT (Rep db))
      , SingE (GetPMT (Rep db))        
      ) => SingCtxBaseDb db ver where

instance ( BaseDatabase db ver
         , SingE (BaseSchema db ver)
         , SingI (BaseSchema db ver)
           
         , SingI db
         , SingE db
         , Database db
           
         , SingI (GetPMT (Rep db))
         , SingE (GetPMT (Rep db))           
         ) => SingCtxBaseDb db ver where  

baseDatabaseInfo :: forall db ver.
                ( SingCtxBaseDb db ver
                ) => Proxy (db :: *) -> Proxy ver -> DatabaseInfo
baseDatabaseInfo _ _ =
  mkDatabaseInfo (mkEntityName (coerce (fromSing (sing :: Sing (GetPMT (Rep db)))))
                               (T.pack (fromSing (sing :: Sing (BaseSchema db ver))))
                 ) [] 0 0 []

baseTableInfo :: forall db tab ver.
             ( SingCtxBase db tab ver             
             ) => Proxy db -> Proxy tab -> Proxy ver -> TableInfo
baseTableInfo db tab ver =
  mkTableInfo (basePkInfo db tab ver)
              (baseFkInfo db tab ver)
              (baseDefInfo db tab ver)
              (baseCksInfo db tab ver)
              (baseUqInfo db tab ver)
              (baseSeqsInfo db tab ver)
              (baseTabNameInfo db tab ver)
              (baseColInfos db tab ver)

basePkInfo :: forall db tab ver.
          ( BaseTable db tab ver
          , SingE (BasePrimaryKeyName db tab ver)
          , SingI (BasePrimaryKeyName db tab ver)
          , SingE (BasePrimaryKey db tab ver)
          , SingI (BasePrimaryKey db tab ver)
          ) => Proxy db -> Proxy tab -> Proxy ver -> Maybe PrimaryKeyInfo
basePkInfo _ _ _ =
  let pkCols = fromSing (sing :: Sing (BasePrimaryKey db tab ver))
  in  case pkCols of
    [] -> Nothing
    _  -> Just $ mkPrimaryKeyInfo (fmap T.pack (fromSing (sing :: Sing (BasePrimaryKeyName db tab ver))))
                                  (map T.pack pkCols)

baseFkInfo :: forall db tab ver.
          ( BaseTable db tab ver
          , SingE (BaseForeignKey db tab ver)
          , SingI (BaseForeignKey db tab ver)
          ) => Proxy db -> Proxy tab -> Proxy ver -> [ForeignKeyInfo]
baseFkInfo _ _ _ = 
  let fkds = fromSing (sing :: Sing (BaseForeignKey db tab ver))
  in map fkInfoOne fkds

  where fkInfoOne (RefByD fkname hsCols refHsTabN hsRefCols) =
             let etName = mkEntityName (T.pack fkname) (T.pack fkname)
             in  mkForeignKeyInfo etName (map T.pack hsCols) refHsTabN (map T.pack hsRefCols)
        fkInfoOne (RefD fkname hsCol refHsTabN) =
             let etName = mkEntityName (T.pack fkname) (T.pack fkname)
                 hsCols = map T.pack [hsCol]
             in  mkForeignKeyInfo etName hsCols refHsTabN hsCols
        
baseUqInfo :: forall db tab ver.
          ( BaseTable db tab ver
          , SingE (BaseUnique db tab ver)
          , SingI (BaseUnique db tab ver)
          ) => Proxy db -> Proxy tab -> Proxy ver -> [UniqueInfo]
baseUqInfo _ _ _ =
  let uniqs = fromSing (sing :: Sing (BaseUnique db tab ver))
  in  map uniqWithMapping uniqs
  
  where uniqWithMapping (uniqFlds, uniqHsName) =
          let etName = mkEntityName (T.pack uniqHsName) (T.pack uniqHsName)
          in  mkUniqueInfo etName (map T.pack uniqFlds)

baseDefInfo :: forall db tab ver.
           ( BaseTable db tab ver
           ) => Proxy db -> Proxy tab -> Proxy ver -> [DefaultInfo]
baseDefInfo _ _ _ = undefined

baseCksInfo :: forall db tab ver.
           ( BaseTable db tab ver
           ) => Proxy db -> Proxy tab -> Proxy ver -> [CheckInfo]
baseCksInfo _ _ _ = undefined

baseSeqsInfo :: forall db tab ver.
            ( BaseTable db tab ver
            , SingI (BaseTableSequence db tab ver)
            , SingE (BaseTableSequence db tab ver)
            ) => Proxy db -> Proxy tab -> Proxy ver -> [SequenceInfo]
baseSeqsInfo _ _ _ =
  let seqs = fromSing (sing :: Sing (BaseTableSequence db tab ver))
  in  map mkSeqInfo seqs

  where mkSeqInfo (seqcol, seqHsn, st) =
          let etName = mkEntityName (T.pack seqHsn) (T.pack seqHsn)
          in  mkSequenceInfo etName (T.pack seqcol) st

baseTabNameInfo :: forall tab db ver.
               ( BaseTable db tab ver
               , SingI tab
               , SingE tab
               ) => Proxy db -> Proxy tab -> Proxy ver -> EntityNameWithType
baseTabNameInfo _ _ _ =
  mkEntityName (coerce (fromSing (sing :: Sing tab)))
               undefined

baseColInfos :: forall tab db ver.
            ( BaseTable db tab ver
            , Database db
            , SingE (GetBaseColumnInfo (DB db) (BaseColumns db tab ver))
            , SingI (GetBaseColumnInfo (DB db) (BaseColumns db tab ver))
            ) => Proxy db -> Proxy tab -> Proxy ver -> [ColumnInfo]
baseColInfos _ _ _ =
  let hsns = fromSing (sing :: Sing (GetBaseColumnInfo (DB db) (BaseColumns db tab ver)))
  in  map go hsns
                         
  where go :: ((T.Text, Bool), String) -> ColumnInfo
        go ((typN, isNull), hsn) =
          let dbn = hsn
              etName = mkEntityName (T.pack hsn) (T.pack dbn)
          in mkColumnInfo isNull etName (coerce typN)

type family GetBaseColumnInfo (db :: DbK) (xs :: [(Symbol, DBTypeK)]) where
  GetBaseColumnInfo db ('(fld, x) ': xs) = 
    '(TagTypeInfo db x, fld) ': GetBaseColumnInfo db xs
  GetBaseColumnInfo db '[] =
    '[]
