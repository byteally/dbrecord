{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE DataKinds, TypeOperators, UndecidableInstances, OverloadedLabels, FlexibleInstances, MultiParamTypeClasses, DuplicateRecordFields, GADTs, TypeApplications, KindSignatures, DeriveGeneric, FlexibleContexts, FunctionalDependencies, ExplicitForAll, TypeFamilies, ScopedTypeVariables, PolyKinds, OverloadedStrings, UndecidableSuperClasses, TypeFamilyDependencies #-}
-- |

module Database.Migration where

import GHC.TypeLits
import GHC.Generics
import GHC.Exts
import Data.Kind
import Data.Proxy
import Data.Typeable
import Data.Type.Equality
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import Data.Int
import Data.ByteString (ByteString)
import Data.Time.LocalTime
import Data.Aeson
import Data.UUID.Types
import Data.Functor.Const
import Data.Functor.Identity
import qualified Database.Types as DBTy
import           Data.CaseInsensitive  (CI)
import           Data.Time.LocalTime (LocalTime)
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (UTCTime)



newtype (f :: Symbol) ::: t = Field t
  deriving (Show, Eq, Generic)

valOf :: (s ::: t) -> t
valOf (Field v) = v

data Col (a :: Symbol) = Col
data DBTable (tab :: Symbol) (cols :: [*]) = DBTable
data DBType (tab :: Symbol) (cols :: [*]) = DBType
data DefSyms = DefSyms [Symbol]

col :: forall (a :: Symbol). Col a
col = Col

class ( -- TypeCxts db (Types db)
      ) => Database (db :: *) where
  type Schema db :: Symbol
  type Schema db = "public"
  type Tables db :: [Type]
  type Types db :: [Type]
  type Types db = '[]
  type TabIgnore db :: [Type]
  type TabIgnore db = '[]

type family CustomPGTypeRep (ty :: *)  :: PGTypeK

type family IsGenTy (ty :: *) :: Constraint where
  IsGenTy (DBType _ _ ) = ()
  IsGenTy (DBTable _ _) = ()
  IsGenTy t             = Generic t

-- Maybe had the cxt be in table class no need for iter
{- and so
type family TableCxts (ts :: [*]) :: Constraint where
  TableCxts ((DBType _ _ ) ': ts) = TableCxts ts
  TableCxts ((DBTable _ _) ': ts) = TableCxts ts
  TableCxts (t             ': ts) = (Generic t, TableCxts ts)
  TableCxts '[]                   = ()
-}
{-
type family TypeCxts (db :: *) (ts :: [*]) :: Constraint where
  TypeCxts db ((DBType tyn flds ) ': ts) = ( KnownSymbol tyn
                                           , SingCols db flds
                                           , TypeCxts db ts
                                           )
  TypeCxts db (t             ': ts)      = ( Generic t
                                           , ShowPGType (GetPGTypeRep t)
                                           , SingCols db (GetTypeFields t)
                                           , TypeCxts db ts
                                           )
  TypeCxts db '[]                        = ()
-}

class ( Database db
      , AssertCxt (Elem (Tables db) tab) ('Text "Database " ':<>: 'ShowType db ':<>: 'Text " does not contain the table: " ':<>: 'ShowType tab)
      , ValidateTableProps tab
      , SingI (PrimaryKey tab)
      , SingI (Unique tab)
      , SingI (ForeignKey tab)
      , SingI (Check tab)
      , SingI ('DefSyms (HasDefault tab))
      , All SingE (PrimaryKey tab)
      , All SingE (Unique tab)
      , All SingE (ForeignKey tab)
      , All SingE (Check tab)
      , SingE ('DefSyms (HasDefault tab))
      , SingCols db (GetTableFields tab)
      , KnownSymbol (TableName tab)
      , IsGenTy tab
      ) => Table (db :: *) (tab :: *) where
  type PrimaryKey tab :: [Symbol]
  type PrimaryKey tab = '[]

  type ForeignKey tab :: [ForeignRef Type]
  type ForeignKey tab = '[]

  type Unique tab     :: [[Symbol]]
  type Unique tab = '[]

  type HasDefault tab :: [Symbol]
  type HasDefault tab = '[]

  type Check tab :: [CheckCT]
  type Check tab = '[]

  type ColIgnore tab :: IgnoredCol
  type ColIgnore tab = 'IgnoreNone

  type TableName tab :: Symbol
  type TableName tab = DefaultTableName tab

  defaults :: DBDefaults db tab
  defaults = DBDefaults Nil

  checks :: DBChecks db tab
  checks = DBChecks Nil

getSchemaName :: forall db.
               ( KnownSymbol (GetSchemaName db)
               ) => Const Text db
getSchemaName = Const $ T.pack $ symbolVal (Proxy @(GetSchemaName db))

getTableName :: forall db tab.
               ( KnownSymbol (TableName tab)
               ) => Const Text (db,tab)
getTableName = Const $ T.pack $ symbolVal (Proxy @(TableName tab))

getTableFields :: forall db tab.
                 ( SingCols db (GetTableFields tab)
                 ) => Const [Column] (db, tab)
getTableFields = Const $ recordToList $ singCols (Proxy @db) (Proxy @(GetTableFields tab))

class SingCols (db :: *) (cols :: [*]) where
  singCols :: Proxy db -> Proxy cols -> HList (Const Column) cols

instance ( SingCols db cols
         , KnownSymbol cn
         , InvalidPGType db ct
         , ShowPGType (GetPGTypeRep ct)
         ) => SingCols db ((cn ::: ct) ': cols) where
  singCols _ _ = let pgTy = showPGType (Proxy :: Proxy (GetPGTypeRep ct))
                     colN = T.pack $ symbolVal (Proxy @cn)
                 in (Const $ Column colN pgTy) :& singCols (Proxy @db) (Proxy :: Proxy cols)

instance SingCols db '[] where
  singCols _ _ = Nil

class SingAttrs (db :: *) (attrs :: [(Symbol, [*])]) where
  singAttrs :: Proxy db -> Proxy attrs -> HList (Const DConAttr) attrs

instance ( SingAttrs db cons
         , SingCols db flds
         , KnownSymbol c
         ) => SingAttrs db ('(c, flds) ': cons) where
  singAttrs pxyDB _ =
    let colHLists = singCols (Proxy @db) (Proxy @flds)
        cn = T.pack $ symbolVal (Proxy @c)
    in Const (DConAttr (cn, recordToList colHLists)) :& singAttrs pxyDB (Proxy @cons)

instance SingAttrs db '[] where
  singAttrs _ _ = Nil

newtype DConAttr = DConAttr (ColName, [Column])

data TypeAttr
  = SumAttr [(ColName, [Column])]
  | ProdAttr [Column]
  | EnumAttr [ColName]

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
  = let addPks = AlterTable tabN $ AddConstraint "pk_" $ AddPrimaryKey $ fmap T.pack $ fromSing (sing :: Sing pks)
        addUqs = let addUq fs = AlterTable tabN $ AddConstraint "uq_" $ AddUnique $ fmap T.pack fs
                 in fmap addUq $ fromSing (sing :: Sing uqs)
        addFks = let addFk (fcols, reft, rcols) = AlterTable tabN $ AddConstraint "fk_" $ AddForeignKey fcols reft rcols
                 in fmap addFk $ fromSing (sing :: Sing fks)
        addChks = let addChk chExpr = AlterTable tabN $ AddConstraint "ch_" $ AddCheck chExpr
                  in fmap addChk $ fromSing (sing :: Sing chks)
        addDefs = let addDef dfExpr = AlterTable tabN $ AlterColumn "col" $ AddDefault dfExpr
                  in fmap addDef $ fromSing (sing :: Sing ('DefSyms defs))
        tabColHList = singCols (Proxy @db) (Proxy :: Proxy (GetTableFields tab))
        createTab = [CreateTable tabN $ recordToList tabColHList]
        tabN = T.pack $ fromSing (sing :: Sing (TableName tab))
    in addPks : concat [ createTab
                       , addUqs
                       , addFks
                       , addChks
                       , addDefs
                       ]

type family TyCxts (db :: *) (tys :: [*]) :: Constraint where
  TyCxts db (ty ': ts) = ( ShowPGType (GetPGTypeRep ty)
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
                  , ShowPGType (GetPGTypeRep ty)
                  , SingAttrs db (GetTypeFields ty)
                  ) => Proxy (db :: *) -> Sing (ty :: *) -> [Migration]
mkMigrationType _ _
  = let tyName = showPGType (Proxy @(GetPGTypeRep ty))
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

recordToList :: HList (Const a) rs -> [a]
recordToList Nil = []
recordToList (x :& xs) = getConst x : recordToList xs

diffMigration :: [Migration] -> [Migration] -> [Migration] -> [Migration]
diffMigration _current _previous _reified = []

data family Sing (a :: k)

data instance Sing (s :: Symbol) where
  SSym :: KnownSymbol s => Sing s

data instance Sing (t :: *) where
  STypeRep :: Typeable t => Sing (t :: *)

data instance Sing (b :: Bool) where
  STrue :: Sing 'True
  SFalse :: Sing 'False

data instance Sing (xs :: [k]) where
  SNil  :: Sing '[]
  SCons :: Sing x -> Sing xs -> Sing (x ': xs)

data instance Sing (fk :: ForeignRef reft) where
  SRefBy :: ( All SingE fcols
           , All SingE rcols
           , KnownSymbol (TableName reft)
           ) => Sing fcols -> Sing reft -> Sing rcols -> Sing ('RefBy fcols reft rcols)
  SRef   :: KnownSymbol (TableName reft) => Sing col -> Sing reft -> Sing ('Ref col reft)

data instance Sing (ch :: CheckCT) where
  SCheck :: ( All SingE cols
           ) => Sing cols -> Sing cname -> Sing ('CheckOn cols cname)

data instance Sing (ch :: DefSyms) where
  SDef :: ( All SingE cols
         ) => Sing cols -> Sing ('DefSyms cols)

class SingI (a :: k) where
  sing :: Sing a

instance SingI 'True where
  sing = STrue

instance SingI 'False where
  sing = SFalse

instance SingI '[] where
  sing = SNil

instance (SingI x, SingI xs) => SingI (x ': xs) where
  sing = SCons sing sing

instance (KnownSymbol sy) => SingI (sy :: Symbol) where
  sing = SSym

instance (Typeable t) => SingI (t :: *) where
  sing = STypeRep

instance ( SingI fcols
         , SingI reft
         , SingI rcols
         , KnownSymbol (TableName reft)
         , All SingE fcols
         , All SingE rcols
         ) => SingI ('RefBy fcols reft rcols) where
  sing = SRefBy sing sing sing

instance ( SingI col
         , SingI reft
         , KnownSymbol (TableName reft)
         ) => SingI ('Ref col reft) where
  sing = SRef sing sing

instance ( SingI cols
         , KnownSymbol cname
         , All SingE cols
         ) => SingI ('CheckOn cols cname) where
  sing = SCheck sing sing

instance (SingI cols, All SingE cols) => SingI ('DefSyms cols) where
  sing = SDef sing

class SingE (a :: k) where
  type Demote a :: *
  fromSing :: Sing a -> Demote (Any :: k)

instance SingE (b :: Bool) where
  type Demote b = Bool
  fromSing STrue  = True
  fromSing SFalse = False

instance SingE (sy :: Symbol) where
  type Demote sy = String
  fromSing SSym = symbolVal (Proxy :: Proxy sy)

instance All SingE xs => SingE (xs :: [k]) where
  type Demote (xs :: [k]) = [Demote (Any :: k)]
  fromSing SNil         = []
  fromSing (SCons x xs) = fromSing x : fromSing xs

tabName :: forall t proxy.KnownSymbol (TableName t) => proxy t -> String
tabName _ = symbolVal (Proxy :: Proxy (TableName t))

instance SingE (ft :: ForeignRef reft) where
  type Demote ft = ([Text], Text, [Text])
  fromSing (SRefBy fcols reft rcols) = ( fmap T.pack $ fromSing fcols
                                       , T.pack $ tabName reft
                                       , fmap T.pack $ fromSing rcols
                                       )
  fromSing (SRef coln reft) = ( [T.pack $ fromSing coln]
                             , T.pack $ tabName reft
                             , []
                             )

instance SingE (ch :: CheckCT) where
  type Demote ch = CheckExpr
  fromSing (SCheck cols _cname) = T.pack $ concat $ fromSing cols

instance SingE (defs :: DefSyms) where
  type Demote defs = [DefExpr]
  fromSing (SDef cols) = fmap T.pack $ fromSing cols

newtype I a = I a
  deriving (Show, Eq)

class (AllF f xs) => All (f :: k -> Constraint) (xs :: [k])
instance (AllF f xs) => All f xs

type family AllF (c :: k -> Constraint) (xs :: [k]) :: Constraint
type instance AllF _c '[]       = ()
type instance AllF  c (x ': xs) = (c x, All c xs)

class (AllF (All f) xss) => All2 f xss
instance (AllF (All f) xss) => All2 f xss

type family If (c :: Bool) (t :: k) (f :: k) :: k where
  If 'True t f  = t
  If 'False t f = f

type family ValidateTableProps (tab :: *) :: Constraint where
  ValidateTableProps tab = ( MissingField tab (ElemFields1 (GetTableFields tab) (PrimaryKey tab))
                           , MissingField tab (ElemFields1 (GetTableFields tab) (HasDefault tab))
                           , MissingField tab (ElemFields2 (GetTableFields tab) (Unique tab))
                           , ValidateTabFk tab (ForeignKey tab)
                           , ValidateTabCk tab (Check tab)
                           , ValidateTabIx tab
                           )

type family ValidateTabPk (tab :: *) (pks :: [Symbol]) :: Constraint where
  ValidateTabPk tab (p ': ps) = If (ElemField (GetTableFields tab) p) (ValidateTabPk tab ps) (TypeError ('Text "column " ':<>: ('ShowType p) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab)))
  ValidateTabPk tab '[]       = ()

type family ValidateTabFk tab (fks :: [ForeignRef Type]) :: Constraint where
  ValidateTabFk tab ('Ref fn reft ': fks) = (MatchFkRefFld tab reft fn (FindField (GetTableFields tab) fn) (FindField (GetTableFields reft) (HeadPk reft (PrimaryKey reft))),  ValidateTabFk tab fks)
  ValidateTabFk tab ('RefBy fkeys reft rkeys ': fks) = ValidateTabFk tab fks
  ValidateTabFk tab '[]         = ()

type family HeadPk (tab :: *) (pks :: [Symbol]) where
  HeadPk tab '[pk] = pk
  HeadPk tab '[]   = TypeError ('Text "Invalid foreign key! Referenced table does not have primary key: " ':<>: 'ShowType tab)
  HeadPk tab pks   = TypeError ('Text "Invalid foreign key! Referenced table have composite primary key: " ':<>: 'ShowType tab)

type family MatchFkRefFld tab reft (fn :: Symbol) (t1 :: Maybe *) (t2 :: Maybe *) :: Constraint where
  MatchFkRefFld tab reft fn ('Just t) ('Just t)   = ()
  MatchFkRefFld tab reft fn ('Just t1) ('Just t2) = TypeError ('Text "Type mismatch between foreign key and primary key")
  MatchFkRefFld tab reft fn ('Just t) 'Nothing    = ()
  MatchFkRefFld tab reft fn 'Nothing t            = TypeError ('Text "column " ':<>: ('ShowType fn) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab))


type family ValidateTabCk tab (chks :: [CheckCT]) :: Constraint where
  ValidateTabCk tab ('CheckOn fs cn ': chks) = ValidateTabCk' (ElemFields1 (GetTableFields tab) fs) tab cn chks
  ValidateTabCk tab '[] = ()

type family ValidateTabCk' (mis :: Maybe Symbol) (tab :: *) (cn :: Symbol) (chks :: [CheckCT]) where
  ValidateTabCk' ('Just fn) tab cn chks = (TypeError ('Text "column " ':<>: ('ShowType fn) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab)))
  ValidateTabCk' 'Nothing tab cn chks   = ValidateTabCk tab chks

type family ValidateTabIx tab :: Constraint where
  ValidateTabIx tab = ()

type family ElemFields1 (flds :: [*]) (fs :: [Symbol]) :: Maybe Symbol where
  ElemFields1 flds (f :fs) = If (ElemField flds f) (ElemFields1 flds fs) ('Just f)
  ElemFields1 flds '[]     = 'Nothing

type family ElemFields2 (flds :: [*]) (fss :: [[Symbol]]) :: Maybe Symbol where
  ElemFields2 flds (fs :fss) = ElemFields2' (ElemFields1 flds fs) flds fss
  ElemFields2 flds '[]       = 'Nothing

type family ElemFields2' (may :: Maybe Symbol) (flds :: [*]) (fss :: [[Symbol]])  :: Maybe Symbol where
  ElemFields2' ('Just fn) flds fss = ('Just fn)
  ElemFields2' 'Nothing flds fss   = ElemFields2 flds fss

type family MissingField (tab :: *) (fn :: Maybe Symbol) :: Constraint where
  MissingField tab ('Just fn) = TypeError ('Text "column " ':<>: ('ShowType fn) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab))
  MissingField tab 'Nothing   = ()

data ForeignRef refd
  = RefBy [Symbol] refd [Symbol]
  | Ref Symbol refd

data CheckCT = CheckOn [Symbol] Symbol

data Ix = Ix Symbol

data IgnoredCol
  = IgnoreRest
  | IgnoreOnly [Symbol]
  | IgnoreExcept [Symbol]
  | IgnoreNone

data HList :: (k -> *) -> [k] -> * where
  Nil  :: HList f '[]
  (:&) :: f t -> HList f ts -> HList f (t ': ts)

infixr 7 :&

infixr 5 :++
type family (:++) (as :: [k]) (bs :: [k]) :: [k] where
  '[] :++ bs       = bs
  (a ': as) :++ bs = a ': (as :++ bs)

data Def (tab :: *) (fn :: Symbol) = forall v.Def v

def :: forall (fn :: Symbol) (tab :: *) v.(ValidateDBFld tab fn v) => v -> Def tab fn
def = Def

data DBDefaults (db :: *) tab = forall xs.DBDefaults (HList (Def tab) xs)

end :: HList f '[]
end = Nil

dbDefaults :: forall tab db xs.HList (Def tab) xs -> DBDefaults db tab
dbDefaults = DBDefaults

data Chk (tab :: *) (chk :: CheckCT) = forall val.Chk val

data DBChecks (db :: *) tab = forall chks.DBChecks (HList (Chk tab) chks)

type family LookupCheck (chks :: [CheckCT]) (cn :: Symbol) :: Maybe [Symbol] where
  LookupCheck ('CheckOn args cn ': chks) cn  = 'Just args
  LookupCheck ('CheckOn args cn1 ': chks) cn = LookupCheck chks cn
  LookupCheck '[] cn                         = 'Nothing

type family UnifyCheck (tab :: *) (cn :: Symbol) (flds :: [*]) (args :: Maybe [Symbol]) (val :: *) :: Constraint where
  UnifyCheck tab cn flds 'Nothing val = TypeError ('Text "check constraint " ':<>: 'ShowType cn ':<>: 'Text " does not exist on table " ':<>: 'ShowType tab)
  UnifyCheck tab cn flds ('Just args) val = UnifyCheckFn tab args val flds

type family UnifyCheckFn (tab :: *) (args :: [Symbol]) (val :: *) (flds :: [*]) :: Constraint where
  UnifyCheckFn tab '[] (t -> r) flds = TypeError ('Text "Arity mismatch! Check function accepts more argument than required")
  UnifyCheckFn tab (fn ': fs) (t -> r) flds = (UnifyField flds (fn ::: t) ('Text "column " ':<>: ('ShowType fn) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab)), UnifyCheckFn tab fs r flds)
  UnifyCheckFn tab (fn ': fs) r flds = TypeError ('Text "Arity mismatch! Check function accepts less argument than required")
  UnifyCheckFn tab '[] r flds = (r ~ Bool)

type family PartialJust (may :: Maybe k) :: k where
  PartialJust ('Just m) = m

check :: forall (cn :: Symbol) (tab :: *) val args.
        ( args ~ LookupCheck (Check tab) cn
        , UnifyCheck tab cn (GetTableFields tab) args val
        ) => val -> Chk tab ('CheckOn (PartialJust args) cn)
check = Chk

dbChecks :: forall tab (db :: *) chks.HList (Chk tab) chks -> DBChecks db tab
dbChecks = DBChecks

type family ValidateDBFld tab (fn :: Symbol) t :: Constraint where
  ValidateDBFld tab fn t = UnifyField (GetTableFields tab) (fn ::: t) ('Text "column " ':<>: ('ShowType fn) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab))

type family GetTypeName (t :: *) :: Symbol where
  GetTypeName (DBType ty fs) = ty
  GetTypeName t              = GenTyCon (Rep t)

type family DefaultTableName (t :: *) :: Symbol where
  DefaultTableName (DBTable tab fs) = tab
  DefaultTableName t                = GenTyCon (Rep t)

type family GetSchemaName (t :: *) :: Symbol where
  -- TODO: () instance is a hack to get constraint
  -- KnownSymbol (GetSchemaName t) instead of
  -- KnownSymbol (Schema t)
  GetSchemaName ()               = Schema ()
  GetSchemaName db               = Schema db

type family GetTableFields (t :: *) :: [*] where
  GetTableFields (DBTable tab fs) = fs
  GetTableFields (DBType ty fs)   = TypeError ('Text "GetTableFields cannot be applied on " ':<>: 'ShowType (DBType ty fs))
  GetTableFields t                = GenTabFields (Rep t)

type family GetTypeFields (t :: *) :: [(Symbol, [*])] where
  GetTypeFields (DBType ty fs) = '[ '(ty, fs)]
  GetTypeFields t              = GenTyFields (Rep t)

newtype EnumType a = EnumType a
newtype SumType a = SumType a

type family Elem (xs :: [k]) (v :: k) :: Bool where
  Elem (x ': xs) x = 'True
  Elem (x ': xs) y = Elem xs y
  Elem '[]       x = 'False

type family FindField (xs :: [*]) (fn :: Symbol) :: (Maybe *) where
  FindField ((fn ::: t) ': xs) fn  = 'Just t
  FindField ((fn' ::: t) ': xs) fn = FindField xs fn
  FindField '[] fn                 = 'Nothing

type family ElemField (xs :: [*]) (fn :: Symbol) :: Bool where
  ElemField ((fn ::: t) ': xs) fn  = 'True
  ElemField ((fn' ::: t) ': xs) fn = ElemField xs fn
  ElemField '[] fn                 = 'False


type family AssertCxt (c :: Bool) (a :: ErrorMessage) :: Constraint where
  AssertCxt 'True msg  = ()
  AssertCxt 'False msg = TypeError msg

type family UnifyField (flds :: [*]) (f :: *) (nfMsg :: ErrorMessage) :: Constraint where
  UnifyField ((fn ::: ft') ': fs) (fn ::: ft) nfMsg  = (ft ~ ft')
  UnifyField ((fn' ::: ft') ': fs) (fn ::: ft) nfMsg = UnifyField fs (fn ::: ft) nfMsg
  UnifyField '[] (fn ::: ft) nfMsg                   = TypeError nfMsg

type family Concat (xss :: [[k]]) :: [k] where
  Concat (xs ': xss) = xs   -- TODO:
  Concat '[]         = '[]

{-
ERROR:  foreign key constraint "pk_users" cannot be implemented
DETAIL:  Key columns "id" and "id" are of incompatible types: text and integer.
-
ERROR:  column "id" referenced in foreign key constraint does not exist
-
ERROR:  there is no unique constraint matching given keys for referenced table "super_user"
-}

{- Constraints Naming Convention
 "ix": 'ix_%(column_0_label)s',
 "uq": "uq_%(table_name)s_%(column_0_name)s",
 "ck": "ck_%(table_name)s_%(constraint_name)s",
 "fk": "fk_%(table_name)s_%(column_0_name)s_%(referred_table_name)s",
 "pk": "pk_%(table_name)s"
-}

type family DBConstraintFmt (tab :: *) (ct :: k) :: [Symbol] where
  DBConstraintFmt tab (pks :: [Symbol])    = "pk" ': (TableName tab) ': '[]
  DBConstraintFmt tab (uqs :: [[Symbol]])  = "uk" ': (TableName tab) ': (Concat uqs)
  DBConstraintFmt tab ('Ref fld reft)     = "fk" ': (TableName tab) ': fld ': '[(TableName reft)]
  DBConstraintFmt tab ('RefBy fs reft ft) = "fk" ': (TableName tab) ': (TableName reft) ': fs -- TODO: Maybe change the ord of reft.
  DBConstraintFmt tab ('CheckOn fs cn)    = "ck" ': (TableName tab) ': '[cn]
  DBConstraintFmt tab ('Ix col)           = '["ix", col]

-- mig

type TabName  = Text
type TypeName = Text
type ColName  = Text
type ColType  = Text
type ConstraintName = Text
type CheckExpr = Text
type DefExpr = Text
data Column = Column !ColName !ColType
  deriving (Show)
type PrimaryKeys = [Text]

data Migration
  = CreateTable !TabName [Column]
  | CreateType !TypeName [Column]
  | CreateEnum !TypeName [Text]
  | DropTable !TabName
  | DropType !TypeName
  | AlterTable !TabName AlterTable
  | AlterType !TypeName AlterType
  deriving (Show)

data AlterTable
  = AddColumn Column
  | DropColumn !ColName
  | RenameColumn !ColName !ColName
  | AlterColumn !ColName AlterColumn
  | RenameTable !TabName
  | AddConstraint !ConstraintName AddConstraint
  | DropConstraint !ConstraintName
  deriving (Show)

data AddConstraint
  = AddPrimaryKey [ColName]
  | AddUnique [ColName]
  | AddCheck CheckExpr
  | AddForeignKey [ColName] !TabName [ColName]
  deriving (Show)

data AlterColumn
  = SetNotNull
  | DropNotNull
  | ChangeType !ColType
  | AddDefault DefExpr
  | DropDefault
  deriving (Show)

data AlterType
  = RenameType !TypeName
  | AddAttribute Column
  | DropAttribute !ColName
  | AlterAttribute !ColName AlterAttribute
  | AddAfterEnumVal !Text !Text
  deriving (Show)

data AlterAttribute
  = ChangeAttrType !ColType
  deriving (Show)

migrationSql :: Migration -> Text
migrationSql (CreateTable tab cols) = T.concat
  [ "CREATE TABLE "
  , doubleQuote tab
  , " ("
  , T.intercalate ", " $ fmap columnSql cols
  , ");"
  ]
migrationSql (CreateType ty cols) = T.concat
  [ "CREATE TYPE "
  , ty
  , " AS ("
  , T.intercalate ", " $ fmap columnSql cols
  , ");"
  ]
migrationSql (CreateEnum ty cols) = T.concat
  [ "CREATE TYPE "
  , ty
  , " AS ENUM ("
  , T.intercalate ", " (fmap singleQuote cols)
  , ");"
  ]
migrationSql (DropTable tab) = T.concat
  [ "DROP TABLE "
  , doubleQuote tab
  ,";"
  ]
migrationSql (DropType ty) = T.concat
  [ "DROP TYPE "
  , ty
  , ";"
  ]
migrationSql (AlterTable tab alter) = T.concat
  [ "ALTER TABLE "
  , doubleQuote tab
  , alterTableSql alter
  , ";"
  ]

columnSql :: Column -> Text
columnSql (Column name ty) = T.concat [doubleQuote name, " ", ty]

alterTableSql :: AlterTable -> Text
alterTableSql (AddColumn coln) = T.concat
  [ " ADD COLUMN "
  , columnSql coln
  ]

alterTableSql (DropColumn coln) = T.concat
  [ " DROP COLUMN "
  , coln
  ]
alterTableSql (RenameColumn oldn newn) = T.concat
  [ " RENAME COLUMN "
  , oldn
  , " TO "
  , newn
  ]
alterTableSql (AlterColumn coln alter) = T.concat
  [ " ALTER COLUMN "
  , coln
  , alterColumnSql alter
  ]
alterTableSql (RenameTable newn) = T.concat
  [ " RENAME TO "
  , newn
  ]
alterTableSql (AddConstraint cname con) = T.concat
  [ " ADD CONSTRAINT "
  , cname
  , addConstraintSql con
  ]
alterTableSql (DropConstraint cname) = T.concat
  [ " DROP CONSTRAINT "
  , cname
  ]

addConstraintSql :: AddConstraint -> Text
addConstraintSql (AddPrimaryKey cols) = T.concat
  [ " PRIMARY KEY "
  , "("
  , sepByComma cols
  , ")"
  ]
addConstraintSql (AddUnique cols) = T.concat
  [ " UNIQUE "
  , "("
  , sepByComma cols
  , ")"
  ]
addConstraintSql (AddCheck chkExpr) = T.concat
  [ " CHECK "
  , "("
  , chkExpr
  , ")"
  ]
addConstraintSql (AddForeignKey fcols rtab rcols) = T.concat
  [ " FOREIGN KEY "
  , "("
  , sepByComma fcols
  , ") REFERENCES "
  , doubleQuote rtab
  , " ("
  , sepByComma rcols
  , ")"
  ]

alterColumnSql :: AlterColumn -> Text
alterColumnSql SetNotNull = " SET NOT NULL"
alterColumnSql DropNotNull = " DROP NOT NULL"
alterColumnSql (ChangeType ctype) = T.concat
  [ " TYPE "
  , ctype
  ]
alterColumnSql (AddDefault defV) = T.concat
  [ " SET DEFAULT "
  , defV
  ]
alterColumnSql DropDefault = " DROP DEFAULT"

alterType :: AlterType -> Text
alterType (RenameType newTy) = T.concat
  [ " RENAME TO "
  , doubleQuote newTy
  ]
alterType (AddAttribute colN) = T.concat
  [ " ADD ATTRIBUTE "
  , columnSql colN
  ]
alterType (DropAttribute cname) = T.concat
  [ " DROP ATTRIBUTE "
  , cname
  ]
alterType (AddAfterEnumVal newVal oldVal) = T.concat
  [ " ADD VALUE "
  , newVal
  , " AFTER "
  , oldVal
  ]
alterType (AlterAttribute cname alter) = T.concat
  [ " ALTER ATTRIBUTE "
  , cname
  , alterAttr alter
  ]

alterAttr :: AlterAttribute -> Text
alterAttr (ChangeAttrType ty) = T.concat
  [ " SET DATA TYPE "
  , ty
  ]

sepByComma :: [Text] -> Text
sepByComma names = T.intercalate ", " names

singleQuote :: Text -> Text
singleQuote = quoteBy '\'' (Just '\'')

doubleQuote :: Text -> Text
doubleQuote = quoteBy '"' (Just '"')

quoteBy :: Char -> Maybe Char -> Text -> Text
quoteBy ch esc s = T.pack $ ch : go esc (T.unpack s) ++ (ch:[])
  where
    go Nothing s'           = s'
    go (Just _) ""          = ""
    go (Just esch) (ch':xs)
      | ch' == esch          = esch : ch': go esc xs
    go esc' (x:xs)          = x : go esc' xs

type family InvalidPGType (db :: *) a :: Constraint where
  InvalidPGType _ Int           = ()
  InvalidPGType _ Double        = ()
  InvalidPGType _ Int64         = ()
  InvalidPGType _ Int32         = ()
  InvalidPGType _ Bool          = ()
  InvalidPGType _ String        = ()
  InvalidPGType _ Text          = ()
  InvalidPGType _ (CI Text)     = ()
  InvalidPGType _ ByteString    = ()
  InvalidPGType _ Value         = ()
  InvalidPGType _ UTCTime       = ()
  InvalidPGType _ LocalTime     = ()
  InvalidPGType _ TimeOfDay     = ()
  InvalidPGType _ Day           = ()
  InvalidPGType _ UUID          = ()
  InvalidPGType db (Maybe a)     = InvalidPGType db a
  InvalidPGType db (Vector a)    = InvalidPGType db a
  InvalidPGType db (Json a)      = InvalidPGType db a
  InvalidPGType db (JsonStr a)   = InvalidPGType db a
  InvalidPGType _ (DBType _ _)  = ()
  InvalidPGType db (CustomType a) = ()
  InvalidPGType db a = ValidateCustTy db (IsNewTy (Rep a)) a
{-  InvalidPGType db a = AssertCxt (Elem (Types db) (UnWrapNT (IsNewTy (Rep a)) a))
    ('Text "Invalid postgres type: " :<>: (ShowType (UnWrapNT (IsNewTy (Rep a)) a))
     :$$: 'Text "Hint: Add " :<>: (ShowType (UnWrapNT (IsNewTy (Rep a)) a)) :<>: ('Text " to Types field in Database instance of ") :<>: (ShowType db)
    )
-}

type family ValidateCustTy (db :: *) (isNewTy :: Bool) (t :: *) :: Constraint where
  ValidateCustTy db 'True t = InvalidPGType db (InnerTy t)
  ValidateCustTy db 'False t = AssertCxt (Elem (Types db) t)
    ('Text "Invalid postgres type: " ':<>: 'ShowType t
     ':$$: 'Text "Hint: Add " ':<>: 'ShowType t ':<>: ('Text " to Types field in Database instance of ") ':<>: ('ShowType db)
    )
type family UnWrapNT (isNewTy :: Bool) (t :: *) where
  UnWrapNT 'True t  = InnerTy t
  UnWrapNT 'False t = t

data PGSqlName (t :: *)

data PGTypeRep (rep :: PGTypeK)

type instance DBTy.Column PGSqlName t = PGTypeRep (GetPGTypeRep t)

data PGTypeK
  = PGInt4
  | PGInt8
  | PGInt2
  | PGFloat4
  | PGFloat8
  | PGBool
  | PGNumeric Nat Nat
  | PGChar Nat
  | PGText
  | PGCiText
  | PGDate
  | PGTime
  | PGTimestamp
  | PGTimestamptz
  | PGUuid
  | PGByteArr
  | PGJson
  | PGJsonB
  | PGArray PGTypeK
  | PGNullable PGTypeK
  | PGCustomType Type PGTypeK Bool
  | PGTypeName Symbol


type family GetPGTypeRep (t :: *) = (r :: PGTypeK) | r -> t where
  GetPGTypeRep Int                = 'PGInt4
  GetPGTypeRep Int16              = 'PGInt2
  GetPGTypeRep Int64              = 'PGInt8
  GetPGTypeRep Double             = 'PGFloat8
  GetPGTypeRep Char               = 'PGChar 1
  GetPGTypeRep Text               = 'PGText
  GetPGTypeRep (CI Text)          = 'PGCiText
  GetPGTypeRep ByteString         = 'PGByteArr
  GetPGTypeRep Bool               = 'PGBool
  GetPGTypeRep Day                = 'PGDate
  GetPGTypeRep UTCTime            = 'PGTimestamptz
  GetPGTypeRep LocalTime          = 'PGTimestamp
  GetPGTypeRep TimeOfDay          = 'PGTime
  GetPGTypeRep Value              = 'PGJsonB
  GetPGTypeRep (Json a)           = 'PGCustomType (Json a) 'PGJsonB 'False
  GetPGTypeRep (JsonStr a)        = 'PGCustomType (JsonStr a) 'PGJson 'False
  GetPGTypeRep UUID               = 'PGUuid
  GetPGTypeRep (Maybe t)          = 'PGNullable (GetPGTypeRep t)
  GetPGTypeRep (Vector t)         = 'PGArray (GetPGTypeRep t)
  GetPGTypeRep (DBType ty flds)   = 'PGCustomType (DBType ty flds) ('PGTypeName ty) 'False
  GetPGTypeRep (CustomType a)     = 'PGCustomType (CustomType a) (CustomPGTypeRep a) 'False
  GetPGTypeRep a                  = 'PGCustomType a ('PGTypeName (GetTypeName a)) (IsNewType (Rep a))

newtype CustomType a = CustomType a

type family GenTyCon (rep :: * -> *) :: Symbol where
  GenTyCon (D1 ('MetaData tyName _ _ _) _) = tyName
  GenTyCon r                               = TypeError ('Text "GenTyCon expects only generic rep of type, but found " ':<>: 'ShowType r)

type family IsNewType (rep :: * -> *) :: Bool where
  IsNewType (D1 ('MetaData _ _ _ isNew) _) = isNew

type family InnerTy (t :: *) :: * where
  InnerTy Int                = Int
  InnerTy Int16              = Int16
  InnerTy Int64              = Int64
  InnerTy Double             = Double
  InnerTy Char               = Char
  InnerTy Text               = Text
  InnerTy (CI Text)          = CI Text
  InnerTy ByteString         = ByteString
  InnerTy Bool               = Bool
  InnerTy Day                = Day
  InnerTy UTCTime            = UTCTime
  InnerTy LocalTime          = LocalTime
  InnerTy TimeOfDay          = TimeOfDay
  InnerTy Value              = Value
  InnerTy (Json a)           = Json a
  InnerTy (JsonStr a)        = JsonStr a
  InnerTy UUID               = UUID
  InnerTy (Maybe t)          = Maybe t
  InnerTy (Vector t)         = Vector t
  InnerTy (DBType ty flds)   = DBType ty flds
  InnerTy (CustomType a)     = CustomType a
  InnerTy a                  = GenInnerTy (Rep a)

type family GenInnerTy (rep :: * -> *) :: * where
  GenInnerTy (D1 _ (C1 _ (S1 _ (K1 _ t)))) = InnerTy t
  GenInnerTy r = TypeError ('Text "Expecting a newtype rep but found: " ':<>: 'ShowType r)

type family IsNewTy (rep :: * -> *) :: Bool where
  IsNewTy (D1 _ (C1 _ (S1 _ (K1 _ t)))) = 'True
  IsNewTy _                             = 'False

type family (a :: Bool) && (b :: Bool) :: Bool where
  'True && 'True = 'True
  a     && b     = 'False

data AdtK = EnumTy | SumTy Bool | ProdTy Bool

type family IsEnumLike (rep :: * -> *) :: Bool where
  IsEnumLike (D1 i f)                    = IsEnumLike f
  IsEnumLike ((C1 i1 c1) :+: (C1 i2 c2)) = IsEnumLike c1 && IsEnumLike c2
  IsEnumLike U1                          = 'True
  IsEnumLike r                           = 'False

type family ADTType (rep :: * -> *) :: AdtK where
  ADTType (D1 i f)                    = ADTType f
  ADTType ((C1 i1 U1) :+: (C1 i2 U1)) = 'EnumTy
  ADTType ((C1 i1 U1) :+: (C1 i2 c))  = ADTType c
  ADTType ((C1 i1 c)  :+: (C1 i2 U1)) = ADTType c
  ADTType ((C1 i1 c1) :+: (C1 i2 c2)) = ADTType c1
  ADTType (C1 ('MetaCons cn _ 'False) _) = TypeError ('Text "The constructor " ':<>: 'ShowType cn ':<>: 'Text " does not have named fields")

type family GenTyFields (rep :: * -> *) :: [(Symbol, [*])] where
  GenTyFields (D1 i f)  = GenTyFields f
  GenTyFields (f :+: g) = (GenTyFields f) :++ (GenTyFields g)
  GenTyFields (C1 ('MetaCons cn i t) c)  = '[ '(cn, GenProdTyFields (C1 ('MetaCons cn i t) c))]

type family GenProdTyFields (rep :: * -> *) :: [*] where
  GenProdTyFields (C1 i c)  = GenProdTyFields c
  GenProdTyFields U1        = '[]
  GenProdTyFields (f :*: g) = GenProdTyFields f :++ GenProdTyFields g
  GenProdTyFields (S1 ('MetaSel ('Just sn) _ _ _) (K1 i f)) = '[sn ::: f]

type family GenTabFields (rep :: * -> *) :: [*] where
  GenTabFields (D1 i f)  = GenTabFields f
  GenTabFields (f :+: g) = TypeError ('Text "Table cannot be a sum type")
  GenTabFields (C1 ('MetaCons cn _ 'False) _) = TypeError ('Text "The constructor " ':<>: 'ShowType cn ':<>: 'Text " does not have named fields")
  GenTabFields (C1 i c) = GenTabFields c
  GenTabFields (f :*: g) = GenTabFields f :++ GenTabFields g
  GenTabFields (S1 ('MetaSel ('Just sn) _ _ _) (K1 i f)) = '[sn ::: f]

newtype JsonStr a = JsonStr a
newtype Json a = Json a

json :: (ToJSON a) => a -> Json a
json = Json

jsonStr :: (ToJSON a) => a -> JsonStr a
jsonStr = JsonStr

class ShowPGType (pgTy :: PGTypeK) where
  showPGType :: Proxy pgTy -> Text

instance ShowPGType 'PGInt2 where
  showPGType _ = "SMALLINT"

instance ShowPGType 'PGInt4 where
  showPGType _ = "INTEGER"

instance ShowPGType 'PGInt8 where
  showPGType _ = "BIGINT"

instance ShowPGType 'PGBool where
  showPGType _ = "BOOLEAN"

instance ShowPGType 'PGFloat8 where
  showPGType _ = "DOUBLE PRECISION"

instance KnownNat n => ShowPGType ('PGChar n) where
  showPGType _ = T.pack $ "CHARACTER (" ++ (show $ natVal $ Proxy @n) ++ ")"

instance ShowPGType 'PGText where
  showPGType _ = "TEXT"

instance ShowPGType 'PGByteArr where
  showPGType _ = "BYTEA"

instance ShowPGType 'PGTimestamptz where
  showPGType _ = "TIMESTAMPTZ"

instance ShowPGType 'PGTimestamp where
  showPGType _ = "TIMESTAMP"

instance ShowPGType 'PGDate where
  showPGType _ = "DATE"

instance ShowPGType 'PGTime where
  showPGType _ = "TIME"

instance ShowPGType 'PGUuid where
  showPGType _ = "UUID"

instance ShowPGType 'PGJsonB where
  showPGType _ = "JSONB"

instance ShowPGType pgTy => ShowPGType ('PGNullable pgTy) where
  showPGType _ = showPGType (Proxy :: Proxy pgTy)

instance ShowPGType pgTy => ShowPGType ('PGArray pgTy) where
  showPGType _ = showPGType (Proxy :: Proxy pgTy) `T.append` "[]"

instance KnownSymbol tab => ShowPGType ('PGTypeName tab) where
  showPGType _ = T.toUpper $ T.pack $ symbolVal (Proxy :: Proxy tab)

instance (ShowPGType (GetPGTypeRep (InnerTy ty))) => ShowPGType ('PGCustomType ty pgTy 'True) where
  showPGType _ = showPGType (Proxy :: Proxy (GetPGTypeRep (InnerTy ty)))

instance (ShowPGType pgTy, Typeable pgTy) => ShowPGType ('PGCustomType ty pgTy 'False) where
  showPGType _ = if typeRepTyCon (typeRep (Proxy @pgTy)) == typeRepTyCon (typeRep (Proxy @ 'PGTypeName))
                 then doubleQuote $ showPGType (Proxy :: Proxy pgTy)
                 else showPGType (Proxy :: Proxy pgTy)


class (Applicative f) => GTypeToRec f rep xs | rep -> xs where
  gTypeToRec :: rep a -> HList f xs

instance GTypeToRec f d ts => GTypeToRec f (D1 ('MetaData tyn pkgn modn isNew) d) ts where
  gTypeToRec (M1 a) = gTypeToRec a

instance GTypeToRec f c ts => GTypeToRec f (C1 ('MetaCons cn fix 'True) c) ts where
  gTypeToRec (M1 a) = gTypeToRec a

instance ( GTypeToRec m f fs
         , GTypeToRec m g gs
         , ts ~ (fs :++ gs)
         ) => GTypeToRec m (f :*: g) ts where
  gTypeToRec (f :*: g) = gTypeToRec f `rappend` gTypeToRec g

instance (GTypeToRec m s '[t], Applicative m) => GTypeToRec m (S1 ('MetaSel ('Just fn) pack strict inf) s) '[fn ::: t] where
  gTypeToRec (M1 a) = case gTypeToRec a :: HList m '[t] of
    x :& Nil -> (Field <$> x) :& Nil

instance (Applicative m) => GTypeToRec m (K1 k f) '[f] where
  gTypeToRec (K1 a) = pure a :& Nil

rappend
  :: HList f as
  -> HList f bs
  -> HList f (as :++ bs)
rappend Nil ys = ys
rappend (x :& xs) ys = x :& (xs `rappend` ys)

typeToRec :: (Generic t, GTypeToRec f (Rep t) ts) => t -> HList f ts
typeToRec t = gTypeToRec (from t)

recToType :: (Generic t, GRecToType f (Rep t) ts, Extract f) => HList f ts -> t
recToType r = to $ gRecToType r

class GRecToType f rep ts where
  gRecToType :: Extract f => HList f ts -> rep a

instance (GRecToType f d xs) => GRecToType f (D1 ('MetaData tyn pkgn modn isNew) d) xs where
  gRecToType hrec = M1 $ gRecToType hrec

instance (GRecToType f c xs) => GRecToType f (C1 ('MetaCons cn fix 'True) c) xs where
  gRecToType hrec = M1 $ gRecToType hrec

instance (GRecToType f a xs, GRecToType f b xs) => GRecToType f (a :*: b) xs where
  gRecToType hrec = gRecToType hrec :*: gRecToType hrec

instance (RElem g fn ((fn' ::: t) ': xs) (fn == fn') f) => GRecToType g (S1 ('MetaSel ('Just fn) pack strict inf) (K1 k f)) ((fn' ::: t) ': xs) where
  gRecToType hrec = M1 $ K1 $ extract $ rGet (Proxy :: Proxy (fn == fn')) (Proxy :: Proxy fn) hrec

--instance GRecToType (K1 k f) xs where

class RElem f (fn :: Symbol) xs (mat :: Bool) r | fn xs -> r where
  rGet :: Proxy mat -> Proxy fn -> HList f xs -> f r

instance ( isMat ~ (fn == fn2)
         , RElem f fn ((fn2 ::: t2) ': xs) isMat t
         ) => RElem f fn ((fn1 ::: t1) ': (fn2 ::: t2) ': xs) 'False t where
  rGet _ fn (_ :& xs) = rGet (Proxy :: Proxy isMat) fn xs

instance TypeError ('Text "Unable to find field " ':<>: 'ShowType fn) => RElem f fn ((fn1 ::: t1) ': '[]) 'False () where
  rGet _ _ _ = error "Unreachable code"

instance Functor f => RElem f fn ((fn' ::: t) ': xs) 'True t where
  rGet _ _ (v :& _) = valOf <$> v

instance Extract Identity where
  extract (Identity i) = i

class Extract w where
  extract :: w a -> a

{- Invariants
   **********
User Def Type
============
A type can be used a both table and db-type
 Table
 -----
 Table can't be sum types
 Table should have named fields
 Type
 ---
 Type can be EnumLike Sum
 Un-named fields is not supported in initial version
  Can be sum type
 Type can't be U1 & V1

-}

getColumns :: IO ()
getColumns = do
  let _sql = T.concat ["SELECT "
                     ,"column_name "
                     ,",is_nullable "
                     ,",udt_name "
                     ,",column_default "
                     ,",numeric_precision "
                     ,",numeric_scale "
                     ,"FROM information_schema.columns "
                     ,"WHERE table_catalog=current_database() "
                     ,"AND table_schema=current_schema() "
                     ,"AND table_name=? "
                     ,"AND column_name <> ?"]
  let _sqlc = T.concat ["SELECT "
                      ,"c.constraint_name, "
                      ,"c.column_name "
                      ,"FROM information_schema.key_column_usage c, "
                      ,"information_schema.table_constraints k "
                      ,"WHERE c.table_catalog=current_database() "
                      ,"AND c.table_catalog=k.table_catalog "
                      ,"AND c.table_schema=current_schema() "
                      ,"AND c.table_schema=k.table_schema "
                      ,"AND c.table_name=? "
                      ,"AND c.table_name=k.table_name "
                      ,"AND c.column_name <> ? "
                      ,"AND c.constraint_name=k.constraint_name "
                      ,"AND NOT k.constraint_type IN ('PRIMARY KEY', 'FOREIGN KEY') "
                      ,"ORDER BY c.constraint_name, c.column_name"]
  return ()

type ConnectionString = ByteString
