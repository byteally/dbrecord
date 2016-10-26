{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE OverloadedStrings       #-}

module Database.Schema where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits
import GHC.Generics
import GHC.Exts
import Data.Kind
import Data.Typeable
import Data.Aeson
import Data.UUID.Types
import Data.Functor.Const
import Data.Time.LocalTime
import Data.ByteString (ByteString)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.CaseInsensitive  (CI)
import Data.Int
import Data.Vector (Vector)

{-
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
import           Data.CaseInsensitive  (CI)
import           Data.Time.LocalTime (LocalTime)
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (UTCTime)
-}

newtype (f :: Symbol) ::: t = Field t
  deriving (Show, Eq, Generic)

valOf :: (s ::: t) -> t
valOf (Field v) = v

data Col (a :: Symbol) = Col
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
      , SingCols db (OriginalTableFields tab) (ColumnNames tab)
      , KnownSymbol (TableName tab)
      , Generic tab
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

  type ColumnNames tab :: [(Symbol, Symbol)]
  type ColumnNames tab = '[]

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
                 ( SingCols db (OriginalTableFields tab) (ColumnNames tab)
                 ) => Const [Column] (db, tab)
getTableFields = Const $ recordToList $ singCols (Proxy @db) (Proxy @(OriginalTableFields tab)) (Proxy @(ColumnNames tab))

getTableHFields ::  forall db tab.
                   ( SingCols db (OriginalTableFields tab) (ColumnNames tab)
                   ) => Proxy db -> Proxy tab -> HList (Const Column) (OriginalTableFields tab)
getTableHFields _ _ = singCols (Proxy @db) (Proxy @(OriginalTableFields tab)) (Proxy @(ColumnNames tab))
                 

class SingCols (db :: *) (cols :: [*]) (colMap :: [(Symbol, Symbol)]) where
  singCols :: Proxy db -> Proxy cols -> Proxy colMap -> HList (Const Column) cols

instance ( SingCols db cols colMap
         , KnownSymbol cn
         , InvalidPGType db ct
         , ShowPGType (GetPGTypeRep ct)
         , aliasedCol ~ AliasedCol cn colMap
         , KnownSymbol aliasedCol
         ) => SingCols db ((cn ::: ct) ': cols) colMap where
  singCols _ _ _ = let pgTy = showPGType (Proxy :: Proxy (GetPGTypeRep ct))
                       colN = T.pack $ symbolVal (Proxy @aliasedCol)
                   in (Const $ Column colN pgTy) :& singCols (Proxy @db) (Proxy :: Proxy cols) (Proxy @colMap)

instance SingCols db '[] colMap where
  singCols _ _ _ = Nil

class SingAttrs (db :: *) (attrs :: [(Symbol, [*])]) where
  singAttrs :: Proxy db -> Proxy attrs -> HList (Const DConAttr) attrs

instance ( SingAttrs db cons
         , SingCols db flds '[] -- TODO: composite type alias
         , KnownSymbol c
         ) => SingAttrs db ('(c, flds) ': cons) where
  singAttrs pxyDB _ =
    let colHLists = singCols (Proxy @db) (Proxy @flds) (Proxy @('[]))
        cn = T.pack $ symbolVal (Proxy @c)
    in Const (DConAttr (cn, recordToList colHLists)) :& singAttrs pxyDB (Proxy @cons)

instance SingAttrs db '[] where
  singAttrs _ _ = Nil

newtype DConAttr = DConAttr (ColName, [Column])

type ColName = Text
type ColType  = Text
data Column = Column !ColName !ColType
  deriving (Show)


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
  ValidateTableProps tab = ( MissingField tab (ElemFields1 (OriginalTableFields tab) (PrimaryKey tab))
                           , MissingField tab (ElemFields1 (OriginalTableFields tab) (HasDefault tab))
                           , MissingField tab (ElemFields2 (OriginalTableFields tab) (Unique tab))
                           , ValidateTabFk tab (ForeignKey tab)
                           , ValidateTabCk tab (Check tab)
                           , ValidateTabIx tab
                           , ValidateColumnAlias tab (OriginalTableFields tab) (ColumnNames tab)
                           )

type family ValidateColumnAlias (tab :: *) (flds :: [*]) (colMap :: [(Symbol, Symbol)]) :: Constraint where
  ValidateColumnAlias tab flds ('(fn, _) ': colMaps) = (ValidateColumnAlias' tab flds fn, ValidateColumnAlias tab flds colMaps)
  ValidateColumnAlias _ flds '[] = ()

type family ValidateColumnAlias' (tab :: *) (flds :: [*]) (aliased :: Symbol) :: Constraint where
  ValidateColumnAlias' _ (fn ::: _ ': flds) fn   = ()
  ValidateColumnAlias' tab (fn ::: _ ': flds) cn = ValidateColumnAlias' tab flds cn
  ValidateColumnAlias' tab '[] cn                = TypeError ('Text "column " ':<>: ('ShowType cn) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab))
  
type family ValidateTabPk (tab :: *) (pks :: [Symbol]) :: Constraint where
  ValidateTabPk tab (p ': ps) = If (ElemField (OriginalTableFields tab) p) (ValidateTabPk tab ps) (TypeError ('Text "column " ':<>: ('ShowType p) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab)))
  ValidateTabPk tab '[]       = ()

type family ValidateTabFk tab (fks :: [ForeignRef Type]) :: Constraint where
  ValidateTabFk tab ('Ref fn reft ': fks) = (MatchFkRefFld tab reft fn (FindField (OriginalTableFields tab) fn) (FindField (OriginalTableFields reft) (HeadPk reft (PrimaryKey reft))),  ValidateTabFk tab fks)
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
  ValidateTabCk tab ('CheckOn fs cn ': chks) = ValidateTabCk' (ElemFields1 (OriginalTableFields tab) fs) tab cn chks
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
        , UnifyCheck tab cn (OriginalTableFields tab) args val
        ) => val -> Chk tab ('CheckOn (PartialJust args) cn)
check = Chk

dbChecks :: forall tab (db :: *) chks.HList (Chk tab) chks -> DBChecks db tab
dbChecks = DBChecks

type family ValidateDBFld tab (fn :: Symbol) t :: Constraint where
  ValidateDBFld tab fn t = UnifyField (OriginalTableFields tab) (fn ::: t) ('Text "column " ':<>: ('ShowType fn) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab))

type family GetTypeName (t :: *) :: Symbol where
  GetTypeName t              = GenTyCon (Rep t)

type family DefaultTableName (t :: *) :: Symbol where
  DefaultTableName t                = GenTyCon (Rep t)

type family GetSchemaName (t :: *) :: Symbol where
  -- TODO: () instance is a hack to get constraint
  -- KnownSymbol (GetSchemaName t) instead of
  -- KnownSymbol (Schema t)
  GetSchemaName ()               = Schema ()
  GetSchemaName db               = Schema db

type OriginalTableFields t = GenTabFields (Rep t)

type family TableFields (t :: *) :: [*] where
  TableFields t = TableFields' (GenTabFields (Rep t)) (ColumnNames t)

type family TableFields' (flds :: [*]) (colMap :: [(Symbol, Symbol)]) :: [*] where
  TableFields' ((fn ::: ft) ': flds) colMap = (AliasedCol fn colMap ::: ft) ': (TableFields' flds colMap)
  TableFields' '[] colMap = '[]

type family AliasedCol (fn :: Symbol) (colMap :: [(Symbol, Symbol)]) :: Symbol where
  AliasedCol fn ('(fn, alias) ': colMap) = alias
  AliasedCol fn (_ ': colMap)            = AliasedCol fn colMap
  AliasedCol fn '[]                      = fn
  
type family GetTypeFields (t :: *) :: [(Symbol, [*])] where
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

-- type instance DBTy.Column PGSqlName t = PGTypeRep (GetPGTypeRep t)

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

type CheckExpr = Text
type DefExpr = Text

recordToList :: HList (Const a) rs -> [a]
recordToList Nil = []
recordToList (x :& xs) = getConst x : recordToList xs

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
