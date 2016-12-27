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

module DBRecord.Internal.Schema where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits
import GHC.Generics
import GHC.Exts
import Data.Kind
import Data.Typeable
import Data.Functor.Const
import DBRecord.Internal.Types
import DBRecord.Internal.Common

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

  type DB db :: DbK
  type DB db = TypeError ('Text "DB type is not configured in the Database instance for type " ':<>: 'ShowType db ':$$:
                          'Text "Hint: add following to the Database instance for type "       ':<>: 'ShowType db ':$$:
                          'Text "type DB " ':<>: 'ShowType db ':<>: 'Text " = " ':<>: 'ShowType 'Postgres
                         )

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

class ( Generic ty
      ) => UDType (db :: *) (ty :: *) where
  type TypeMappings ty :: UDTypeMappings
  type TypeMappings ty = 'Flat '[]

data UDTypeMappings = Composite [(Symbol, Symbol)]
                    | Flat [(Symbol, Symbol)]

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
                             , [T.pack $ fromSing coln]
                             )

instance SingE (ch :: CheckCT) where
  type Demote ch = CheckExpr
  fromSing (SCheck cols _cname) = T.pack $ concat $ fromSing cols

instance SingE (defs :: DefSyms) where
  type Demote defs = [DefExpr]
  fromSing (SDef cols) = fmap T.pack $ fromSing cols

newtype I a = I a
  deriving (Show, Eq)

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
  UnifyCheck tab cn flds ('Just args) val = UnifyOrErr (SeqEither (MkCheckFn tab args val flds)) val

type family UnifyOrErr (res :: Either ErrorMessage [Type]) (v :: Type) :: Constraint where
  UnifyOrErr ('Right lhs) rhs = (MkFun lhs) ~ rhs
  UnifyOrErr ('Left err) _    = TypeError err
  
type family MkCheckFn (tab :: *) (args :: [Symbol]) (val :: *) (flds :: [*]) :: [Either ErrorMessage *] where
  MkCheckFn tab (fn ': fs) chkFun flds = Note (ColNotFoundMsg fn tab) (FindField flds fn) ': MkCheckFn tab fs chkFun flds
  MkCheckFn tab '[] r flds = '[ 'Right Bool]

type ColNotFoundMsg (col :: Symbol) (tab :: Type) = ('Text "column " ':<>: ('ShowType col) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab))  

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

type CheckExpr = Text
type DefExpr = Text

recordToList :: HList (Const a) rs -> [a]
recordToList Nil = []
recordToList (x :& xs) = getConst x : recordToList xs

class SingCols (db :: *) (cols :: [*]) (colMap :: [(Symbol, Symbol)]) where
  singCols :: Proxy db -> Proxy cols -> Proxy colMap -> HList (Const Column) cols

newtype DConAttr = DConAttr (ColName, [Column])

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
