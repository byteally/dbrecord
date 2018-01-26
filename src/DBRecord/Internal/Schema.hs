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
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE OverloadedStrings       #-}


module DBRecord.Internal.Schema where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits
import GHC.Generics (Generic (..), D1 (..), Meta (..))
import GHC.Exts
import GHC.OverloadedLabels
import Data.Kind
import Data.Typeable
import Data.Functor.Const
import DBRecord.Internal.Types
import DBRecord.Internal.Common
-- import DBRecord.Internal.Expr
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.DBTypes
import qualified Data.List as L
import DBRecord.Internal.Lens (unsafeFind, (^.), Lens', coerceL)
import Data.Monoid ((<>))

data Col (a :: Symbol) = Col
data DefSyms = DefSyms [Symbol]

type ColName  = Text
type ColType  = Text
data Column   = Column !ColName !ColType
  deriving (Show)
           
class ( -- TypeCxts db (Types db)
      ) => Database (db :: *) where
  type Schema db :: Symbol
  type Schema db = "public"
  
  type Tables db :: [Type]
  
  type Types db :: [Type]
  type Types db = '[]

  type TabIgnore db :: [Type]
  type TabIgnore db = '[]
  
  type Baseline db :: Nat
  type Baseline db = 0
  
  type Version db :: Nat
  type Version db = 0  

  type DB db :: DbK
  type DB db = TypeError ('Text "DB type is not configured in the Database instance for type " ':<>: 'ShowType db ':$$:
                          'Text "Hint: add following to the Database instance for type "       ':<>: 'ShowType db ':$$:
                          'Text "type DB " ':<>: 'ShowType db ':<>: 'Text " = " ':<>: 'ShowType 'Postgres
                         )

class ( Database db
      , AssertCxt (Elem (Tables db) tab) ('Text "Database " ':<>: 'ShowType db ':<>: 'Text " does not contain the table: " ':<>: 'ShowType tab)
      , ValidateTableProps db tab    
      , Generic tab
      ) => Table (db :: *) (tab :: *) where
  type PrimaryKey db tab :: [Symbol]
  type PrimaryKey db tab = '[]

  type PrimaryKeyName db tab :: Maybe Symbol
  type PrimaryKeyName db tab = 'Nothing

  type ForeignKey db tab :: [ForeignRef Type]
  type ForeignKey db tab = '[]

  type ForeignKeyNames db tab :: [(Symbol, Symbol)]
  type ForeignKeyNames db tab = '[]

  type Unique db tab     :: [UniqueCT]
  type Unique db tab = '[]

  type UniqueNames db tab :: [(Symbol, Symbol)]
  type UniqueNames db tab = '[]

  type HasDefault db tab :: [Symbol]
  type HasDefault db tab = '[]

  type Check db tab :: [CheckCT]
  type Check db tab = '[]

  type CheckNames db tab :: [(Symbol, Symbol)]
  type CheckNames db tab = '[]
  
  type ColIgnore db tab :: IgnoredCol
  type ColIgnore db tab = 'IgnoreNone

  type TableName db tab :: Symbol
  type TableName db tab = DefaultTableName tab

  type TableSequence db tab :: [Sequence]
  type TableSequence db tab = '[]

  type SequenceNames db tab :: [(Symbol, Symbol)]
  type SequenceNames db tab = '[]
  
  type ColumnNames db tab :: [(Symbol, Symbol)]
  type ColumnNames db tab = '[]

  defaults :: DBDefaults db tab
  defaults = DBDefaults Nil

  checks :: DBChecks db tab
  checks = DBChecks Nil

data Sequence = PGSerial Symbol   -- ^ Column
                         Symbol   -- ^ Sequence Name
              | PGOwned  Symbol   -- ^ Column
                         Symbol   -- ^ Sequence Name

type family Serial (cname :: Symbol) (seqname :: Symbol) where
  Serial cname seqname = 'PGSerial cname seqname

type family Owned (cname :: Symbol) (seqname :: Symbol) where
  Owned cname seqname = 'PGOwned cname seqname

class ( Generic ty
      ) => UDType (db :: *) (ty :: *) where
  type TypeMappings db ty :: UDTypeMappings
  -- type TypeMappings db ty = 'Flat '[]

-- TODO: Support other type mappings as well
data UDTypeMappings = EnumType Symbol [Symbol]
                    -- | Composite Symbol [(Symbol, DBTypeK)]
                    -- | Flat [(Symbol, DBTypeK)]
                    -- | EnumText [Symbol]
                    -- | Sum [(Symbol, [(Symbol, DBTypeK)])]

type family GetTypeMappings (db :: *) where
  GetTypeMappings db = GetTypeMappings' db (Types db)

type family GetTypeMappings' (db :: *) (ts :: [*]) where
  GetTypeMappings' db (t ': ts) = '(GetPMT (Rep t), TypeMappings db t) ': GetTypeMappings' db ts
  GetTypeMappings' db '[]       = '[]

data TagHK b a = Tag b a

type family TagEach (db :: tk) (ent :: [k]) :: [TagHK tk k] where
  TagEach db (ent ': ents) = 'Tag db ent ': TagEach db ents
  TagEach db '[]           = '[]

type family UnTag (t :: TagHK k1 k) :: k where
  UnTag ('Tag _ a) = a

data family Sing (a :: k)

data instance Sing (s :: Symbol) where
  SSym :: KnownSymbol s => Sing s

data instance Sing (v :: Nat) where
  SNat :: KnownNat v => Sing v

data instance Sing (t :: *) where
  STypeRep :: Typeable t => Sing (t :: *)

data instance Sing (b :: Bool) where
  STrue :: Sing 'True
  SFalse :: Sing 'False

data instance Sing (t :: Maybe k) where
  SJust     :: Sing (a :: k) -> Sing ('Just a)
  SNothing  :: Sing 'Nothing

data instance Sing (t :: (k1, k2)) where
  STuple :: Sing (a :: k1) -> Sing (b :: k2) -> Sing '(a, b)

data instance Sing (xs :: [k]) where
  SNil  :: Sing '[]
  SCons :: Sing x -> Sing xs -> Sing (x ': xs)

data instance Sing (t :: DBTypeK) where
  SDBInt4        :: Sing 'DBInt4
  SDBInt8        :: Sing 'DBInt8
  SDBInt2        :: Sing 'DBInt2
  SDBFloat4      :: Sing 'DBFloat4
  SDBFloat8      :: Sing 'DBFloat8
  SDBBool        :: Sing 'DBBool
  SDBNumeric     :: Sing n1 -> Sing n2 -> Sing ('DBNumeric n1 n2)
  SDBChar        :: Sing n -> Sing ('DBChar n)
  SDBText        :: Sing 'DBText
  SDBCiText      :: Sing 'DBCiText
  SDBDate        :: Sing 'DBDate
  SDBTime        :: Sing 'DBTime
  SDBTimestamp   :: Sing 'DBTimestamp
  SDBTimestamptz :: Sing 'DBTimestamptz
  SDBUuid        :: Sing 'DBUuid
  SDBByteArr     :: Sing 'DBByteArr
  SDBJson        :: Sing 'DBJson
  SDBJsonB       :: Sing 'DBJsonB
  SDBInterval    :: Sing 'DBInterval
  SDBArray       :: Sing a -> Sing ('DBArray a)
  SDBNullable    :: Sing a -> Sing ('DBNullable a)
  SDBCustomType  :: Sing t -> Sing dbt -> Sing b -> Sing ('DBCustomType t dbt b)
  SDBTypeName    :: Sing sym -> Sing ('DBTypeName sym)

data instance Sing (db :: DbK) where
  SPostgres  :: Sing 'Postgres
  SMySQL     :: Sing 'MySQL
  SSQLite    :: Sing 'SQLite
  SCassandra :: Sing 'Cassandra
  SPresto    :: Sing 'Presto
  
data instance Sing (a :: TagHK tk k) where
  STag :: Sing tag -> Sing a -> Sing ('Tag tag a)

data instance Sing (uq :: UniqueCT) where
  SUniqueOn :: Sing uniqFlds -> Sing uniqOn -> Sing ('UniqueOn uniqFlds uniqOn)

data instance Sing (fk :: ForeignRef a) where
  SRefBy :: Sing cols -> Sing reft -> Sing refCols -> Sing fkname -> Sing ('RefBy cols reft refCols fkname)
  SRef   :: Sing col -> Sing reft -> Sing fkname -> Sing ('Ref col reft fkname)

data instance Sing (ch :: CheckCT) where
  SCheck :: Sing cols -> Sing cname -> Sing ('CheckOn cols cname)

data instance Sing (uq :: Sequence) where
  SPGSerial :: Sing col -> Sing seqn -> Sing ('PGSerial col seqn)
  SPGOwned  :: Sing col -> Sing seqn -> Sing ('PGOwned col seqn)

data instance Sing (t :: TypeName Symbol) where
  STypeName :: Sing (pkgN :: Symbol) -> Sing (modN :: Symbol) -> Sing (typN :: Symbol) -> Sing ('TypeName pkgN modN typN)

data instance Sing (tm :: UDTypeMappings) where
  SEnumType :: Sing (tn :: Symbol) -> Sing (dcons :: [Symbol]) -> Sing ('EnumType tn dcons)

class SingI (a :: k) where
  sing :: Sing a

instance SingI 'True where
  sing = STrue

instance SingI 'False where
  sing = SFalse

instance SingI 'DBInt4 where
  sing = SDBInt4

instance SingI 'DBInt8 where
  sing = SDBInt8

instance SingI 'DBInt2 where
  sing = SDBInt2

instance SingI 'DBFloat4 where
  sing = SDBFloat4

instance SingI 'DBFloat8 where
  sing = SDBFloat8

instance SingI 'DBBool where
  sing = SDBBool

instance ( SingI n1
         , SingI n2
         ) => SingI ('DBNumeric n1 n2) where
  sing = SDBNumeric sing sing

instance (SingI n) => SingI ('DBChar n) where
  sing = SDBChar sing

instance SingI 'DBText where
  sing = SDBText

instance SingI 'DBCiText where
  sing = SDBCiText

instance SingI 'DBDate where
  sing = SDBDate

instance SingI 'DBTime where
  sing = SDBTime

instance SingI 'DBTimestamp where
  sing = SDBTimestamp

instance SingI 'DBTimestamptz where
  sing = SDBTimestamptz

instance SingI 'DBUuid where
  sing = SDBUuid

instance SingI 'DBByteArr where
  sing = SDBByteArr

instance SingI 'DBJson where
  sing = SDBJson

instance SingI 'DBJsonB where
  sing = SDBJsonB

instance SingI 'DBInterval where
  sing = SDBInterval

instance (SingI dbt) => SingI ('DBArray dbt) where
  sing = SDBArray sing

instance (SingI dbt) => SingI ('DBNullable dbt) where
  sing = SDBNullable sing

instance ( SingI t
         , SingI dbt
         , SingI b
         ) => SingI ('DBCustomType t dbt b) where
  sing = SDBCustomType sing sing sing

instance (SingI sym) => SingI ('DBTypeName sym) where
  sing = SDBTypeName sing
  
instance (SingI a, SingI b) => SingI ( '(a, b)) where
  sing = STuple sing sing

instance (SingI a) => SingI ('Just a) where
  sing = SJust sing

instance SingI 'Nothing where
  sing = SNothing

instance SingI '[] where
  sing = SNil

instance (SingI x, SingI xs) => SingI (x ': xs) where
  sing = SCons sing sing

instance (KnownSymbol sy) => SingI (sy :: Symbol) where
  sing = SSym

instance (KnownNat n) => SingI (n :: Nat) where
  sing = SNat

instance ( SingI a
         , SingI tag
         ) => SingI ('Tag tag a)  where
  sing = STag sing sing

instance (Typeable t) => SingI (t :: *) where
  sing = STypeRep

instance (SingI uniqFlds, SingI uniqOn) => SingI ('UniqueOn uniqFlds uniqOn) where
  sing = SUniqueOn sing sing 

instance (SingI cols, SingI reft, SingI refcols, SingI fkname) => SingI ('RefBy cols reft refcols fkname) where
  sing = SRefBy sing sing sing sing

instance (SingI col, SingI reft, SingI fkname) => SingI ('Ref col reft fkname) where
  sing = SRef sing sing sing

instance (SingI col, SingI seqn) => SingI ('PGSerial col seqn)  where
  sing = SPGSerial sing sing

instance (SingI col, SingI seqn) => SingI ('PGOwned col seqn)  where
  sing = SPGOwned sing sing

instance ( SingI pkgN
         , SingI modN
         , SingI typN
         ) => SingI ('TypeName (pkgN :: Symbol) (modN :: Symbol) (typN :: Symbol)) where
  sing = STypeName sing sing sing

instance ( SingI cols
         , KnownSymbol cname
         , All SingE cols
         ) => SingI ('CheckOn cols cname) where
  sing = SCheck sing sing

instance SingI 'Postgres where
  sing = SPostgres

instance SingI 'MySQL where
  sing = SMySQL

instance SingI 'SQLite where
  sing = SSQLite

instance SingI 'Cassandra where
  sing = SCassandra

instance SingI 'Presto where
  sing = SPresto

instance ( SingI tn
         , SingI dcons
         ) => SingI ('EnumType tn dcons) where
  sing = SEnumType sing sing
  
class SingE (a :: k) where
  type Demote a :: *
  fromSing :: Sing a -> Demote (Any :: k)

instance SingE (b :: Bool) where
  type Demote b = Bool
  fromSing STrue  = True
  fromSing SFalse = False

instance SingE (sy :: Symbol) where
  type Demote sy = T.Text
  fromSing SSym = T.pack (symbolVal (Proxy :: Proxy sy))

instance SingE (n :: Nat) where
  type Demote sy = Integer
  fromSing SNat = natVal (Proxy :: Proxy n)

type family Fst (tup :: (k1, k2)) :: k1 where
  Fst '(a, b) = a

type family Snd (tup :: (k1, k2)) :: k2 where
  Snd '(a, b) = b

type family MaybeCtx (ctx :: k -> Constraint) (m :: Maybe k) :: Constraint where
  MaybeCtx ctx ('Just m) = ctx m
  MaybeCtx _   'Nothing  = ()

type family UqCtx (ctx :: Symbol -> Constraint) (uq :: UniqueCT) :: Constraint where
  UqCtx ctx ('UniqueOn uniqFlds uniqOn) = (All ctx uniqFlds, ctx uniqOn)

type family FKCtxTy (ctx :: Symbol -> Constraint) (fk :: ForeignRef Type) :: Constraint where
  FKCtxTy ctx ('RefBy cols reft refcols name) = (All ctx cols, All ctx refcols, ctx name, SingE (GetPMT (Rep reft)), Generic reft
                                                , SingI (GetPMT (Rep reft))
                                              )
  FKCtxTy ctx ('Ref col reft name)            = ( ctx col, ctx name, SingE (GetPMT (Rep reft)), Generic reft
                                                , SingI (GetPMT (Rep reft))
                                              )

type family FKCtxTyN (ctx :: Symbol -> Constraint) (fk :: ForeignRef (TypeName Symbol)) :: Constraint where
  FKCtxTyN ctx ('RefBy cols reft refcols name) = ( All ctx cols, All ctx refcols, SingE reft, ctx name
                                                 )
  FKCtxTyN ctx ('Ref col reft name)            = ( ctx col, SingE reft, ctx name
                                                 )

instance ( SingE (Fst tup)
         , SingE (Snd tup)
         ) => SingE (tup :: (k1, k2)) where
  type Demote (tup :: (k1, k2)) = (Demote (Any :: k1), Demote (Any :: k2))
  fromSing (STuple x y) = (fromSing x, fromSing y)

instance (MaybeCtx SingE m) => SingE (m :: Maybe k) where
  type Demote (m :: Maybe k) = Maybe (Demote (Any :: k))
  fromSing SNothing   = Nothing
  fromSing (SJust x)  = Just (fromSing x)
  
instance All SingE xs => SingE (xs :: [k]) where
  type Demote (xs :: [k]) = [Demote (Any :: k)]
  fromSing SNil         = []
  fromSing (SCons x xs) = fromSing x : fromSing xs

type family DBTypeCtx (taggedDbt :: TagHK DbK DBTypeK) where
  DBTypeCtx ('Tag dbT dbTy) = (ShowDBType dbT dbTy)

instance (DBTypeCtx taggedDbt) => SingE (taggedDbt :: TagHK DbK DBTypeK) where
  type Demote taggedDbt     = Text
  fromSing (STag sdb stype) = showDBTypeSing sdb stype

type family UDTCtx (taggedDbt :: UDTypeMappings) where
  UDTCtx ('EnumType tn dcons) = (SingE tn, SingE dcons)

instance (UDTCtx udt) => SingE (udt :: UDTypeMappings) where
  type Demote udt = TypeNameMap
  fromSing (SEnumType stn sdcons) =
    EnumTypeNM (fromSing stn) (fromSing sdcons)

showDBTypeSing :: forall db dbTy.
                   (ShowDBType db dbTy
                   ) => Sing (db :: DbK) -> Sing (dbTy :: DBTypeK) -> Text
showDBTypeSing dbK dbT = showDBType (reproxy dbK) (reproxy dbT)

instance (UqCtx SingE uq) => SingE (uq :: UniqueCT) where
  type Demote (uq :: UniqueCT)         = (Demote (Any :: [Symbol]), Demote (Any :: Symbol))
  fromSing (SUniqueOn uniqFlds uniqOn) = (fromSing uniqFlds, fromSing uniqOn)

instance ( FKCtxTy SingE fk
         ) => SingE (fk :: ForeignRef Type) where
  type Demote (fk :: (ForeignRef Type)) = ForeignRefD
  fromSing (SRefBy cols reft refcols fkname) =
    RefByD (fromSing fkname) (fromSing cols)
           (fromSing (singTypeName reft)) (fromSing refcols)
  fromSing (SRef coln reft fkname) =
    RefD  (fromSing fkname) (fromSing coln) (fromSing (singTypeName reft))

instance ( FKCtxTyN SingE fk
         ) => SingE (fk :: ForeignRef (TypeName Symbol)) where
  type Demote (fk :: ForeignRef (TypeName Symbol)) = ForeignRefD
  fromSing (SRefBy cols reft refcols fkname) =
    RefByD (fromSing fkname) (fromSing cols)
           (fromSing reft) (fromSing refcols)
  fromSing (SRef coln reft fkname) =
    RefD  (fromSing fkname) (fromSing coln) (fromSing reft)

instance SingE (db :: DbK) where
  type Demote dbK = DbK
  fromSing SPostgres  = Postgres
  fromSing SMySQL     = MySQL
  fromSing SSQLite    = SQLite
  fromSing SCassandra = Cassandra
  fromSing SPresto    = Presto

type family GetPMT (rep :: * -> *) :: TypeName Symbol where
  GetPMT (D1 ('MetaData tyName modName pkgName _) _) =
    'TypeName pkgName modName tyName

singTypeName :: forall t.
                 ( Generic t
                 , SingI (GetPMT (Rep t))
                 ) => Sing t -> Sing (GetPMT (Rep t))
singTypeName _ = sing                 

fromSingDefTabName :: forall reft.
                      ( SingI (DefaultTableName reft)
                      , SingE (DefaultTableName reft)
                      ) => Sing reft -> Demote (DefaultTableName reft :: Symbol)
fromSingDefTabName _ = fromSing (sing :: Sing (DefaultTableName reft))
                      
tabName :: forall db t proxy.
  KnownSymbol (TableName db t) => proxy db -> proxy t -> String
tabName _ _ = symbolVal (Proxy :: Proxy (TableName db t))

instance SingE (seq :: Sequence) where
  type Demote seq = (Text, Text, SequenceType)
  fromSing (SPGSerial coln seqn) = (fromSing coln, fromSing seqn, SeqSerial)
  fromSing (SPGOwned coln seqn)  = (fromSing coln, fromSing seqn, SeqOwned)

instance SingE (t :: TypeName Symbol) where
  type Demote t = TypeName Text
  fromSing (STypeName p m f) = TypeName (fromSing p)
                                        (fromSing m)
                                        (fromSing f)

type family ValidateTableProps (db :: *) (tab :: *) :: Constraint where
  ValidateTableProps db tab =
    ( MissingField tab (ElemFields1 (OriginalTableFields tab) (PrimaryKey db tab))
    , MissingField tab (ElemFields1 (OriginalTableFields tab) (HasDefault db tab))
    , MissingField tab (ElemUniqs (OriginalTableFields tab) (Unique db tab))
    , ValidateTabFk db tab (ForeignKey db tab)
    , ValidateTabCk tab (Check db tab)
    , ValidateTabIx tab
    , ValidateNames tab ('Text "at the usage of ColumnNames") (OriginalTableFields tab) (ColumnNames db tab)
    , ValidateNames tab ('Text "at the usage of ForeignKeyNames") (OriginalFKNames db tab) (ForeignKeyNames db tab)
    , ValidateNames tab ('Text "at the usage of UniqueNames") (OriginalUQNames db tab) (UniqueNames db tab)
    , ValidateNames tab ('Text "at the usage of CheckNames") (OriginalCheckNames db tab) (CheckNames db tab)
    , ValidateNames tab ('Text "at the usage of SequenceNames") (OriginalSequenceNames db tab) (SequenceNames db tab)            
    )

type family OriginalFKNames db tab :: [Symbol] where
  OriginalFKNames db tab = GetFKNames (ForeignKey db tab)

type family GetFKNames (fks :: [ForeignRef a]) :: [Symbol] where
  GetFKNames ('RefBy _ _ _ n ': refs) = n ': GetFKNames refs
  GetFKNames ('Ref _ _ n ': refs)     = n ': GetFKNames refs
  GetFKNames '[]                      = '[]

type family OriginalUQNames db tab :: [Symbol] where
  OriginalUQNames db tab = GetUQNames (Unique db tab)

type family GetUQNames (uqs :: [UniqueCT]) :: [Symbol] where
  GetUQNames ('UniqueOn _ n ': uqs) = n ': GetUQNames uqs
  GetUQNames '[]                    = '[]

type family OriginalCheckNames db tab :: [Symbol] where
  OriginalCheckNames db tab = GetCheckNames (Check db tab)

type family GetCheckNames (chks :: [CheckCT]) :: [Symbol] where
  GetCheckNames ('CheckOn _ n ': chks) = n ': GetCheckNames chks
  GetCheckNames '[]                   = '[]

type family OriginalSequenceNames db tab :: [Symbol] where
  OriginalSequenceNames db tab = GetSequenceNames (TableSequence db tab)

type family GetSequenceNames (seqs :: [Sequence]) :: [Symbol] where
  GetSequenceNames ('PGSerial _ n ': seqs) = n ': GetSequenceNames seqs
  GetSequenceNames ('PGOwned _ n ': seqs)  = n ': GetSequenceNames seqs
  GetSequenceNames '[]                     = '[]

type family ValidateNames (tab :: *) (msg :: ErrorMessage) (flds :: [k]) (map :: [(Symbol, Symbol)]) :: Constraint where
  ValidateNames tab msg flds ('(fn, _) ': maps) = (ValidateAlias' tab msg flds fn, ValidateNames tab msg flds maps)
  ValidateNames _ _ flds '[] = ()

type family ValidateAlias' (tab :: *) (msg :: ErrorMessage) (flds :: [k]) (aliased :: Symbol) :: Constraint where
  ValidateAlias' _ _ (fn ::: _ ': flds) fn     = ()
  ValidateAlias' _ _ (fn ': flds) fn           = ()  
  ValidateAlias' tab msg (fn ': flds) cn       = ValidateAlias' tab msg flds cn  
  ValidateAlias' tab msg '[] cn                = TypeError ('Text "column " ':<>: ('ShowType cn) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab) ':$$: msg)
  
type family ValidateTabPk (tab :: *) (pks :: [Symbol]) :: Constraint where
  ValidateTabPk tab (p ': ps) = If (ElemField (OriginalTableFields tab) p) (ValidateTabPk tab ps) (TypeError ('Text "column " ':<>: ('ShowType p) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab)))
  ValidateTabPk tab '[]       = ()

type family ValidateTabFk db tab (fks :: [ForeignRef Type]) :: Constraint where
  ValidateTabFk db tab ('Ref fn reft _ ': fks)
    = ( MatchFkFields db tab reft (FindFields (OriginalTableFields tab) '[fn]) (FindFields (OriginalTableFields reft) '[fn])
--        MatchFkRefFld tab reft fn (FindField (OriginalTableFields tab) fn) (FindField (OriginalTableFields reft) (HeadPk reft (PrimaryKey db reft)))
      , ValidateRefPk reft '[fn] (PrimaryKey db reft)
      , ValidateTabFk db tab fks
      )
  ValidateTabFk db tab ('RefBy fkeys reft rkeys _ ': fks)
    = ( MatchFkFields db tab reft (FindFields (OriginalTableFields tab) fkeys) (FindFields (OriginalTableFields reft) rkeys)
      , ValidateRefPk reft rkeys (PrimaryKey db reft)
      , ValidateTabFk db tab fks
      )
  ValidateTabFk db tab '[]         = ()

type family ValidateRefPk (reft :: *) (rkeys :: [Symbol]) (pkeys :: [Symbol]) :: Constraint where
  ValidateRefPk reft keys keys = ()
  ValidateRefPk reft rpkeys pkeys = TypeError ('Text "In foreign key declaration:" ':$$: 'ShowType rpkeys ':<>: 'Text " is not a primary key of table " ':<>: 'ShowType reft)

type family MatchFkFields db tab reft (fkeys :: [Either Symbol *]) (rkeys :: [Either Symbol *]) :: Constraint where
  MatchFkFields db tab reft ('Right (fn1 ::: t) ': fkeys) ('Right (fn2 ::: t) ': rkeys)
    = MatchFkFields db tab reft fkeys rkeys
  MatchFkFields db tab reft ('Right (fn1 ::: t1) ': fkeys) ('Right (fn2 ::: t2) ': rkeys)
    = ( TypeError ('Text "Type mismatch between foreign key and primary key"
                  ':$$: ('ShowType fn1) ':<>: 'Text ": " ':<>: ('ShowType t1)
                  ':$$: ('ShowType fn2) ':<>: 'Text ": " ':<>: ('ShowType t2)
                  )
      , MatchFkFields db tab reft fkeys rkeys
      )
  MatchFkFields db tab reft ('Left fn1 ': fkeys) ('Right _ ': rkeys)
    = (TypeError ('Text "column " ':<>: ('ShowType fn1) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab)), MatchFkFields db tab reft fkeys rkeys)
  MatchFkFields db tab reft ('Right _ ': fkeys) ('Left fn2 ': rkeys)
    = (TypeError ('Text "column " ':<>: ('ShowType fn2) ':<>: 'Text " does not exist in table " ':<>: ('ShowType reft)), MatchFkFields db tab reft fkeys rkeys)
  MatchFkFields db tab reft ('Left fn1 ': fkeys) ('Left fn2 ': rkeys)
    = ( TypeError ('Text "column " ':<>: ('ShowType fn1) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab))
      , TypeError ('Text "column " ':<>: ('ShowType fn2) ':<>: 'Text " does not exist in table " ':<>: ('ShowType reft))
      , MatchFkFields db tab reft fkeys rkeys
      )
  MatchFkFields db tab reft '[] (_ ': _)
    = TypeError ('Text "Number of foreign key column is less than that of its referenced primary keys")
  MatchFkFields db tab reft (_ ': _) '[]
    = TypeError ('Text "Number of foreign key column is greater than that of its referenced primary keys")
  MatchFkFields _ _ _ '[] '[] = ()
  
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

type family ElemUniqs (flds :: [*]) (uniqs :: [UniqueCT]) :: Maybe Symbol where
  ElemUniqs flds ('UniqueOn fs _ : uqs) = ElemUniqs' (ElemFields1 flds fs) flds uqs
  ElemUniqs flds '[]                  = 'Nothing

type family ElemUniqs' (may :: Maybe Symbol) (flds :: [*]) (fss :: [UniqueCT])  :: Maybe Symbol where
  ElemUniqs' ('Just fn) flds uqs = 'Just fn
  ElemUniqs' 'Nothing flds uqs    = ElemUniqs flds uqs

type family MissingField (tab :: *) (fn :: Maybe Symbol) :: Constraint where
  MissingField tab ('Just fn) = TypeError ('Text "column " ':<>: ('ShowType fn) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab))
  MissingField tab 'Nothing   = ()

type family MissingDefault (tab :: *) (fn :: Symbol) (isElem :: Bool) :: Constraint where
  MissingDefault _ _ 'True     = ()
  MissingDefault tab fn 'False = TypeError ('Text "Default for column " ':<>: ('ShowType fn) ':<>: 'Text " is not set in table " ':<>: ('ShowType tab))

type family MissingCheck (tab :: *) (fn :: Symbol) (isElem :: Bool) :: Constraint where
  MissingCheck _ _ 'True     = ()
  MissingCheck tab fn 'False = TypeError ('Text "Check expression for column " ':<>: ('ShowType fn) ':<>: 'Text " is not set in table " ':<>: ('ShowType tab))

type family GetAllUniqs (uqs :: [UniqueCT]) (colMap :: [(Symbol, Symbol)]) :: [[Symbol]] where
   GetAllUniqs ('UniqueOn fs _ : uqs) colMap = (MapAliasedCol fs colMap) ': GetAllUniqs uqs colMap
   GetAllUniqs '[]                        _  = '[]

type family GetNonNulls (db :: DbK) (ts :: [*]) (colMap :: [(Symbol, Symbol)]) :: [Symbol] where
   GetNonNulls db ((fld ::: t) ': ts) colMap = GetNonNulls' db (AliasedCol fld colMap) (GetDBTypeRep db t) ts colMap
   GetNonNulls _ '[] _                       = '[]

type family GetNonNulls' (db :: DbK) (fld :: Symbol) (rep :: DBTypeK) (ts :: [*]) (flds :: [(Symbol, Symbol)]) :: [Symbol] where
  GetNonNulls' db fld ('DBNullable _) ts colMap = GetNonNulls db ts colMap
  GetNonNulls' db fld  _  ts colMap             = fld ': GetNonNulls db ts colMap

type family GetUniqBy (un :: Symbol) (uqs :: [UniqueCT]) :: Maybe [Symbol] where
  GetUniqBy un ('UniqueOn fs un : uqs)   = 'Just fs
  GetUniqBy un1 ('UniqueOn fs un2 : uqs) = GetUniqBy un1 uqs
  GetUniqBy _ '[]                        = 'Nothing

data ForeignRef a
  = RefBy [Symbol] a [Symbol] Symbol
  | Ref Symbol a Symbol

data UniqueCT = UniqueOn [Symbol] Symbol
data Uq (un :: Symbol) = Uq

instance un ~ uqn => IsLabel un (Uq uqn) where
#if __GLASGOW_HASKELL__ > 800
  fromLabel = Uq
#else
  fromLabel _ = Uq
#endif

data CheckCT = CheckOn [Symbol] Symbol

data Ix = Ix Symbol

data IgnoredCol
  = IgnoreRest
  | IgnoreOnly [Symbol]
  | IgnoreExcept [Symbol]
  | IgnoreNone

data Def (db :: *) (tab :: k) (fn :: Symbol) = forall v.Def (Expr '[] v)

def :: forall (fn :: Symbol) (tab :: *) (db :: *) v.(ValidateDBFld tab fn v) => Expr '[] v -> Def db tab fn
def = Def

instance ( ValidateDBFld tab un a
         , un ~ fn
         , v ~ Expr '[] a
         ) => IsLabel un (v -> Def db (tab :: *) fn) where
#if __GLASGOW_HASKELL__ > 800
  fromLabel v = def @un @tab v
#else
  fromLabel _ v = def @un @tab v
#endif

data DBDefaults (db :: *) tab = forall xs. (All KnownSymbol xs) => DBDefaults (HList (Def db tab) xs)

end :: HList f '[]
end = Nil

dbDefaults :: forall tab db xs.
              ( All KnownSymbol xs
              , ValidateDefExprs db tab (HasDefault db tab) xs
              ) => HList (Def db tab) xs -> DBDefaults db tab
dbDefaults = DBDefaults

type family ValidateDefExprs (db :: *) (tab :: *) (defs :: [Symbol]) (setDefs :: [Symbol]) :: Constraint where
  ValidateDefExprs db tab (def ': defs) setDefs =
    ( MissingDefault tab def (Elem setDefs def)
    , ValidateDefExprs db tab defs setDefs
    )
  ValidateDefExprs db tab '[] _ = ()

data Chk (db :: *) (tab :: k) (chk :: CheckCT) = forall val.(ApCheckOnExpr chk val) => Chk val

data DBChecks (db :: *) tab = forall chks. (All CheckExpr chks) => DBChecks (HList (Chk db tab) chks)

type family ApCheckOnExpr chk val where
  ApCheckOnExpr ('CheckOn cols name) v = ApCheckExpr cols name v

type family LookupCheck (chks :: [CheckCT]) (cn :: Symbol) :: Maybe [Symbol] where
  LookupCheck ('CheckOn args cn ': chks) cn  = 'Just args
  LookupCheck ('CheckOn args cn1 ': chks) cn = LookupCheck chks cn
  LookupCheck '[] cn                         = 'Nothing

type family UnifyCheck (tab :: *) (cn :: Symbol) (flds :: [*]) (args :: Maybe [Symbol]) (val :: *) :: Constraint where
  UnifyCheck tab cn flds 'Nothing val = TypeError ('Text "check constraint " ':<>: 'ShowType cn ':<>: 'Text " does not exist on table " ':<>: 'ShowType tab)
  UnifyCheck tab cn flds ('Just args) val = UnifyOrErr (SeqEither (MkCheckFn tab args val flds)) val
  
type family MkCheckFn (tab :: *) (args :: [Symbol]) (val :: *) (flds :: [*]) :: [Either ErrorMessage *] where
  MkCheckFn tab (fn ': fs) chkFun flds = Note (ColNotFoundMsg fn tab) (FMapMaybe (Expr flds) (FindField flds fn)) ': MkCheckFn tab fs chkFun flds
  MkCheckFn tab '[] r flds = '[ 'Right (Expr flds Bool)]

instance ( un ~ cn
         , args ~ LookupCheck (Check db tab) cn
         , UnifyCheck tab cn (OriginalTableFields tab) args val
         , ApCheckExpr (PartialJust args) cn val
         , res ~ ('CheckOn (PartialJust args) cn)
         ) => IsLabel un (val -> Chk db tab res) where
#if __GLASGOW_HASKELL__ > 800
  fromLabel = check
#else
  fromLabel _ = check
#endif

check :: forall (cn :: Symbol) (db :: *) (tab :: *) val args.
        ( args ~ LookupCheck (Check db tab) cn
        , UnifyCheck tab cn (OriginalTableFields tab) args val
        , ApCheckExpr (PartialJust args) cn val
        ) => val -> Chk db tab ('CheckOn (PartialJust args) cn)
check = Chk

dbChecks :: forall tab (db :: *) chks.
            ( All CheckExpr chks
            , ValidateCheckExpr db tab (Check db tab) chks
            ) => HList (Chk db tab) chks -> DBChecks db tab
dbChecks = DBChecks

type family ValidateCheckExpr db tab (chks :: [CheckCT]) (setChks :: [CheckCT]) :: Constraint where
  ValidateCheckExpr db tab ('CheckOn _ chkName ': chks) setChks =
    ( MissingCheck tab chkName (ElemCheck chkName setChks)
    , ValidateCheckExpr db tab chks setChks
    )
  ValidateCheckExpr _ _ _ _ = ()

type family ElemCheck (chkName :: Symbol) (setChks :: [CheckCT]) where
  ElemCheck cn (('CheckOn _ cn) ': chks) = 'True
  ElemCheck cn (_ ': chks)               = ElemCheck cn chks
  ElemCheck _  '[]                       = 'False

type family ValidateDBFld tab (fn :: Symbol) t :: Constraint where
  ValidateDBFld tab fn t = UnifyField (OriginalTableFields tab) fn t ('Text "column " ':<>: ('ShowType fn) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab))

type DefaultTableName t    = DefaultTypeName t
type DefaultDatabaseName t = DefaultTypeName t

type family DefaultTypeName (t :: *) :: Symbol where
  DefaultTypeName t = GenTyCon (Rep t)

type family GetSchemaName (t :: *) :: Symbol where
  GetSchemaName ()               = Schema ()
  GetSchemaName db               = Schema db

type OriginalTableFields t = GenTabFields (Rep t)

type family TableFields (db :: *) (t :: *) :: [*] where
  TableFields db t = TableFields' (GenTabFields (Rep t)) (ColumnNames db t)

type family TableFields' (flds :: [*]) (colMap :: [(Symbol, Symbol)]) :: [*] where
  TableFields' ((fn ::: ft) ': flds) colMap = (AliasedCol fn colMap ::: ft) ': (TableFields' flds colMap)
  TableFields' '[] colMap = '[]

type family MapAliasedCol (fns :: [Symbol]) (colMap :: [(Symbol, Symbol)]) :: [Symbol] where
  MapAliasedCol (fn ': fns) colMap = AliasedCol fn colMap ': MapAliasedCol fns colMap
  MapAliasedCol '[]         _      = '[]

type family AliasedCol (fn :: Symbol) (colMap :: [(Symbol, Symbol)]) :: Symbol where
  AliasedCol fn ('(fn, alias) ': colMap) = alias
  AliasedCol fn (_ ': colMap)            = AliasedCol fn colMap
  AliasedCol fn '[]                      = fn
  
type family GetTypeFields (t :: *) :: [(Symbol, [*])] where
  GetTypeFields t              = GenTyFields (Rep t)

-- newtype EnumType a = EnumType a
-- newtype SumType a = SumType a

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

happlyChkExpr :: (All CheckExpr chks) => [ColumnInfo] -> [(T.Text, T.Text)] -> HList (Chk db tab) chks -> [(T.Text, PQ.PrimExpr)]
happlyChkExpr cis chkMap (v :& vs) = checkExpr cis chkMap v : happlyChkExpr cis chkMap vs
happlyChkExpr _cis _chkMap Nil     = []

happlyDefExprs :: (All KnownSymbol xs) => [ColumnInfo] -> HList (Def db tab) xs -> [(T.Text, PQ.PrimExpr)]
happlyDefExprs cis (v :& vs) = defExpr cis v : happlyDefExprs cis vs
happlyDefExprs cis Nil       = []

class CheckExpr (chk :: CheckCT) where
  checkExpr :: [ColumnInfo] -> [(T.Text, T.Text)] -> Chk db tab chk -> (T.Text, PQ.PrimExpr)

instance CheckExpr ('CheckOn chkOns chkName) where
  checkExpr cis chkMaps (Chk val) = apCheckExpr (Proxy @chkOns) (Proxy @chkName) cis chkMaps val

class ApCheckExpr (chkOns :: [Symbol]) (chkName :: Symbol) val where
  apCheckExpr :: Proxy chkOns -> Proxy chkName -> [ColumnInfo] -> [(T.Text, T.Text)] -> val -> (T.Text, PQ.PrimExpr)

instance ( ApCheckExpr chkOns chkName b
         , KnownSymbol chkOn
         ) => ApCheckExpr (chkOn ': chkOns) chkName (Expr sc a -> b) where
  apCheckExpr _ pChkN cis chkMaps v =
    let colE = unsafeCol [dbColN]
        colN = T.pack (symbolVal (Proxy @chkOn))
        dbColN = getDbColumnName cis colN
    in  apCheckExpr (Proxy @chkOns) pChkN cis chkMaps (v colE)

instance (KnownSymbol chkName) => ApCheckExpr '[] chkName (Expr sc a) where
  apCheckExpr _ _ _ chkMaps e = (dbChkName, getExpr e)
    where dbChkName = getDbCheckName chkMaps (T.pack (symbolVal (Proxy @chkName)))

defExpr :: forall db tab fld. (KnownSymbol fld) => [ColumnInfo] -> Def db tab fld -> (T.Text, PQ.PrimExpr)
defExpr cis (Def (Expr e)) = (dbColN, e)
  where dbColN = getDbColumnName cis (T.pack (symbolVal (Proxy @fld)))

-- Value level counterparts
type HaskName = Text
type DBName   = Text
-- NOTE: Step count start from 0
--       If there are there are 3 steps, then there will be
--       corresponding 2 changesets [0 -> 1, 1 -> 2]
type Step = Int

mkTypeName :: Text -> Text -> Text -> TypeName Text
mkTypeName p m t =
  TypeName { _packageName = p
           , _moduleName  = m
           , _typeName    = t
           }

data TypeName a = TypeName { _packageName :: a
                           , _moduleName  :: a
                           , _typeName    :: a
                           }
                deriving (Show, Eq)

packageName :: Functor f => (a -> f a) -> TypeName a -> f (TypeName a)
packageName k t = fmap (\a -> t { _packageName = a }) (k (_packageName t))

moduleName :: Functor f => (a -> f a) -> TypeName a -> f (TypeName a)
moduleName k t = fmap (\a -> t { _moduleName = a }) (k (_moduleName t))

typeName :: Functor f => (a -> f a) -> TypeName a -> f (TypeName a)
typeName k t = fmap (\a -> t { _typeName = a }) (k (_typeName t))

newtype DBType = DBType { _dbType :: Text }
               deriving (Show, Eq)

dbType :: Functor f => (Text -> f Text) -> DBType -> f DBType
dbType k t = fmap coerce (k (coerce t))

data EntityName a = EntityName { _hsName :: a
                               , _dbName :: DBName
                               } deriving (Show, Eq)

mkEntityName :: a -> DBName -> EntityName a
mkEntityName hsn dbn = EntityName { _hsName = hsn
                                   , _dbName = dbn
                                   }

hsName :: Functor f => (a -> f a) -> EntityName a -> f (EntityName a)
hsName k t = fmap (\a -> t { _hsName = a }) (k (_hsName t))

dbName :: Functor f => (DBName -> f DBName) -> EntityName a -> f (EntityName a)
dbName k t = fmap (\a -> t { _dbName = a }) (k (_dbName t))

type EntityNameWithHask = EntityName HaskName
type EntityNameWithType = EntityName (TypeName Text)

eqBy :: (Eq a) => Lens' s a -> s -> s -> Bool
eqBy l old new = (old ^. l) == (new ^. l)

mkTypeNameInfo :: TypeName Text -> TypeNameMap -> TypeNameInfo
mkTypeNameInfo et tnm =
  TypeNameInfo { _typeNameVal = et
               , _typeNameMap = tnm
               }

data TypeNameInfo = TypeNameInfo { _typeNameVal   :: TypeName Text
                                 , _typeNameMap   :: TypeNameMap
                                 } deriving (Show, Eq)

data TypeNameMap = EnumTypeNM Text [Text]
                 -- | CompositeNM Text [(Text, TypeName Text)]
                 -- | FlatNM [(Text, TypeName Text)]
                 -- | EnumTextNM [Text]
                 -- | SumNM [(Text, [(Text, DBTypeK)])]
                 deriving (Show, Eq)

addEnumValAfter :: Text -> Text -> TypeNameMap -> TypeNameMap
addEnumValAfter eVal eAfter (EnumTypeNM et evs) =
  EnumTypeNM et (go eVal eAfter evs)

  where go eVal' eAfter' (ev : evs) = case ev == eAfter' of
          True  -> ev : eVal' : evs
          False -> ev : go eVal' eAfter' evs
  

addEnumValBefore :: Text -> Text -> TypeNameMap -> TypeNameMap
addEnumValBefore eVal eBefore (EnumTypeNM et evs) =
  EnumTypeNM et (go eVal eBefore evs)

  where go eVal' eBefore' (ev : evs) = case ev == eBefore' of
          True  -> eVal' : ev : evs
          False -> ev : go eVal' eBefore' evs

addEnumVal :: Text -> TypeNameMap -> TypeNameMap
addEnumVal eVal (EnumTypeNM et evs) =
  EnumTypeNM et (evs ++ [eVal])

typeNameVal :: Functor f => (TypeName Text -> f (TypeName Text)) -> TypeNameInfo -> f TypeNameInfo
typeNameVal k t = fmap (\a -> t { _typeNameVal = a }) (k (_typeNameVal t))

typeNameMap :: Functor f => (TypeNameMap -> f TypeNameMap) -> TypeNameInfo -> f TypeNameInfo
typeNameMap k t = fmap (\a -> t { _typeNameMap = a }) (k (_typeNameMap t))

mkDatabaseInfo :: EntityNameWithType -> [TypeNameInfo] -> Step -> Step -> TableInfos -> DatabaseInfo
mkDatabaseInfo n tnis b v tis =
  DatabaseInfo { _name          = n
               , _typeNameInfos = tnis
               , _ignoredTabs   = ()
               , _baseline      = b
               , _version       = v
               , _tableInfos    = tis
               } 
             
data DatabaseInfo = DatabaseInfo { _name           :: EntityNameWithType
                                 , _typeNameInfos  :: [TypeNameInfo]
                                 , _ignoredTabs    :: ()
                                 , _baseline       :: Step
                                 , _version        :: Step
                                 , _tableInfos     :: TableInfos
                                 } deriving (Show, Eq)

newtype TableInfos = TableInfos { _getTableInfos :: [TableInfo] }
                   deriving (Show, Eq)

name :: Functor f => (EntityNameWithType -> f EntityNameWithType) -> DatabaseInfo -> f DatabaseInfo
name k t = fmap (\a -> t { _name = a }) (k (_name t))

typeNameInfos :: Functor f => ([TypeNameInfo] -> f [TypeNameInfo]) -> DatabaseInfo -> f DatabaseInfo
typeNameInfos k t = fmap (\a -> t { _typeNameInfos = a }) (k (_typeNameInfos t))

typeNameInfoAt :: Functor f => TypeName Text -> (TypeNameInfo -> f TypeNameInfo) -> DatabaseInfo -> f DatabaseInfo
typeNameInfoAt et = typeNameInfos . unsafeFind et (_typeNameVal)

baseline :: Functor f => (Step -> f Step) -> DatabaseInfo -> f DatabaseInfo
baseline k t = fmap (\a -> t { _baseline = a }) (k (_baseline t))

version :: Functor f => (Step -> f Step) -> DatabaseInfo -> f DatabaseInfo
version k t = fmap (\a -> t { _version = a }) (k (_version t))

tableInfos :: Functor f => (TableInfos -> f TableInfos) -> DatabaseInfo -> f DatabaseInfo
tableInfos k t = fmap (\a -> t { _tableInfos = a }) (k (_tableInfos t))

tableInfoAt :: Functor f => TypeName T.Text -> (TableInfo -> f TableInfo) -> DatabaseInfo -> f DatabaseInfo
tableInfoAt et = tableInfos . coerceL . unsafeFind et (_hsName . _tableName)

mkTableInfo :: Maybe PrimaryKeyInfo -> [ForeignKeyInfo] -> [DefaultInfo] -> [CheckInfo] -> [UniqueInfo] -> [SequenceInfo] -> EntityNameWithType -> [ColumnInfo] -> TableInfo
mkTableInfo pki fki di cki uqi sqi tn ci =
  TableInfo { _primaryKeyInfo = pki
            , _foreignKeyInfo = fki
            , _defaultInfo    = di
            , _checkInfo      = cki
            , _uniqueInfo     = uqi
            , _sequenceInfo   = sqi
            , _tableName      = tn
            , _columnInfo     = ci
            , _ignoredCols    = ()
            }

data TableInfo = TableInfo { _primaryKeyInfo   :: Maybe PrimaryKeyInfo
                           , _foreignKeyInfo   :: [ForeignKeyInfo]
                           , _defaultInfo      :: [DefaultInfo]
                           , _checkInfo        :: [CheckInfo]
                           , _uniqueInfo       :: [UniqueInfo]
                           , _sequenceInfo     :: [SequenceInfo]
                           , _tableName        :: EntityNameWithType
                           , _columnInfo       :: [ColumnInfo]
                           , _ignoredCols      :: ()
                           } deriving (Show, Eq)

primaryKeyInfo :: Functor f => (Maybe PrimaryKeyInfo -> f (Maybe PrimaryKeyInfo)) -> TableInfo -> f TableInfo
primaryKeyInfo k t = fmap (\a -> t { _primaryKeyInfo = a }) (k (_primaryKeyInfo t))

foreignKeyInfoAt :: Functor f => HaskName -> (ForeignKeyInfo -> f ForeignKeyInfo) -> TableInfo -> f TableInfo
foreignKeyInfoAt hsN = foreignKeyInfo . unsafeFind hsN (_hsName . _fkeyName)

foreignKeyInfoAtDb :: Functor f => DBName -> (ForeignKeyInfo -> f ForeignKeyInfo) -> TableInfo -> f TableInfo
foreignKeyInfoAtDb dbN = foreignKeyInfo . unsafeFind dbN (_dbName . _fkeyName)

foreignKeyInfo :: Functor f => ([ForeignKeyInfo] -> f [ForeignKeyInfo]) -> TableInfo -> f TableInfo
foreignKeyInfo k t = fmap (\a -> t { _foreignKeyInfo = a }) (k (_foreignKeyInfo t))

defaultInfo :: Functor f => ([DefaultInfo] -> f [DefaultInfo]) -> TableInfo -> f TableInfo
defaultInfo k t = fmap (\a -> t { _defaultInfo = a }) (k (_defaultInfo t))

checkInfoAt :: Functor f => HaskName -> (CheckInfo -> f CheckInfo) -> TableInfo -> f TableInfo
checkInfoAt hsN = checkInfo . unsafeFind hsN (_hsName . _checkName)

checkInfoAtDb :: Functor f => HaskName -> (CheckInfo -> f CheckInfo) -> TableInfo -> f TableInfo
checkInfoAtDb dbN = checkInfo . unsafeFind dbN (_dbName . _checkName)

checkInfo :: Functor f => ([CheckInfo] -> f [CheckInfo]) -> TableInfo -> f TableInfo
checkInfo k t = fmap (\a -> t { _checkInfo = a }) (k (_checkInfo t))


uniqueInfo :: Functor f => ([UniqueInfo] -> f [UniqueInfo]) -> TableInfo -> f TableInfo
uniqueInfo k t = fmap (\a -> t { _uniqueInfo = a }) (k (_uniqueInfo t))

uniqueInfoAt :: Functor f => HaskName -> (UniqueInfo -> f UniqueInfo) -> TableInfo -> f TableInfo
uniqueInfoAt et = uniqueInfo . unsafeFind et (_hsName . _uqName)

uniqueInfoAtDb :: Functor f => DBName -> (UniqueInfo -> f UniqueInfo) -> TableInfo -> f TableInfo
uniqueInfoAtDb dbN = uniqueInfo . unsafeFind dbN (_dbName . _uqName)

sequenceInfo :: Functor f => ([SequenceInfo] -> f [SequenceInfo]) -> TableInfo -> f TableInfo
sequenceInfo k t = fmap (\a -> t { _sequenceInfo = a }) (k (_sequenceInfo t))

tableName :: Functor f => (EntityNameWithType -> f EntityNameWithType) -> TableInfo -> f TableInfo
tableName k t = fmap (\a -> t { _tableName = a }) (k (_tableName t))

columnInfo :: Functor f => ([ColumnInfo] -> f [ColumnInfo]) -> TableInfo -> f TableInfo
columnInfo k t = fmap (\a -> t { _columnInfo = a }) (k (_columnInfo t))

columnInfoAt :: Functor f => HaskName -> (ColumnInfo -> f ColumnInfo) -> TableInfo -> f TableInfo
columnInfoAt hsN = columnInfo . unsafeFind hsN (_hsName . _columnNameInfo)

columnInfoAtDb :: Functor f => DBName -> (ColumnInfo -> f ColumnInfo) -> TableInfo -> f TableInfo
columnInfoAtDb dbN = columnInfo . unsafeFind dbN (_dbName . _columnNameInfo)

mkColumnInfo :: Bool -> EntityNameWithHask -> DBType -> ColumnInfo
mkColumnInfo isn cni ctn =
  ColumnInfo { _isNullable = isn
             , _columnNameInfo = cni
             , _columnTypeName = ctn
             } 

data ColumnInfo = ColumnInfo { _isNullable     :: Bool
                             , _columnNameInfo :: EntityNameWithHask
                             , _columnTypeName :: DBType
                             } deriving (Show, Eq)

isNullable :: Functor f => (Bool -> f Bool) -> ColumnInfo -> f ColumnInfo
isNullable k t = fmap (\a -> t { _isNullable = a }) (k (_isNullable t))

columnNameInfo :: Functor f => (EntityNameWithHask -> f EntityNameWithHask) -> ColumnInfo -> f ColumnInfo
columnNameInfo k t = fmap (\a -> t { _columnNameInfo = a }) (k (_columnNameInfo t))

columnTypeName :: Functor f => (DBType -> f DBType) -> ColumnInfo -> f ColumnInfo
columnTypeName k t = fmap (\a -> t { _columnTypeName = a }) (k (_columnTypeName t))

mkPrimaryKeyInfo :: Text -> [HaskName] -> PrimaryKeyInfo
mkPrimaryKeyInfo pkn pkcols =
  PrimaryKeyInfo { _pkeyName    = pkn
                 , _pkeyColumns = pkcols
                 }

data PrimaryKeyInfo = PrimaryKeyInfo { _pkeyName    :: Text
                                     , _pkeyColumns :: [HaskName]
                                     } deriving (Eq, Show)

pkeyName :: Functor f => (Text -> f Text) -> PrimaryKeyInfo -> f PrimaryKeyInfo
pkeyName k t = fmap (\a -> t { _pkeyName = a }) (k (_pkeyName t))

pkeyColumns :: Functor f => ([HaskName] -> f [HaskName]) -> PrimaryKeyInfo -> f PrimaryKeyInfo
pkeyColumns k t = fmap (\a -> t { _pkeyColumns = a }) (k (_pkeyColumns t))

mkForeignKeyInfo :: EntityNameWithHask -> [HaskName] -> TypeName Text -> [HaskName] -> ForeignKeyInfo
mkForeignKeyInfo n cols reft refCols =
  ForeignKeyInfo { _fkeyName = n
                 , _fkeyColumns = cols
                 , _fkeyRefTable = reft
                 , _fkeyRefColumns = refCols
                 }

data ForeignKeyInfo = ForeignKeyInfo { _fkeyName       :: EntityNameWithHask
                                     , _fkeyColumns    :: [HaskName]
                                     , _fkeyRefTable   :: TypeName Text
                                     , _fkeyRefColumns :: [HaskName]
                                     } deriving (Show, Eq)

fkeyName :: Functor f => (EntityNameWithHask -> f EntityNameWithHask) -> ForeignKeyInfo -> f ForeignKeyInfo
fkeyName k t = fmap (\a -> t { _fkeyName = a }) (k (_fkeyName t))

fkeyColumns :: Functor f => ([HaskName] -> f [HaskName]) -> ForeignKeyInfo -> f ForeignKeyInfo
fkeyColumns k t = fmap (\a -> t { _fkeyColumns = a }) (k (_fkeyColumns t))

fkeyRefTable :: Functor f => (TypeName Text -> f (TypeName Text)) -> ForeignKeyInfo -> f ForeignKeyInfo
fkeyRefTable k t = fmap (\a -> t { _fkeyRefTable = a }) (k (_fkeyRefTable t))

fkeyRefColumns :: Functor f => ([HaskName] -> f [HaskName]) -> ForeignKeyInfo -> f ForeignKeyInfo
fkeyRefColumns k t = fmap (\a -> t { _fkeyRefColumns = a }) (k (_fkeyRefColumns t))

mkUniqueInfo :: EntityNameWithHask -> [HaskName] -> UniqueInfo
mkUniqueInfo n cols =
  UniqueInfo { _uqName = n
             , _uqColumns = cols
             } 
                             
data UniqueInfo = UniqueInfo { _uqName    :: EntityNameWithHask
                             , _uqColumns :: [HaskName]
                             } deriving (Show, Eq)

uqName :: Functor f => (EntityNameWithHask -> f EntityNameWithHask) -> UniqueInfo -> f UniqueInfo
uqName k t = fmap (\a -> t { _uqName = a }) (k (_uqName t))

uqColumns :: Functor f => ([HaskName] -> f [HaskName]) -> UniqueInfo -> f UniqueInfo
uqColumns k t = fmap (\a -> t { _uqColumns = a }) (k (_uqColumns t))

mkDefaultInfo :: HaskName -> PQ.PrimExpr -> DefaultInfo
mkDefaultInfo n e =
  DefaultInfo { _defaultOn  = n
              , _defaultExp = e
              } 

data DefaultInfo = DefaultInfo { _defaultOn  :: HaskName
                               , _defaultExp :: PQ.PrimExpr
                               } deriving (Show, Eq)

defaultOn :: Functor f => (HaskName -> f HaskName) -> DefaultInfo -> f DefaultInfo
defaultOn k t = fmap (\a -> t { _defaultOn = a }) (k (_defaultOn t))

defaultExp :: Functor f => (PQ.PrimExpr -> f PQ.PrimExpr) -> DefaultInfo -> f DefaultInfo
defaultExp k t = fmap (\a -> t { _defaultExp = a }) (k (_defaultExp t))

mkCheckInfo :: EntityNameWithHask -> PQ.PrimExpr -> CheckInfo
mkCheckInfo et e =
  CheckInfo { _checkExp  = e
            , _checkName = et
            }

data CheckInfo = CheckInfo { _checkExp  :: PQ.PrimExpr
                           , _checkName :: EntityNameWithHask
                           } deriving (Show, Eq)

checkName :: Functor f => (EntityNameWithHask -> f EntityNameWithHask) -> CheckInfo -> f CheckInfo
checkName k t = fmap (\a -> t { _checkName = a }) (k (_checkName t))

checkExp :: Functor f => (PQ.PrimExpr -> f PQ.PrimExpr) -> CheckInfo -> f CheckInfo
checkExp k t = fmap (\a -> t { _checkExp = a }) (k (_checkExp t))

mkSequenceInfo :: EntityNameWithHask -> HaskName -> SequenceType -> SequenceInfo
mkSequenceInfo n col ty =
  SequenceInfo { _seqName = n
               , _seqOn   = col
               , _seqType = ty
               }

data SequenceInfo = SequenceInfo { _seqName   :: EntityNameWithHask
                                 , _seqOn     :: HaskName
                                 , _seqType   :: SequenceType
                                 } deriving (Show, Eq)

seqName :: Functor f => (EntityNameWithHask -> f EntityNameWithHask) -> SequenceInfo -> f SequenceInfo
seqName k t = fmap (\a -> t { _seqName = a }) (k (_seqName t))

seqOn :: Functor f => (HaskName -> f HaskName) -> SequenceInfo -> f SequenceInfo
seqOn k t = fmap (\a -> t { _seqOn = a }) (k (_seqOn t))

seqType :: Functor f => (SequenceType -> f SequenceType) -> SequenceInfo -> f SequenceInfo
seqType k t = fmap (\a -> t { _seqType = a }) (k (_seqType t))

data SequenceType = SeqOwned | SeqSerial
                  deriving (Show, Eq)

data ForeignRefD = RefByD Text   -- ^ fk name
                          [Text] -- ^ cols
                          (TypeName Text) -- ^ ref tab name
                          [Text] -- ^ ref cols
                 | RefD   Text   -- ^ fk name
                          Text   -- ^ col
                          (TypeName Text) -- ^ ref tab name

headDatabaseInfo :: forall db.
                ( SingCtxDb db
                ) => Proxy db -> DatabaseInfo
headDatabaseInfo pdb =
  mkDatabaseInfo (headDbNameInfo pdb) (headTypeNameInfo pdb) 0 0 (coerce (headTableInfos pdb (sing :: Sing (Tables db))))

headTypeNameInfo :: forall db.
  ( SingI (GetTypeMappings db)
  , SingE (GetTypeMappings db)
  , All (UDType db) (Types db)
  , All Generic     (Types db)
  ) => Proxy db -> [TypeNameInfo]
headTypeNameInfo pdb =
  let typMappings = fromSing (sing :: Sing (GetTypeMappings db))
  in  map (uncurry mkTypeNameInfo) typMappings

dbTypeName :: TypeNameMap -> Text
dbTypeName tyMap =
  case tyMap of
    EnumTypeNM tyN _ -> tyN
            
headTableInfos :: (All (SingCtx db) xs) => Proxy (db :: *) -> Sing (xs :: [*]) -> [TableInfo]
headTableInfos pdb (SCons st sxs) =
  headTableInfo pdb st : headTableInfos pdb sxs
headTableInfos _ _ = []  

headTableInfo :: forall db tab.
             ( SingCtx db tab               
             ) => Proxy db -> Sing tab -> TableInfo
headTableInfo db stab =
  let hti = headTabNameInfo db tab 
      hci = headColInfos db tab
      tab = Proxy :: Proxy tab
  in mkTableInfo (headPkInfo db tab hti)
                 (headFkInfo db tab hti)
                 (headDefInfo db tab hti hci)
                 (headCksInfo db tab hti hci)
                 (headUqInfo db tab hti)
                 (headSeqsInfo db tab hti)
                 hti
                 hci

headPkInfo :: forall db tab.
          ( Table db tab
          , SingE (PrimaryKeyName db tab)
          , SingI (PrimaryKeyName db tab)
          , SingE (PrimaryKey db tab)
          , SingI (PrimaryKey db tab)
          ) => Proxy db -> Proxy tab -> EntityNameWithType -> Maybe PrimaryKeyInfo
headPkInfo _ _ et =
  let pkDefN = let hsn    = genTabName (et ^. hsName)
               in  genKeyName (PkNameGen hsn pkCols)
      pkCols = fromSing (sing :: Sing (PrimaryKey db tab))
  in  case pkCols of
    [] -> Nothing
    _  -> let dbn = maybe pkDefN id (fromSing (sing :: Sing (PrimaryKeyName db tab)))
          in Just $ mkPrimaryKeyInfo dbn pkCols

headFkInfo :: forall db tab.
          ( Table db tab
          , SingE (ForeignKey db tab)
          , SingI (ForeignKey db tab)
          , SingE (ForeignKeyNames db tab)
          , SingI (ForeignKeyNames db tab)
          ) => Proxy db -> Proxy tab -> EntityNameWithType -> [ForeignKeyInfo]
headFkInfo _ _ et = 
  let fkds = fromSing (sing :: Sing (ForeignKey db tab))
      fkNameMappings = fromSing (sing :: Sing (ForeignKeyNames db tab))
  in map (fkInfoOne et fkNameMappings) fkds

fkInfoOne ::  EntityNameWithType -> [(Text, Text)] -> ForeignRefD -> ForeignKeyInfo
fkInfoOne et fkMappings ref =
  case ref of
    (RefByD fkname hsCols refHsTabN hsRefCols) -> 
             let etName = mkEntityName fkname (getDbFkName et hsCols refHsTabN fkname fkMappings)
             in  mkForeignKeyInfo etName hsCols refHsTabN hsRefCols
    (RefD fkname hsCol refHsTabN) ->
             let etName = mkEntityName fkname (getDbFkName et [hsCol] refHsTabN fkname fkMappings)
                 hsCols = [hsCol]
             in  mkForeignKeyInfo etName hsCols refHsTabN hsCols
                 
  where getDbFkName et hsCols refHsTabN fkname fkMappings =
          case L.lookup fkname fkMappings of
            Just fkmapped -> fkmapped
            Nothing       -> let dbn    = genTabName (et ^. hsName)
                                 refn   = genTabName refHsTabN
                             in  genKeyName (FkNameGen dbn hsCols refn)
                                                                         
headUqInfo :: forall db tab.
          ( Table db tab
          , SingE (Unique db tab)
          , SingE (UniqueNames db tab)
          , SingI (Unique db tab)
          , SingI (UniqueNames db tab)
          ) => Proxy db -> Proxy tab -> EntityNameWithType -> [UniqueInfo]
headUqInfo _ _ et =
  let uniqs = fromSing (sing :: Sing (Unique db tab))
      uniqNameMappings = fromSing (sing :: Sing (UniqueNames db tab))
  in  map (uniqWithMapping et uniqNameMappings) uniqs
  
uniqWithMapping :: EntityNameWithType -> [(HaskName, DBName)] -> ([HaskName], HaskName) -> UniqueInfo
uniqWithMapping et uniqMaps (uniqFlds, uniqHsName) =
  let etName = mkEntityName uniqHsName (lookupUniqMapping uniqFlds uniqHsName uniqMaps)
  in  mkUniqueInfo etName uniqFlds

  where lookupUniqMapping hsCols uniqHsName uniqMaps = 
          case L.lookup uniqHsName uniqMaps of
            Just uniqDbName -> uniqDbName
            _               -> let hstn = genTabName (et ^. hsName)
                               in  genKeyName (UqNameGen hstn hsCols)

headDefInfo :: forall db tab.
           ( Table db tab
           ) => Proxy db -> Proxy tab -> EntityNameWithType -> [ColumnInfo] -> [DefaultInfo]
headDefInfo _ _ _et cis = case (defaults :: DBDefaults db tab) of
  DBDefaults hl -> map mkDefInfo (happlyDefExprs cis hl)

  where mkDefInfo (n, expr) = mkDefaultInfo n expr

headCksInfo :: forall db tab.
           ( Table db tab
           , SingE (CheckNames db tab)
           , SingI (CheckNames db tab)
           ) => Proxy db -> Proxy tab -> EntityNameWithType -> [ColumnInfo] -> [CheckInfo]
headCksInfo _ _ et cis =  case (checks :: DBChecks db tab) of
  DBChecks hls -> map (checkInfoOne et chkNameMaps) (happlyChkExpr cis chkNameMaps hls)
  where chkNameMaps = fromSing (sing :: Sing (CheckNames db tab))

checkInfoOne :: EntityNameWithType -> [(Text, Text)] -> (Text, PQ.PrimExpr) -> CheckInfo
checkInfoOne et chkNameMaps (n, expr) =
  let etName = mkEntityName n (lookupchkMappings et n chkNameMaps)
  in mkCheckInfo etName expr

  where lookupchkMappings et checkHsName chkMaps =  
          case L.lookup checkHsName chkMaps of
            Just checkDbName -> checkDbName
            _                -> let hstn = genTabName (et ^. hsName)
                                in  genKeyName (CkNameGen hstn checkHsName) 

headSeqsInfo :: forall db tab.
            ( Table db tab
            , SingI (TableSequence db tab)
            , SingE (TableSequence db tab)
            , SingI (SequenceNames db tab)
            , SingE (SequenceNames db tab)
            ) => Proxy db -> Proxy tab -> EntityNameWithType -> [SequenceInfo]
headSeqsInfo _ _ et =
  let seqs = fromSing (sing :: Sing (TableSequence db tab))
      seqNameMappings = fromSing (sing :: Sing (SequenceNames db tab))
  in  map (mkSeqInfoOne et seqNameMappings) seqs

mkSeqInfoOne :: EntityNameWithType -> [(Text, Text)] -> (Text, Text, SequenceType) -> SequenceInfo
mkSeqInfoOne et seqNameMaps (seqcol, seqHsn, st) =
  let etName = mkEntityName seqHsn (lookupSeqMapping et seqcol seqHsn seqNameMaps)
  in  mkSequenceInfo etName seqcol st
      
  where lookupSeqMapping et hsCol seqHsName seqMaps =  
          case L.lookup seqHsName seqMaps of
            Just seqDbName -> seqDbName
            _              -> let hstn    = genTabName (et ^. hsName)
                              in  genKeyName (SeqNameGen hstn hsCol Nothing)

headDbNameInfo :: forall db.
               ( Database db
               , SingE (Schema db)
               , SingI (Schema db)
               , SingE (GetPMT (Rep db))
               , SingI (GetPMT (Rep db))
               ) => Proxy (db :: *) -> EntityNameWithType
headDbNameInfo _ =
  (mkEntityName (coerce (fromSing (sing :: Sing (GetPMT (Rep db)))))
                               (fromSing (sing :: Sing (Schema db)))
                 )

headTabNameInfo :: forall tab db.
               ( Table db tab
               , KnownSymbol (TableName db tab)
               , SingE (GetPMT (Rep tab))
               , SingI (GetPMT (Rep tab))
               ) => Proxy (db :: *) -> Proxy (tab :: *) -> EntityNameWithType
headTabNameInfo _ _ =
  mkEntityName (coerce (fromSing (sing :: Sing (GetPMT (Rep tab)))))
                       (fromSing (sing :: Sing (TableName db tab)))

headColInfos :: forall tab db.
            ( Table db tab
            , SingE (ColumnNames db tab)
            , SingI (ColumnNames db tab)
            , SingE (OriginalTableFieldInfo db tab)
            , SingI (OriginalTableFieldInfo db tab)
            ) => Proxy db -> Proxy tab -> [ColumnInfo]
headColInfos _ _ =
  let colMap = fromSing (sing :: Sing (ColumnNames db tab))
      hsns   = fromSing (sing :: Sing (OriginalTableFieldInfo db tab))
  in  map (colInfoOne colMap) hsns
                         
colInfoOne :: [(Text, Text)] -> ((Text, Bool), Text) -> ColumnInfo
colInfoOne cMap ((typN, isNull), hsn) =
  let dbn = case L.lookup hsn cMap of
        Just dbn' -> dbn'
        _         -> hsn
      etName = mkEntityName hsn dbn
  in mkColumnInfo isNull etName (coerce typN)

getDbColumnName :: [ColumnInfo] -> HaskName -> DBName
getDbColumnName cis n = (getColumnInfo cis n) ^. columnNameInfo . dbName

getDbColumnNames :: [ColumnInfo] -> [HaskName] -> [DBName]
getDbColumnNames cis = map (getDbColumnName cis)

getDbCheckName :: [(T.Text, T.Text)] -> T.Text -> T.Text
getDbCheckName chkMap k = fromJust (L.lookup k chkMap)
  where fromJust (Just m) = m
        fromJust Nothing  = k

{-
getDbColumnName :: TypeName Text -> [TableInfo] -> HaskName -> DBName
getDbColumnName = undefined

getDbColumnNames :: TypeName Text -> [TableInfo] -> [HaskName] -> [DBName]
getDbColumnNames tn tis = map (getDbColumnName tn tis)

getHaskTypeName :: Text -> [TableInfo] -> TypeName Text
getHaskTypeName = undefined

getHaskColumnName :: TypeName Text -> [TableInfo] -> DBName -> HaskName
getHaskColumnName = undefined

getHaskColumnNames :: TypeName Text -> [TableInfo] -> [DBName] -> [HaskName]
getHaskColumnNames = undefined

getDbColumnInfoNames :: [ColumnInfo] -> [Text]
getDbColumnInfoNames = map (_dbName . _columnNameInfo)


-}

filterColumns :: [Text] -> [ColumnInfo] -> [ColumnInfo]
filterColumns hsns cis = map (getColumnInfo cis) hsns

getNullableColumns :: [ColumnInfo] -> [ColumnInfo]
getNullableColumns = filter _isNullable

getNonNullableColumns :: [ColumnInfo] -> [ColumnInfo]
getNonNullableColumns = filter (not . _isNullable)

getColumnInfo :: [ColumnInfo] -> Text -> ColumnInfo
getColumnInfo cis hsn = 
  let mci = L.find (\ci -> _hsName (_columnNameInfo ci) == hsn) cis
  in case mci of
       Just ci -> ci
       Nothing  -> error $ "Panic: invalid column name lookup for (hs)column: " ++ show hsn


class ( Table db tab
      , KnownSymbol (TableName db tab)
      , KnownSymbol (DefaultTableName tab)
        
      , SingE (ColumnNames db tab)
      , SingI (ColumnNames db tab)                  
      , SingE (OriginalTableFieldInfo db tab)
      , SingI (OriginalTableFieldInfo db tab)

      , SingE (PrimaryKeyName db tab)
      , SingI (PrimaryKeyName db tab)
      , SingE (PrimaryKey db tab)
      , SingI (PrimaryKey db tab)

      , SingE (Unique db tab)
      , SingE (UniqueNames db tab)
      , SingI (Unique db tab)
      , SingI (UniqueNames db tab)

      , SingE (ForeignKey db tab)
      , SingI (ForeignKey db tab)
      , SingE (ForeignKeyNames db tab)
      , SingI (ForeignKeyNames db tab)

      , SingI (TableSequence db tab)
      , SingE (TableSequence db tab)
      , SingI (SequenceNames db tab)
      , SingE (SequenceNames db tab)

      , SingE (CheckNames db tab)
      , SingI (CheckNames db tab)

      , SingI (GetPMT (Rep tab))
      , SingE (GetPMT (Rep tab))        
      ) => SingCtx db tab where

instance ( Table db tab
      , KnownSymbol (TableName db tab)
      , KnownSymbol (DefaultTableName tab)
        
      , SingE (ColumnNames db tab)
      , SingI (ColumnNames db tab)                  
      , SingE (OriginalTableFieldInfo db tab)
      , SingI (OriginalTableFieldInfo db tab)

      , SingE (PrimaryKeyName db tab)
      , SingI (PrimaryKeyName db tab)
      , SingE (PrimaryKey db tab)
      , SingI (PrimaryKey db tab)

      , SingE (Unique db tab)
      , SingE (UniqueNames db tab)
      , SingI (Unique db tab)
      , SingI (UniqueNames db tab)

      , SingE (ForeignKey db tab)
      , SingI (ForeignKey db tab)
      , SingE (ForeignKeyNames db tab)
      , SingI (ForeignKeyNames db tab)

      , SingI (TableSequence db tab)
      , SingE (TableSequence db tab)
      , SingI (SequenceNames db tab)
      , SingE (SequenceNames db tab)

      , SingE (CheckNames db tab)
      , SingI (CheckNames db tab)

      , SingI (GetPMT (Rep tab))
      , SingE (GetPMT (Rep tab))
      ) => SingCtx db tab

class ( Database db
      , SingE (Schema db)
      , SingI (Schema db)
      , SingI (GetPMT (Rep db))
      , SingE (GetPMT (Rep db))
      , All (SingCtx db) (Tables db)
      , SingI (Tables db)
      , SingI (GetTypeMappings db)
      , SingE (GetTypeMappings db)
      , All (UDType db) (Types db)
      , All Generic     (Types db)
      ) => SingCtxDb db where

instance ( Database db
         , SingE (DefaultDatabaseName db)
         , SingE (Schema db)
         , SingI (DefaultDatabaseName db)
         , SingI (Schema db)
         , SingI (GetPMT (Rep db))
         , SingE (GetPMT (Rep db))
         , All (SingCtx db) (Tables db)
         , SingI (Tables db)
         , SingI (GetTypeMappings db)
         , SingE (GetTypeMappings db)
         , All (UDType db) (Types db)
         , All Generic     (Types db)
         ) => SingCtxDb db where  
  
type family OriginalTableFieldInfo (db :: *) (tab :: *) :: [((TagHK DbK DBTypeK, Bool), Symbol)] where
  OriginalTableFieldInfo db tab = GetFieldInfo (DB db) (OriginalTableFields tab)

type family GetFieldInfo (db :: DbK) (xs :: [*]) :: [((TagHK DbK DBTypeK, Bool), Symbol)] where
  GetFieldInfo db (fld ::: x ': xs) = '(TagTypeInfo db (GetDBTypeRep db x), fld) ': GetFieldInfo db xs
  GetFieldInfo db '[]               = '[]

type family TagTypeInfo (db :: DbK) (dbt :: DBTypeK) :: (TagHK DbK DBTypeK, Bool) where
  TagTypeInfo db t              =  '( 'Tag db t, (IsNullable t))

type family IsNullable (dbt :: DBTypeK) where
  IsNullable ('DBNullable t) = 'True
  IsNullable _               = 'False

reproxy :: proxy a -> Proxy a
reproxy _ = Proxy

col :: forall (db :: *) (tab :: *) (col :: Symbol) (a :: *) sc.
  ( KnownSymbol col
  , UnifyField sc col a ('Text "Unable to find column " ':<>: 'ShowType col)
  , Table db tab
  , Database db
  , SingE (ColumnNames db tab)
  , SingI (ColumnNames db tab)
  , SingE (OriginalTableFieldInfo db tab)
  , SingI (OriginalTableFieldInfo db tab)
  ) => Proxy (DBTag db tab col) -> Expr sc a
col _ = Expr (PQ.AttrExpr sym)
  where sym = maybe (error "Panic: Empty col @col_") id (PQ.toSym [dbColN])
        dbColN = _dbName (_columnNameInfo (getColumnInfo (headColInfos (Proxy @db) (Proxy @tab)) fld))
        fld = T.pack (symbolVal (Proxy @col))

insert :: a -> [a] -> [a]
insert = (:)

deleteErr :: (Eq b) => b -> (a -> b) -> [a] -> [a]
deleteErr = undefined

delete :: (Eq b) => b -> (a -> b) -> [a] -> [a]
delete = undefined

data KeyNameGen
  = PkNameGen T.Text [T.Text]
  | FkNameGen T.Text [T.Text] T.Text
  | UqNameGen T.Text [T.Text]
  | CkNameGen T.Text T.Text
  | SeqNameGen T.Text T.Text (Maybe T.Text)
  deriving (Show, Eq)

genKeyName :: KeyNameGen -> T.Text
genKeyName (PkNameGen tab _cols)          = T.intercalate "_" ("pk":tab:[])
genKeyName (FkNameGen tab cols reft)      = T.intercalate "_" (("fk":tab:cols) ++ [reft])
genKeyName (UqNameGen tab cols)           = T.intercalate "_" ("uq":tab:cols)
genKeyName (CkNameGen tab cn)             = T.intercalate "_" ["ck",tab,cn]
genKeyName (SeqNameGen tab cn Nothing)    = T.intercalate "_" ["seq",tab,cn]
genKeyName (SeqNameGen tab cn (Just n))   = T.intercalate "_" ["seq",tab, cn, n]

-- camelToSnake, PascalToSnake 

genTabName :: TypeName T.Text -> T.Text
genTabName tn = tn ^. typeName

{-
ppDatabaseInfo :: DatabaseInfo -> String
ppDatabaseInfo di =
  "Database Name: " <> ppEntityName (di ^. databaseName) <>
  "Tables 
-}  
