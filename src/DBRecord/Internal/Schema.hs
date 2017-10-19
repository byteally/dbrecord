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


module DBRecord.Internal.Schema where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits
import GHC.Generics
import GHC.Exts
import GHC.OverloadedLabels
import Data.Kind
import Data.Typeable
import Data.Functor.Const
import DBRecord.Internal.Types
import DBRecord.Internal.Common
import DBRecord.Internal.Expr
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.DBTypes
import qualified Data.List as L

data Col (a :: Symbol) = Col
data DefSyms = DefSyms [Symbol]

class ( -- TypeCxts db (Types db)
      ) => Database (db :: *) where
  type Schema db :: Symbol
  type Schema db = "public"
  
  type Tables db :: [Type]
  
  type Types db :: [Type]
  type Types db = '[]
  
  type TabIgnore db :: [Type]
  type TabIgnore db = '[]
  
  type Baseline db :: *
  type Baseline db = ()
  
  type Version db :: *
  type Version db = ()  

  type DB db :: DbK
  type DB db = TypeError ('Text "DB type is not configured in the Database instance for type " ':<>: 'ShowType db ':$$:
                          'Text "Hint: add following to the Database instance for type "       ':<>: 'ShowType db ':$$:
                          'Text "type DB " ':<>: 'ShowType db ':<>: 'Text " = " ':<>: 'ShowType 'Postgres
                         )

class ( Database db
      , AssertCxt (Elem (Tables db) tab) ('Text "Database " ':<>: 'ShowType db ':<>: 'Text " does not contain the table: " ':<>: 'ShowType tab)
      , ValidateTableProps db tab
      , KnownSymbol (TableName db tab)
      , Generic tab
      ) => Table (db :: *) (tab :: *) where
  type PrimaryKey db tab :: [Symbol]
  type PrimaryKey db tab = '[]

  type PrimaryKeyName db tab :: Maybe Symbol
  type PrimaryKeyName db tab = 'Nothing

  type ForeignKey db tab :: [ForeignRef]
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
  type TypeMappings ty :: UDTypeMappings
  type TypeMappings ty = 'Flat '[]

data UDTypeMappings = Composite [(Symbol, Symbol)]
                    | Flat [(Symbol, Symbol)]

data TagHK (b :: tk) (a :: k) = Tag b a

type family TagEach (db :: tk) (ent :: [k]) :: [TagHK tk k] where
  TagEach db (ent ': ents) = Tag db ent ': TagEach db ents
  TagEach db '[]           = '[]

data family Sing (a :: k)

data instance Sing (s :: Symbol) where
  SSym :: KnownSymbol s => Sing s

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
  
data instance Sing (a :: TagHK tk k) where
  STag :: Sing tag -> Sing a -> Sing ('Tag tag a)

data instance Sing (uq :: UniqueCT) where
  SUniqueOn :: Sing uniqFlds -> Sing uniqOn -> Sing (UniqueOn uniqFlds uniqOn)

data instance Sing (fk :: ForeignRef) where
  SRefBy :: Sing cols -> Sing reft -> Sing refCols -> Sing fkname -> Sing ('RefBy cols reft refCols fkname)
  SRef   :: Sing col -> Sing reft -> Sing fkname -> Sing ('Ref col reft fkname)

data instance Sing (ch :: CheckCT) where
  SCheck :: Sing cols -> Sing cname -> Sing ('CheckOn cols cname)

data instance Sing (uq :: Sequence) where
  SPGSerial :: Sing col -> Sing seqn -> Sing ('PGSerial col seqn)
  SPGOwned  :: Sing col -> Sing seqn -> Sing ('PGOwned col seqn)
  
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

instance SingI DBText where
  sing = SDBText

instance SingI DBCiText where
  sing = SDBCiText

instance SingI DBDate where
  sing = SDBDate

instance SingI DBTime where
  sing = SDBTime

instance SingI DBTimestamp where
  sing = SDBTimestamp

instance SingI DBTimestamptz where
  sing = SDBTimestamptz

instance SingI DBUuid where
  sing = SDBUuid

instance SingI DBByteArr where
  sing = SDBByteArr

instance SingI DBJson where
  sing = SDBJson

instance SingI DBJsonB where
  sing = SDBJsonB

instance SingI DBInterval where
  sing = SDBInterval

instance (SingI dbt) => SingI (DBArray dbt) where
  sing = SDBArray sing

instance (SingI dbt) => SingI (DBNullable dbt) where
  sing = SDBNullable sing

instance ( SingI t
         , SingI dbt
         , SingI b
         ) => SingI (DBCustomType t dbt b) where
  sing = SDBCustomType sing sing sing

instance (SingI sym) => SingI (DBTypeName sym) where
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

instance ( SingI a
         , SingI tag
         ) => SingI (Tag tag a)  where
  sing = STag sing sing

instance (Typeable t) => SingI (t :: *) where
  sing = STypeRep

instance (SingI uniqFlds, SingI uniqOn) => SingI (UniqueOn uniqFlds uniqOn) where
  sing = SUniqueOn sing sing 

instance (SingI cols, SingI reft, SingI refcols, SingI fkname) => SingI (RefBy cols reft refcols fkname) where
  sing = SRefBy sing sing sing sing

instance (SingI col, SingI reft, SingI fkname) => SingI (Ref col reft fkname) where
  sing = SRef sing sing sing

instance (SingI col, SingI seqn) => SingI (PGSerial col seqn)  where
  sing = SPGSerial sing sing

instance (SingI col, SingI seqn) => SingI (PGOwned col seqn)  where
  sing = SPGOwned sing sing

instance ( SingI cols
         , KnownSymbol cname
         , All SingE cols
         ) => SingI ('CheckOn cols cname) where
  sing = SCheck sing sing

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

type family Fst (tup :: (k1, k2)) :: k1 where
  Fst '(a, b) = a

type family Snd (tup :: (k1, k2)) :: k2 where
  Snd '(a, b) = b

type family MaybeCtx (ctx :: k -> Constraint) (m :: Maybe k) :: Constraint where
  MaybeCtx ctx ('Just m) = ctx m
  MaybeCtx _   'Nothing  = ()

type family UqCtx (ctx :: Symbol -> Constraint) (uq :: UniqueCT) :: Constraint where
  UqCtx ctx ('UniqueOn uniqFlds uniqOn) = (All ctx uniqFlds, ctx uniqOn)

type family FKCtx (ctx :: Symbol -> Constraint) (fk :: TagHK Type ForeignRef) :: Constraint where
  FKCtx ctx (Tag db ('RefBy cols reft refcols name)) = (All ctx cols, All ctx refcols, ctx name, KnownSymbol (DefaultTableName reft)
                                                       , SingE (ColumnNames db reft), SingI (ColumnNames db reft), SingE (TableName db reft)
                                                       , SingE (OriginalTableFieldInfo db reft), SingI (OriginalTableFieldInfo db reft)
                                                       , Table db reft
                                                       )
  FKCtx ctx (Tag db ('Ref col reft name))            = (ctx col, KnownSymbol (DefaultTableName reft), ctx name, SingE (ColumnNames db reft)
                                                       , SingI (ColumnNames db reft), SingE (TableName db reft), Table db reft
                                                       , SingE (OriginalTableFieldInfo db reft), SingI (OriginalTableFieldInfo db reft)
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
  DBTypeCtx (Tag dbT dbTy) = (ShowDBType dbT dbTy)

instance (DBTypeCtx taggedDbt) => SingE (taggedDbt :: TagHK DbK DBTypeK) where
  type Demote taggedDbt     = Text
  fromSing (STag sdb stype) = showDBTypeSing sdb stype

showDBTypeSing :: forall db dbTy.
                   (ShowDBType db dbTy
                   ) => Sing (db :: DbK) -> Sing (dbTy :: DBTypeK) -> Text
showDBTypeSing dbK dbT = showDBType (reproxy dbK) (reproxy dbT)

instance (UqCtx SingE uq) => SingE (uq :: UniqueCT) where
  type Demote (uq :: UniqueCT)         = (Demote (Any :: [Symbol]), Demote (Any :: Symbol))
  fromSing (SUniqueOn uniqFlds uniqOn) = (fromSing uniqFlds, fromSing uniqOn)

instance (FKCtx SingE fk) => SingE (fk :: TagHK Type ForeignRef) where
  type Demote (fk :: TagHK Type ForeignRef) = ForeignRefD
  fromSing (STag db (SRefBy cols reft refcols fkname)) =
    RefByD (fromSing fkname) (fromSing cols)
           (fromSingDefTabName reft) (fromSing refcols)
           (fromSingTabName db reft) (fromSingColNames db reft) 
  fromSing (STag db (SRef col reft fkname)) =
    RefD  (fromSing fkname) (fromSing col) (fromSingDefTabName reft)
          (fromSingTabName db reft) (fromSingColNames db reft)

fromSingDefTabName :: forall reft.
                      ( SingI (DefaultTableName reft)
                      , SingE (DefaultTableName reft)
                      ) => Sing reft -> Demote (DefaultTableName reft :: Symbol)
fromSingDefTabName _ = fromSing (sing :: Sing (DefaultTableName reft))

fromSingTabName :: forall db reft.
                      ( SingI (TableName db reft)
                      , SingE (TableName db reft)
                      , Table db reft
                      ) => Sing db -> Sing reft -> Demote (TableName db reft :: Symbol)
fromSingTabName _ _ = fromSing (sing :: Sing (TableName db reft))

fromSingColNames :: forall db reft.
                      ( Table db reft
                      , SingE (ColumnNames db reft)
                      , SingE (OriginalTableFieldInfo db reft)
                      , SingI (OriginalTableFieldInfo db reft)
                      , SingI (ColumnNames db reft)
                      ) => Sing db -> Sing reft -> [ColumnInfo]
fromSingColNames _ _ = colInfos (Proxy :: Proxy db) (Proxy :: Proxy reft)
                      
tabName :: forall db t proxy.
  KnownSymbol (TableName db t) => proxy db -> proxy t -> String
tabName _ _ = symbolVal (Proxy :: Proxy (TableName db t))

instance SingE (seq :: Sequence) where
  type Demote seq = (String, String, SequenceType)
  fromSing (SPGSerial col seqn) = (fromSing col, fromSing seqn, SeqSerial)
  fromSing (SPGOwned col seqn)  = (fromSing col, fromSing seqn, SeqOwned)
  
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

type family GetFKNames (fks :: [ForeignRef]) :: [Symbol] where
  GetFKNames (RefBy _ _ _ n ': refs) = n ': GetFKNames refs
  GetFKNames (Ref _ _ n ': refs)     = n ': GetFKNames refs
  GetFKNames '[]                     = '[]

type family OriginalUQNames db tab :: [Symbol] where
  OriginalUQNames db tab = GetUQNames (Unique db tab)

type family GetUQNames (uqs :: [UniqueCT]) :: [Symbol] where
  GetUQNames (UniqueOn _ n ': uqs) = n ': GetUQNames uqs
  GetUQNames '[]                   = '[]

type family OriginalCheckNames db tab :: [Symbol] where
  OriginalCheckNames db tab = GetCheckNames (Check db tab)

type family GetCheckNames (chks :: [CheckCT]) :: [Symbol] where
  GetCheckNames (CheckOn _ n ': chks) = n ': GetCheckNames chks
  GetCheckNames '[]                   = '[]

type family OriginalSequenceNames db tab :: [Symbol] where
  OriginalSequenceNames db tab = GetSequenceNames (TableSequence db tab)

type family GetSequenceNames (seqs :: [Sequence]) :: [Symbol] where
  GetSequenceNames (PGSerial _ n ': seqs) = n ': GetSequenceNames seqs
  GetSequenceNames (PGOwned _ n ': seqs)  = n ': GetSequenceNames seqs
  GetSequenceNames '[]                    = '[]

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

type family ValidateTabFk db tab (fks :: [ForeignRef]) :: Constraint where
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

data ForeignRef
  = RefBy [Symbol] Type [Symbol] Symbol
  | Ref Symbol Type Symbol

data UniqueCT = UniqueOn [Symbol] Symbol

data CheckCT = CheckOn [Symbol] Symbol

data Ix = Ix Symbol

data IgnoredCol
  = IgnoreRest
  | IgnoreOnly [Symbol]
  | IgnoreExcept [Symbol]
  | IgnoreNone

data Def (tab :: *) (fn :: Symbol) = forall v.Def (Expr '[] v)

def :: forall (fn :: Symbol) (tab :: *) v.(ValidateDBFld tab fn v) => Expr '[] v -> Def tab fn
def = Def

instance ( ValidateDBFld tab un a
         , un ~ fn
         , v ~ Expr '[] a
         ) => IsLabel un (v -> Def tab fn) where
#if __GLASGOW_HASKELL__ > 800
  fromLabel v = def @un @tab v
#else
  fromLabel _ v = def @un @tab v
#endif

data DBDefaults (db :: *) tab = forall xs.(AllF (DefExpr db tab) xs) => DBDefaults (HList (Def tab) xs)

end :: HList f '[]
end = Nil

dbDefaults :: forall tab db xs.
              ( AllF (DefExpr db tab) xs
              , ValidateDefExprs db tab (HasDefault db tab) xs
              ) => HList (Def tab) xs -> DBDefaults db tab
dbDefaults = DBDefaults

type family ValidateDefExprs (db :: *) (tab :: *) (defs :: [Symbol]) (setDefs :: [Symbol]) :: Constraint where
  ValidateDefExprs db tab (def ': defs) setDefs =
    ( MissingDefault tab def (Elem setDefs def)
    , ValidateDefExprs db tab defs setDefs
    )
  ValidateDefExprs db tab '[] _ = ()

data Chk (db :: *) (tab :: *) (chk :: CheckCT) = forall val.(ApCheckOnExpr db tab chk val) => Chk val

data DBChecks (db :: *) tab = forall chks. (All (CheckExpr db tab) chks) => DBChecks (HList (Chk db tab) chks)

type family ApCheckOnExpr db tab chk val where
  ApCheckOnExpr db tab ('CheckOn cols name) v = ApCheckExpr db tab cols name v

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
         , ApCheckExpr db tab (PartialJust args) cn val
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
        , ApCheckExpr db tab (PartialJust args) cn val
        ) => val -> Chk db tab ('CheckOn (PartialJust args) cn)
check = Chk

dbChecks :: forall tab (db :: *) chks.
            ( All (CheckExpr db tab) chks
            , ValidateCheckExpr db tab (Check db tab) chks
            ) => HList (Chk db tab) chks -> DBChecks db tab
dbChecks = DBChecks

type family ValidateCheckExpr db tab (chks :: [CheckCT]) (setChks :: [CheckCT]) :: Constraint where
  ValidateCheckExpr db tab (CheckOn _ chkName ': chks) setChks =
    ( MissingCheck tab chkName (ElemCheck chkName setChks)
    , ValidateCheckExpr db tab chks setChks
    )
  ValidateCheckExpr _ _ _ _ = ()

type family ElemCheck (chkName :: Symbol) (setChks :: [CheckCT]) where
  ElemCheck cn (('CheckOn _ cn) ': chks) = 'True
  ElemCheck cn (_ ': chks)               = ElemCheck cn chks
  ElemCheck _  '[]                       = 'False

type family ValidateDBFld tab (fn :: Symbol) t :: Constraint where
  ValidateDBFld tab fn t = UnifyField (OriginalTableFields tab) (fn ::: t) ('Text "column " ':<>: ('ShowType fn) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab))

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

newtype EnumType a = EnumType a
newtype SumType a = SumType a

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

happlyChkExpr :: (AllF (CheckExpr db tab) chks) => Proxy db -> [ColumnInfo] -> HList (Chk db tab) chks -> [(String, PQ.PrimExpr)]
happlyChkExpr p cis (v :& vs) = checkExpr cis v : happlyChkExpr p cis vs
happlyChkExpr _ _    Nil       = []

happlyDefExprs :: (AllF (DefExpr db tab) xs) => Proxy db -> HList (Def tab) xs -> [(String, PQ.PrimExpr)]
happlyDefExprs p (v :& vs) = defExpr p v : happlyDefExprs p vs
happlyDefExprs _  Nil      = []

class CheckExpr db tab (chk :: CheckCT) where
  checkExpr :: [ColumnInfo] -> Chk db tab chk -> (String, PQ.PrimExpr)

instance CheckExpr db tab ('CheckOn chkOns chkName) where
  checkExpr cis (Chk val) = apCheckExpr (Proxy @db) (Proxy @tab) (Proxy @chkOns) (Proxy @chkName) cis val

class ApCheckExpr db tab (chkOns :: [Symbol]) (chkName :: Symbol) val where
  apCheckExpr :: Proxy db -> Proxy tab -> Proxy chkOns -> Proxy chkName -> [ColumnInfo] -> val -> (String, PQ.PrimExpr)

instance ( ApCheckExpr db tab chkOns chkName b
         , KnownSymbol chkOn
         ) => ApCheckExpr db tab (chkOn ': chkOns) chkName (Expr sc a -> b) where
  apCheckExpr pDb pTab _ pChkN cis v =
    let colE = unsafeCol [colN]
        colN = dbColumnName (columnNameInfo (getColumnInfo cis (symbolVal (Proxy @chkOn))))
    in  apCheckExpr pDb pTab (Proxy @chkOns) pChkN cis (v colE)

instance (KnownSymbol chkName) => ApCheckExpr db tab '[] chkName (Expr sc a) where
  apCheckExpr _ _ _ _ _ e = (symbolVal (Proxy @chkName), getExpr e)

class ( KnownSymbol fld
      , Table db tab
      ) => DefExpr db tab fld

instance ( KnownSymbol fld
         , Table db tab
         ) => DefExpr db tab fld      

defExpr :: forall db tab fld.
           ( Table db tab
           , KnownSymbol fld
           ) => Proxy db -> Def tab fld -> (String, PQ.PrimExpr)
defExpr _ (Def (Expr e)) = (symbolVal (Proxy @fld), e)

-- Value level counterparts
newtype TypeName = TypeName { typeName :: Text }
                 deriving (Show, Eq)
                          
data DatabaseInfo = DatabaseInfo { hsName       :: Text
                                 , schemaName   :: Text
                                 , typeNames    :: [TypeName]
                                 , ignoredTabs  :: ()
                                 , baseline     :: ()
                                 , version      :: ()
                                 , tableInfos   :: [TableInfo]
                                 } deriving (Show, Eq)

data TableInfo = TableInfo { primaryKeyInfo   :: PrimaryKeyInfo
                           , foreignKeyInfo   :: [ForeignKeyInfo]
                           , defaultInfo      :: [DefaultInfo]
                           , checkInfo        :: [CheckInfo]
                           , uniqueInfo       :: [UniqueInfo]
                           , sequenceInfo     :: [SequenceInfo]
                           , tableName        :: TableNameInfo
                           , columnInfo      :: [ColumnInfo]
                           , ignoredCols      :: ()
                           } deriving (Show, Eq)

data TableNameInfo = TableNameInfo { dbTableName   :: Text
                                   , tableTypeName :: TypeName
                                   } deriving (Show, Eq)

data ColumnNameInfo = ColumnNameInfo { hsColumnName :: Text
                                     , dbColumnName :: Text
                                     } deriving (Show, Eq)

data ColumnInfo = ColumnInfo { isNullable     :: Bool
                             , columnNameInfo :: ColumnNameInfo
                             , columnTypeName :: TypeName
                             } deriving (Show, Eq)

data PrimaryKeyInfo = PrimaryKeyInfo { pkeyName    :: Maybe Text
                                     , pkeyColumns :: [ColumnInfo]
                                     } deriving (Eq, Show)

data ForeignKeyInfo = ForeignKeyInfo { fkeyHsName  :: Text
                                     , fkeyDbName  :: Text
                                     , fkeyType    :: ForeignKeyType
                                     } deriving (Show, Eq)

data ForeignKeyType         = ForeignKeyRefBy [ColumnInfo]    -- ^ columns
                                              TableNameInfo   -- ^ ref table
                                              [ColumnInfo]    -- ^ ref cols
                            | ForeignKeyRef   ColumnInfo      -- ^ col
                                              TableNameInfo   -- ^ ref table
                                              ColumnInfo      -- ^ ref col
                    deriving (Show, Eq)
                             
data UniqueInfo = UniqueInfo { uqName    :: Text
                             , uqColumns :: [ColumnInfo]
                             } deriving (Show, Eq)

data DefaultInfo = DefaultInfo { defaultOn  :: ColumnInfo
                               , defaultExp :: PQ.PrimExpr
                               } deriving (Show, Eq)

data CheckInfo = CheckInfo { checkExp  :: PQ.PrimExpr
                           , checkName :: Text
                           } deriving (Show, Eq)

data SequenceInfo = SequenceInfo { seqName :: Text
                                 , seqOn   :: ColumnInfo
                                 , seqType :: SequenceType
                                 } deriving (Show, Eq)
                                            
data SequenceType = SeqOwned | SeqSerial
                  deriving (Show, Eq)

data ForeignRefD = RefByD String -- ^ fk name
                          [String] -- ^ cols
                          String   -- ^ ref tab name
                          [String] -- ^ ref cols
                          String   -- ^ db tab
                          [ColumnInfo] -- ^ ref col map
                 | RefD   String   -- ^ fk name
                          String   -- ^ col
                          String   -- ^ ref tab name
                          String   -- ^ db tab
                          [ColumnInfo] -- ^ ref col map

databaseInfo :: forall db.
                ( Database db
                , SingE (DefaultDatabaseName db)
                , SingE (Schema db)
                , SingI (DefaultDatabaseName db)
                , SingI (Schema db)
                ) => Proxy db -> DatabaseInfo
databaseInfo _ =
  DatabaseInfo { hsName = T.pack (fromSing (sing :: Sing (DefaultDatabaseName db)))
               , schemaName = T.pack (fromSing (sing :: Sing (Schema db)))
               , typeNames = []
               , ignoredTabs = ()
               , baseline = ()
               , version = ()
               , tableInfos = []
               }

tableInfo :: forall db tab.
             ( SingCtx db tab               
             ) => Proxy db -> Proxy tab -> TableInfo
tableInfo db tab =
  let tabNI  = tabNameInfo db tab
      colIs  = colInfos db tab
  in  TableInfo { primaryKeyInfo  = pkInfo db tab tabNI colIs
                , foreignKeyInfo  = fkInfo db tab tabNI colIs
                , uniqueInfo      = uqInfo db tab tabNI colIs
                , defaultInfo     = defInfo db tab tabNI colIs
                , checkInfo       = cksInfo db tab tabNI colIs
                , sequenceInfo    = seqsInfo db tab tabNI colIs
                , tableName       = tabNI
                , columnInfo     = colIs
                , ignoredCols     = ()
                }

pkInfo :: forall db tab.
          ( Table db tab
          , SingE (PrimaryKeyName db tab)
          , SingI (PrimaryKeyName db tab)
          , SingE (PrimaryKey db tab)
          , SingI (PrimaryKey db tab)
          ) => Proxy db -> Proxy tab -> TableNameInfo -> [ColumnInfo] -> PrimaryKeyInfo
pkInfo _ _ tni cis =
  PrimaryKeyInfo { pkeyName    = fmap T.pack (fromSing (sing :: Sing (PrimaryKeyName db tab)))
                 , pkeyColumns = filterColumns (fromSing (sing :: Sing (PrimaryKey db tab))) cis
                 }

fkInfo :: forall db tab.
          ( Table db tab
          , SingE (TagEach db (ForeignKey db tab))
          , SingI (TagEach db (ForeignKey db tab))
          , SingE (ForeignKeyNames db tab)
          , SingI (ForeignKeyNames db tab)
          ) => Proxy db -> Proxy tab -> TableNameInfo -> [ColumnInfo] -> [ForeignKeyInfo]
fkInfo _ _ tni cis = 
  let fkds = fromSing (sing :: Sing (TagEach db (ForeignKey db tab)))
      fkNameMappings = fromSing (sing :: Sing (ForeignKeyNames db tab))
  in map (fkInfoOne fkNameMappings) fkds

  where fkInfoOne fkMappings (RefByD fkname cols refHsTabN refcols refDbTabN refcolmap) =
          let colInfos = filterColumns cols cis
              refColInfos = filterColumns refcols refcolmap
              tabInfo  = TableNameInfo { dbTableName     = T.pack refDbTabN
                                       , tableTypeName = coerce (T.pack refHsTabN)
                                       } 
          in ForeignKeyInfo { fkeyHsName = T.pack fkname
                            , fkeyDbName = getDbFkName fkname fkMappings
                            , fkeyType   = ForeignKeyRefBy colInfos tabInfo refColInfos
                            }
        fkInfoOne fkMappings (RefD fkname col refHsTabN refDbTabN refcolmap) =
          let colInfo = getColumnInfo cis col
              refcolInfo = getColumnInfo refcolmap col
              tabInfo  = TableNameInfo { tableTypeName = coerce (T.pack refHsTabN)
                                       , dbTableName     = T.pack refDbTabN
                                       }               
          in ForeignKeyInfo { fkeyHsName = T.pack fkname
                            , fkeyDbName = getDbFkName fkname fkMappings
                            , fkeyType   = ForeignKeyRef colInfo tabInfo refcolInfo
                            }
        getDbFkName fkname fkMappings = T.pack $ 
          case L.lookup fkname fkMappings of
            Just fkmapped -> fkmapped
            Nothing       -> fkname
                                                                         
uqInfo :: forall db tab.
          ( Table db tab
          , SingE (Unique db tab)
          , SingE (UniqueNames db tab)
          , SingI (Unique db tab)
          , SingI (UniqueNames db tab)
          ) => Proxy db -> Proxy tab -> TableNameInfo -> [ColumnInfo] -> [UniqueInfo]
uqInfo _ _ tni cis =
  let uniqs = fromSing (sing :: Sing (Unique db tab))
      uniqNameMappings = fromSing (sing :: Sing (UniqueNames db tab))
  in  map (uniqWithMapping uniqNameMappings) uniqs
  
  where uniqWithMapping uniqMaps (uniqFlds, uniqHsName) =
          UniqueInfo { uqColumns = filterColumns uniqFlds   cis
                     , uqName    = lookupUniqMapping uniqHsName uniqMaps
                     }
        lookupUniqMapping uniqHsName uniqMaps = T.pack $ 
          case L.lookup uniqHsName uniqMaps of
            Just uniqDbName -> uniqDbName
            _               -> uniqHsName

defInfo :: forall db tab.
           ( Table db tab
           ) => Proxy db -> Proxy tab -> TableNameInfo -> [ColumnInfo] -> [DefaultInfo]
defInfo _ _ _ cis = case (defaults :: DBDefaults db tab) of
  DBDefaults hl -> map mkDefInfo ((happlyDefExprs (Proxy :: Proxy db)) hl)

  where mkDefInfo (n, exp) = DefaultInfo { defaultOn  = getColumnInfo cis n
                                         , defaultExp = exp
                                         }

cksInfo :: forall db tab.
           ( Table db tab
           , SingE (CheckNames db tab)
           , SingI (CheckNames db tab)
           ) => Proxy db -> Proxy tab -> TableNameInfo -> [ColumnInfo] -> [CheckInfo]
cksInfo _ _ _ cis =  case (checks :: DBChecks db tab) of
  DBChecks hls -> map mkCheckInfo (happlyChkExpr (Proxy @db) cis hls)

  where mkCheckInfo (n, exp) = CheckInfo { checkExp  = exp
                                         , checkName = lookupchkMappings n chkNameMaps
                                         }

        chkNameMaps       = fromSing (sing :: Sing (CheckNames db tab))
        lookupchkMappings checkHsName chkMaps = T.pack $ 
          case L.lookup checkHsName chkMaps of
            Just checkDbName -> checkDbName
            _                -> checkHsName
        
seqsInfo :: forall db tab.
            ( Table db tab
            , SingI (TableSequence db tab)
            , SingE (TableSequence db tab)
            , SingI (SequenceNames db tab)
            , SingE (SequenceNames db tab)
            ) => Proxy db -> Proxy tab -> TableNameInfo -> [ColumnInfo] -> [SequenceInfo]
seqsInfo _ _ _ cis =
  let seqs = fromSing (sing :: Sing (TableSequence db tab))
      seqNameMappings = fromSing (sing :: Sing (SequenceNames db tab))
  in  map (mkSeqInfo seqNameMappings) seqs

  where mkSeqInfo seqNameMaps (seqcol, seqHsn, st) =
          SequenceInfo { seqName = lookupSeqMapping seqHsn seqNameMaps
                       , seqOn   = getColumnInfo cis seqcol
                       , seqType = st
                       } 
        lookupSeqMapping seqHsName seqMaps = T.pack $ 
          case L.lookup seqHsName seqMaps of
            Just seqDbName -> seqDbName
            _              -> seqHsName
       

tabNameInfo :: forall tab db.
               ( Table db tab
               , KnownSymbol (TableName db tab)
               , KnownSymbol (DefaultTableName tab)
               ) => Proxy (db :: *) -> Proxy (tab :: *) -> TableNameInfo
tabNameInfo _ _ =
  TableNameInfo { tableTypeName = coerce (T.pack (fromSing (sing :: Sing (DefaultTableName tab))))
                , dbTableName   = T.pack (fromSing (sing :: Sing (TableName db tab)))
                }

colInfos :: forall tab db.
            ( Table db tab
            , SingE (ColumnNames db tab)
            , SingI (ColumnNames db tab)
            , SingE (OriginalTableFieldInfo db tab)
            , SingI (OriginalTableFieldInfo db tab)
            ) => Proxy db -> Proxy tab -> [ColumnInfo]
colInfos _ _ =
  let colMap = fromSing (sing :: Sing (ColumnNames db tab))
      hsns   = fromSing (sing :: Sing (OriginalTableFieldInfo db tab))
  in  map (go colMap) hsns
                         
  where go :: [(String, String)] -> ((Text, Bool), String) -> ColumnInfo
        go cMap ((typN, isNull), hsn) =
          let dbn = case L.lookup hsn cMap of
               Just dbn' -> dbn'
               _         -> hsn
          in ColumnInfo { columnNameInfo =
                            ColumnNameInfo { hsColumnName   = T.pack hsn
                                           , dbColumnName   = T.pack dbn
                                           }
                        , isNullable     = isNull
                        , columnTypeName = coerce typN
                        }

filterColumns :: [String] -> [ColumnInfo] -> [ColumnInfo]
filterColumns hsns cis = map (getColumnInfo cis) hsns

getDbColumnNames :: [ColumnInfo] -> [Text]
getDbColumnNames = map (dbColumnName . columnNameInfo)

getNullableColumns :: [ColumnInfo] -> [ColumnInfo]
getNullableColumns = filter isNullable

getNonNullableColumns :: [ColumnInfo] -> [ColumnInfo]
getNonNullableColumns = filter (not . isNullable)

getColumnInfo :: [ColumnInfo] -> String -> ColumnInfo
getColumnInfo cis hsn = 
  let mci = L.find (\ci -> hsColumnName (columnNameInfo ci) == T.pack hsn) cis
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

      , SingE (TagEach db (ForeignKey db tab))
      , SingI (TagEach db (ForeignKey db tab))
      , SingE (ForeignKeyNames db tab)
      , SingI (ForeignKeyNames db tab)

      , SingI (TableSequence db tab)
      , SingE (TableSequence db tab)
      , SingI (SequenceNames db tab)
      , SingE (SequenceNames db tab)

      , SingE (CheckNames db tab)
      , SingI (CheckNames db tab)
      ) => SingCtx db tab where

type family OriginalTableFieldInfo (db :: *) (tab :: *) where
  OriginalTableFieldInfo db tab = GetFieldInfo (DB db) (OriginalTableFields tab)

type family GetFieldInfo (db :: DbK) (xs :: [*]) :: [((TagHK DbK DBTypeK, Bool), Symbol)] where
  GetFieldInfo db (fld ::: x ': xs) = '(TagTypeInfo db (GetDBTypeRep db x), fld) ': GetFieldInfo db xs
  GetFieldInfo db '[]               = '[]

type family TagTypeInfo (db :: DbK) (dbt :: DBTypeK) :: (TagHK DbK DBTypeK, Bool) where
  TagTypeInfo db (DBNullable t) = '(Tag db (DBNullable t), 'True)
  TagTypeInfo db t              = '(Tag db t, 'False)

reproxy :: proxy a -> Proxy a
reproxy _ = Proxy
