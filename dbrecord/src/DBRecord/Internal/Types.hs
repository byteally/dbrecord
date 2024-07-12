{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, TypeOperators, GADTs, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, CPP, GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies, UndecidableInstances, UndecidableSuperClasses, ScopedTypeVariables, FunctionalDependencies, AllowAmbiguousTypes, RankNTypes, FlexibleContexts, TypeApplications #-}
module DBRecord.Internal.Types where

import GHC.TypeLits
import qualified Data.Text as T
import Data.Kind
import Data.Typeable
import GHC.Exts
import Data.Text (Text)
-- import qualified Data.HashMap.Strict as HM

data DbK = Postgres
         | MySQL
         | SQLite
         | Cassandra
         | Presto
         | MSSQL
         deriving (Eq, Show)

data Max = Max
         deriving (Show, Eq, Ord, Read)

data DBTypeK
  = TDBInt4
  | TDBInt8
  | TDBInt2
  -- | TDBFloat4 -- float(24)
  -- | TDBFloat8 -- float(53)
  | TDBFloat Nat
  | TDBNumeric Nat Nat
  | TDBChar Nat
  | TDBVarchar (Either Max Nat)
  | TDBBool
  | TDBDate
  | TDBTime Nat
  | TDBTimetz Nat
  | TDBTimestamp Nat
  | TDBTimestamptz Nat
  | TDBInterval (Maybe Type) Nat
  | TDBNullable DBTypeK
  | TDBXml
  | TDBJson
  -- NOTE: Non SQL 92
  | TDBBinary Nat
  | TDBVarbinary (Either Max Nat)
  -- NOTE: Non standard
  | TDBText
  | TDBCiText
  | TDBUuid
  | TDBBit    Nat
  | TDBVarbit Nat
  | TDBJsonB
  | TDBArray DBTypeK
  | TDBLTree

data DBTypeNameK = TDBTypeName Symbol [TypeArgK]

data TypeArgK = SymArg Symbol
              | NatArg Nat

data DBType = DBInt4
            | DBInt8
            | DBInt2
            | DBFloat   Integer
            | DBNumeric Integer Integer
            | DBChar Integer
            | DBVarchar (Either Max Integer)
            | DBBool
            | DBDate
            | DBTime Integer
            | DBTimetz Integer
            | DBTimestamp Integer
            | DBTimestamptz Integer
            | DBInterval (Maybe ()) Integer
            | DBNullable DBType
            | DBXml
            | DBJson
            | DBBinary Integer
            | DBVarbinary (Either Max Integer)
            | DBText
            | DBCiText
            | DBUuid
            | DBBit    Integer
            | DBVarbit Integer
            | DBJsonB
            | DBArray DBType
            | DBLTree
            | OtherType DBTypeName
            deriving (Show, Eq, Ord, Read)

data DBTypeName = DBTypeName TypeNameQual T.Text [TypeArg]
                deriving (Show, Eq, Ord, Read)

data TypeArg = TextArg    T.Text
             | IntegerArg Integer
             deriving (Show, Eq, Ord, Read)

data TypeNameQual
  = SchemaQualified Text
  | DBQualified Text Text
  | NoQualification
  deriving (Show, Eq, Ord, Read)

              
data UDTypeK
  = UDRec UDRecK -- ^ Invariant: Haskell Record Type
  | UDEnum UDEnumK -- ^ Invariant: Haskell Sum-Of-Nullary Type
  | TaggedSum UDEnumK UDRecK -- ^ Invariant: Haskell Sum-Of-AnyUniaryOrNullary Type
  | TaggedSumMono UDEnumK Type UDRecK -- ^ Invariant: Haskell Sum-Of-UnaryOrNullary Type, where all the argument is of same type allowing single column to be used for all the variants
  | SumOfCol UDRecK -- ^ Invariant: Haskell Sum-Of-NonNUllNativeUniary Type where the column name is the discriminator. All Ctor Arg should be non-null Native
  | SerializedBlob ContentType -- ^ Invariant: Any serializable Haskell Type

-- ^ Invariant: Haskell Record Type
-- Flat inside Flat: Allowed
-- Flat inside Composite: NotAllowed
-- Composite inside Flat: Allowed
-- Composite inside Composite: Allowed
-- Flat cannot be ArrayObj
data UDRecK
  = CompositeRec -- ^ Native
  | FlatRec -- ^ Synthetic
  | JsonRec -- ^ Native

-- ^ Invariant: Haskell Sum-Of-Nullary Type
data UDEnumK
  = EnumType -- ^ Native
  | EnumText -- ^ Native
  | EnumNum -- ^ Native

data ContentType
  = JsonContent (Maybe Type)
  | TextContent (Maybe Type)
  | XmlContent (Maybe Type)
  | BinaryContent Type

data DBSupportK
  = Native
  | Synthesized

type family GetDBSupportOf (udt :: UDTypeK) :: DBSupportK where
  GetDBSupportOf ('UDRec 'CompositeRec) = 'Native
  GetDBSupportOf ('UDRec 'JsonRec) = 'Native
  GetDBSupportOf ('UDRec 'FlatRec) = 'Synthesized
  GetDBSupportOf ('UDEnum _) = 'Native
  GetDBSupportOf ('SerializedBlob _) = 'Native
  GetDBSupportOf ('TaggedSum _ 'FlatRec) = 'Synthesized
  GetDBSupportOf ('TaggedSum _ _) = 'Native
  GetDBSupportOf ('TaggedSumMono _ _ 'FlatRec) = 'Synthesized
  GetDBSupportOf ('TaggedSumMono _ _ _) = 'Native
  GetDBSupportOf ('SumOfCol 'FlatRec) = 'Synthesized
  GetDBSupportOf ('SumOfCol _) = 'Native

type family GetDBEnumK (db :: DbK) :: UDEnumK where
  GetDBEnumK 'Postgres = 'EnumType
  GetDBEnumK 'SQLite = 'EnumText
  GetDBEnumK 'MySQL = 'EnumType
  GetDBEnumK 'MSSQL = 'EnumType
  GetDBEnumK 'Cassandra = 'EnumType
  GetDBEnumK 'Presto = 'EnumType

data family Sing (a :: k)

class SingI (a :: k) where
  sing :: Sing a

class SingE (a :: k) where
  type Demote a :: Type
  fromSing :: Sing a -> Demote (Any :: k)

data instance Sing (s :: Symbol) where
  SSym :: KnownSymbol s => Sing s

data instance Sing (v :: Nat) where
  SNat :: KnownNat v => Sing v

data instance Sing (t :: Type) where
  STypeRep :: Typeable t => Sing (t :: Type)

data instance Sing (b :: Bool) where
  STrue :: Sing 'True
  SFalse :: Sing 'False

data instance Sing (t :: Maybe k) where
  SJust     :: Sing (a :: k) -> Sing ('Just a)
  SNothing  :: Sing 'Nothing

data instance Sing (t :: Either k1 k2) where
  SLeft   :: Sing (a :: k1) -> Sing ('Left  a)
  SRight  :: Sing (a :: k2) -> Sing ('Right a)

data instance Sing (t :: (k1, k2)) where
  STuple :: Sing (a :: k1) -> Sing (b :: k2) -> Sing '(a, b)

data instance Sing (xs :: [k]) where
  SNil  :: Sing '[]
  SCons :: Sing x -> Sing xs -> Sing (x ': xs)

data instance Sing (db :: DbK) where
  SPostgres  :: Sing 'Postgres
  SMySQL     :: Sing 'MySQL
  SSQLite    :: Sing 'SQLite
  SCassandra :: Sing 'Cassandra
  SPresto    :: Sing 'Presto
  SMSSQL     :: Sing 'MSSQL

instance SingI 'True where
  sing = STrue

instance SingI 'False where
  sing = SFalse

instance (KnownSymbol s) => SingI (s :: Symbol) where
  sing = SSym

instance (KnownNat n) => SingI (n :: Nat) where
  sing = SNat

instance (Typeable t) => SingI (t :: Type) where
  sing = STypeRep

instance (SingI t) => SingI ('Just t :: Maybe k) where
  sing = SJust sing

instance SingI ('Nothing :: Maybe k) where
  sing = SNothing

instance (SingI t) => SingI ('Left t :: Either k1 k2) where
  sing = SLeft sing

instance (SingI t) => SingI ('Right t :: Either k1 k2) where
  sing = SRight sing

instance (SingI t1, SingI t2) => SingI ( '(t1, t2) :: (,) k1 k2) where
  sing = STuple sing sing

instance (SingI x, SingI xs) => SingI ((x ': xs) :: [] k) where
  sing = SCons sing sing

instance SingI ('[] :: [] k) where
  sing = SNil

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

instance SingI 'MSSQL where
  sing = SMSSQL

instance SingE (b :: Bool) where
  type Demote b = Bool
  fromSing STrue  = True
  fromSing SFalse = False

instance SingE (sy :: Symbol) where
  type Demote sy = T.Text
  fromSing SSym = T.pack (symbolVal (Proxy :: Proxy sy))

instance SingE (n :: Nat) where
  type Demote n = Integer
  fromSing SNat = natVal (Proxy :: Proxy n)

type family Fst (tup :: (k1, k2)) :: k1 where
  Fst '(a, b) = a

type family Snd (tup :: (k1, k2)) :: k2 where
  Snd '(a, b) = b

type family MaybeCtx (ctx :: k -> Constraint) (m :: Maybe k) :: Constraint where
  MaybeCtx ctx ('Just m) = ctx m
  MaybeCtx _   'Nothing  = ()

type family EitherCtx (ctxl :: k1 -> Constraint) (ctxr :: k2 -> Constraint) (m :: Either k1 k2) :: Constraint where
  EitherCtx ctxl _ ('Left m)  = ctxl m
  EitherCtx _ ctxr ('Right m) = ctxr m

instance ( SingE (Fst tup)
         , SingE (Snd tup)
         ) => SingE (tup :: (k1, k2)) where
  type Demote (tup :: (k1, k2)) = (Demote (Any :: k1), Demote (Any :: k2))
  fromSing (STuple x y) = (fromSing x, fromSing y)

instance (MaybeCtx SingE m) => SingE (m :: Maybe k) where
  type Demote (m :: Maybe k) = Maybe (Demote (Any :: k))
  fromSing SNothing   = Nothing
  fromSing (SJust x)  = Just (fromSing x)

instance (EitherCtx SingE SingE m) => SingE (m :: Either k1 k2) where
  type Demote (m :: Either k1 k2) = Either (Demote (Any :: k1)) (Demote (Any :: k2))
  fromSing (SLeft x)  = Left (fromSing x)
  fromSing (SRight x) = Right (fromSing x)

instance All SingE xs => SingE (xs :: [k]) where
  type Demote (xs :: [k]) = [Demote (Any :: k)]
  fromSing SNil         = []
  fromSing (SCons x xs) = fromSing x : fromSing xs

instance SingE (db :: DbK) where
  type Demote db = DbK
  fromSing SPostgres  = Postgres
  fromSing SMySQL     = MySQL
  fromSing SSQLite    = SQLite
  fromSing SCassandra = Cassandra
  fromSing SPresto    = Presto
  fromSing SMSSQL     = MSSQL

data instance Sing (t :: DBTypeK) where
  SDBInt4        :: Sing 'TDBInt4
  SDBInt8        :: Sing 'TDBInt8
  SDBInt2        :: Sing 'TDBInt2
  SDBFloat       :: Sing n -> Sing ('TDBFloat n)
  SDBNumeric     :: Sing n1 -> Sing n2 -> Sing ('TDBNumeric n1 n2)
  SDBChar        :: Sing n -> Sing ('TDBChar n)
  SDBVarchar     :: Sing n -> Sing ('TDBVarchar n)
  SDBBool        :: Sing 'TDBBool
  SDBDate        :: Sing 'TDBDate
  SDBTime        :: Sing n -> Sing ('TDBTime n)
  SDBTimetz      :: Sing n -> Sing ('TDBTimetz n)
  SDBTimestamp   :: Sing n -> Sing ('TDBTimestamp n)
  SDBTimestamptz :: Sing n -> Sing ('TDBTimestamptz n)
  SDBInterval    :: Sing n1 -> Sing n2 -> Sing ('TDBInterval n1 n2)
  SDBNullable    :: Sing a -> Sing ('TDBNullable a)
  SDBXml         :: Sing 'TDBXml
  SDBJson        :: Sing 'TDBJson
  SDBBinary      :: Sing n -> Sing ('TDBBinary n)
  SDBVarbinary   :: Sing n -> Sing ('TDBVarbinary n)
  SDBText        :: Sing 'TDBText
  SDBCiText      :: Sing 'TDBCiText
  SDBUuid        :: Sing 'TDBUuid
  SDBBit         :: Sing n -> Sing ('TDBBit n)
  SDBVarbit      :: Sing n -> Sing ('TDBVarbit n)
  SDBJsonB       :: Sing 'TDBJsonB
  SDBArray       :: Sing a -> Sing ('TDBArray a)
  SDBLTree       :: Sing 'TDBLTree
  -- SDBCustomType  :: Sing sc -> Sing t -> Sing dbt -> Sing ('TDBCustomType sc t dbt)

data instance Sing (t :: DBTypeNameK) where
  SDBTypeName :: Sing s -> Sing args -> Sing ('TDBTypeName s args)

data instance Sing (t :: TypeArgK) where
  SSymArg :: Sing n -> Sing ('SymArg n)
  SNatArg :: Sing n -> Sing ('NatArg n)

data instance Sing (m :: Max) where
  SMax :: Sing 'Max

instance (SingI n) => SingI ('SymArg n) where
  sing = SSymArg sing

instance (SingI n) => SingI ('NatArg n) where
  sing = SNatArg sing

instance SingI 'TDBInt4 where
  sing = SDBInt4

instance SingI 'TDBInt8 where
  sing = SDBInt8

instance SingI 'TDBInt2 where
  sing = SDBInt2

instance (SingI n) => SingI ('TDBFloat n) where
  sing = SDBFloat sing

instance (SingI n1, SingI n2) => SingI ('TDBNumeric n1 n2) where
  sing = SDBNumeric sing sing

instance (SingI n) => SingI ('TDBChar n) where
  sing = SDBChar sing

instance (SingI n) => SingI ('TDBVarchar n) where
  sing = SDBVarchar sing

instance SingI ('TDBBool) where
  sing = SDBBool

instance SingI ('TDBDate) where
  sing = SDBDate

instance (SingI n) => SingI ('TDBTime n) where
  sing = SDBTime sing

instance (SingI n) => SingI ('TDBTimetz n) where
  sing = SDBTimetz sing

instance (SingI n) => SingI ('TDBTimestamp n) where
  sing = SDBTimestamp sing

instance (SingI n) => SingI ('TDBTimestamptz n) where
  sing = SDBTimestamptz sing

instance (SingI n1, SingI n2) => SingI ('TDBInterval n1 n2) where
  sing = SDBInterval sing sing

instance (SingI n) => SingI ('TDBNullable n) where
  sing = SDBNullable sing

instance SingI 'TDBXml where
  sing = SDBXml

instance SingI 'TDBJson where
  sing = SDBJson

instance (SingI n) => SingI ('TDBBinary n) where
  sing = SDBBinary sing

instance (SingI n) => SingI ('TDBVarbinary n) where
  sing = SDBVarbinary sing

instance SingI 'TDBText where
  sing = SDBText

instance SingI 'TDBCiText where
  sing = SDBCiText

instance SingI 'TDBUuid where
  sing = SDBUuid

instance (SingI n) => SingI ('TDBBit n) where
  sing = SDBBit sing

instance (SingI n) => SingI ('TDBVarbit n) where
  sing = SDBVarbit sing

instance SingI ('TDBJsonB) where
  sing = SDBJsonB

instance (SingI n) => SingI ('TDBArray n) where
  sing = SDBArray sing

instance SingI ('TDBLTree) where
  sing = SDBLTree

-- instance ( SingI t, SingI dbt, SingI sc
--          ) => SingI ('TDBCustomType sc t dbt) where
--   sing = SDBCustomType sing sing sing

instance (SingI s, SingI args) => SingI ('TDBTypeName s args) where
  sing = SDBTypeName sing sing

instance SingI 'Max where
  sing = SMax

instance SingE (t :: Max) where
  type Demote (t :: Max) = Max
  fromSing SMax = Max

type family DBTypeCtx (t :: DBTypeK) :: Constraint where
  DBTypeCtx ('TDBFloat v)             = SingE v
  DBTypeCtx ('TDBNumeric v1 v2)       = (SingE v1, SingE v2)
  DBTypeCtx ('TDBChar v)              = SingE v
  DBTypeCtx ('TDBVarchar v)           = EitherCtx SingE SingE v
  DBTypeCtx ('TDBTime v)              = SingE v
  DBTypeCtx ('TDBTimetz v)            = SingE v
  DBTypeCtx ('TDBTimestamp v)         = SingE v
  DBTypeCtx ('TDBTimestamptz v)       = SingE v
  DBTypeCtx ('TDBInterval _ v)        = SingE v
  DBTypeCtx ('TDBNullable v)          = SingE v
  DBTypeCtx ('TDBBinary v)            = SingE v
  DBTypeCtx ('TDBVarbinary v)         = EitherCtx SingE SingE v
  DBTypeCtx ('TDBBit v)               = SingE v
  DBTypeCtx ('TDBVarbit v)            = SingE v
  DBTypeCtx ('TDBArray v)             = SingE v
  DBTypeCtx _                             = ()
  
instance (DBTypeCtx t) => SingE (t :: DBTypeK) where
  type Demote t = DBType
  
  fromSing SDBInt4                 = DBInt4
  fromSing SDBInt8                 = DBInt8
  fromSing SDBInt2                 = DBInt2
  fromSing (SDBFloat v)            = DBFloat (fromSing v)
  fromSing (SDBNumeric n1 n2)      = DBNumeric (fromSing n1) (fromSing n2)
  fromSing (SDBChar n)             = DBChar (fromSing n)
  fromSing (SDBVarchar n)          = DBVarchar (fromSing n)
  fromSing SDBBool                 = DBBool
  fromSing SDBDate                 = DBDate
  fromSing (SDBTime n)             = DBTime (fromSing n)
  fromSing (SDBTimetz n)           = DBTimetz (fromSing n)
  fromSing (SDBTimestamp n)        = DBTimestamp (fromSing n)
  fromSing (SDBTimestamptz n)      = DBTimestamptz (fromSing n)
  fromSing (SDBInterval _ n2)      = DBInterval Nothing (fromSing n2)
  fromSing (SDBNullable n)         = DBNullable (fromSing n)
  fromSing SDBXml                  = DBXml
  fromSing (SDBBinary n)           = DBBinary (fromSing n)
  fromSing (SDBVarbinary n)        = DBVarbinary (fromSing n)
  fromSing SDBText                 = DBText
  fromSing SDBCiText               = DBCiText
  fromSing SDBUuid                 = DBUuid
  fromSing (SDBBit n)              = DBBit (fromSing n)
  fromSing (SDBVarbit n)           = DBVarbit (fromSing n)
  fromSing SDBJson                 = DBJson  
  fromSing SDBJsonB                = DBJsonB
  fromSing (SDBArray a)            = DBArray (fromSing a)
  fromSing SDBLTree                = DBLTree

--

class (AllF f xs) => All (f :: k -> Constraint) (xs :: [k])
instance (AllF f xs) => All f xs

type family AllF (c :: k -> Constraint) (xs :: [k]) :: Constraint
type instance AllF _c '[]       = ()
type instance AllF  c (x ': xs) = (c x, All c xs)

class (AllF (All f) xss) => All2 f xss
instance (AllF (All f) xss) => All2 f xss

---
