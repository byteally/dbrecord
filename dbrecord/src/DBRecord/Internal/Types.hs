{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, TypeOperators, GADTs, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, CPP, GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies, UndecidableInstances, UndecidableSuperClasses, ScopedTypeVariables, FunctionalDependencies, AllowAmbiguousTypes, RankNTypes #-}
module DBRecord.Internal.Types where

import GHC.Generics
import GHC.TypeLits
import GHC.OverloadedLabels
import Data.Aeson
import qualified Data.Text as T
import Data.Kind
import Data.Typeable
import GHC.Exts


data DBTag (db :: *) (tab :: *) (v :: k)

newtype (f :: Symbol) ::: t = Field { getField :: t }
  deriving (Show, Eq, Generic, Num)

instance (fn ~ fn1, s ~ t) => IsLabel fn (s -> (fn1 ::: t)) where
#if __GLASGOW_HASKELL__ > 800
  fromLabel = Field
#else
  fromLabel _ = Field
#endif


valOf :: (s ::: t) -> t
valOf (Field v) = v

newtype JsonStr a = JsonStr { getJsonStr :: a }
newtype Json a = Json { getJson :: a }
               deriving (Show, Generic, FromJSON, ToJSON)

json :: (ToJSON a) => a -> Json a
json = Json

jsonStr :: (ToJSON a) => a -> JsonStr a
jsonStr = JsonStr

newtype CustomType a = CustomType a

data HList :: (k -> *) -> [k] -> * where
  Nil  :: HList f '[]
  (:&) :: f t -> HList f ts -> HList f (t ': ts)

infixr 7 :&

instance (Show (f x), Show (HList f xs)) => Show (HList f (x ': xs)) where
  show (x :& xs) = show x ++ ", " ++ show xs
  
instance Show (HList f '[]) where
  show Nil = "Done"

hnat :: (forall a. f a -> g a) -> HList f xs -> HList g xs
hnat f (a :& as) = f a :& hnat f as
hnat _ Nil       = Nil
  
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
  = DBInt4
  | DBInt8
  | DBInt2
  -- | DBFloat4 -- float(24)
  -- | DBFloat8 -- float(53)
  | DBFloat Nat
  | DBNumeric Nat Nat
  | DBChar Nat
  | DBVarchar (Either Max Nat)
  | DBBool
  | DBDate
  | DBTime Nat
  | DBTimetz Nat    
  | DBTimestamp Nat
  | DBTimestamptz Nat
  | DBInterval (Maybe Type) Nat
  | DBNullable DBTypeK
  | DBXml
  | DBJson    
  -- NOTE: Non SQL 92
  | DBBinary Nat                
  | DBVarbinary (Either Max Nat)
  -- NOTE: Non standard
  | DBText
  | DBCiText    
  | DBUuid
  | DBBit    Nat
  | DBVarbit Nat
  | DBJsonB
  | DBArray DBTypeK
  | DBCustomType Type DBTypeNameK

data DBTypeNameK = DBTypeName Symbol [TypeArgK] UDTypeMappings

data TypeArgK = SymArg Symbol
              | NatArg Nat
    
{-
instance (FromJSON a, Typeable a) => FromField (Json a) where
  fromField f dat = Json <$> fromJSONField f dat
-}

data UDTypeMappings = EnumType
                               (Maybe Symbol)      -- ^ Alias for typename
                               [(Symbol, Symbol)]  -- ^ Aliases for datacons
                               
                    | Composite (Maybe Symbol)     -- ^ Alias for typename
                                [(Symbol, Symbol)] -- ^ Alias for composite field names
                      
                    | EnumText [(Symbol, Symbol)]  -- ^ Aliases for datacons
                    
                    | Flat [(Symbol, Symbol)]      -- ^ Aliases for flattened field names
                    
                    | Sum (Maybe Symbol)           -- ^ Alias for tag column
                          [(Symbol, [(Symbol, Symbol)])] -- ^ Alias for flattened field names, in a particular con
                    -- | Json

newtype Interval = Interval T.Text
                 deriving (Show, Generic, FromJSON, ToJSON) --, FromField)

data TableTypes =
    UpdatableView
  | NonUpdatableView
  | BaseTable
  deriving (Show, Eq, Generic)

-- newtype Only a = Only { fromOnly :: a }
--                deriving (Eq, Ord, Read, Show, Typeable, Functor)
                        
{-
instance (ToJSON a, Typeable a) => ToField (Json a) where
  toField = toJSONField . getJson
-}

data family Sing (a :: k)

class SingI (a :: k) where
  sing :: Sing a

class SingE (a :: k) where
  type Demote a :: *
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

instance SingE (ttyp :: TableTypes) where
  type Demote ttyp = TableTypes
  fromSing SUpdatableView = UpdatableView
  fromSing SNonUpdatableView = NonUpdatableView
  fromSing SBaseTable = BaseTable

data instance Sing (t :: TableTypes) where
  SUpdatableView :: Sing 'UpdatableView
  SNonUpdatableView :: Sing 'NonUpdatableView
  SBaseTable :: Sing 'BaseTable

data instance Sing (t :: DBTypeK) where
  SDBInt4        :: Sing 'DBInt4
  SDBInt8        :: Sing 'DBInt8
  SDBInt2        :: Sing 'DBInt2
  SDBFloat       :: Sing n -> Sing ('DBFloat n)
  SDBNumeric     :: Sing n1 -> Sing n2 -> Sing ('DBNumeric n1 n2)
  SDBChar        :: Sing n -> Sing ('DBChar n)
  SDBVarchar     :: Sing n -> Sing ('DBVarchar n)
  SDBBool        :: Sing 'DBBool
  SDBDate        :: Sing 'DBDate
  SDBTime        :: Sing n -> Sing ('DBTime n)
  SDBTimetz      :: Sing n -> Sing ('DBTimetz n)
  SDBTimestamp   :: Sing n -> Sing ('DBTimestamp n)
  SDBTimestamptz :: Sing n -> Sing ('DBTimestamptz n)
  SDBInterval    :: Sing n1 -> Sing n2 -> Sing ('DBInterval n1 n2)
  SDBNullable    :: Sing a -> Sing ('DBNullable a)
  SDBXml         :: Sing 'DBXml
  SDBJson        :: Sing 'DBJson
  SDBBinary      :: Sing n -> Sing ('DBBinary n)
  SDBVarbinary   :: Sing n -> Sing ('DBVarbinary n)
  SDBText        :: Sing 'DBText
  SDBCiText      :: Sing 'DBCiText
  SDBUuid        :: Sing 'DBUuid
  SDBBit         :: Sing n -> Sing ('DBBit n)
  SDBVarbit      :: Sing n -> Sing ('DBVarbit n)
  SDBJsonB       :: Sing 'DBJsonB  
  SDBArray       :: Sing a -> Sing ('DBArray a)
  SDBCustomType  :: Sing t -> Sing dbt -> Sing ('DBCustomType t dbt)

data instance Sing (t :: DBTypeNameK) where
  SDBTypeName :: Sing s -> Sing args -> Sing udm -> Sing ('DBTypeName s args udm)

data instance Sing (t :: TypeArgK) where
  SSymArg :: Sing n -> Sing ('SymArg n)
  SNatArg :: Sing n -> Sing ('NatArg n)

data instance Sing (t :: UDTypeMappings) where
  SEnumType :: Sing s -> Sing ss -> Sing ('EnumType s ss)
  SComposite :: Sing s -> Sing tss -> Sing ('Composite s tss)
  SEnumText :: Sing ss -> Sing ('EnumText ss)
  SFlat :: Sing tss -> Sing ('Flat tss)  
  SSum :: Sing tagn -> Sing tsss -> Sing ('Sum tagn tsss)

data instance Sing (m :: Max) where
  SMax :: Sing 'Max

instance (SingI n) => SingI ('SymArg n) where
  sing = SSymArg sing

instance (SingI n) => SingI ('NatArg n) where
  sing = SNatArg sing

instance SingI 'DBInt4 where
  sing = SDBInt4

instance SingI 'DBInt8 where
  sing = SDBInt8

instance SingI 'DBInt2 where
  sing = SDBInt2

instance (SingI n) => SingI ('DBFloat n) where
  sing = SDBFloat sing

instance (SingI n1, SingI n2) => SingI ('DBNumeric n1 n2) where
  sing = SDBNumeric sing sing

instance (SingI n) => SingI ('DBChar n) where
  sing = SDBChar sing

instance (SingI n) => SingI ('DBVarchar n) where
  sing = SDBVarchar sing

instance SingI ('DBBool) where
  sing = SDBBool

instance SingI ('DBDate) where
  sing = SDBDate

instance (SingI n) => SingI ('DBTime n) where
  sing = SDBTime sing

instance (SingI n) => SingI ('DBTimetz n) where
  sing = SDBTimetz sing

instance (SingI n) => SingI ('DBTimestamp n) where
  sing = SDBTimestamp sing

instance (SingI n) => SingI ('DBTimestamptz n) where
  sing = SDBTimestamptz sing

instance (SingI n1, SingI n2) => SingI ('DBInterval n1 n2) where
  sing = SDBInterval sing sing

instance (SingI n) => SingI ('DBNullable n) where
  sing = SDBNullable sing

instance SingI 'DBXml where
  sing = SDBXml

instance SingI 'DBJson where
  sing = SDBJson

instance (SingI n) => SingI ('DBBinary n) where
  sing = SDBBinary sing

instance (SingI n) => SingI ('DBVarbinary n) where
  sing = SDBVarbinary sing

instance SingI 'DBText where
  sing = SDBText

instance SingI 'DBCiText where
  sing = SDBCiText

instance SingI 'DBUuid where
  sing = SDBUuid

instance (SingI n) => SingI ('DBBit n) where
  sing = SDBBit sing

instance (SingI n) => SingI ('DBVarbit n) where
  sing = SDBVarbit sing

instance SingI ('DBJsonB) where
  sing = SDBJsonB

instance (SingI n) => SingI ('DBArray n) where
  sing = SDBArray sing

instance ( SingI t, SingI dbt
         ) => SingI ('DBCustomType t dbt) where
  sing = SDBCustomType sing sing

instance (SingI s, SingI args, SingI udm) => SingI ('DBTypeName s args udm) where
  sing = SDBTypeName sing sing sing

instance (SingI s, SingI ss) => SingI ('EnumType s ss) where
  sing = SEnumType sing sing

instance (SingI s, SingI tss) => SingI ('Composite s tss) where
  sing = SComposite sing sing

instance (SingI tss) => SingI ('Flat tss) where
  sing = SFlat sing

instance (SingI ss) => SingI ('EnumText ss) where
  sing = SEnumText sing

instance (SingI tsss, SingI tagn) => SingI ('Sum tagn tsss) where
  sing = SSum sing sing

instance SingI 'Max where
  sing = SMax

instance SingE (t :: Max) where
  type Demote (t :: Max) = Max
  fromSing SMax = Max

--

class (AllF f xs) => All (f :: k -> Constraint) (xs :: [k])
instance (AllF f xs) => All f xs

type family AllF (c :: k -> Constraint) (xs :: [k]) :: Constraint
type instance AllF _c '[]       = ()
type instance AllF  c (x ': xs) = (c x, All c xs)

class (AllF (All f) xss) => All2 f xss
instance (AllF (All f) xss) => All2 f xss

---

-- NOTE: from https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst

class HasField x r a | x r -> a where
  -- | Function to get and set a field in a record.
  hasField :: r -> (a -> r, a)
