{-# LANGUAGE TypeFamilies, KindSignatures, TypeOperators, DataKinds, PolyKinds, UndecidableInstances, MultiParamTypeClasses, UndecidableSuperClasses, FlexibleInstances, FunctionalDependencies, FlexibleContexts, ScopedTypeVariables, TypeFamilyDependencies, RankNTypes #-}
module DBRecord.Internal.Common where

import Data.Type.Equality
import Data.Proxy
import DBRecord.Internal.Types
import GHC.Generics
import GHC.TypeLits
import Data.UUID (UUID)
import Data.Time.LocalTime
import Data.ByteString (ByteString)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.CaseInsensitive  (CI)
import Data.Int
import Data.Vector (Vector)
import Data.Aeson
import Data.Text
import GHC.Exts
import Data.Functor.Identity
import Data.Kind

type family GenTyCon (rep :: * -> *) :: Symbol where
  GenTyCon (D1 ('MetaData tyName _ _ _) _) = tyName
  GenTyCon r                               = TypeError ('Text "GenTyCon expects only generic rep of type, but found " ':<>: 'ShowType r)

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

type family (a :: Bool) && (b :: Bool) :: Bool where
  'True && 'True = 'True
  a     && b     = 'False

data AdtK = EnumTy | SumTy Bool | ProdTy Bool

-- NOTE: l should be <= r 
type family Range (l :: Nat) (r :: Nat) :: [Nat] where
  Range l l = '[]
  Range l r = l ': Range (l + 1) r

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

type family GetTypeName (t :: *) :: Symbol where
  GetTypeName t              = GenTyCon (Rep t)

type family IsNewType (rep :: * -> *) :: Bool where
  IsNewType (D1 ('MetaData _ _ _ isNew) _) = isNew

type family NewtypeRep t where
  NewtypeRep t = NewtypeRep' (Rep t)

type family NewtypeRep' t :: Maybe Type where
  NewtypeRep' (D1 ('MetaData _ _ _ 'False) _) = 'Nothing
  NewtypeRep' (D1 _ (C1 _ (S1 _ (K1 _ a))))   = 'Just a

infixr 5 :++
type family (:++) (as :: [k]) (bs :: [k]) :: [k] where
  '[] :++ bs       = bs
  (a ': as) :++ bs = a ': (as :++ bs)

type family TypesOf (xs :: [*]) :: [*] where
  TypesOf (x ': xs) = TypeOf x ': TypesOf xs
  TypesOf '[]       = '[]

type family TypeOf (x :: *) :: * where
  TypeOf (fld ::: t) = t

type family FieldsOf (xs :: [*]) :: [Symbol] where
  FieldsOf (t ': ts) = FieldOf t ': FieldsOf ts
  FieldsOf '[]       = '[]

type family FieldOf (x :: *) :: Symbol where
  FieldOf (fld ::: t) = fld

type family Elem (xs :: [k]) (v :: k) :: Bool where
  Elem (x ': xs) x = 'True
  Elem (x ': xs) y = Elem xs y
  Elem '[]       x = 'False

type family Find (xs :: [k]) (v :: k) :: Maybe k where
  Find (x ': xs) x = 'Just x
  Find (x ': xs) y = Find xs y
  Find '[]       x = 'Nothing

type family FindAlias (xs :: [(Symbol, Symbol)]) (fn :: Symbol) :: (Maybe Symbol) where
  FindAlias ('(fn, alFn) ': fns) fn  = 'Just alFn
  FindAlias (_ ': fns) fn            = FindAlias fns fn
  FindAlias '[] fn                   = 'Nothing

type family FindField (xs :: [*]) (fn :: Symbol) :: (Maybe *) where
  FindField ((fn ::: t) ': xs) fn  = 'Just t
  FindField ((fn' ::: t) ': xs) fn = FindField xs fn
  FindField '[] fn                 = 'Nothing

type family FindFieldOrErr (xs :: [*]) (fn :: Symbol) (msg :: ErrorMessage) :: * where
  FindFieldOrErr ((fn ::: t) ': xs) fn msg  = t
  FindFieldOrErr ((fn' ::: t) ': xs) fn msg = FindFieldOrErr xs fn msg
  FindFieldOrErr '[] fn msg                 = TypeError msg

type family FindFields (xs :: [*]) (fns :: [Symbol]) :: [Either Symbol *] where
  FindFields xs (fn ': fns) = Note fn (FMapMaybe ((:::) fn) (FindField xs fn)) ': FindFields xs fns
  FindFields _ '[]          = '[]

type family ElemField (xs :: [*]) (fn :: Symbol) :: Bool where
  ElemField ((fn ::: t) ': xs) fn  = 'True
  ElemField ((fn' ::: t) ': xs) fn = ElemField xs fn
  ElemField '[] fn                 = 'False

type family AssertCxt (c :: Bool) (a :: ErrorMessage) :: Constraint where
  AssertCxt 'True msg  = ()
  AssertCxt 'False msg = TypeError msg

type family UnifyField (flds :: [*]) (fn :: Symbol) (match :: *) (nfMsg :: ErrorMessage) :: Constraint where
  UnifyField ((fn ::: ft') ': fs) fn ft nfMsg  = (ft ~ ft')
  UnifyField ((fn' ::: ft') ': fs) fn ft nfMsg = UnifyField fs fn ft nfMsg
  UnifyField '[] _ _ nfMsg                     = TypeError nfMsg

type family Concat (xss :: [[k]]) :: [k] where
  Concat (xs ': xss) = xs   -- TODO:
  Concat '[]         = '[]

type family Note (note :: k) (may :: Maybe k1) :: Either k k1 where
  Note _ ('Just v)   = 'Right v
  Note note 'Nothing = 'Left note

type family SeqEither (eithers :: [Either k k1]) :: Either k [k1] where
  SeqEither ('Left l ': _)   = 'Left l
  SeqEither ('Right r ': es) = SeqEither' (SeqEither es) r
  SeqEither '[]              = 'Right '[]

type family SeqEither' (res :: Either k [k1]) (r :: k1) :: Either k [k1] where
  SeqEither' ('Left l) _   = 'Left l
  SeqEither' ('Right rs) r = 'Right (r ': rs)

type family MkFun (tys :: [*]) :: * where
  MkFun (t ': '[]) = t 
  MkFun (t ': ts)  = t -> MkFun ts

type family UnifyOrErr (res :: Either ErrorMessage [*]) (v :: *) :: Constraint where
  UnifyOrErr ('Right lhs) rhs = (MkFun lhs) ~ rhs
  UnifyOrErr ('Left err) _    = TypeError err

type ColNotFoundMsg (col :: Symbol) (tab :: *) = ('Text "column " ':<>: ('ShowType col) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab))  

type family PartialJust (may :: Maybe k) :: k where
  PartialJust ('Just m) = m

type family FMapMaybe (fn :: k -> *) (may :: Maybe k) where
  FMapMaybe fn ('Just v) = 'Just (fn v)
  FMapMaybe _ 'Nothing   = 'Nothing

type family FromRights (xs :: [Either k k1]) :: [k1] where
  FromRights (x ': xs) = FromRight x ': FromRights xs
  FromRights '[]       = '[]

type family FromRight (x :: Either k k1) where
  FromRight ('Right t) = t

type family FromLeft (x :: Either k k1) where
  FromLeft ('Left t) = t

type family FromJust (x :: Maybe k) where
  FromJust ('Just t) = t

type family If (c :: Bool) (t :: k) (f :: k) :: k where
  If 'True t f  = t
  If 'False t f = f


data T1 (t :: *)
type family Break (c :: Constraint) (rep :: Type -> Type) :: Constraint where
  Break _ T1 = ((), ())
  Break _ _  = ()

data T0
type family Break0 (c :: Constraint) (rep :: Type) :: Constraint where
  Break0 _ T0 = ((), ())
  Break0 _ _  = ()  

type family NoGeneric t where
  NoGeneric x = TypeError ('Text "No instance for " ':<>: 'ShowType (Generic x))  

class (t ~ HListToTuple (TupleToHList t)) => ToHList t where
  toHList :: t -> (forall a. a -> f a) -> HList f (TupleToHList t)

instance ToHList () where
  toHList () _ = Nil

instance ToHList (Identity v) where
  toHList (Identity v) lift = lift v :& Nil

instance ToHList (x1, x2) where
  toHList (x1, x2) lift = lift x1 :& lift x2 :& Nil

instance ToHList (x1, x2, x3) where
  toHList (x1, x2, x3) lift = lift x1 :& lift x2 :& lift x3 :& Nil  

instance ToHList (x1, x2, x3, x4) where
  toHList (x1, x2, x3, x4) lift =
    lift x1 :& lift x2 :& lift x3 :& lift x4 :& Nil  

instance ToHList (x1, x2, x3, x4, x5) where
  toHList (x1, x2, x3, x4, x5) lift =
    lift x1 :& lift x2 :& lift x3 :& lift x4 :& lift x5 :& Nil  

instance ToHList (x1, x2, x3, x4, x5, x6) where
  toHList (x1, x2, x3, x4, x5, x6) lift =
    lift x1 :& lift x2 :& lift x3 :& lift x4 :& lift x5 :& lift x6 :& Nil

instance ToHList (x1, x2, x3, x4, x5, x6, x7) where
  toHList (x1, x2, x3, x4, x5, x6, x7) lift =
    lift x1 :& lift x2 :& lift x3 :& lift x4 :& lift x5 :& lift x6 :& lift x7 :& Nil
 
instance ToHList (x1, x2, x3, x4, x5, x6, x7, x8) where
  toHList (x1, x2, x3, x4, x5, x6, x7, x8) lift =
    lift x1 :& lift x2 :& lift x3 :& lift x4 :& lift x5 :& lift x6 :& lift x7 :& lift x8 :& Nil    

instance ToHList (x1, x2, x3, x4, x5, x6, x7, x8, x9) where
  toHList (x1, x2, x3, x4, x5, x6, x7, x8, x9) lift =
    lift x1 :& lift x2 :& lift x3 :& lift x4 :& lift x5 :& lift x6 :& lift x7 :& lift x8 :& lift x9 :& Nil    

instance ToHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) where
  toHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) lift =
    lift x1 :& lift x2 :& lift x3 :& lift x4 :& lift x5 :& lift x6 :& lift x7 :& lift x8 :& lift x9 :& lift x10 :& Nil

instance ToHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) where
  toHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) lift =
    lift x1 :& lift x2 :& lift x3 :& lift x4 :& lift x5 :& lift x6 :& lift x7 :& lift x8 :& lift x9 :& lift x10 :& lift x11 :& Nil

instance ToHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) where
  toHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) lift =
    lift x1 :& lift x2 :& lift x3 :& lift x4 :& lift x5 :& lift x6 :& lift x7 :& lift x8 :& lift x9 :& lift x10 :& lift x11 :& lift x12 :& Nil

instance ToHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) where
  toHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) lift =
    lift x1 :& lift x2 :& lift x3 :& lift x4 :& lift x5 :& lift x6 :& lift x7 :& lift x8 :& lift x9 :& lift x10 :& lift x11 :& lift x12 :& lift x13 :& Nil

instance ToHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) where
  toHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) lift =
    lift x1 :& lift x2 :& lift x3 :& lift x4 :& lift x5 :& lift x6 :& lift x7 :& lift x8 :& lift x9 :& lift x10 :& lift x11 :& lift x12 :& lift x13 :& lift x14 :& Nil

instance ToHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) where
  toHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) lift =
    lift x1 :& lift x2 :& lift x3 :& lift x4 :& lift x5 :& lift x6 :& lift x7 :& lift x8 :& lift x9 :& lift x10 :& lift x11 :& lift x12 :& lift x13 :& lift x14 :& lift x15 :& Nil    

type family HListToTuple (xs :: [*]) = (ret :: *) | ret -> xs where
  HListToTuple '[]  = ()
  HListToTuple '[x] = Identity x
  HListToTuple '[x1, x2] = (x1, x2)
  HListToTuple '[x1, x2, x3] = (x1, x2, x3)
  HListToTuple '[x1, x2, x3, x4] = (x1, x2, x3, x4)
  HListToTuple '[x1, x2, x3, x4, x5] = (x1, x2, x3, x4, x5)
  HListToTuple '[x1, x2, x3, x4, x5, x6] = (x1, x2, x3, x4, x5, x6)
  HListToTuple '[x1, x2, x3, x4, x5, x6, x7] = (x1, x2, x3, x4, x5, x6, x7)
  HListToTuple '[x1, x2, x3, x4, x5, x6, x7, x8] = (x1, x2, x3, x4, x5, x6, x7, x8)
  HListToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9] = (x1, x2, x3, x4, x5, x6, x7, x8, x9)
  HListToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  HListToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  HListToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
  HListToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)
  HListToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)
  HListToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)

type family TupleToHList (t :: *) = (res :: [*]) | res -> t where
  TupleToHList ()           = '[]
  TupleToHList (Identity x) = '[x]
  TupleToHList (x1, x2) = '[x1, x2]
  TupleToHList (x1, x2, x3) = '[x1, x2, x3]
  TupleToHList (x1, x2, x3, x4) = '[x1, x2, x3, x4]
  TupleToHList (x1, x2, x3, x4, x5) = '[x1, x2, x3, x4, x5]
  TupleToHList (x1, x2, x3, x4, x5, x6) = '[x1, x2, x3, x4, x5, x6]
  TupleToHList (x1, x2, x3, x4, x5, x6, x7) = '[x1, x2, x3, x4, x5, x6, x7]
  TupleToHList (x1, x2, x3, x4, x5, x6, x7, x8) = '[x1, x2, x3, x4, x5, x6, x7, x8]
  TupleToHList (x1, x2, x3, x4, x5, x6, x7, x8, x9) = '[x1, x2, x3, x4, x5, x6, x7, x8, x9]
  TupleToHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10]
  TupleToHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11]
  TupleToHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12]
  TupleToHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13]
  TupleToHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14]
  TupleToHList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15]

type family FilterNonDefaults (xs :: [*]) (defs :: [Symbol]) :: [*] where
  FilterNonDefaults '[] _ = '[]
  FilterNonDefaults ((x ::: t)': xs) defs = FilterNonDefaults' (Elem defs x) (x ::: t) xs defs
 
type family FilterNonDefaults' (isDef :: Bool) (c :: *) (xs :: [*]) (defs :: [Symbol]) :: [*] where
  FilterNonDefaults' 'True _ xs defs = FilterNonDefaults xs defs
  FilterNonDefaults' 'False x xs defs = x ': FilterNonDefaults xs defs
  
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

type family ApplyF (f :: k -> *) (xs :: [k]) where
  ApplyF f (x ': xs) = f x ': ApplyF f xs
  ApplyF f '[]       = '[]

type family ExtractF (f :: k -> *) (xs :: [*]) :: [k] where
  ExtractF f (f x ': xs) = x ': ExtractF f xs
  ExtractF f '[]         = '[]

-- type family ToTableFields (xs :: [(Symbol, 

{-

type family GenTabFields (db :: *) (rep :: * -> *) :: [*] where
  GenTabFields db (D1 i f)  = GenTabFields f
  GenTabFields db (f :+: g) = TypeError ('Text "Table cannot be a sum type")
  GenTabFields db (C1 ('MetaCons cn _ 'False) _) = TypeError ('Text "The constructor " ':<>: 'ShowType cn ':<>: 'Text " does not have named fields")
  GenTabFields db (C1 i c) = GenTabFields c
  GenTabFields db (f :*: g) = GenTabFields f :++ GenTabFields g
  GenTabFields db (S1 ('MetaSel ('Just sn) _ _ _) (K1 i f)) = '[sn ::: GetDBTypeRep db (Rep f)]

type family GetTypeName (t :: *) :: Symbol where
  GetTypeName t              = GenTyCon (Rep t)

data TypeKind = EnumT
              | NewtypeT
              | ProductT
              | SumT
              deriving Show

type family GetTypeKind (rep :: * -> *) :: TypeKind where
  GetTypeKind (D1 ('MetaData _ _ _ isNew) _) = 'NewtypeT
  GetTypeKind (D1 _ (C1 _ (_ :*: _)))        = 'ProductT  
  GetTypeKind (D1 _ (C1 _ (S1 _ _)))        = 'ProductT
  GetTypeKind (D1 _ cs)                      = IsEnumOrSum cs

type family IsEnumOrSum (rep :: * -> *) :: TypeKind where
  IsEnumOrSum (C1 _ U1 :+: c) = IsEnumOrSum c
  IsEnumOrSum (C1 _ _  :+: c) = 'SumT
  IsEnumOrSum (C1 _ U1)       = 'NewtypeT


newtype Defs tab a = Defs [a]

[] :: Defs tab (Def tab)
[#foo 1, #bar 2]
-}


