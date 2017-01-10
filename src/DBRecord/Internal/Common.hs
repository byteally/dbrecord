{-# LANGUAGE TypeFamilies, KindSignatures, TypeOperators, DataKinds, PolyKinds, UndecidableInstances, MultiParamTypeClasses, UndecidableSuperClasses, FlexibleInstances, FunctionalDependencies, FlexibleContexts, ScopedTypeVariables #-}
module DBRecord.Internal.Common where

import Data.Type.Equality
import Data.Proxy
import Data.Functor.Identity
import DBRecord.Internal.Types
import GHC.Generics
import GHC.TypeLits
import Data.UUID.Types
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

type family GetTypeName (t :: *) :: Symbol where
  GetTypeName t              = GenTyCon (Rep t)

type family IsNewType (rep :: * -> *) :: Bool where
  IsNewType (D1 ('MetaData _ _ _ isNew) _) = isNew

infixr 5 :++
type family (:++) (as :: [k]) (bs :: [k]) :: [k] where
  '[] :++ bs       = bs
  (a ': as) :++ bs = a ': (as :++ bs)

type family Elem (xs :: [k]) (v :: k) :: Bool where
  Elem (x ': xs) x = 'True
  Elem (x ': xs) y = Elem xs y
  Elem '[]       x = 'False

type family FindField (xs :: [*]) (fn :: Symbol) :: (Maybe *) where
  FindField ((fn ::: t) ': xs) fn  = 'Just t
  FindField ((fn' ::: t) ': xs) fn = FindField xs fn
  FindField '[] fn                 = 'Nothing

type family FindFieldOrErr (xs :: [*]) (fn :: Symbol) (msg :: ErrorMessage) :: * where
  FindFieldOrErr ((fn ::: t) ': xs) fn msg  = t
  FindFieldOrErr ((fn' ::: t) ': xs) fn msg = FindFieldOrErr xs fn msg
  FindFieldOrErr '[] fn msg                 = TypeError msg  

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

type family Note (note :: k) (may :: Maybe *) :: Either k * where
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