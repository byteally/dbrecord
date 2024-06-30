{-# LANGUAGE TypeFamilies, KindSignatures, TypeOperators, DataKinds, PolyKinds, UndecidableInstances, MultiParamTypeClasses, UndecidableSuperClasses, FlexibleInstances, FunctionalDependencies, FlexibleContexts, ScopedTypeVariables, TypeFamilyDependencies, RankNTypes, MultiWayIf #-}
module DBRecord.Internal.Common where

import GHC.Generics
import GHC.TypeLits
import GHC.Exts
import Data.Kind
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Record

type family GenTyCon (rep :: Type -> Type) :: Symbol where
  GenTyCon (D1 ('MetaData tyName _ _ _) _) = tyName
  GenTyCon r                               = TypeError ('Text "GenTyCon expects only generic rep of type, but found " ':<>: 'ShowType r)

type family GetTypeName (t :: Type) :: Symbol where
  GetTypeName t              = GenTyCon (Rep t)

type family GGetFieldsOrEmpty (t :: Type) (rep :: Type -> Type) :: [(Symbol, Type)] where
  GGetFieldsOrEmpty t (D1 _ f) = GGetFieldsOrEmpty t f
  GGetFieldsOrEmpty t (f :+: g) = '[]
  GGetFieldsOrEmpty t _ = GGetFields t (Rep t)

type family ValidatePfxConName (ty :: Type) (k :: Symbol) (pfx :: Char) (rep :: Type -> Type) (unconsedConName :: Maybe (Char, Symbol)) :: Constraint where
  ValidatePfxConName _ _ _ _ 'Nothing = TypeError ('Text "Invalid Constructor Name: " ':<>: 'Text " for type " ':<>: 'Text "")
  ValidatePfxConName ty k pfx rep ('Just '(pfx, cn)) = ErrorOnFalse (ConNameMatch ty cn rep) ('Text "[DBR-123] " ':<>: 'ShowType cn ':<>: 'Text " is not a valid constructor name for type " ':<>: 'ShowType ty)
  ValidatePfxConName ty k pfx rep ('Just '(pfx', cn)) = TypeError ('Text "Invalid Constructor Name: " ':<>: 'Text " for type " ':<>: 'Text "")

type ValidateConName :: Type -> Symbol -> (Type -> Type) -> Constraint
type ValidateConName ty cn rep = ErrorOnFalse (ConNameMatch ty cn rep) ('Text "[DBR-123] " ':<>: 'ShowType cn ':<>: 'Text " is not a valid constructor name for type " ':<>: 'ShowType ty)

type family ConNameMatch (ty :: Type) (cn :: Symbol) (rep :: Type -> Type) :: Bool where
  ConNameMatch ty cn (D1 _ f) = ConNameMatch ty cn f
  ConNameMatch ty cn (f :+: g) = ConNameMatch1 ty cn g (ConNameMatch ty cn f)
  ConNameMatch _ cn (C1 ('MetaCons cn _ _) _) = 'True
  ConNameMatch _ cn (C1 ('MetaCons cn' _ _) _) = 'False

type family ConNameMatch1 (ty :: Type) (cn :: Symbol) (rep :: Type -> Type) (mat :: Bool) :: Bool where
  ConNameMatch1 ty cn krep 'False = ConNameMatch ty cn krep
  ConNameMatch1 _ _ _ 'True = 'True

type family ErrorOnFalse (b :: Bool) (emsg :: ErrorMessage) :: Constraint where
  ErrorOnFalse 'True _ = ()
  ErrorOnFalse 'False emsg = TypeError emsg


data T1 (t :: Type)
type family Break (c :: Constraint) (rep :: Type -> Type) :: Constraint where
  Break _ T1 = ((), ())
  Break _ _  = ()

data T0
type family Break0 (c :: Constraint) (rep :: Type) :: Constraint where
  Break0 _ T0 = ((), ())
  Break0 _ _  = ()

type family NoGeneric t where
  NoGeneric x = TypeError ('Text "[DBR-00100] No instance for " ':<>: 'ShowType (Generic x))

defHSNameToDBName :: Text -> Text
defHSNameToDBName = LT.toStrict . LTB.toLazyText .  T.foldl'
  (\b c ->
     if
       | c == '\'' -> b <> LTB.singleton '_' <> LTB.singleton c
       | isUpper c -> if b == mempty
                      then LTB.singleton (toLower c)
                      else b <> LTB.singleton '_' <> LTB.singleton (toLower c)
       | otherwise -> b <> LTB.singleton c
  ) mempty

doubleQuote :: T.Text -> T.Text
doubleQuote = quoteBy '"' (Just '"')

quoteBy :: Char -> Maybe Char -> T.Text -> T.Text
quoteBy ch esc s = T.pack $ ch : go esc (T.unpack s) ++ (ch:[])
  where
    go Nothing s'           = s'
    go (Just _) ""          = ""
    go (Just esch) (ch':xs)
      | ch' == esch          = esch : ch': go esc xs
    go esc' (x:xs)          = x : go esc' xs
