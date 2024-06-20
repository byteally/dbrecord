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

type family GenTyCon (rep :: Type -> Type) :: Symbol where
  GenTyCon (D1 ('MetaData tyName _ _ _) _) = tyName
  GenTyCon r                               = TypeError ('Text "GenTyCon expects only generic rep of type, but found " ':<>: 'ShowType r)

type family GetTypeName (t :: Type) :: Symbol where
  GetTypeName t              = GenTyCon (Rep t)

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
