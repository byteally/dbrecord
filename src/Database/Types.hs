{-# LANGUAGE DataKinds, TypeFamilyDependencies #-}
-- | 

module Database.Types where

type family Column (f :: * -> *) (a :: *) = r | r -> f a
