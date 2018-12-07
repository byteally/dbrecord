{-# LANGUAGE RankNTypes #-}
module DBRecord.Internal.Lens where

import Data.Functor.Const
import Data.Functor.Identity
import qualified Control.Monad.State as State
import qualified Data.List as L
import GHC.Stack
import Data.Coerce
import Data.Monoid

infixr 4 %~, .~, ^.
infix  4 .=, %=
infixl 8 ^?

type Traversal s t a b = forall f. (Applicative f) => (a -> f b) -> s -> f t
type Traversal' s a    = Traversal s s a a

{-# INLINE ixBy #-}
ixBy :: (Eq v) => v -> (k -> v) -> Traversal' [k] k
ixBy k pf f xs0 = go xs0 k where
    go [] _ = pure []
    go (a:as) k | pf a == k     = (:as) <$> f a
                | otherwise    = (a:) <$> (go as k)
  
type ASetter s t a b = (a -> Identity b) -> s -> Identity t


{-# INLINE (^?) #-}
(^?) :: s -> Getting (First a) s a -> Maybe a
(^?) s g = getFirst . getConst . g (Const . First . Just) $ s

(%~) :: ASetter s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

(.~) :: ASetter s t a b -> b -> s -> t
(.~) = set
{-# INLINE (.~) #-}

(%=) :: State.MonadState s m => ASetter s s a b -> (a -> b) -> m ()
l %= f = State.modify (l %~ f)
{-# INLINE (%=) #-}

(.=) :: State.MonadState s m => ASetter s s a b -> b -> m ()
l .= b = State.modify (l .~ b)
{-# INLINE (.=) #-}

set :: ASetter s t a b -> b -> s -> t
set l b = runIdentity . l (\_ -> Identity b)
{-# INLINE set #-}

over :: ASetter s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)
{-# INLINE over #-}

type Getting r s a = (a -> Const r a) -> s -> Const r s

(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}

use :: State.MonadState s m => Getting a s a -> m a
use l = State.gets (getConst . l Const)

preuse :: State.MonadState s m => Getting (First a) s a -> m (Maybe a)
preuse l = State.gets (getFirst . getConst . l (Const . First . Just))

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

coerceL :: (Coercible a s) => Lens' a s
coerceL k t = fmap coerce (k (coerce t))

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a    = Lens s s a a

unsafeFind :: (Eq v, Show v, Show a, HasCallStack) => v -> (a -> v) -> Lens' [a] a
unsafeFind eqv f = lens gt st
  where gt as = case L.find (\ti -> f ti == eqv) as of
            Just ti -> ti
            _       -> error $ "Panic: Invariant violated @unsafeFind in: " ++ show as ++ "\nwhile looking for value: " ++ show eqv
        st as b =
          let (done', t) = L.mapAccumL (\done x -> case eqv == f x of
                                  True  -> (True, b)
                                  False -> (done, x)
                                     ) False as
          in  case done' of
                False -> error $ "Panic: Invariant violated @unsafeFind in: " ++ show as ++ "\nwhile looking for value: " ++ show eqv
                True  -> t
                              

