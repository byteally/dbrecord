{-# LANGUAGE RankNTypes #-}
module DBRecord.Internal.Lens where

import Data.Functor.Const
import Data.Functor.Identity
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import qualified Data.List as L
import GHC.Stack

infixr 4 %~, .~, ^.
infix  4 .=, %=

type ASetter s t a b = (a -> Identity b) -> s -> Identity t

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

view :: State.MonadState s m => Getting a s a -> m a
view l = State.gets (getConst . l Const)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

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
                              
