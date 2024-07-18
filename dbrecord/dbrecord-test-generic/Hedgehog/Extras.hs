{-# OPTIONS_HADDOCK not-home #-}
module Hedgehog.Extras (
  trippingM
  ) where

import Hedgehog.Internal.Property (MonadTest, Diff(..), success, failWith)
import Hedgehog.Internal.Show (valueDiff, mkValue, showPretty)
import Hedgehog.Internal.Source (HasCallStack, withFrozenCallStack)
import Control.Exception.Safe


-- | Similar to tripping, but with a monadic action.
trippingM ::
     (MonadTest m, Applicative f, Show b, Show (f a), Eq (f a), HasCallStack, MonadCatch m)
  => a
  -> (a -> m b)
  -> (b -> m (f a))
  -> m ()
trippingM x encode decode = do
  let
    mx =
      pure x

  i <- encode x `catchAny` \e -> withFrozenCallStack $
    failWith Nothing $ unlines [
    "━━━ Exception while encoding ━━━"
    , show e
    , "━━━ Original ━━━"
    , showPretty mx
    ]

  my <- decode i `catchAny` \e -> withFrozenCallStack $
    failWith Nothing $ unlines [
    "━━━ Exception while decoding ━━━"
    , show e
    , "━━━ Original ━━━"
    , showPretty mx
    , "━━━ Intermediate ━━━"
    , showPretty i
    ]
  if mx == my
    then success
    else
      case valueDiff <$> mkValue mx <*> mkValue my of
        Nothing ->
          withFrozenCallStack $
            failWith Nothing $ unlines [
                "━━━ Original ━━━"
              , showPretty mx
              , "━━━ Intermediate ━━━"
              , showPretty i
              , "━━━ Roundtrip ━━━"
              , showPretty my
              ]

        Just diff ->
          withFrozenCallStack $
            failWith
              (Just $
                Diff "━━━ " "- Original" ") (" "+ Roundtrip" " ━━━" diff) $
              unlines [
                  "━━━ Intermediate ━━━"
                , showPretty i
                ]
