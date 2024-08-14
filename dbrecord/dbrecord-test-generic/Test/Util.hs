{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Test.Util
  ( module Test.Util
  ) where

import Hedgehog
import Hedgehog.Extras
import DBRecord.Prelude
import DBRecord.Driver
import Data.Proxy
import Data.Kind
import Data.Typeable
import Control.Monad.Reader
import qualified Record as R
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control
import Control.Exception.Safe

-- TODO: Remove int deps
import DBRecord.Internal.DBTypes


newtype ShowableQ sc a = ShowableQ (Query sc a -> String, Query sc a)

runShowableQ :: ShowableQ sc a -> Query sc a
runShowableQ (ShowableQ (_, q)) = q

instance Show (ShowableQ sc a) where
  show (ShowableQ (showQ, q)) = showQ q

exprTripping :: forall a (sc :: Type) (sut :: Type -> Type) m env driver.
  ( Typeable a
  , Eq a
  , Show a
  , AutoConstExpr sc a (ToDBType (DB (SchemaDB sc)) a) (AutoCodec (DB (SchemaDB sc)) a)
  , MonadTest m
  , MonadReader env sut
  , HasSessionConfig env driver
  , Session driver
  , FromDBRow driver (R.Rec '[ '("col", a)])
  , DBRepr (DB (SchemaDB sc)) a
  , MonadUnliftIO sut
  , MonadBaseControl IO sut
  , HasQuery driver
  , ShowQuery driver
  , MonadCatch m
  ) => (forall x. sut x -> m x) -> Proxy sc -> a -> m ()
exprTripping toTestM _ v = trippingM v (\a -> do showQ <- toTestM getQueryShow
                                                 pure $ ShowableQ . (showQ,) $ selectExpr (#col .= constExpr a)) (toTestM . (fmap . fmap) (\r -> r.col) . runQuery @sc . runShowableQ)


ddlTripping :: forall (scc :: Type) (sut :: Type -> Type) m env driver.
  ( MonadTest m
  , MonadReader env sut
  , HasSessionConfig env driver
  , Session driver
  ) => (forall x. sut x -> m x) -> Proxy scc -> m ()
ddlTripping _toTestM _ = pure ()
