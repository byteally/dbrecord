{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Util
  ( module Test.Util
  ) where

import Hedgehog
import Hedgehog.Extras
import DBRecord.Prelude
import DBRecord.Driver
import DBRecord.Query.DDL
import Data.Proxy
import Data.Kind
import Data.Typeable
import Control.Monad.Reader
import qualified Record as R
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control
import Control.Exception.Safe
import GHC.TypeLits
import qualified Data.Text as T

-- TODO: Remove int deps
import DBRecord.Internal.DBTypes
import DBRecord.Internal.Schema
import DBRecord.Internal.Types


newtype ShowableQ sc a = ShowableQ (Query sc a -> T.Text, Query sc a)

runShowableQ :: ShowableQ sc a -> Query sc a
runShowableQ (ShowableQ (_, q)) = q

instance Show (ShowableQ sc a) where
  show (ShowableQ (showQ, q)) = T.unpack (showQ q)

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


ddlEvolution :: forall (scc :: Nat -> Type) (current :: Nat) (sut :: Type -> Type) m env driver.
  ( MonadTest m
  , MonadReader env sut
  , HasSessionConfig env driver
  , Session driver
  , EvolveSchema scc current 1 (CmpNat 1 current)
  ) => (forall x. sut x -> m x) -> Proxy (scc current) -> m ()
ddlEvolution _toTestM _ = do
  let (_up, _down) = evolveSchema @scc @current
  pure ()

evolveSchema :: forall (scc :: Nat -> Type) (current :: Nat). (EvolveSchema scc current 1 (CmpNat 1 current)) => (UpDDLGraph (scc current), DownDDLGraph (scc current))
evolveSchema = evolveSchema' (Proxy @'(1, CmpNat 1 current)) Proxy Nothing

class EvolveSchema (scc :: Nat -> Type) (current :: Nat) (hist :: Nat) (cmp :: Ordering) where
  evolveSchema' :: Proxy '(hist, cmp) -> Proxy (scc current) -> Maybe (UpDDLGraph (scc (hist-1)), DownDDLGraph (scc (hist-1))) -> (UpDDLGraph (scc current), DownDDLGraph (scc current))

instance ( EvolveSchema scc curr (hist + 1) (CmpNat (hist + 1) curr)
         , SchemaCatalog (scc hist)
         , SingI (Tables (scc hist))
         , SingI (Types (scc hist))
         , AllMigCxt (SchemaOf (scc hist)) (Tables (scc hist))
         , AllMigCxt (SchemaOf (scc hist)) (Types (scc hist))
         , hist ~ ((hist + 1) -1)
         ) => EvolveSchema scc curr hist 'LT where
  evolveSchema' _ pscc prevHistMig =
    let prev = getDDLForSchema (Proxy @(scc hist)) (undefined prevHistMig)
    in evolveSchema' (Proxy @'(hist + 1, CmpNat (hist + 1) curr)) pscc (Just prev)

instance ( SchemaCatalog (scc hist)
         , SingI (Tables (scc hist))
         , SingI (Types (scc hist))
         , AllMigCxt (SchemaOf (scc hist)) (Tables (scc hist))
         , AllMigCxt (SchemaOf (scc hist)) (Types (scc hist))
         , hist ~ curr
         ) => EvolveSchema scc curr hist 'EQ where
  evolveSchema' _ _ prevHistMig = getDDLForSchema (Proxy @(scc hist)) (undefined prevHistMig)

instance (TypeError ('Text "Panic: Requested current version: " ':<>: 'ShowType curr ':<>: 'Text " is less than historical version: " ':<>: 'ShowType hist)) => EvolveSchema scc curr hist 'GT where
  evolveSchema' = error "Panic: Unreachable code"
