{-# LANGUAGE KindSignatures, DataKinds, CPP #-}
module DBRecord.Internal.Window where

import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Expr
import DBRecord.Internal.Order
import GHC.TypeLits
import Data.Binary
#if MIN_VERSION_base(4,10,0)
import Data.Semigroup (Semigroup (..))
import Data.Monoid (Monoid (..))
#else
import Data.Monoid (Monoid (..))
#endif

newtype Window (w :: Symbol) (sc :: [*]) = Window { getPartitions :: PQ.WindowPart }

newtype Partition (sc :: [*]) = Partition { getPartExprs :: [PQ.PrimExpr] }

#if MIN_VERSION_base(4,10,0)
instance Semigroup (Partition sc) where
  (Partition p1) <> (Partition p2) = Partition (p1 <> p2)
#endif

instance Monoid (Partition sc) where
  mempty = Partition []
#if MIN_VERSION_base(4,10,0)
#else
  (Partition p1) `mappend` (Partition p2) = Partition (p1 `mappend` p2)
#endif

  
partition :: Expr sc a -> Partition sc
partition = Partition . (:[]) . getExpr

window :: Partition sc -> Order sc -> Window w sc
window pe oe = Window (PQ.WindowPart (getPartExprs pe) (getOrder oe))

{-
-- We trust the binary input
instance Binary (Window w sc) where
  put = put . getPartitions
  get = Window <$> get

-}
