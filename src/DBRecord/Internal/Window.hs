{-# LANGUAGE KindSignatures, DataKinds #-}
module DBRecord.Internal.Window where

import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Expr
import DBRecord.Internal.Order
import GHC.TypeLits
import Data.Binary
import Data.Monoid

newtype Window (w :: Symbol) (sc :: [*]) = Window { getPartitions :: PQ.WindowPart }

newtype Partition (sc :: [*]) = Partition { getPartExprs :: [PQ.PrimExpr] }

instance Monoid (Partition sc) where
  (Partition p1) `mappend` (Partition p2) = Partition (p1 <> p2)
  mempty = Partition []
  
partition :: Expr sc a -> Partition sc
partition = Partition . (:[]) . getExpr

window :: Partition sc -> Order sc -> Window w sc
window pe oe = Window (PQ.WindowPart (getPartExprs pe) (getOrder oe))

-- We trust the binary input
instance Binary (Window w sc) where
  put = put . getPartitions
  get = Window <$> get

