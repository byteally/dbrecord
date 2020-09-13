{-# LANGUAGE KindSignatures, DataKinds, CPP #-}
module DBRecord.Internal.Window where

import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Expr
import DBRecord.Internal.Order
import GHC.TypeLits

newtype Window (w :: Symbol) (sc :: *) (scopes :: [*]) = Window { getPartitions :: PQ.WindowPart }

newtype Partition sc (scopes :: [*]) = Partition { getPartExprs :: [PQ.PrimExpr] }

#if MIN_VERSION_base(4,10,0)
instance Semigroup (Partition sc scopes) where
  (Partition p1) <> (Partition p2) = Partition (p1 <> p2)
#endif

instance Monoid (Partition sc scopes) where
  mempty = Partition []
#if MIN_VERSION_base(4,10,0)
#else
  (Partition p1) `mappend` (Partition p2) = Partition (p1 `mappend` p2)
#endif

  
partition :: Expr sc scopes a -> Partition sc scopes
partition = Partition . (:[]) . getExpr

window :: Partition sc scopes -> Order sc scopes -> Window w sc scopes
window pe oe = Window (PQ.WindowPart (getPartExprs pe) (getOrder oe))

windowExpr :: Partition sc scopes -> Order sc scopes -> Expr sc scopes a -> Expr sc scopes a
windowExpr ps os = Expr . PQ.AnonWindowExpr (getPartExprs ps) (getOrder os) . getExpr

{-
-- We trust the binary input
instance Binary (Window w sc) where
  put = put . getPartitions
  get = Window <$> get
-}
