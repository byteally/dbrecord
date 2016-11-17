{-# LANGUAGE KindSignatures, DataKinds #-}
module DBRecord.Internal.Window where

import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Expr
import DBRecord.Internal.Order
import GHC.TypeLits
import Data.Binary

newtype Window (w :: Symbol) (sc :: [*]) = Window { getPartitions :: PQ.WindowPart }

window :: Expr sc a -> Order sc -> Window w sc
window e oe = Window (PQ.WindowPart [getExpr e] (getOrder oe))

addWindowExpr :: Expr sc a -> Window w sc -> Window w sc
addWindowExpr e (Window (PQ.WindowPart es os)) = Window (PQ.WindowPart (es ++ [getExpr e]) os)

-- We trust the binary input
instance Binary (Window w sc) where
  put = put . getPartitions
  get = Window <$> get

