{-# LANGUAGE KindSignatures, DataKinds #-}
module DBRecord.Internal.Predicate where

import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Expr

newtype Predicate (sc :: [*]) a = Predicate { getPredicate :: PQ.PrimExpr -> PQ.PrimExpr }

predicate :: (Expr sc a -> Expr sc Bool) -> Predicate sc a
predicate p = Predicate $ \pexp -> getExpr $ p (Expr pexp)

