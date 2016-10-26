{-# LANGUAGE KindSignatures, DataKinds #-}
module Database.Internal.Predicate where

import qualified Database.Internal.PrimQuery as PQ
import Database.Internal.Expr

newtype Predicate (sc :: [*]) a = Predicate { getPredicate :: PQ.PrimExpr -> PQ.PrimExpr }

predicate :: (Expr sc a -> Expr sc Bool) -> Predicate sc a
predicate p = Predicate $ \pexp -> getExpr $ p (Expr pexp)

