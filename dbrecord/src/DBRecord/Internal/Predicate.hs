{-# LANGUAGE KindSignatures, DataKinds #-}
module DBRecord.Internal.Predicate where

import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Expr

newtype Predicate (sc :: *) (scope :: [*]) a = Predicate { getPredicate :: PQ.PrimExpr -> PQ.PrimExpr }

predicate :: (Expr sc scope a -> Expr sc scope Bool) -> Predicate sc scope a
predicate p = Predicate $ \pexp -> getExpr $ p (Expr pexp)

