{-# LANGUAGE KindSignatures, DataKinds #-}
module Database.Internal.Order where

import qualified Database.Internal.PrimQuery as PQ
import Database.Internal.Expr

-- TODO: Restrict ordering only for valid types
newtype Order (scopes :: [*]) = Order { getOrder :: [PQ.OrderExpr] }

instance Monoid (Order sc) where
  mempty = Order mempty
  mappend (Order o1) (Order o2) = Order (o1 `mappend` o2)

order :: PQ.OrderOp -> Expr sc a -> Order sc
order op (Expr expr) = Order $ [PQ.OrderExpr op expr]

asc :: Expr sc a -> Order sc
asc = order PQ.OrderOp { PQ.orderDirection = PQ.OpAsc
                       , PQ.orderNulls = PQ.NullsLast
                       }

desc :: Expr sc a -> Order sc
desc = order PQ.OrderOp { PQ.orderDirection = PQ.OpDesc
                        , PQ.orderNulls = PQ.NullsFirst
                        }

ascNullsFirst :: Expr sc a -> Order sc
ascNullsFirst = order PQ.OrderOp { PQ.orderDirection = PQ.OpAsc
                                 , PQ.orderNulls = PQ.NullsFirst
                                 }

descNullsLast :: Expr sc a -> Order sc
descNullsLast = order PQ.OrderOp { PQ.orderDirection = PQ.OpDesc
                                 , PQ.orderNulls = PQ.NullsLast
                                 }

