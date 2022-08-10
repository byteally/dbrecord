{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE KindSignatures, DataKinds, FlexibleContexts, UndecidableInstances, OverloadedStrings, PatternSynonyms #-}
module DBRecord.Internal.Order where

import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Expr
import Data.Kind

newtype Order (sc :: Type) = Order { getOrder :: [PQ.OrderExpr] }

instance Semigroup (Order sc) where
  (Order o1) <> (Order o2) = Order (o1 <> o2)

instance Monoid (Order sc) where
  mempty = Order []

order :: OrdExpr sc a => PQ.OrderOp -> Expr sc a -> Order sc
order op (Expr expr) = Order $ [PQ.OrderExpr op expr]

asc :: OrdExpr sc a => Expr sc a -> Order sc
asc = order PQ.OrderOp { PQ.orderDirection = PQ.OpAsc
                       , PQ.orderNulls = PQ.NullsLast
                       }

desc :: OrdExpr sc a => Expr sc a -> Order sc
desc = order PQ.OrderOp { PQ.orderDirection = PQ.OpDesc
                        , PQ.orderNulls = PQ.NullsFirst
                        }

ascNullsFirst :: OrdExpr sc a => Expr sc a -> Order sc
ascNullsFirst = order PQ.OrderOp { PQ.orderDirection = PQ.OpAsc
                                 , PQ.orderNulls = PQ.NullsFirst
                                 }

descNullsLast :: OrdExpr sc a => Expr sc a -> Order sc
descNullsLast = order PQ.OrderOp { PQ.orderDirection = PQ.OpDesc
                                 , PQ.orderNulls = PQ.NullsLast
                                 }

pattern AnyOrder :: Order sc
pattern AnyOrder = Order []
