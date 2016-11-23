{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE KindSignatures, DataKinds, FlexibleContexts, UndecidableInstances, OverloadedStrings #-}
module DBRecord.Internal.Order where

import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Expr

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.HashMap.Strict as HM
import Data.Proxy
import Data.Binary
import qualified Data.Text as T
import Data.Semigroup

newtype Order (scopes :: [*]) = Order { getOrder :: [PQ.OrderExpr] }

instance Semigroup (Order sc) where
  (Order o1) <> (Order o2) = Order (o1 <> o2)

order :: OrdExpr a => PQ.OrderOp -> Expr sc a -> Order sc
order op (Expr expr) = Order $ [PQ.OrderExpr op expr]

asc :: OrdExpr a => Expr sc a -> Order sc
asc = order PQ.OrderOp { PQ.orderDirection = PQ.OpAsc
                       , PQ.orderNulls = PQ.NullsLast
                       }

desc :: OrdExpr a => Expr sc a -> Order sc
desc = order PQ.OrderOp { PQ.orderDirection = PQ.OpDesc
                        , PQ.orderNulls = PQ.NullsFirst
                        }

ascNullsFirst :: OrdExpr a => Expr sc a -> Order sc
ascNullsFirst = order PQ.OrderOp { PQ.orderDirection = PQ.OpAsc
                                 , PQ.orderNulls = PQ.NullsFirst
                                 }

descNullsLast :: OrdExpr a => Expr sc a -> Order sc
descNullsLast = order PQ.OrderOp { PQ.orderDirection = PQ.OpDesc
                                 , PQ.orderNulls = PQ.NullsLast
                                 }

instance ToJSON (Order sc) where
  toEncoding e = pairs  ("trusted" .= getOrder e)
  toJSON     e = object ["trusted" .= getOrder e]

instance (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => FromJSON (Order sc) where
  parseJSON = parseJSONOrder

parseJSONOrder :: (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => Value -> Parser (Order sc)
parseJSONOrder (Object eobj) = do
  tst  <- eobj .:? "trusted"
  case tst of
    Just e -> pure (Order e)
    Nothing -> case withText "Expr" go <$> (HM.lookup "untrusted" eobj) of
      Just p  -> p
      Nothing -> fail "key not found"    
  where go torder = case parseOrder torder of
          Left  errs -> fail (T.unpack (renderErrs errs))
          Right v    -> pure v
parseJSONOrder e = typeMismatch "Order" e

parseOrder :: (ToScopeRep sc (Proxy ('[] :: [* -> *]))) => T.Text -> Validation (Order sc)
parseOrder = undefined

-- We trust the binary input
instance Binary (Order sc) where
  put = put . getOrder
  get = Order <$> get
