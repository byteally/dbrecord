{-# LANGUAGE OverloadedStrings #-}
module DBRecord.Postgres.Internal.TextSearch
       ( (@@)
       ) where

import DBRecord.Postgres.Internal.TextSearch.Vector
import DBRecord.Postgres.Internal.TextSearch.Query
import DBRecord.Internal.Expr
import qualified DBRecord.Internal.PrimQuery as PQ

(@@) :: Expr sc Vector -> Expr sc Query -> Expr sc Bool
(@@) (Expr v) (Expr q) = Expr (PQ.BinExpr tsOp v q)
  where tsOp = PQ.OpOther "@@"
