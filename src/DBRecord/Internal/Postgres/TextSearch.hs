{-# LANGUAGE OverloadedStrings #-}
module DBRecord.Internal.Postgres.TextSearch
       ( (@@)
       ) where

import DBRecord.Internal.Postgres.TextSearch.Vector
import DBRecord.Internal.Postgres.TextSearch.Query
import DBRecord.Internal.Expr
import qualified DBRecord.Internal.PrimQuery as PQ

(@@) :: Expr sc Vector -> Expr sc Query -> Expr sc Bool
(@@) (Expr v) (Expr q) = Expr (PQ.BinExpr tsOp v q)
  where tsOp = PQ.OpOther "@@"
