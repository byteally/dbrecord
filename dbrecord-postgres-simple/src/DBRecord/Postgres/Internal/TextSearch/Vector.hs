{-# LANGUAGE OverloadedStrings #-}
module DBRecord.Postgres.Internal.TextSearch.Vector
       ( const
       , exp
       , mappendDoc
       , vector
       , Document
       , Vector
       ) where

import DBRecord.Internal.Expr
import qualified DBRecord.Internal.PrimQuery as PQ
import qualified Data.Text as T
import Prelude hiding ((<>), const, exp)

data Document
data Vector

const :: T.Text -> Expr sc Document
const = unsafeCoerceExpr . text

exp :: Expr sc T.Text -> Expr sc Document
exp = unsafeCoerceExpr

mappendDoc :: Expr sc Document -> Expr sc Document -> Expr sc Document
mappendDoc (Expr e1) (Expr e2) = Expr (PQ.BinExpr combineOp e1 e2)
  where combineOp = PQ.OpOther "||"

vector :: Expr sc Document -> Expr sc Vector
vector (Expr doc) = Expr (PQ.FunExpr "to_tsvector" [doc])
