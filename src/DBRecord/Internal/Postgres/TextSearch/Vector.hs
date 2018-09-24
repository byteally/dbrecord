{-# LANGUAGE OverloadedStrings #-}
module DBRecord.Internal.Postgres.TextSearch.Vector
       ( const
       , exp
       , mappendDoc
       , vector
       , Document
       , Vector
       ) where

import Prelude hiding (const, exp)
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
