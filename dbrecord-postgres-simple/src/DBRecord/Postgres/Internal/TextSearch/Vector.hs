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

const :: T.Text -> Expr sc scopes Document
const = unsafeCoerceExpr . text

exp :: Expr sc scopes T.Text -> Expr sc scopes Document
exp = unsafeCoerceExpr

mappendDoc :: Expr sc scopes Document -> Expr sc scopes Document -> Expr sc scopes Document
mappendDoc (Expr e1) (Expr e2) = Expr (PQ.BinExpr combineOp e1 e2)
  where combineOp = PQ.OpOther "||"

vector :: Expr sc scopes Document -> Expr sc scopes Vector
vector (Expr doc) = Expr (PQ.FunExpr "to_tsvector" [doc])
