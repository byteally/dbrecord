{-# LANGUAGE OverloadedStrings #-}
module DBRecord.Internal.Postgres.TextSearch.Query
       ( glexeme
       , prefix
       , const
       , queryText
       , (.&&)
       , (.||)
       , not
       , query
       , Lexeme
       , QueryText
       , Query
       ) where

import Prelude hiding (const, not, lex)
import qualified Data.Text as T
import qualified Data.List as L
import DBRecord.Internal.Expr hiding ((.&&), (.||))
import qualified DBRecord.Internal.PrimQuery as PQ
import Data.Monoid ((<>))

newtype Lexeme = Lexeme { getLexeme :: T.Text }

data Weight = A | B | C | D

pprWeight :: Weight -> T.Text
pprWeight A = "A"
pprWeight B = "B"
pprWeight C = "C"
pprWeight D = "D"

pprWeights :: [Weight] -> T.Text
pprWeights = L.foldl' (\acc a -> acc <> pprWeight a) ""

glexeme :: [Weight] -> Bool -> T.Text -> Lexeme
glexeme weights isPfx lex = Lexeme $
  case isPfx of
    True  -> lex <> ":*" <> pprWeights weights
    False -> lex <> ":"  <> pprWeights weights
  
prefix :: T.Text -> Lexeme
prefix = glexeme [] True  

const  :: T.Text -> Lexeme
const = glexeme [] False

newtype QueryText = QueryText { getQueryText :: T.Text }

queryText :: Lexeme -> QueryText
queryText (Lexeme lex) = QueryText lex

infixr 3 .&&
infixr 2 .||

(.&&) :: QueryText -> QueryText -> QueryText
(.&&) (QueryText l1) (QueryText l2) = QueryText (parens (pprAnd l1 l2))

(.||) :: QueryText -> QueryText -> QueryText
(.||) (QueryText l1) (QueryText l2) = QueryText (parens (pprOr l1 l2))

not  :: QueryText -> QueryText
not (QueryText l) = QueryText (parens (pprNot l))

parens :: T.Text -> T.Text
parens t = "(" <> t <> ")"

pprAnd :: T.Text -> T.Text -> T.Text
pprAnd t1 t2 = t1 <> " & " <> t2

pprOr :: T.Text -> T.Text -> T.Text
pprOr t1 t2 = t1 <> " | " <> t2

pprNot :: T.Text -> T.Text
pprNot t = "!" <> t

data Query

query :: QueryText -> Expr sc Query
query (QueryText t) = Expr (PQ.FunExpr "to_tsquery" [pqText t])
  where pqText = PQ.ConstExpr . PQ.String

plainQuery :: Text -> Expr sc Query
plainQuery t = Expr (PQ.FunExpr "plainto_tsquery" [pqText t])
  where pqText = PQ.ConstExpr . PQ.String
