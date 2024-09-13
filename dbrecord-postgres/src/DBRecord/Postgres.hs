{-# LANGUAGE OverloadedStrings          #-}
module DBRecord.Postgres
  ( module DBRecord.Postgres
  ) where


import           DBRecord
import qualified DBRecord.Internal.PrimQuery as PQ
import           DBRecord.Prelude
import           Data.Text (Text)

-- Functions
-- String Functions and Operators

-- >>> bitLength "jose"
bitLength :: Expr sc Text -> Expr sc Int32
bitLength = undefined

charLength, characterLength :: Expr sc Text -> Expr sc Int32
charLength = undefined
characterLength = charLength

lower :: Expr sc Text -> Expr sc Text
lower = undefined

data UnicodeNormalizationForm
  = NFC -- ^ Default
  | NFD
  | NFKC
  | NFKD
  deriving (Show)

normalize :: Expr sc Text -> Maybe (Expr sc UnicodeNormalizationForm) -> Expr sc Text
normalize = undefined

octetLength :: Expr sc Text -> Expr sc Int32
octetLength = undefined

overlay :: Expr sc Text -> Expr sc Text -> Expr sc Int32 -> Maybe (Expr sc Int32) -> Expr sc Text
overlay = undefined

position :: Expr sc Text -> Expr sc Text -> Expr sc Int32
position = undefined

data SubstringOpt sc
  = SubstrStartCount (Expr sc Int32) (Expr sc Int32)
  | SubstrStart (Expr sc Int32)
  | SubstrCount (Expr sc Int32)
  | SubstrRegex (Expr sc Text) (Maybe (Expr sc Text))
  deriving (Show)
  
substring :: Expr sc Text -> SubstringOpt sc -> Expr sc Text
substring = undefined

data TrimType
  = TrimLeading
  | TrimTrailing
  | TrimBoth
  deriving (Show)

trim :: Maybe TrimType -> Maybe (Expr sc Text) -> Expr sc Text -> Expr sc Text
trim = undefined

upper :: Expr sc Text -> Expr sc Text
upper = undefined

-- Enum Support Functions


cardinality :: Expr sc [a] -> Expr sc Int
cardinality e = Expr $ PQ.FunExpr "cardinality" [getExpr e]

(~~) :: Expr sc LTree -> Expr sc LQuery -> Expr sc Bool
a ~~ b = binOp (PQ.OpOther "~") a b

prefixQ :: LTree -> LQuery
prefixQ (LTree xs) = LQuery (LTree (xs <> ["*"]))

(@>) :: Expr sc LTree -> Expr sc LTree -> Expr sc Bool
a @> b = binOp (PQ.OpOther "@>") a b
