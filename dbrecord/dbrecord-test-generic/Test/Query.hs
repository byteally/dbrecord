{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Query
  ( module Test.Query
  ) where

import DBRecord.Prelude
import Data.Typeable
import Data.Text (Text)

exprAsQ :: forall a sc.(Typeable a) => Expr sc a -> Query sc (Rec '[ '("col", a)])
exprAsQ e = selectExpr (#col .= e)

testSelList :: Query sc (Rec '[ '("col1", Text), '("col2", Bool), '("col3", Int64)])
testSelList = selectList $
  #col1 .= constExpr ("test" :: Text) .&
  #col2 .= constExpr True .&
  #col3 .= constExpr (123 :: Int64) .&
  end
