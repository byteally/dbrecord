{-# LANGUAGE DataKinds, TypeApplications, DeriveGeneric, KindSignatures, TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleContexts, DuplicateRecordFields #-}


module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Attoparsec.Text as A
-- import Data.Either
import DBRecord.MSSQL.Internal.Sql.Parser
import DBRecord.Internal.Sql.DML
import Data.Text as T


main :: IO ()
main = defaultMain tests 

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "2+2=4" $
      2+2 @?= 4
--   , testCase "7 is even" $
--       assertBool "Oops, 7 is odd" (even 7)

  , testCase "Binary Operator Expression" $ 
      (@=?) 
        (parseIntoSqlExpr "([employee_id]>=(1))")
        (ParensSqlExpr (BinSqlExpr OpGtEq (ColumnSqlExpr (SqlColumn ["employee_id"])) (ParensSqlExpr (ConstSqlExpr (IntegerSql 1)))))


    -- The following result is not really correct. We need to change the AST (FunSqlExpr)
  , testCase "Binary Operator Function Call and GtEq comparison" $ 
      (@=?)
        (parseIntoSqlExpr "([dbo].[CheckFnctn]()>=(1))")
        (ParensSqlExpr (BinSqlExpr OpGtEq (FunSqlExpr "dboCheckFnctn" []) (ParensSqlExpr (ConstSqlExpr (IntegerSql 1)))))


-- Binary Bitwise Operators
  , testCase "Binary Bitwise AND" $ 
      (@=?)
        (parseIntoSqlExpr "12 & 20")
        (BinSqlExpr OpBitAnd (ConstSqlExpr (IntegerSql 12)) (ConstSqlExpr (IntegerSql 20)))
  , testCase "Binary Bitwise OR" $ 
      (@=?)
        (parseIntoSqlExpr "170 | 75")
        (BinSqlExpr OpBitOr (ConstSqlExpr (IntegerSql 170)) (ConstSqlExpr (IntegerSql 75)))
  , testCase "Binary Bitwise NOT" $ 
      (@=?)
        (parseIntoSqlExpr "~ 170")
        (PrefixSqlExpr OpBitwiseNot (ConstSqlExpr (IntegerSql 170)))


-- Arithmetic Operators

  , testCase "Addition" $ 
      (@=?)
        (parseIntoSqlExpr "2 + 2")
        (BinSqlExpr OpPlus (ConstSqlExpr (IntegerSql 2)) (ConstSqlExpr (IntegerSql 2)))

  , testCase "Subtraction" $ 
      (@=?)
        (parseIntoSqlExpr "3 - 1")
        (BinSqlExpr OpMinus (ConstSqlExpr (IntegerSql 3)) (ConstSqlExpr (IntegerSql 1)))

  , testCase "Multiplication" $ 
      (@=?)
        (parseIntoSqlExpr "3 * 3")
        (BinSqlExpr OpMul (ConstSqlExpr (IntegerSql 3)) (ConstSqlExpr (IntegerSql 3)))
 
  , testCase "Division" $ 
      (@=?)
        (parseIntoSqlExpr "10 / 5")
        (BinSqlExpr OpDiv (ConstSqlExpr (IntegerSql 10)) (ConstSqlExpr (IntegerSql 5)))

  , testCase "Modulo" $ 
      (@=?)
        (parseIntoSqlExpr "10 % 2")
        (BinSqlExpr OpMod (ConstSqlExpr (IntegerSql 10)) (ConstSqlExpr (IntegerSql 2)))


-- Logical Operators

--   , testCase "Logical ALL" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

  , testCase "Logical AND without parenthesis (Precedence test)" $ 
      (@=?)
        (parseIntoSqlExpr "10 > 5 AND 20 > 30")
        (BinSqlExpr OpAnd (BinSqlExpr OpGt (ConstSqlExpr (IntegerSql 10)) (ConstSqlExpr (IntegerSql 5)) ) (BinSqlExpr OpGt (ConstSqlExpr (IntegerSql 20)) (ConstSqlExpr (IntegerSql 30))))

  , testCase "Logical AND" $ 
      (@=?)
        (parseIntoSqlExpr "(10>3) AND (4>1)")
        (BinSqlExpr OpAnd (ParensSqlExpr (BinSqlExpr OpGt (ConstSqlExpr (IntegerSql 10)) (ConstSqlExpr (IntegerSql 3)))) (ParensSqlExpr (BinSqlExpr OpGt (ConstSqlExpr (IntegerSql 4)) (ConstSqlExpr (IntegerSql 1)))))

--   , testCase "Logical ANY" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "Logical BETWEEN" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "Logical EXISTS" $  -- SubQuery
--       (@=?)
--         (parseIntoSqlExpr "")
        -- ()

--   , testCase "Logical IN" $      -- SubQuery
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "Logical SOME" $    -- SubQuery
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()



--TODO : 
--   , testCase "Logical LIKE" $ 
--       (@=?)
--         (parseIntoSqlExpr "'hellowmfcsaed' LIKE 'hello%'")
--         ()

  , testCase "Logical NOT" $ 
      (@=?)
        (parseIntoSqlExpr "NOT (20 > 30)")
        (PrefixSqlExpr OpNot (ParensSqlExpr (BinSqlExpr OpGt (ConstSqlExpr (IntegerSql 20)) (ConstSqlExpr (IntegerSql 30)))))

  , testCase "Logical OR without parenthesis (precedence test)" $ 
      (@=?)
        (parseIntoSqlExpr "10 > 5 OR 20 > 30")
        (BinSqlExpr OpOr (BinSqlExpr OpGt (ConstSqlExpr (IntegerSql 10)) (ConstSqlExpr (IntegerSql 5)) ) (BinSqlExpr OpGt (ConstSqlExpr (IntegerSql 20)) (ConstSqlExpr (IntegerSql 30))))

  , testCase "Logical OR" $ 
      (@=?)
        (parseIntoSqlExpr "(10 > 5) OR (20 > 30)")
        (BinSqlExpr OpOr (ParensSqlExpr (BinSqlExpr OpGt (ConstSqlExpr (IntegerSql 10)) (ConstSqlExpr (IntegerSql 5)))) (ParensSqlExpr (BinSqlExpr OpGt (ConstSqlExpr (IntegerSql 20)) (ConstSqlExpr (IntegerSql 30)))))




-- Comparison Operators

  , testCase "Equals" $ 
      (@=?)
        (parseIntoSqlExpr "10 = 11")
        (BinSqlExpr OpEq (ConstSqlExpr (IntegerSql 10)) (ConstSqlExpr (IntegerSql 11)))

  , testCase "Greater than" $ 
      (@=?)
        (parseIntoSqlExpr "10 > 9")
        (BinSqlExpr OpGt (ConstSqlExpr (IntegerSql 10)) (ConstSqlExpr (IntegerSql 9)))

  , testCase "Less than" $ 
      (@=?)
        (parseIntoSqlExpr "8 < 9")
        (BinSqlExpr OpLt (ConstSqlExpr (IntegerSql 8)) (ConstSqlExpr (IntegerSql 9)))

  , testCase "Greater than or equal to " $ 
      (@=?)
        (parseIntoSqlExpr "10 >= 3")
        (BinSqlExpr OpGtEq (ConstSqlExpr (IntegerSql 10)) (ConstSqlExpr (IntegerSql 3)))

  , testCase "Less than or equal to" $ 
      (@=?)
        (parseIntoSqlExpr "3 <= 10")
        (BinSqlExpr OpLtEq (ConstSqlExpr (IntegerSql 3)) (ConstSqlExpr (IntegerSql 10)))

  , testCase "Not equal to" $ 
      (@=?)
        (parseIntoSqlExpr "10 <> 11")
        (BinSqlExpr OpNotEq (ConstSqlExpr (IntegerSql 10)) (ConstSqlExpr (IntegerSql 11)))

  , testCase "Not equal to (non-ISO)" $ 
      (@=?)
        (parseIntoSqlExpr "10 != 11")
        (BinSqlExpr OpNotEq (ConstSqlExpr (IntegerSql 10)) (ConstSqlExpr (IntegerSql 11)))


  , testCase "Not Less than (non-ISO)" $ 
      (@=?)
        (parseIntoSqlExpr "10 !< 8")
        (BinSqlExpr OpNotLt (ConstSqlExpr (IntegerSql 10)) (ConstSqlExpr (IntegerSql 8)))

  , testCase "Not Greater than (non-ISO)" $ 
      (@=?)
        (parseIntoSqlExpr "10 !> 12")
        (BinSqlExpr OpNotGt (ConstSqlExpr (IntegerSql 10)) (ConstSqlExpr (IntegerSql 12)))


-- String Operators (String matching)

  , testCase "String match Wildcard %" $ 
      (@=?)
        (parseIntoSqlExpr "'hello%'")
        (ConstSqlExpr (StringSql "hello%"))

  , testCase "String match any char in range - Wildcard []" $ 
      (@=?)
        (parseIntoSqlExpr "'hello[a-e]'")
        (ConstSqlExpr (StringSql "hello[a-e]"))

  , testCase "String do not match char - Wildcard [^]" $ 
      (@=?)
        (parseIntoSqlExpr "'h[^e]llo'")
        (ConstSqlExpr (StringSql "h[^e]llo"))

  , testCase "String match Wildcard single character" $ 
      (@=?)
        (parseIntoSqlExpr "'h_llo'")
        (ConstSqlExpr (StringSql "h_llo"))


  ]
 where 
    parseIntoSqlExpr :: Text -> SqlExpr
    parseIntoSqlExpr txt = either parsePanic id . parseOnly sqlExpr $ txt

    parsePanic :: String -> SqlExpr 
    parsePanic e = error $ "Panic while parsing: " ++ show e -- ++ " , " ++ "while parsing " ++ show t
 
--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()


--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()

--   , testCase "" $ 
--       (@=?)
--         (parseIntoSqlExpr "")
--         ()
