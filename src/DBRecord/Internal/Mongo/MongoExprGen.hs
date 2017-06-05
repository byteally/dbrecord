{-# LANGUAGE OverloadedStrings #-}
module DBRecord.Internal.Mongo.MongoExprGen where

import qualified DBRecord.Internal.PrimQuery as PQ
import Database.MongoDB.Query
import Data.Bson
import Data.Text (Text)

type MongoSelect = Query
type MongoTable = Collection
type MongoOrder = Order
type MongoExpr = Field
type MongoColExpr = Field

mongoQuery :: PQ.PrimQuery -> MongoSelect
mongoQuery = PQ.foldPrimQuery mongoQueryGenerator

mongoQueryGenerator :: PQ.PrimQueryFold MongoSelect
mongoQueryGenerator = PQ.PrimQueryFold
  { PQ.baseTable = baseTable
--  , PQ.product
  }

baseTable :: PQ.TableId -> PQ.Clauses -> MongoSelect
baseTable tabId cs = (select (map toMongoExpr (PQ.criteria cs)) (toMongoTable tabId))
  { project = map toMongoBinding (PQ.projections cs)
  -- , skip = maybe 0 fromIntegral $ PQ.offset cs
  -- , limit = maybe 0 fromIntegral $ PQ.limit cs
  , sort = concatMap toMongoOrder (PQ.orderbys cs)
  }

toMongoTable :: PQ.TableId -> MongoTable
toMongoTable (PQ.TableId s tn) = tn

toMongoOrder :: PQ.OrderExpr -> MongoOrder
toMongoOrder = undefined

toMongoBinding :: (PQ.Sym, PQ.PrimExpr) -> MongoExpr
toMongoBinding (_, pe) = toMongoExpr pe

toMongoExpr :: PQ.PrimExpr -> MongoExpr
toMongoExpr = mongoExpr defaultMongoGenerator

data MongoGenerator = MongoGenerator
  { mongoExpr :: PQ.PrimExpr -> MongoExpr
  } 

defaultMongoGenerator :: MongoGenerator
defaultMongoGenerator = mkMongoGenerator defaultMongoGenerator

mkMongoGenerator :: MongoGenerator -> MongoGenerator
mkMongoGenerator gen = MongoGenerator
  { mongoExpr = defaultMongoExpr gen
  }

-- TODO: Handle prefix
defaultMongoExpr :: MongoGenerator -> PQ.PrimExpr -> MongoExpr
defaultMongoExpr gen expr = case expr of
  PQ.AttrExpr t          -> (PQ.symField t) =: True
  PQ.BaseTableAttrExpr a -> a =: False
  PQ.BinExpr op e1 e2    ->
    let
      leftE = mongoExpr gen e1
      rightE = mongoExpr gen e2
    in ("leftE") =: ((showMongoBinOp op) =: rightE)
  PQ.UnExpr op e         -> undefined


data MongoQueryType
  = FindQuery
  | AggregateQuery
  | MapReduceQuery
  deriving (Show, Eq)

showMongoBinOp :: PQ.BinOp -> Text
showMongoBinOp PQ.OpEq         = "$eq"
showMongoBinOp PQ.OpNotEq      = "$ne"
showMongoBinOp PQ.OpLt         = "$lt"
showMongoBinOp PQ.OpLtEq       = "$le"
showMongoBinOp PQ.OpGt         = "$gt"
showMongoBinOp PQ.OpGtEq       = "$ge"
showMongoBinOp PQ.OpAnd        = "$and"
showMongoBinOp PQ.OpOr         = "$or"
showMongoBinOp PQ.OpLike       = "$regex" -- =: (RegEx "needle" "<opt>")
showMongoBinOp PQ.OpIn         = "$in"
-- showMongoBinOp PQ.OpNotIn      = "$nin"
showMongoBinOp PQ.OpCat        = "$concat"
showMongoBinOp PQ.OpPlus       = "$add"
showMongoBinOp PQ.OpMinus      = "$subtract"
showMongoBinOp PQ.OpMul        = "$multiply"
showMongoBinOp PQ.OpDiv        = "$divide"
showMongoBinOp PQ.OpMod        = "$mod"
{- TODO:
showMongoBinOp PQ.OpBitNot     = ""
showMongoBinOp PQ.OpBitAnd     = ""
showMongoBinOp PQ.OpBitOr      = ""
showMongoBinOp PQ.OpBitXor     = ""
showMongoBinOp PQ.OpAsg        = ""
showMongoBinOp PQ.OpAtTimeZone = ""
-}









mongoUnOp :: PQ.UnOp -> MongoExpr -> MongoExpr
mongoUnOp PQ.OpNot e = "$not" =: e
mongoUnOp PQ.OpIsNull e = "$eq" =: Null
mongoUnOp PQ.OpIsNotNull e = "$ne" =: Null

showMongoAggrOp :: PQ.AggrOp -> Text
showMongoAggrOp PQ.AggrCount          = "$count"
showMongoAggrOp PQ.AggrSum            = "$sum"
showMongoAggrOp PQ.AggrAvg            = "$avg"
showMongoAggrOp PQ.AggrMin            = "$min"
showMongoAggrOp PQ.AggrMax            = "$max"
showMongoAggrOp PQ.AggrStdDev         = "$stdDevSamp"
showMongoAggrOp PQ.AggrStdDevP        = "$stdDevPop"
showMongoAggrOp PQ.AggrArr            = "$push"
showMongoAggrOp PQ.AggrVar            = ""-- "{ $pow: [ { $stdDevSamp: "$scores.score" }, 2 ] }"
showMongoAggrOp PQ.AggrVarP           = ""-- "{ $pow: [ { $stdDevPop: "$scores.score" }, 2 ] }"
{- TODO:
showMongoAggrOp PQ.AggrBoolAnd        = ""
showMongoAggrOp PQ.AggrBoolOr         = ""
showMongoAggrOp (PQ.AggrStringAggr _) = ""
showMongoAggrOp (PQ.AggrOther s)      = s
-}





  
