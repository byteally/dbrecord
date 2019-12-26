{-# LANGUAGE OverloadedStrings #-}
module Test.DBRecord.PPCheck where

import qualified DBRecord.Postgres.Internal.Sql.Pretty as PG
import qualified DBRecord.MSSQL.Internal.Sql.Pretty as MSSQL
import qualified DBRecord.MySQL.Internal.Sql.Pretty as MySQL
import qualified DBRecord.Sqlite.Internal.Sql.Pretty as SQLite
import DBRecord.Internal.PrimQuery
import DBRecord.Internal.Expr as E
import Data.Aeson
import DBRecord.Query
import DBRecord.Internal.Schema hiding (insert, name, version)
import DBRecord.Internal.Common
import DBRecord.Internal.Types
import DBRecord.Schema
import DBRecord.Internal.DBTypes
import DBRecord.Internal.Sql.SqlGen
import qualified Data.List.NonEmpty as NEL


ppUpsert :: Int -> Int -> Int -> {-Value ->-} IO ()
ppUpsert qid groupId viewId {-offset-} = do
    let updRunQ = toUpdQuery "queue_offset" runFlts runKeyVals
        runKeyVals = [ -- ("current_offset", getExpr . E.toNullable . toJson $ offset)
          ("view_id", toConst viewId)
                     ]
        runFlts = [ BinExpr OpEq (AttrExpr (Sym ["queue_offset"] "queue_id")) (toConst qid)
                  , BinExpr OpEq (AttrExpr (Sym ["queue_offset"] "consumer_group_id")) (toConst groupId)
                  -- , BinExpr OpEq (AttrExpr (Sym [] "view_id")) (toConst viewId)                  
                  ]

        insQ = toInsQuery "queue_offset" keyVals (Just (Conflict ConflictAnon (ConflictUpdate updRunQ))) []
        keyVals = [ -- ("current_offset", getExpr . E.toNullable . toJson $ offset)
                    ("queue_id", toConst qid)
                  , ("consumer_group_id", toConst groupId)
                  -- , ("view_id", toConst viewId)                      
                  ]
        insQSql = insertSql insQ 
                  
    putStrLn "-----Postgresql-------"
    putStrLn (PG.renderInsert insQSql)
    putStrLn "----------------------"
    
    putStrLn "--------MSSQL---------"
    putStrLn (MSSQL.renderInsert insQSql)
    putStrLn "----------------------"    

    putStrLn "-------SQLITE---------"
    putStrLn (SQLite.renderInsert insQSql)
    putStrLn "----------------------"

    putStrLn "--------MySQL----------"
    putStrLn (MySQL.renderInsert insQSql)
    putStrLn "----------------------"

    

toInsQuery tab keyVals ret =
  let tabId = TableId "workflow" "public" tab
      (keys, vals) = unzip keyVals
  in InsertQuery tabId keys (vals NEL.:| []) ret

toUpdQuery tab flts keyVals =
  let tabId = TableId "workflow" "public" tab
  in UpdateQuery tabId flts keyVals []

toDelQuery tab flts =
  let tabId = TableId "workflow" "public" tab
  in DeleteQuery tabId flts


toConst :: ConstExpr t => t -> PrimExpr
toConst = getExpr . constExpr
