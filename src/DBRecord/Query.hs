{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- | 

module DBRecord.Query
       ( module DBRecord.Internal.Order
       , module DBRecord.Internal.Expr
       , module DBRecord.Internal.Predicate
       , get, getBy, getAll, update
       , updateBy, updateAll, delete
       , deleteBy, deleteAll, insert
       , insertRet, upsert, count                       
       ) where

import DBRecord.Schema

import DBRecord.Internal.Order
import DBRecord.Internal.Expr
import DBRecord.Internal.Predicate
import DBRecord.Internal.PrimQuery

get :: Table db tab => db -> tab -> ()
getBy :: ()
getAll :: ()
update :: ()
updateBy :: ()
updateAll :: ()
delete :: ()
deleteBy :: ()
deleteAll :: ()
insert :: ()
insertRet :: ()
upsert :: ()
count :: ()


get _ _ = ()
getBy = ()
getAll = ()
update = ()
updateBy = ()
updateAll = ()
delete = ()
deleteBy = ()
deleteAll = ()
insert = ()
insertRet = ()
upsert = ()
count = ()
