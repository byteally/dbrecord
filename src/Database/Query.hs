{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- | 

module Database.Query
       ( module Database.Internal.Order
       , module Database.Internal.Expr
       , module Database.Internal.Predicate
       , module Database.Internal.PrimQuery
       , get, getBy, getAll, update
       , updateBy, updateAll, delete
       , deleteBy, deleteAll, insert
       , insertRet, upsert, count                       
       ) where

import Database.Schema

import Database.Internal.Order
import Database.Internal.Expr
import Database.Internal.Predicate
import Database.Internal.PrimQuery

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
