module DBRecord.Interface
       ( module DBRecord.Schema.Interface
       , module DBRecord.Query.Interface
       ) where

import DBRecord.Schema.Interface hiding (tableName, toNullable)
import DBRecord.Query.Interface hiding (TableName)

