module DBRecord.Internal.Postgres
       ( module DBRecord.Internal.Postgres.Types
       , module DBRecord.Internal.Postgres.Pretty
       , module DBRecord.Internal.Postgres.SqlGen
       , ppPGExpr
       ) where

import DBRecord.Internal.Postgres.Types
import DBRecord.Internal.Postgres.Pretty
import DBRecord.Internal.Postgres.SqlGen
import DBRecord.Internal.PrimQuery
import Text.PrettyPrint.HughesPJ 

ppPGExpr :: PrimExpr -> Doc
ppPGExpr e = 
 let sExpr = defaultSqlExpr defaultSqlGenerator e
 in  (ppSqlExpr sExpr)
