{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module DBRecord.Internal.Migration.Validation where

-- import DBRecord.Internal.Migration.Types hiding (CheckExpr)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text  (Text)
import Data.Proxy
import qualified Data.List as L

import Data.Attoparsec.Text (parseOnly)
import Data.Either (either)
-- POSTGRES only info
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Postgres.Parser
import DBRecord.Internal.Postgres.SqlGen (primExprGen)
import DBRecord.Internal.Migration.Types ( PrimDDL (..), TypeName (..), EnumVal (..)
                                         , ColName (..), ColType (..), Column (..), TabName (..)
                                         , AlterTable (..), AddConstraint (..), ConstraintName (..)
                                         , AlterColumn (..), DefExpr (..)
                                         )
import DBRecord.Internal.Migration.Types ( createTable
                                         , addPrimaryKey
                                         , addUnique
                                         , addForeignKey
                                         , addCheck
                                         , addDefault
                                         , setNotNull
                                         , column
                                         , single
                                         )
import qualified DBRecord.Internal.Migration.Types as MT
import Data.Coerce
import Data.Maybe
import DBRecord.Internal.Schema
import qualified DBRecord.Internal.Schema as S
import qualified Data.HashMap.Strict as HM

data EnumInfo = EnumInfo { enumTypeName :: Text
                         , enumCons     :: Vector Text
                         } deriving (Show, Eq)

data TableColInfo = TableColInfo { dbTableName  :: Text
                                 , dbColumnName :: Text
                                 , dbPosition :: Int
                                 , dbColDefault :: Maybe Text
                                 , dbIsNullable :: Text
                                 , dbTypeName :: Text
                                 , dbLength :: Maybe Int
                                 } deriving (Show, Eq)
                                            
data CheckCtx = CheckCtx Text Text Text
               deriving (Show, Eq)

type CheckExpr = (Text, Text)

data PrimKey = PrimKey Text Text Text Int
             deriving (Show, Eq, Ord)

data UniqKey = UniqKey Text Text Text Int
             deriving (Show, Eq, Ord)

data FKey = FKey Text Text Text Int Text Text Int
          deriving (Show, Eq, Ord)

type TableContent a = HM.HashMap Text [a]

toPrimKeyInfo :: [PrimKey] -> HM.HashMap Text PrimaryKeyInfo
toPrimKeyInfo = HM.fromList . map toPrimKeyInfo . groupByTableName
  where groupByTableName = L.groupBy (\(PrimKey _ tna _ _) (PrimKey _ tnb _ _) -> tna == tnb)

        toPrimKeyInfo pks@(PrimKey kna tna _ _ : _) = (mkHaskName tna, PrimaryKeyInfo { _pkeyName    = Just kna
                                                                                      , _pkeyColumns = getPKCols pks
                                                                                      }
                                                      )
        toPrimKeyInfo []                            = error "impossible: empty group"
        
        getPKCols = map snd . L.sortBy cmpByFst . map getPKCol
        getPKCol (PrimKey _ _ col i) = (i, col)
        cmpByFst a b = compare (fst a) (fst b)

toUniqKeyInfo :: [UniqKey] -> TableContent UniqueInfo
toUniqKeyInfo = HM.fromListWith (++) . concatMap (map toUniqKeyInfo . groupByKeyName) . groupByTableName
  where groupByTableName = L.groupBy (\(UniqKey _ tna _ _) (UniqKey _ tnb _ _) -> tna == tnb)
        groupByKeyName   = L.groupBy (\(UniqKey kna _ _ _) (UniqKey knb _ _ _) -> kna == knb)

        toUniqKeyInfo pks@(UniqKey kna tna _ _ : _) = (mkHaskName tna, [UniqueInfo { _uqName     = mkEntityName (mkHaskName kna) kna
                                                                                   , _uqColumns  = (getUQCols pks)
                                                                                   }
                                                                         ]
                                                      )
        toUniqKeyInfo []                            = error "impossible: empty group"
        
        getUQCols = map snd . L.sortBy cmpByFst . map getUQCol
        getUQCol (UniqKey _ _ col i) = (i, col)
        cmpByFst a b = compare (fst a) (fst b)

toTableInfo :: TableContent ColumnInfo -> TableContent CheckInfo -> TableContent DefaultInfo -> HM.HashMap Text PrimaryKeyInfo -> TableContent UniqueInfo -> TableContent ForeignKeyInfo -> [TableInfo]
toTableInfo cols chks defs pk uqs fks =
  let tabNs = HM.keys cols
  in  L.map (\haskTN ->
              let tUqs = HM.lookupDefault [] haskTN uqs
                  tPk  = HM.lookup haskTN pk
                  tFks = HM.lookupDefault [] haskTN fks
                  tDefs = HM.lookupDefault [] haskTN defs
                  tChks = HM.lookupDefault [] haskTN chks
                  tCols = HM.lookupDefault [] haskTN cols
              in TableInfo { _primaryKeyInfo = tPk
                           , _foreignKeyInfo = tFks
                           , _uniqueInfo     = tUqs
                           , _defaultInfo    = tDefs
                           , _checkInfo      = tChks
                           , _sequenceInfo   = undefined
                           , _tableName      = mkEntityName (mkHaskTypeName haskTN) haskTN
                           , _columnInfo     = tCols
                           , _ignoredCols    = ()
                           }
             ) tabNs

toCheckInfo :: [CheckCtx] -> TableContent CheckInfo
toCheckInfo = HM.fromListWith (++) . map chkInfo
  where chkInfo (CheckCtx chkName chkOn chkExp) =
          ( mkHaskName chkOn
          , [CheckInfo { _checkExp  = unsafeParseExpr chkExp
                       , _checkName = mkEntityName chkName chkName
                      }
            ]
          )

toDefaultInfo :: [TableColInfo] -> TableContent DefaultInfo
toDefaultInfo = HM.fromListWith (++) . map defaultInfo
  where defaultInfo tci =
          ( mkHaskName (dbColumnName tci)
          , [DefaultInfo { _defaultOn  = (mkHaskName (dbColumnName tci))
                         , _defaultExp = unsafeParseExpr (fromJust (dbColDefault tci))
                        }
            ]
          )

toForeignKeyInfo :: [FKey] -> TableContent ForeignKeyInfo
toForeignKeyInfo = HM.fromListWith (++) . concatMap (map toForeignKeyInfo . groupByKeyName) . groupByTableName
  where groupByTableName = L.groupBy (\(FKey _ tna _ _ _ _ _) (FKey _ tnb _ _ _ _ _) -> tna == tnb)
        groupByKeyName   = L.groupBy (\(FKey kna _ _ _ _ _ _) (FKey knb _ _ _ _ _ _) -> kna == knb)

        toForeignKeyInfo fks@(FKey kna tna _ _ rtna _ _ : _) =
          let fki = mkForeignKeyInfo (mkEntityName (mkHaskName kna) kna)
                                     (getFKCols fks)
                                     (mkHaskTypeName rtna)
                                     (getFKRefCols fks)
          in (mkHaskName tna , [fki])
        toForeignKeyInfo []                                        = error "impossible: empty group"

        getFKCols = map snd . L.sortBy cmpByFst . map getFKCol
        getFKCol (FKey _ _ col i _ _ _) = (i, col)

        getFKRefCols = map snd . L.sortBy cmpByFst . map getFKRefCol
        getFKRefCol (FKey _ _ _ _ _ col i) = (i, col)
        
        cmpByFst a b = compare (fst a) (fst b)

toTabColInfo :: [TableColInfo] -> TableContent ColumnInfo
toTabColInfo = HM.fromListWith (++) . map colInfo
  where nullable a = case dbIsNullable a of
          "YES" -> True
          "NO"  -> False
          _     -> error "Panic: Invalid nullability information from DB"

        colInfo tci =
          let ci = mkColumnInfo (nullable tci)
                                (mkEntityName (mkHaskName (dbColumnName tci)) (dbColumnName tci))
                                (coerce (dbTypeName tci))
                   
          in ( dbTableName tci, [ci])

instance FromRow EnumInfo where
  fromRow = EnumInfo <$> field <*> field

instance FromRow TableColInfo where
  fromRow = TableColInfo <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow CheckCtx where
  fromRow = CheckCtx <$> field <*> field <*> field

instance FromRow PrimKey where
  fromRow = PrimKey <$> field <*> field <*> field <*> field

instance FromRow UniqKey where
  fromRow = UniqKey <$> field <*> field <*> field <*> field

instance FromRow FKey where
  fromRow = FKey <$> field <*> field <*> field <*> field <*> field <*> field <*> field

enumQ :: Query
enumQ =
  "SELECT pg_type.typname AS enumtype, array_agg(pg_enum.enumlabel) AS enumlabel \
   \FROM pg_type \
   \JOIN pg_enum \ 
     \ON pg_enum.enumtypid = pg_type.oid \
 \GROUP BY enumtype"

tableColQ :: Query
tableColQ =
 "SELECT col.table_name as table_name,\
        \col.column_name,\ 
        \col.ordinal_position as pos,\
        \col.column_default,\ 
        \col.is_nullable,\ 
        \col.data_type,\ 
        \col.character_maximum_length\
 \FROM  (SELECT table_name,\ 
               \column_name,\ 
               \ordinal_position,\
               \column_default,\
               \is_nullable,\ 
               \data_type,\
               \character_maximum_length\
        \FROM information_schema.columns\
        \WHERE table_schema = '?'\
         \) as col\
  \JOIN\
        \(SELECT table_name\
         \FROM information_schema.tables WHERE table_schema='?' AND table_type='BASE TABLE') as tab\
  \ON col.table_name = tab.table_name\
  \ORDER BY table_name, pos"
  
checksQ :: Query
checksQ =
  "SELECT cc.constraint_name, table_name, check_clause \
  \FROM information_schema.check_constraints AS cc \
  \JOIN information_schema.table_constraints AS tc \
  \ON   cc.constraint_name = tc.constraint_name AND\
       \cc.constraint_schema = tc.constraint_schema\
  \WHERE cc.constraint_schema = '?'"

primKeysQ :: Query
primKeysQ =
 "SELECT kcu.constraint_name as constraint_name, kcu.table_name, kcu.column_name, kcu.ordinal_position\
 \FROM  (SELECT constraint_name\
              \, table_name\
              \, column_name\
              \, ordinal_position\
        \FROM information_schema.key_column_usage\
        \WHERE constraint_schema = '?'\
       \) as kcu\
  \JOIN  (SELECT constraint_name\
              \, table_name\
         \FROM information_schema.table_constraints\
         \WHERE constraint_type = 'PRIMARY KEY' AND constraint_schema = '?'\
        \) as tc\
  \ON    kcu.constraint_name = tc.constraint_name AND\
        \kcu.table_name = tc.table_name\
  \ORDER BY table_name, ordinal_position"

uniqKeysQ :: Query
uniqKeysQ =
 "SELECT kcu.constraint_name as constraint_name, kcu.table_name, kcu.column_name, kcu.ordinal_position\
 \FROM  (SELECT constraint_name\
              \, table_name\
              \, column_name\
              \, ordinal_position\
        \FROM information_schema.key_column_usage\
        \WHERE constraint_schema = '?'\
       \) as kcu\
  \JOIN  (SELECT constraint_name\
              \, table_name\
         \FROM information_schema.table_constraints\
         \WHERE constraint_type = 'UNIQUE' AND constraint_schema = '?'\
        \) as tc\
  \ON    kcu.constraint_name = tc.constraint_name AND\
        \kcu.table_name = tc.table_name\
  \ORDER BY table_name, constraint_name, ordinal_position"

foreignKeysQ :: Query
foreignKeysQ =
 "SELECT\
     \KCU1.CONSTRAINT_NAME AS FK_CONSTRAINT_NAME\
    \,KCU1.TABLE_NAME AS FK_TABLE_NAME\
    \,KCU1.COLUMN_NAME AS FK_COLUMN_NAME\
    \,KCU1.ORDINAL_POSITION AS FK_ORDINAL_POSITION\
    \,KCU2.CONSTRAINT_NAME AS REFERENCED_CONSTRAINT_NAME\
    \,KCU2.TABLE_NAME AS REFERENCED_TABLE_NAME\ 
    \,KCU2.COLUMN_NAME AS REFERENCED_COLUMN_NAME\
    \,KCU2.ORDINAL_POSITION AS REFERENCED_ORDINAL_POSITION\
 \FROM (SELECT * FROM INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS\
               \WHERE CONSTRAINT_SCHEMA = '?') AS RC\
 \INNER JOIN (SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE\
                     \WHERE CONSTRAINT_SCHEMA = '?'\
           \) AS KCU1\
    \ON KCU1.CONSTRAINT_CATALOG = RC.CONSTRAINT_CATALOG\
    \AND KCU1.CONSTRAINT_SCHEMA = RC.CONSTRAINT_SCHEMA\
    \AND KCU1.CONSTRAINT_NAME = RC.CONSTRAINT_NAME\

 \INNER JOIN (SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE\
                     \WHERE CONSTRAINT_SCHEMA = '?'\
           \) AS KCU2\
    \ON KCU2.CONSTRAINT_CATALOG = RC.UNIQUE_CONSTRAINT_CATALOG\
    \AND KCU2.CONSTRAINT_SCHEMA = RC.UNIQUE_CONSTRAINT_SCHEMA\
    \AND KCU2.CONSTRAINT_NAME = RC.UNIQUE_CONSTRAINT_NAME\
    \AND KCU2.ORDINAL_POSITION = KCU1.ORDINAL_POSITION"
  
type SchemaName = Text
               
getDbSchemaInfo :: SchemaName -> Connection -> IO [TableInfo]
getDbSchemaInfo sn conn = do
  -- enumTs <- query_ conn enumQ
  tcols <- query_ conn tableColQ
  tchks <- query_ conn checksQ
  prims <- query_ conn primKeysQ
  uniqs <- query_ conn uniqKeysQ
  fks   <- query_ conn foreignKeysQ  
  pure $ (toTableInfo (toTabColInfo tcols) (toCheckInfo tchks) (toDefaultInfo tcols) (toPrimKeyInfo prims) (toUniqKeyInfo uniqs) (toForeignKeyInfo fks))

unsafeParseExpr :: Text -> PQ.PrimExpr
unsafeParseExpr t = primExprGen . either parsePanic id . parseOnly sqlExpr $ t
  where parsePanic e = error $ "Panic while parsing: " ++ show e ++ " , " ++ "while parsing " ++ show t

-- conversion to migrations

-- Only enums for now
{-
mkMigrationTypes :: [EnumInfo] -> [PrimDDL]
mkMigrationTypes = map mkMigrationType
  where mkMigrationType e =
          let tn = TypeName (enumTypeName e)
              vals = map EnumVal (V.toList (enumCons e))
          in CreateEnum tn vals

mkMigrations :: [EnumInfo] -> [TableInfo] -> [PrimDDL]
mkMigrations enumInfos tabInfos =
  mkMigrationTypes enumInfos ++ mkMigrationTables tabInfos
-}

mkHaskName :: Text -> Text
mkHaskName = id

mkHaskTypeName :: Text -> S.TypeName Text
mkHaskTypeName = mkTypeName "DBPackage" "DBModule"
  
{-
mkMigrationTables :: [TableInfo] -> [PrimDDL]
mkMigrationTables =
  concatMap mkMigrationTable

mkMigrationTable :: TableInfo -> [PrimDDL]
mkMigrationTable tabInfo =
  concat $ 
    [ single createTab
    , alterPKs
    , alterUqs
    , alterFKs
    , checkExprs
    , defaultExprs
    ]
    
  where createTab =
          let cols = map colInfo (columns tabInfo)
              colInfo c = column (coerce (columnName c)) (coerce (typeN c))
          in  createTable (coerce tabN) cols
          
        alterPKs = map alterPK (primaryKeys tabInfo)
        alterPK pk =
          let pkCols = primKeyColumns pk
              pkName = primKeyName pk
          in addPrimaryKey (coerce tabN) (coerce pkName) (coerce pkCols)

        alterUqs = map alterUq (uniqueKeys tabInfo)
        alterUq uq =
          let uqCols = uniqKeyColumns uq
              uqName = uniqKeyName uq
          in  addUnique (coerce tabN) (coerce uqName) (coerce uqCols)

        alterFKs = map alterFK (foreignKeys tabInfo)
        alterFK fk =
          let fkCols  = fkOnCols fk
              refCols = fkRefCols fk
              fkN     = fkName fk
              fkRefN  = fkRefTab fk
          in addForeignKey (coerce tabN) (coerce fkN) (coerce fkCols)
                           (coerce fkRefN) (coerce refCols)

        checkExprs = map checkExpr (checks tabInfo)
        checkExpr chkInfo =
          let chkName = checkName chkInfo
              chkExpr = check chkInfo
          in case isNotNullExp chkExpr of
                  (Just col)  -> setNotNull (coerce tabN) (coerce col)
                  Nothing     -> addCheck (coerce tabN) (coerce chkName) (coerce chkExpr)

        defaultExprs = map defaultExpr (defaults tabInfo)
        defaultExpr defInfo =
          let colName = defaultOnColumn defInfo
              defExp  = defaultExp defInfo
          in addDefault (coerce tabN) (coerce colName) (coerce defExp)               
        tabN = tableName tabInfo        

        isNotNullExp exp = case exp of
          PQ.UnExpr PQ.OpIsNotNull (PQ.BaseTableAttrExpr coln) -> Just coln
          _                                                    -> Nothing
                                        
-}
