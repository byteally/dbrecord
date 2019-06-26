{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, ScopedTypeVariables, DeriveGeneric #-}
module DBRecord.MSSQL.Internal.Reify where
{-
>>>>>>> mssql-reify
      --  ( 
      --    getMSSQLDbSchemaInfo
      --  , defHints
      --  , columnNameHint
      --  , tableNameHint
      --  , databaseNameHint
      --  , uniqueNameHint
      --  , foreignKeyNameHint
      --  , seqNameHint
      --  , checkNameHint
      --  ) where

-- import DBRecord.Internal.Migration.Types hiding (CheckExpr)
import GHC.Generics
-- import Database.PostgreSQL.Simple.FromRow
-- import Database.PostgreSQL.Simple
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text  (Text)
import qualified Data.Text as T
import Data.Proxy
import qualified Data.List as L
import Data.Hashable
import Data.Attoparsec.Text (parseOnly)
import Data.Either (either)
import qualified DBRecord.Internal.PrimQuery as PQ
-- import DBRecord.Postgres.Internal.Sql.Parser
import DBRecord.MSSQL.Internal.Sql.Parser
import DBRecord.Internal.Sql.SqlGen (primExprGen)
import DBRecord.Internal.DDL ( createTable
                             , addPrimaryKey
                             , addUnique
                             , addForeignKey
                             , addCheck
                             , addDefault
                             , setNotNull
                             , column
                             , single
                             )
import Data.Coerce
import Data.Maybe
import DBRecord.Internal.Schema hiding (Sequence, dbTypeName, DbKeyName (..))
import qualified DBRecord.Internal.Schema as S
import qualified Data.HashMap.Strict as HM
import DBRecord.Internal.Lens
import Data.Int
import Data.Word
import DBRecord.Internal.Sql.DML (SqlExpr) 

import Database.MsSQL
import qualified Data.Vector.Storable as SV
import Data.Functor.Identity

import qualified Data.ByteString as BS
import Data.Text.Encoding
import Control.Exception (throwIO)
import DBRecord.Internal.Types

-- import Debug.Trace


data Test1 = Test1
        {
          -- tabName :: T.Text
          tabCount :: Int
        } deriving (Generic, Eq, Show)

instance FromRow Test1        



testConnectInfo :: Text -> ConnectInfo  
testConnectInfo dbNameStr = connectInfo "Driver={ODBC Driver 17 for SQL Server};Server=localhost;Database=TimeClockPlusv7;UID=sa;PWD=P@ssw0rd;ApplicationIntent=ReadOnly"

odbcConnectionString :: Text
odbcConnectionString = "Driver={ODBC Driver 17 for SQL Server};Server=localhost;Database=Chinook;UID=sa;PWD=P@ssw0rd;ApplicationIntent=ReadOnly"

getMsSQLDbInfo :: Text -> IO [(Text, DatabaseInfo)]
getMsSQLDbInfo dbNameStr = do
  eCon <- connect (testConnectInfo dbNameStr) 
  let con = 
        case eCon of
          Right c -> c
          -- TODO: ThrowIO instead of error
          Left sqlErr -> error $ "SQL Error while creating connection! " ++ (show sqlErr)

  schemaList <- getDataFromEither schemaListQ $ query con schemaListQ :: IO [SchemaName]
  mapM (\schemaData -> do
                let schemaName = T.toTitle $ sName schemaData
                tcols <- getDataFromEither (tableColQ schemaName) $ query con (tableColQ schemaName) :: IO [TableColInfo]
                tchks <- getDataFromEither (checksQ schemaName) $ query con (checksQ schemaName)
                prims <- getDataFromEither (primKeysQ schemaName) $ query con (primKeysQ schemaName) 
                uniqs <- getDataFromEither (uniqKeysQ schemaName) $ query con (uniqKeysQ schemaName) 
                fks <- getDataFromEither (foreignKeysQ schemaName) $ query con (foreignKeysQ schemaName) 

                let hints = defHints
                let tcis = (toTabColInfo hints tcols)
                pure ( schemaName
                      , toDatabaseInfo hints dbNameStr [] tcis
                                      (toCheckInfo hints tcis tchks)
                                      (toDefaultInfo hints tcols)
                                      (toPrimKeyInfo hints prims)
                                      (toUniqKeyInfo hints uniqs)
                                      (toForeignKeyInfo hints fks)
                      ) ) schemaList
  
 where
  getDataFromEither :: Query -> IO (Either SQLErrors (Vector a) ) -> IO [a]
  getDataFromEither currentQuery ioErrVec = do
   eErrVec <- ioErrVec
   case eErrVec of
    Left sqlErr -> throwIO sqlErr
    Right vec -> pure $ V.toList vec


-- main :: IO DatabaseInfo
-- main = do
--   let dockerConnectInfo = 
--         ConnectInfo {  
--                       ciHost = "127.0.0.1"
--                     ,  ciPort =  3306
--                     ,  ciDatabase = "Chinook" 
--                     ,  ciUser = "root" 
--                     ,  ciPassword = "password" 
--                     ,  ciCharset = utf8mb4_unicode_ci
--                     }
--   msSqlConn <- connect dockerConnectInfo
--   ioOK <- ping msSqlConn
--   putStrLn $ "SQL CONN : " ++ (show ioOK)
--   getMySQLDbSchemaInfo "" (Hints HM.empty) dockerConnectInfo

--  where
--   utf8mb4_unicode_ci :: Word8
--   utf8mb4_unicode_ci = 224



data EnumInfo = EnumInfo { enumTypeName :: Text
                         , enumCons     :: Vector Text
                         } deriving (Show, Eq, Generic)

data TableColInfo = TableColInfo { dbTableName  :: Text
                                 , dbColumnName :: Text
                                 , dbPosition :: Int32
                                 , dbColDefault :: Maybe Text
                                 , dbIsNullable :: BS.ByteString
                                 , dbTypeName :: Text
                                 , dbCharacterLength :: Maybe Int32
                                 , dbNumericPrecision :: Maybe Word8
                                 , dbNumericScale :: Maybe Int32
                                 , dbDateTimePrecision :: Maybe Int16
                                --  , dbIntervalPrecision :: Maybe Int
                                 } deriving (Show, Eq, Generic)

-- instance FromRow TableColInfo                                 

data CheckCtx = CheckCtx Text Text Text
               deriving (Show, Eq, Generic)

type CheckExpr = (Text, Text)

data PrimKey = PrimKey Text Text Text Int32
             deriving (Show, Eq, Ord, Generic)

data UniqKey = UniqKey Text Text Text Int32
             deriving (Show, Eq, Ord, Generic)

data FKey = FKey Text Text Text Int32 Text Text Text Int32
          deriving (Show, Eq, Ord, Generic)

data Seq = Seq Text Text Text Text Text Text Text (Maybe Text) (Maybe Text)
         deriving (Show, Eq, Ord, Generic)

type TableContent a = HM.HashMap Text [a]

toPrimKeyInfo :: Hints -> [PrimKey] -> HM.HashMap HaskName PrimaryKeyInfo
toPrimKeyInfo hints = HM.fromList . map toPrimKeyInfo . groupByTableName
  where groupByTableName = L.groupBy (\(PrimKey _ tna _ _) (PrimKey _ tnb _ _) -> tna == tnb)

        toPrimKeyInfo pks@(PrimKey kna tna _ _ : _) =
          let pki      = PrimaryKeyInfo { _pkeyName = kna
                                        , _pkeyColumns = getPKCols tna pks
                                        }
          in  ( tna, pki )
        toPrimKeyInfo []                            = error "impossible: empty group"
        
        getPKCols tna = map (mkHaskColumnName (colNameHints tna) . snd) . L.sortBy cmpByFst . map getPKCol
        getPKCol (PrimKey _ _ col i) = (i, col)
        cmpByFst a b = compare (fst a) (fst b)

        colNameHints tna = columnNameHints tna hints

toUniqKeyInfo :: Hints -> [UniqKey] -> TableContent UniqueInfo
toUniqKeyInfo hints = HM.fromListWith (++) . concatMap (map toUniqKeyInfo . groupByKeyName) . groupByTableName
  where groupByTableName = L.groupBy (\(UniqKey _ tna _ _) (UniqKey _ tnb _ _) -> tna == tnb)
        groupByKeyName   = L.groupBy (\(UniqKey kna _ _ _) (UniqKey knb _ _ _) -> kna == knb)

        toUniqKeyInfo uqs@(UniqKey kna tna _ _ : _) =
          let uqi      = UniqueInfo { _uqName = mkEntityName (mkHaskKeyName uniqNameHints kna) kna
                                    , _uqColumns = getUQCols tna uqs
                                    } 
          in  (tna, [uqi])
        toUniqKeyInfo [] = error "impossible: empty group"
        
        getUQCols tna = map (mkHaskColumnName (colNameHints tna) . snd) . L.sortBy cmpByFst . map getUQCol
        getUQCol (UniqKey _ _ col i) = (i, col)
        cmpByFst a b = compare (fst a) (fst b)

        tabNameHints = tableNameHints hints
        colNameHints tna = columnNameHints tna hints
        uniqNameHints = uniqueNameHints hints


toDatabaseInfo :: Hints -> DatabaseName -> [EnumInfo] -> TableContent ColumnInfo -> TableContent CheckInfo -> TableContent DefaultInfo -> HM.HashMap Text PrimaryKeyInfo -> TableContent UniqueInfo -> TableContent ForeignKeyInfo -> DatabaseInfo
toDatabaseInfo hints dbn eis cols chks defs pk uqs fks =
  let tabNs = HM.keys cols
      dbNameHints = databaseNameHints hints
      tabNameHints = tableNameHints hints      
      custTypeNameHints = customTypeNameHints hints      
      dbt = EntityName { _hsName = mkHaskTypeName dbNameHints dbn , _dbName = dbn }
      types = map (\ei ->
                    let et = parseMSSQLType False defSizeInfo (T.unpack etn)
                        etn = enumTypeName ei
                    in  mkTypeNameInfo et (EnumTypeNM etn (V.toList (enumCons ei)))
                  ) eis
      tabInfos = L.map (\dbTN ->
                         let tUqs = HM.lookupDefault [] dbTN uqs
                             tPk  = HM.lookup dbTN pk
                             tFks = HM.lookupDefault [] dbTN fks
                             tDefs = HM.lookupDefault [] dbTN defs
                             tChks = HM.lookupDefault [] dbTN chks
                             tCols = HM.lookupDefault [] dbTN cols
                         in TableInfo { _primaryKeyInfo = tPk
                                      , _foreignKeyInfo = tFks
                                      , _uniqueInfo     = tUqs
                                      , _defaultInfo    = tDefs
                                      , _checkInfo      = tChks
                                      , _sequenceInfo   = [] -- TODO: fill in
                                      , _tableName      = mkEntityName (mkHaskTypeName tabNameHints dbTN) dbTN
                                      , _columnInfo     = tCols
                                      , _ignoredCols    = ()
                                      }
                       ) tabNs
  in mkDatabaseInfo dbt types 0 0 (coerce tabInfos) MSSQL

toCheckInfo :: Hints -> TableContent ColumnInfo -> [CheckCtx] -> TableContent CheckInfo
toCheckInfo hints tcis = HM.fromListWith (++) . map chkInfo
  where chkInfo (CheckCtx chkName chkOn chkVal) =
          let ckExp = 
                      case basicParseExpr chkVal of
                        Left err -> error $ "Parse Error while parsing Check Expression: " ++ err 
                        Right _ -> PQ.RawExpr chkVal
              chkNameHints = checkKeyNameHints hints
          in ( chkOn
             , [ mkCheckInfo (mkEntityName (mkHaskKeyName chkNameHints chkName) chkName) ckExp])

        -- NOTE: Not null does not comes up as a constraint in MS-SQL

toDefaultInfo :: Hints -> [TableColInfo] -> TableContent DefaultInfo
toDefaultInfo hints = HM.fromListWith (++) . map defaultInfo
  where defaultInfo tci =
          ( dbTableName tci
          , case dbColDefault tci of
              Just colDefault -> 
                let defaultExpr = 
                                  case basicParseExpr colDefault of
                                    Right _ -> PQ.RawExpr colDefault
                                    Left err -> error $ "Parse Error while parsing Check Expression: " ++ err 
                    defInfo = mkDefaultInfo (colName tci) defaultExpr
                in [defInfo] -- [trace (show defInfo) defInfo]
              Nothing -> []
          )
        colName tci = mkHaskColumnName (colNameHints (dbTableName tci)) (dbColumnName tci)
        colNameHints tna = columnNameHints tna hints

toForeignKeyInfo :: Hints -> [FKey] -> TableContent ForeignKeyInfo
toForeignKeyInfo hints = HM.fromListWith (++) . concatMap (map toForeignKeyInfo . groupByKeyName) . groupByTableName
  where groupByTableName = L.groupBy (\(FKey _ tna _ _ _ _ _ _) (FKey _ tnb _ _ _ _ _ _) -> tna == tnb)
        groupByKeyName   = L.groupBy (\(FKey kna _ _ _ _ _ _ _) (FKey knb _ _ _ _ _ _ _) -> kna == knb)

        toForeignKeyInfo fks@(FKey kna tna _ _ _ rtna _ _ : _) =
          let fki = mkForeignKeyInfo (mkEntityName (mkHaskKeyName fkNameHints kna) kna)
                                     (getFKCols tna fks)
                                     (mkHaskTypeName tabNameHints rtna)
                                     (getFKRefCols fks)
          in (tna , [fki])
        toForeignKeyInfo []                                        = error "impossible: empty group"
        getFKCols tna = map (mkHaskColumnName (colNameHints tna) . snd) . L.sortBy cmpByFst . map getFKCol
        getFKCol (FKey _ _ col i _ _ _ _) = (i, col)

        getFKRefCols = map snd . L.sortBy cmpByFst . map getFKRefCol
        getFKRefCol (FKey _ _ _ _ _ rtna col i) = (i, mkHaskColumnName (colNameHints rtna) col)
        
        cmpByFst a b = compare (fst a) (fst b)

        tabNameHints = tableNameHints hints
        colNameHints tna = columnNameHints tna hints
        fkNameHints  = foreignKeyNameHints hints

toTabColInfo :: Hints -> [TableColInfo] -> TableContent ColumnInfo
toTabColInfo hints = HM.fromListWith (++) . map colInfo
  where nullable a = case decodeUtf8 $ dbIsNullable a of
          "YES" -> True
          "NO"  -> False
          _     -> error "Panic: Invalid nullability information from DB"

        colInfo tci =
          let ci = mkColumnInfo (mkEntityName (mkHaskColumnName (columnNameHints (dbTableName tci) hints) (dbColumnName tci)) (dbColumnName tci))
                                (coerce (parseMSSQLType (nullable tci) (sizeInfo tci) (T.unpack (dbTypeName tci))))
                   
          in ( dbTableName tci, [ci])

instance FromRow CheckCtx 
instance FromRow TableColInfo 
instance FromRow PrimKey 
instance FromRow UniqKey 
instance FromRow FKey 

data SchemaName = SchemaName
  {sName :: Text} deriving (Eq, Show, Generic)

instance FromRow SchemaName

-- This query gets a list of names of user-defined schemas
schemaListQ :: Query
schemaListQ =
  "select s.name as schema_name \
   \ from sys.schemas s\
    \ inner join sys.sysusers u\
    \  on u.uid = s.principal_id \
   \ where u.issqluser = 1\
    \ and u.name not in ('sys', 'guest', 'INFORMATION_SCHEMA');"


enumQ :: Query
enumQ =
  "SELECT pg_type.typname AS enumtype, array_agg(pg_enum.enumlabel) AS enumlabel \
   \FROM pg_type \
   \JOIN pg_enum \ 
     \ON pg_enum.enumtypid = pg_type.oid \
 \GROUP BY enumtype"

tableColQ :: Text -> Query
tableColQ schemaName =
  "SELECT col.table_name as table_name, \
        \col.column_name,  \
        \col.ordinal_position as pos,\ 
        \col.column_default,\  
        \col.is_nullable,\  
        \col.data_type,\  
        \col.character_maximum_length,\ 
        \col.numeric_precision,\ 
        \col.numeric_scale,\ 
        \col.datetime_precision \
 \FROM  (SELECT table_name,\  
               \column_name,\ 
               \ordinal_position,\ 
               \column_default,\ 
               \is_nullable,\  
               \DATA_TYPE,\
               \character_maximum_length,\ 
               \numeric_precision,\ 
               \numeric_scale,\ 
               \datetime_precision \
        \FROM information_schema.columns WHERE TABLE_SCHEMA='" <> schemaName <> "' \
         \) as col \
  \JOIN \
        \(SELECT table_name \
         \FROM information_schema.tables WHERE table_schema='" <> schemaName <> "' AND table_type='BASE TABLE') as tab \
  \ON col.table_name = tab.table_name \
  \ORDER BY table_name, pos;"

  
checksQ :: Text -> Query
checksQ schemaName =
  "SELECT cc.constraint_name, table_name, check_clause \
  \FROM information_schema.check_constraints AS cc \
  \JOIN information_schema.table_constraints AS tc \
  \ON   cc.constraint_name = tc.constraint_name AND \
       \cc.constraint_schema = tc.constraint_schema \
  \WHERE cc.constraint_schema = '"  <> schemaName <> "'"

primKeysQ :: Text -> Query
primKeysQ schemaName =
 "SELECT kcu.constraint_name as constraint_name, kcu.table_name, kcu.column_name, kcu.ordinal_position \
 \FROM  (SELECT constraint_name \
              \, table_name \
              \, column_name \
              \, ordinal_position \
        \FROM information_schema.key_column_usage \
        \WHERE constraint_schema = '" <> schemaName <> "' \
       \) as kcu \
  \JOIN  (SELECT constraint_name \
              \, table_name \
         \FROM information_schema.table_constraints \
         \WHERE constraint_type = 'PRIMARY KEY' AND constraint_schema = '"  <> schemaName <> "' \
        \) as tc \
  \ON    kcu.constraint_name = tc.constraint_name AND \
        \kcu.table_name = tc.table_name \
  \ORDER BY table_name, ordinal_position"

uniqKeysQ :: Text -> Query
uniqKeysQ schemaName =
 "SELECT kcu.constraint_name as constraint_name, kcu.table_name, kcu.column_name, kcu.ordinal_position \
 \FROM  (SELECT constraint_name \
              \, table_name \
              \, column_name \
              \, ordinal_position \
        \FROM information_schema.key_column_usage \
        \WHERE constraint_schema = '" <> schemaName <> "' \
       \) as kcu \
  \JOIN  (SELECT constraint_name \
              \, table_name \
         \FROM information_schema.table_constraints \
         \WHERE constraint_type = 'UNIQUE' AND constraint_schema = '" <> schemaName <> "' \
        \) as tc \
  \ON    kcu.constraint_name = tc.constraint_name AND \
        \kcu.table_name = tc.table_name \
  \ORDER BY table_name, constraint_name, ordinal_position"

foreignKeysQ :: Text -> Query
foreignKeysQ schemaName =
 "SELECT \
    \KCU1.CONSTRAINT_NAME AS FK_CONSTRAINT_NAME \
    \,KCU1.TABLE_NAME AS FK_TABLE_NAME \
    \,KCU1.COLUMN_NAME AS FK_COLUMN_NAME \
    \,KCU1.ORDINAL_POSITION AS FK_ORDINAL_POSITION \
    \,KCU2.CONSTRAINT_NAME AS REFERENCED_CONSTRAINT_NAME \
    \,KCU2.TABLE_NAME AS REFERENCED_TABLE_NAME \ 
    \,KCU2.COLUMN_NAME AS REFERENCED_COLUMN_NAME \
    \,KCU2.ORDINAL_POSITION AS REFERENCED_ORDINAL_POSITION \
 \FROM (SELECT * FROM INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS \
               \WHERE CONSTRAINT_SCHEMA = '" <> schemaName <> "') AS RC \
 \INNER JOIN (SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE \
                     \WHERE CONSTRAINT_SCHEMA = '" <> schemaName <> "' \
           \) AS KCU1 \
    \ON KCU1.CONSTRAINT_CATALOG = RC.CONSTRAINT_CATALOG \
    \AND KCU1.CONSTRAINT_SCHEMA = RC.CONSTRAINT_SCHEMA \
    \AND KCU1.CONSTRAINT_NAME = RC.CONSTRAINT_NAME \

 \INNER JOIN (SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE \
                     \WHERE CONSTRAINT_SCHEMA = '" <> schemaName <> "' \
           \) AS KCU2 \
    \ON KCU2.CONSTRAINT_CATALOG = RC.UNIQUE_CONSTRAINT_CATALOG \
    \AND KCU2.CONSTRAINT_SCHEMA = RC.UNIQUE_CONSTRAINT_SCHEMA \
    \AND KCU2.CONSTRAINT_NAME = RC.UNIQUE_CONSTRAINT_NAME \
    \AND KCU2.ORDINAL_POSITION = KCU1.ORDINAL_POSITION"

seqsQ :: Text -> Query
seqsQ schemaName =
  "SELECT sequence_schema.sequence_name as seq_name \
        \, sequence_schema.start_value as start_value \
        \, sequence_schema.minimum_value as min_value \ 
        \, sequence_schema.maximum_value as max_value \
        \, sequence_schema.increment as inc \
        \, sequence_schema.cycle_option as cycle_opt \
        \, sequence_schema.data_type as data_type \
        \, pg_seq_info.tab as seq_on_tab \
        \, pg_seq_info.col as seq_on_col FROM \
   \(SELECT * FROM information_schema.sequences where sequence_schema = '" <> schemaName <> "') as sequence_schema \
   \LEFT JOIN \
   \(select s.relname as seq, n.nspname as sch, t.relname as tab, a.attname as col \
   \from pg_class s \
     \join pg_depend d on d.objid=s.oid and d.classid='pg_class'::regclass and d.refclassid='pg_class'::regclass \
     \join pg_class t on t.oid=d.refobjid \
     \join pg_namespace n on n.oid=t.relnamespace \
     \join pg_attribute a on a.attrelid=t.oid and a.attnum=d.refobjsubid \
   \where s.relkind='S' and d.deptype='a' and n.nspname = '" <> schemaName <> "') as pg_seq_info \
   \ON pg_seq_info.seq = sequence_schema.sequence_name"
   
 
type DatabaseName = Text

unsafeParseExpr :: Text -> PQ.PrimExpr
unsafeParseExpr t = primExprGen . either parsePanic id . parseOnly sqlExpr $ t
  where parsePanic e = error $ "Panic while parsing: " ++ show e ++ " , " ++ "while parsing " ++ show t

basicParseExpr :: Text -> Either String SqlExpr
basicParseExpr t = parseOnly sqlExpr $ t


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

columnNameHint :: DBName -> DBName -> HaskName -> Hints -> Hints
columnNameHint dbTabN dbColN hsColN = Hints . HM.insert (ColumnName dbTabN dbColN) hsColN . getHints

tableNameHint :: DBName -> HaskName -> Hints -> Hints
tableNameHint dbN hsN = Hints . HM.insert (TableName dbN) hsN . getHints

databaseNameHint :: DBName -> HaskName -> Hints -> Hints
databaseNameHint dbN hsN = Hints . HM.insert (DatabaseName dbN) hsN . getHints

uniqueNameHint :: DBName -> HaskName -> Hints -> Hints
uniqueNameHint dbN hsN = Hints . HM.insert (UniqueName dbN) hsN . getHints

foreignKeyNameHint :: DBName -> HaskName -> Hints -> Hints
foreignKeyNameHint dbN hsN = Hints . HM.insert (ForeignKeyName dbN) hsN . getHints

seqNameHint :: DBName -> HaskName -> Hints -> Hints
seqNameHint dbN hsN = Hints . HM.insert (SeqName dbN) hsN . getHints

checkNameHint :: DBName -> HaskName -> Hints -> Hints
checkNameHint dbN hsN = Hints . HM.insert (CheckName dbN) hsN . getHints


data HintKey  = ColumnName     DBName DBName -- Tablename, Columnname
              | TableName      DBName
              | DatabaseName   DBName                
              | PrimaryKeyName DBName
              | UniqueName     DBName
              | CustomTypeName DBName
              | ForeignKeyName DBName
              | SeqName        DBName
              | CheckName      DBName
              deriving (Show, Eq, Ord, Generic)

instance Hashable HintKey

newtype Hints = Hints { getHints :: HM.HashMap HintKey HaskName }
              deriving (Show)

defHints :: Hints
defHints = Hints HM.empty

databaseNameHints :: Hints -> HM.HashMap DBName HaskName
databaseNameHints = HM.foldrWithKey dbNameHints HM.empty . getHints
  where dbNameHints (DatabaseName k) v m = HM.insert k v m
        dbNameHints _ _ m                = m

tableNameHints :: Hints -> HM.HashMap DBName HaskName
tableNameHints = HM.foldrWithKey tabNameHints HM.empty . getHints
  where tabNameHints (TableName k) v m = HM.insert k v m
        tabNameHints _ _ m             = m

columnNameHints :: DBName -> Hints -> HM.HashMap DBName HaskName
columnNameHints dbTabN = HM.foldrWithKey colNameHints HM.empty . getHints
  where colNameHints (ColumnName tab col) v m
          | dbTabN == tab = HM.insert col v m
          | otherwise    = m
        colNameHints _ _ m = m

uniqueNameHints :: Hints -> HM.HashMap DBName HaskName
uniqueNameHints = HM.foldrWithKey uniqNameHints HM.empty . getHints
  where uniqNameHints (UniqueName k) v m = HM.insert k v m
        uniqNameHints _ _ m              = m

foreignKeyNameHints :: Hints -> HM.HashMap DBName HaskName
foreignKeyNameHints = HM.foldrWithKey fkNameHints HM.empty . getHints
  where fkNameHints (ForeignKeyName k) v m = HM.insert k v m
        fkNameHints _ _ m                  = m

checkKeyNameHints :: Hints -> HM.HashMap DBName HaskName
checkKeyNameHints = HM.foldrWithKey ckNameHints HM.empty . getHints
  where ckNameHints (CheckName k) v m = HM.insert k v m
        ckNameHints _ _ m             = m

seqKeyNameHints :: Hints -> HM.HashMap DBName HaskName
seqKeyNameHints = HM.foldrWithKey seqNameHints HM.empty . getHints
  where seqNameHints (SeqName k) v m = HM.insert k v m
        seqNameHints _ _ m           = m

customTypeNameHints :: Hints -> HM.HashMap DBName HaskName
customTypeNameHints = HM.foldrWithKey ctNameHints HM.empty . getHints
  where ctNameHints (CustomTypeName k) v m = HM.insert k v m
        ctNameHints _ _ m                  = m

sizeInfo :: TableColInfo -> SizeInfo
sizeInfo tci =
  SizeInfo { szCharacterLength   = fromIntegral <$> dbCharacterLength tci
           , szNumericPrecision  = fromIntegral <$> dbNumericPrecision tci
           , szNumericScale      = fromIntegral <$> dbNumericScale tci
           , szDateTimePrecision = fromIntegral <$> dbDateTimePrecision tci
           , szIntervalPrecision = Nothing
          --  , szIntervalPrecision = fromIntegral <$> dbIntervalPrecision tci
           } 

-- data SizeInfo = SizeInfo { szCharacterLength :: Maybe Integer
--                          , szNumericPrecision :: Maybe Integer
--                          , szNumericScale :: Maybe Integer
--                          , szDateTimePrecision :: Maybe Integer
--                          , szIntervalPrecision :: Maybe Integer
--                          } deriving (Show, Eq)

-- defSizeInfo :: SizeInfo
-- defSizeInfo = SizeInfo Nothing Nothing Nothing Nothing Nothing

-}
