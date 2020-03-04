{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, ScopedTypeVariables, DeriveGeneric #-}
module DBRecord.MSSQL.Internal.Reify
       ( getMsSQLDatabaseInfo
       , badQuery
       ) where

import GHC.Generics
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text  (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.Hashable
import Data.Attoparsec.Text (parseOnly)
import Data.Either (either)
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.MSSQL.Internal.Sql.Parser
import Data.Coerce
import DBRecord.Internal.Schema hiding (Sequence, DbKeyName (..), DatabaseName, SchemaName)
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Word
import DBRecord.Internal.Sql.DML (SqlExpr) 
import Database.MsSQL
import qualified Data.ByteString as BS
import Data.Text.Encoding
import Control.Exception (throwIO)
import DBRecord.Internal.Types
import Control.Monad.IO.Class

badQuery :: ConnectInfo -> IO ()
badQuery connInfo = do
  eCon <- connect connInfo
  let con = 
        case eCon of
          Right c -> c
          -- TODO: ThrowIO instead of error
          Left sqlErr -> error $ "SQL Error while creating connection! " ++ (show sqlErr)
  
  print ("QUERY: " ++ show tableColQ1)
  tcols <- query con tableColQ1
  print (tcols :: Either SQLErrors (V.Vector TableColInfo1))

getMsSQLDatabaseInfo :: Text -> ConnectInfo -> IO [SchemaInfo] 
getMsSQLDatabaseInfo dbN connInfo = do
  eCon <- connect connInfo
  let con = 
        case eCon of
          Right c -> c
          -- TODO: ThrowIO instead of error
          Left sqlErr -> error $ "SQL Error while creating connection! " ++ (show sqlErr)

  schemaList <- go con schemaListQ :: IO [SchemaName]
  print schemaList
  mapM (mkSchema con) schemaList
  
 where
  go1 = either throwIO (pure . V.toList)
  go con q = do
    liftIO $ print q
    query con q >>= go1

  mkSchema con schemaData = do
    let scn = T.toTitle qschn
        qschn = sName schemaData
    tcols <- go con (tableColQ qschn)
    tchks <- go con (checksQ qschn)
    prims <- go con (primKeysQ qschn) 
    uniqs <- go con (uniqKeysQ qschn) 
    fks <-   go con (foreignKeysQ qschn)
    let hints = defHints
    let tcis = toTabColInfo hints tcols
    pure (toSchemaInfo hints dbN (coerce scn) [] tcis
                       (toTableType tcols)
                       (toCheckInfo hints tchks)
                       (toDefaultInfo hints tcols)
                       (toPrimKeyInfo hints prims)
                       (toUniqKeyInfo hints uniqs)
                       (toForeignKeyInfo hints fks)
         )

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
                                 , dbTableType :: ASCIIText
                                 } deriving (Show, Eq, Generic)

-- instance FromRow TableColInfo                                 

data CheckCtx = CheckCtx Text Text Text
               deriving (Show, Eq, Generic)

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
toPrimKeyInfo hints = HM.fromList . map toPKInfo . groupByTableName
  where groupByTableName = L.groupBy (\(PrimKey _ tna _ _) (PrimKey _ tnb _ _) -> tna == tnb)


        toPKInfo pks@(PrimKey kna tna _ _ : _) =
          let pki      = PrimaryKeyInfo { _pkeyName = kna
                                        , _pkeyColumns = getPKCols tna pks
                                        }
          in  ( tna, pki )
        toPKInfo [] = error "impossible: empty group"
        
        getPKCols tna = map (mkHaskColumnName (colNameHints tna) . snd) . L.sortBy cmpByFst . map getPKCol
        getPKCol (PrimKey _ _ coln i) = (i, coln)
        cmpByFst a b = compare (fst a) (fst b)

        colNameHints tna = columnNameHints tna hints

toUniqKeyInfo :: Hints -> [UniqKey] -> TableContent UniqueInfo
toUniqKeyInfo hints = HM.fromListWith (++) . concatMap (map toUqKeyInfo . groupByKeyName) . groupByTableName
  where groupByTableName = L.groupBy (\(UniqKey _ tna _ _) (UniqKey _ tnb _ _) -> tna == tnb)
        groupByKeyName   = L.groupBy (\(UniqKey kna _ _ _) (UniqKey knb _ _ _) -> kna == knb)

        toUqKeyInfo uqs@(UniqKey kna tna _ _ : _) =
          let uqi      = UniqueInfo { _uqName = mkEntityName (mkHaskKeyName uniqNameHints kna) kna
                                    , _uqColumns = getUQCols tna uqs
                                    } 
          in  (tna, [uqi])
        toUqKeyInfo [] = error "impossible: empty group"
        
        getUQCols tna = map (mkHaskColumnName (colNameHints tna) . snd) . L.sortBy cmpByFst . map getUQCol
        getUQCol (UniqKey _ _ coln i) = (i, coln)
        cmpByFst a b = compare (fst a) (fst b)

        colNameHints tna = columnNameHints tna hints
        uniqNameHints = uniqueNameHints hints


toSchemaInfo :: Hints -> DatabaseName -> SchemaName -> [EnumInfo] -> TableContent ColumnInfo -> HM.HashMap Text TableTypes -> TableContent CheckInfo -> TableContent DefaultInfo -> HM.HashMap Text PrimaryKeyInfo -> TableContent UniqueInfo -> TableContent ForeignKeyInfo -> SchemaInfo
toSchemaInfo hints dbn scn _eis cols ttyps chks defs pk uqs fks =
  let tabNs = HM.keys cols
      dbNameHints = databaseNameHints hints
      tabNameHints = tableNameHints hints
      sct = EntityName { _hsName = mkHaskTypeName dbNameHints (coerce scn) , _dbName = coerce scn }
      types = []
      dbk = MSSQL
      dbt = EntityName { _hsName = mkHaskTypeName dbNameHints dbn , _dbName = dbn }      
      tabInfos = L.map (\dbTN ->
                         let tUqs = HM.lookupDefault [] dbTN uqs
                             tPk  = HM.lookup dbTN pk
                             tFks = HM.lookupDefault [] dbTN fks
                             tDefs = HM.lookupDefault [] dbTN defs
                             tChks = HM.lookupDefault [] dbTN chks
                             tCols = HM.lookupDefault [] dbTN cols
                             ttyp = HM.lookupDefault BaseTable dbTN ttyps                             
                         in TableInfo { _primaryKeyInfo = tPk
                                      , _foreignKeyInfo = tFks
                                      , _uniqueInfo     = tUqs
                                      , _defaultInfo    = tDefs
                                      , _checkInfo      = tChks
                                      , _sequenceInfo   = [] -- TODO: fill in
                                      , _tableName      = mkEntityName (mkHaskTypeName tabNameHints dbTN) dbTN
                                      , _columnInfo     = tCols
                                      , _ignoredCols    = ()
                                      , _tableType      = ttyp                                      
                                      }
                       ) tabNs
  in mkSchemaInfo sct types 0 0 (coerce tabInfos) dbt dbk

toCheckInfo :: Hints -> [CheckCtx] -> TableContent CheckInfo
toCheckInfo hints = HM.fromListWith (++) . map chkInfo
  where chkInfo (CheckCtx chkName chkOn chkVal) =
          let ckExp = PQ.RawExpr chkVal
              chkNameHints = checkKeyNameHints hints
          in ( chkOn
             , [ mkCheckInfo (mkEntityName (mkHaskKeyName chkNameHints chkName) chkName) ckExp])

        -- NOTE: Not null does not comes up as a constraint in MS-SQL

toDefaultInfo :: Hints -> [TableColInfo] -> TableContent DefaultInfo
toDefaultInfo hints = HM.fromListWith (++) . map dInfo
  where dInfo tci =
          ( dbTableName tci
          , case dbColDefault tci of
              Just colDefault -> 
                let defaultExpr = PQ.RawExpr colDefault
                    defInfo = mkDefaultInfo (haskColName tci) defaultExpr
                in [defInfo] -- [trace (show defInfo) defInfo]
              Nothing -> []
          )
        haskColName tci = mkHaskColumnName (colNameHints (dbTableName tci)) (dbColumnName tci)
        colNameHints tna = columnNameHints tna hints

toForeignKeyInfo :: Hints -> [FKey] -> TableContent ForeignKeyInfo
toForeignKeyInfo hints = HM.fromListWith (++) . concatMap (map toFK . groupByKeyName) . groupByTableName
  where groupByTableName = L.groupBy (\(FKey _ tna _ _ _ _ _ _) (FKey _ tnb _ _ _ _ _ _) -> tna == tnb)
        groupByKeyName   = L.groupBy (\(FKey kna _ _ _ _ _ _ _) (FKey knb _ _ _ _ _ _ _) -> kna == knb)

        toFK fks@(FKey kna tna _ _ _ rtna _ _ : _) =
          let fki = mkForeignKeyInfo (mkEntityName (mkHaskKeyName fkNameHints kna) kna)
                                     (getFKCols tna fks)
                                     (mkHaskTypeName tabNameHints rtna)
                                     (getFKRefCols fks)
          in (tna , [fki])
        toFK [] = error "impossible: empty group"
        
        getFKCols tna = map (mkHaskColumnName (colNameHints tna) . snd) . L.sortBy cmpByFst . map getFKCol
        getFKCol (FKey _ _ coln i _ _ _ _) = (i, coln)

        getFKRefCols = map snd . L.sortBy cmpByFst . map getFKRefCol
        getFKRefCol (FKey _ _ _ _ _ rtna coln i) = (i, mkHaskColumnName (colNameHints rtna) coln)
        
        cmpByFst a b = compare (fst a) (fst b)

        tabNameHints = tableNameHints hints
        colNameHints tna = columnNameHints tna hints
        fkNameHints  = foreignKeyNameHints hints

toTableType :: [TableColInfo] -> HM.HashMap Text TableTypes
toTableType = HM.fromList . map go
  where go tci = (dbTableName tci, ttype tci (getAsciiText (dbTableType tci)))
        ttype _ "BASE TABLE" = BaseTable
        ttype _tci _ = NonUpdatableView

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

newtype SchemaName = SchemaName
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

tableColQ :: Text -> Query
tableColQ scn =
 "SELECT col.table_name as table_name, \
        \col.column_name, \ 
        \col.ordinal_position as pos, \
        \col.column_default, \ 
        \col.is_nullable, \ 
        \col.data_type, \
        \col.character_maximum_length, \
        \col.numeric_precision, \
        \col.numeric_scale, \
        \col.datetime_precision, \
        \tab.table_type \
 \FROM  (SELECT table_name, \ 
               \column_name,\ 
               \ordinal_position, \
               \column_default, \
               \is_nullable, \ 
               \data_type, \
               \character_maximum_length, \
               \numeric_precision, \
               \numeric_scale, \
               \datetime_precision \
        \FROM information_schema.columns \
        \WHERE table_schema='"  <> scn <> "' \
         \) as col \
  \JOIN \
        \(SELECT table_name, table_type \
         \FROM information_schema.tables WHERE table_schema= '"  <> scn <> "') as tab \
  \ON col.table_name = tab.table_name \
  \ORDER BY table_name, pos"

checksQ :: Text -> Query
checksQ scn =
  "SELECT cc.constraint_name, table_name, check_clause \
  \FROM information_schema.check_constraints AS cc \
  \JOIN information_schema.table_constraints AS tc \
  \ON   cc.constraint_name = tc.constraint_name AND \
       \cc.constraint_schema = tc.constraint_schema \
  \WHERE cc.constraint_schema = '"  <> scn <> "'"

primKeysQ :: Text -> Query
primKeysQ scn =
 "SELECT kcu.constraint_name as constraint_name, kcu.table_name, kcu.column_name, kcu.ordinal_position \
 \FROM  (SELECT constraint_name \
              \, table_name \
              \, column_name \
              \, ordinal_position \
        \FROM information_schema.key_column_usage \
        \WHERE constraint_schema = '" <> scn <> "' \
       \) as kcu \
  \JOIN  (SELECT constraint_name \
              \, table_name \
         \FROM information_schema.table_constraints \
         \WHERE constraint_type = 'PRIMARY KEY' AND constraint_schema = '"  <> scn <> "' \
        \) as tc \
  \ON    kcu.constraint_name = tc.constraint_name AND \
        \kcu.table_name = tc.table_name \
  \ORDER BY table_name, ordinal_position"

uniqKeysQ :: Text -> Query
uniqKeysQ scn =
 "SELECT kcu.constraint_name as constraint_name, kcu.table_name, kcu.column_name, kcu.ordinal_position \
 \FROM  (SELECT constraint_name \
              \, table_name \
              \, column_name \
              \, ordinal_position \
        \FROM information_schema.key_column_usage \
        \WHERE constraint_schema = '" <> scn <> "' \
       \) as kcu \
  \JOIN  (SELECT constraint_name \
              \, table_name \
         \FROM information_schema.table_constraints \
         \WHERE constraint_type = 'UNIQUE' AND constraint_schema = '" <> scn <> "' \
        \) as tc \
  \ON    kcu.constraint_name = tc.constraint_name AND \
        \kcu.table_name = tc.table_name \
  \ORDER BY table_name, constraint_name, ordinal_position"

foreignKeysQ :: Text -> Query
foreignKeysQ scn =
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
               \WHERE CONSTRAINT_SCHEMA = '" <> scn <> "') AS RC \
 \INNER JOIN (SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE \
                     \WHERE CONSTRAINT_SCHEMA = '" <> scn <> "' \
           \) AS KCU1 \
    \ON KCU1.CONSTRAINT_CATALOG = RC.CONSTRAINT_CATALOG \
    \AND KCU1.CONSTRAINT_SCHEMA = RC.CONSTRAINT_SCHEMA \
    \AND KCU1.CONSTRAINT_NAME = RC.CONSTRAINT_NAME \

 \INNER JOIN (SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE \
                     \WHERE CONSTRAINT_SCHEMA = '" <> scn <> "' \
           \) AS KCU2 \
    \ON KCU2.CONSTRAINT_CATALOG = RC.UNIQUE_CONSTRAINT_CATALOG \
    \AND KCU2.CONSTRAINT_SCHEMA = RC.UNIQUE_CONSTRAINT_SCHEMA \
    \AND KCU2.CONSTRAINT_NAME = RC.UNIQUE_CONSTRAINT_NAME \
    \AND KCU2.ORDINAL_POSITION = KCU1.ORDINAL_POSITION"

type DatabaseName = Text

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
  where colNameHints (ColumnName tab coln) v m
          | dbTabN == tab = HM.insert coln v m
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

data TableColInfo1 = TableColInfo1 { -- _dbTableName  :: Text
                                   -- _dbColumnName :: Text
                                   -- dbPosition :: Int32
                                   -- _dbColDefault :: Maybe Text
                                   -- _dbIsNullable :: BS.ByteString
                                   -- , dbTypeName :: Text
                                   -- , dbCharacterLength :: Maybe Int32
                                   -- , dbNumericPrecision :: Maybe Word8
                                   -- , dbNumericScale :: Maybe Int32
                                   -- , dbDateTimePrecision :: Maybe Int16
                                   -- dbTableType :: Text
                                   -- dbIsInsertableInto :: Text
                                   } deriving (Show, Eq, Generic)
instance FromRow TableColInfo1


tableColQ1 :: Query
tableColQ1 =
 "SELECT col.is_nullable \ 
 \FROM  (SELECT table_name, \ 
               \column_name,\ 
               \ordinal_position, \
               \column_default, \
               \is_nullable, \ 
               \data_type, \
               \character_maximum_length, \
               \numeric_precision, \
               \numeric_scale, \
               \datetime_precision \
        \FROM information_schema.columns \
        \WHERE table_schema='dbo' \
         \) as col \
  \JOIN \
        \(SELECT table_name, table_type \
         \FROM information_schema.tables WHERE table_schema='dbo') as tab \
  \ON col.table_name = tab.table_name \
  \ORDER BY tab.table_name"
