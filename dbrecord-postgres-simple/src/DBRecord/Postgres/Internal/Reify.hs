{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, ScopedTypeVariables, DeriveGeneric #-}
module DBRecord.Postgres.Internal.Reify
       ( getPostgresDbSchemaInfo       
       ) where

-- import DBRecord.Internal.Migration.Types hiding (CheckExpr)
import GHC.Generics
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text  (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.Hashable
import Data.Attoparsec.Text (parseOnly)
import Data.Either (either)
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Postgres.Internal.Sql.Parser
import DBRecord.Internal.Sql.SqlGen (primExprGen)
import Data.Coerce
import Data.Maybe
import DBRecord.Internal.Schema hiding (Sequence, dbTypeName, DbKeyName (..), DatabaseName, SchemaName)
import qualified Data.HashMap.Strict as HM
import DBRecord.Internal.Lens
import DBRecord.Internal.Types

data EnumInfo = EnumInfo { enumTypeName :: Text
                         , enumCons     :: Vector Text
                         } deriving (Show, Eq, Generic)

data TableColInfo = TableColInfo { dbTableName  :: Text
                                 , dbColumnName :: Text
                                 , dbPosition :: Int
                                 , dbColDefault :: Maybe Text
                                 , dbIsNullable :: Text
                                 , dbTypeName :: Text
                                 , dbUdTypeName :: Text
                                 , dbCharacterLength :: Maybe Int
                                 , dbNumericPrecision :: Maybe Int
                                 , dbNumericScale :: Maybe Int
                                 , dbDateTimePrecision :: Maybe Int
                                 , dbIntervalPrecision :: Maybe Int
                                 , dbTableType :: Text
                                 , dbIsInsertableInto :: Text
                                 } deriving (Show, Eq)

data CheckCtx = CheckCtx Text Text Text
               deriving (Show, Eq, Generic)

data PrimKey = PrimKey Text Text Text Int
             deriving (Show, Eq, Ord, Generic)

data UniqKey = UniqKey Text Text Text Int
             deriving (Show, Eq, Ord, Generic)

data FKey = FKey Text Text Text Int Text Text Text Int
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
toUniqKeyInfo hints = HM.fromListWith (++) . concatMap (map toUQKInfo . groupByKeyName) . groupByTableName
  where groupByTableName = L.groupBy (\(UniqKey _ tna _ _) (UniqKey _ tnb _ _) -> tna == tnb)
        groupByKeyName   = L.groupBy (\(UniqKey kna _ _ _) (UniqKey knb _ _ _) -> kna == knb)

        toUQKInfo uqs@(UniqKey kna tna _ _ : _) =
          let uqi      = UniqueInfo { _uqName = mkEntityName (mkHaskKeyName uniqNameHints kna) kna
                                    , _uqColumns = getUQCols tna uqs
                                    } 
          in  (tna, [uqi])
        toUQKInfo [] = error "impossible: empty group"
        
        getUQCols tna = map (mkHaskColumnName (colNameHints tna) . snd) . L.sortBy cmpByFst . map getUQCol
        getUQCol (UniqKey _ _ coln i) = (i, coln)
        cmpByFst a b = compare (fst a) (fst b)

        colNameHints tna = columnNameHints tna hints
        uniqNameHints = uniqueNameHints hints

toSchemaInfo :: Hints -> DatabaseName -> SchemaName -> [EnumInfo] -> TableContent ColumnInfo -> HM.HashMap Text TableTypes -> TableContent CheckInfo -> TableContent DefaultInfo -> HM.HashMap Text PrimaryKeyInfo -> TableContent UniqueInfo -> TableContent ForeignKeyInfo -> SchemaInfo
toSchemaInfo hints dbn scn eis cols ttyps chks defs pk uqs fks = 
  let tabNs = HM.keys cols
      dbNameHints = databaseNameHints hints
      tabNameHints = tableNameHints hints      
      sct = EntityName { _hsName = mkHaskTypeName dbNameHints scn , _dbName = scn }
      dbk = Postgres
      dbt = EntityName { _hsName = mkHaskTypeName dbNameHints dbn , _dbName = dbn }
      types = map (\ei ->
                    let et = parsePGType False defSizeInfo (T.unpack etn)
                        etn = enumTypeName ei
                    in  mkTypeNameInfo et (EnumTypeNM (Just etn) (map (\a -> (a, a)) $ V.toList (enumCons ei)))
                  ) eis
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

toCheckInfo :: Hints -> TableContent ColumnInfo -> [CheckCtx] -> TableContent CheckInfo
toCheckInfo hints tcis = HM.fromListWith (++) . catMaybes . map chkInfo
  where chkInfo (CheckCtx chkName chkOn chkVal) =
          let ckExp = unsafeParseExpr chkVal
          in  case isNotNullCk tcis chkOn ckExp of
                True -> Nothing
                False -> Just ( chkOn
                              , [ mkCheckInfo (mkEntityName (mkHaskKeyName chkNameHints chkName) chkName) ckExp
                                ]
                              )

        -- NOTE: Not null also comes up as constraints
        isNotNullCk itcis chkOn (PQ.PostfixExpr PQ.OpIsNotNull (PQ.BaseTableAttrExpr coln)) =
          maybe False (const True) $ do
            itcis' <- HM.lookup chkOn itcis
            L.find (\tci -> (tci ^. columnNameInfo . dbName) == coln) itcis'          
        isNotNullCk _ _ _ = True
        chkNameHints = checkKeyNameHints hints

toDefaultInfo :: Hints -> [TableColInfo] -> TableContent DefaultInfo
toDefaultInfo _hints = HM.fromListWith (++) . map dInfo
  where dInfo tci =
          ( dbTableName tci
          , [ {-mkDefaultInfo (colName tci)
                            (unsafeParseExpr (fromJust (dbColDefault tci)))-}
            ]
          )
        -- colName tci = mkHaskColumnName (colNameHints (dbTableName tci)) (dbColumnName tci)
        -- colNameHints tna = columnNameHints tna hints

toForeignKeyInfo :: Hints -> [FKey] -> TableContent ForeignKeyInfo
toForeignKeyInfo hints = HM.fromListWith (++) . concatMap (map toFKInfo . groupByKeyName) . groupByTableName
  where groupByTableName = L.groupBy (\(FKey _ tna _ _ _ _ _ _) (FKey _ tnb _ _ _ _ _ _) -> tna == tnb)
        groupByKeyName   = L.groupBy (\(FKey kna _ _ _ _ _ _ _) (FKey knb _ _ _ _ _ _ _) -> kna == knb)

        toFKInfo fks@(FKey kna tna _ _ _ rtna _ _ : _) =
          let fki = mkForeignKeyInfo (mkEntityName (mkHaskKeyName fkNameHints kna) kna)
                                     (getFKCols tna fks)
                                     (mkHaskTypeName tabNameHints rtna)
                                     (getFKRefCols fks)
          in (tna , [fki])
        toFKInfo [] = error "impossible: empty group"
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
  where go tci = (dbTableName tci, ttype tci (dbTableType tci))
        ttype tci "VIEW" = case dbIsInsertableInto tci of
          "YES" -> UpdatableView
          _     -> NonUpdatableView
        ttype _ _ = BaseTable
          

toTabColInfo :: Hints -> [TableColInfo] -> TableContent ColumnInfo
toTabColInfo hints = HM.fromListWith (++) . map colInfo
  where nullable a = case dbIsNullable a of
          "YES" -> True
          "NO"  -> False
          _     -> error "Panic: Invalid nullability information from DB"

        colInfo tci =
          let ci = mkColumnInfo (mkEntityName (mkHaskColumnName (columnNameHints (dbTableName tci) hints) (dbColumnName tci)) (dbColumnName tci))
                                (coerce (parsePGType (nullable tci) (sizeInfo tci) typN))
              typN = T.unpack $ case dbTypeName tci of
                dty | dty == "USER-DEFINED" -> dbUdTypeName tci
                    | otherwise            -> dty
                   
          in ( dbTableName tci, [ci])

instance FromRow EnumInfo
instance FromRow TableColInfo where
  fromRow = TableColInfo <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow CheckCtx
instance FromRow PrimKey
instance FromRow UniqKey
instance FromRow FKey
instance FromRow Seq

type SchemaName   = Text
type DatabaseName = Text

getPostgresDbSchemaInfo :: ConnectInfo -> IO [SchemaInfo] 
getPostgresDbSchemaInfo connInfo = do
  let dbn = T.pack (connectDatabase connInfo)
  conn <- connect connInfo
  (scs :: [Only SchemaName]) <- query_ conn schemasQ  
  mapM (mkSchema conn dbn . fromOnly) scs
  
  where mkSchema conn dbn schn = do
          let hints = defHints
          enumTs <- query_ conn enumQ
          print tableColQ
          tcols <- query conn tableColQ (schn, schn)
          tchks <- query conn checksQ (Only schn)
          pkeys <- query conn primKeysQ (schn, schn)
          uniqs <- query conn uniqKeysQ (schn, schn)
          fks   <- query conn foreignKeysQ (schn, schn, schn)
          -- seqs  <- query conn seqsQ (schn, schn)
          let tcis = (toTabColInfo hints tcols)
          
          pure $ (toSchemaInfo hints dbn schn enumTs tcis
                               (toTableType tcols)
                               (toCheckInfo hints tcis tchks)
                               (toDefaultInfo hints tcols)
                               (toPrimKeyInfo hints pkeys)
                               (toUniqKeyInfo hints uniqs)
                               (toForeignKeyInfo hints fks)
                 )

          
unsafeParseExpr :: Text -> PQ.PrimExpr
unsafeParseExpr t = PQ.RawExpr t

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

{-
seqKeyNameHints :: Hints -> HM.HashMap DBName HaskName
seqKeyNameHints = HM.foldrWithKey seqNameHints HM.empty . getHints
  where seqNameHints (SeqName k) v m = HM.insert k v m
        seqNameHints _ _ m           = m
-}

sizeInfo :: TableColInfo -> SizeInfo
sizeInfo tci =
  SizeInfo { szCharacterLength   = fromIntegral <$> dbCharacterLength tci
           , szNumericPrecision  = fromIntegral <$> dbNumericPrecision tci
           , szNumericScale      = fromIntegral <$> dbNumericScale tci
           , szDateTimePrecision = fromIntegral <$> dbDateTimePrecision tci
           , szIntervalPrecision = fromIntegral <$> dbIntervalPrecision tci
           } 

enumQ :: Query
enumQ =
  "SELECT pg_type.typname AS enumtype, array_agg(pg_enum.enumlabel) AS enumlabel \
   \FROM pg_catalog.pg_type \
   \JOIN pg_enum \ 
     \ON pg_enum.enumtypid = pg_type.oid \
 \GROUP BY enumtype"

tableColQ :: Query
tableColQ =
 "SELECT col.table_name as table_name, \
        \col.column_name, \ 
        \col.ordinal_position as pos, \
        \col.column_default, \ 
        \col.is_nullable, \ 
        \col.data_type, \
        \col.udt_name, \        
        \col.character_maximum_length, \
        \col.numeric_precision, \
        \col.numeric_scale, \
        \col.datetime_precision, \
        \col.interval_precision, \        
        \tab.table_type, \
        \tab.is_insertable_into \
 \FROM  (SELECT table_name, \ 
               \column_name,\ 
               \ordinal_position, \
               \column_default, \
               \is_nullable, \ 
               \data_type, \
               \udt_name, \
               \character_maximum_length, \
               \numeric_precision, \
               \numeric_scale, \
               \datetime_precision, \
               \interval_precision \
        \FROM information_schema.columns \
        \WHERE table_schema= ? \
         \) as col \
  \JOIN \
        \(SELECT table_name, table_type, is_insertable_into \
         \FROM information_schema.tables WHERE table_schema= ? ) as tab \
  \ON col.table_name = tab.table_name \
  \ORDER BY table_name, pos"
  
checksQ :: Query
checksQ =
  "SELECT cc.constraint_name, table_name, check_clause \
  \FROM information_schema.check_constraints AS cc \
  \JOIN information_schema.table_constraints AS tc \
  \ON   cc.constraint_name = tc.constraint_name AND \
       \cc.constraint_schema = tc.constraint_schema \
  \WHERE cc.constraint_schema = ?"

primKeysQ :: Query
primKeysQ =
 "SELECT kcu.constraint_name as constraint_name, kcu.table_name, kcu.column_name, kcu.ordinal_position \
 \FROM  (SELECT constraint_name \
              \, table_name \
              \, column_name \
              \, ordinal_position \
        \FROM information_schema.key_column_usage \
        \WHERE constraint_schema = ? \
       \) as kcu \
  \JOIN  (SELECT constraint_name \
              \, table_name \
         \FROM information_schema.table_constraints \
         \WHERE constraint_type = 'PRIMARY KEY' AND constraint_schema = ? \
        \) as tc \
  \ON    kcu.constraint_name = tc.constraint_name AND \
        \kcu.table_name = tc.table_name \
  \ORDER BY table_name, ordinal_position"

uniqKeysQ :: Query
uniqKeysQ =
 "SELECT kcu.constraint_name as constraint_name, kcu.table_name, kcu.column_name, kcu.ordinal_position \
 \FROM  (SELECT constraint_name \
              \, table_name \
              \, column_name \
              \, ordinal_position \
        \FROM information_schema.key_column_usage \
        \WHERE constraint_schema = ? \
       \) as kcu \
  \JOIN  (SELECT constraint_name \
              \, table_name \
         \FROM information_schema.table_constraints \
         \WHERE constraint_type = 'UNIQUE' AND constraint_schema = ? \
        \) as tc \
  \ON    kcu.constraint_name = tc.constraint_name AND \
        \kcu.table_name = tc.table_name \
  \ORDER BY table_name, constraint_name, ordinal_position"

foreignKeysQ :: Query
foreignKeysQ =
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
               \WHERE CONSTRAINT_SCHEMA = ?) AS RC \
 \INNER JOIN (SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE \
                     \WHERE CONSTRAINT_SCHEMA = ? \
           \) AS KCU1 \
    \ON KCU1.CONSTRAINT_CATALOG = RC.CONSTRAINT_CATALOG \
    \AND KCU1.CONSTRAINT_SCHEMA = RC.CONSTRAINT_SCHEMA \
    \AND KCU1.CONSTRAINT_NAME = RC.CONSTRAINT_NAME \

 \INNER JOIN (SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE \
                     \WHERE CONSTRAINT_SCHEMA = ? \
           \) AS KCU2 \
    \ON KCU2.CONSTRAINT_CATALOG = RC.UNIQUE_CONSTRAINT_CATALOG \
    \AND KCU2.CONSTRAINT_SCHEMA = RC.UNIQUE_CONSTRAINT_SCHEMA \
    \AND KCU2.CONSTRAINT_NAME = RC.UNIQUE_CONSTRAINT_NAME \
    \AND KCU2.ORDINAL_POSITION = KCU1.ORDINAL_POSITION"

seqsQ :: Query
seqsQ =
  "SELECT sequence_schema.sequence_name as seq_name \
        \, sequence_schema.start_value as start_value \
        \, sequence_schema.minimum_value as min_value \ 
        \, sequence_schema.maximum_value as max_value \
        \, sequence_schema.increment as inc \
        \, sequence_schema.cycle_option as cycle_opt \
        \, sequence_schema.data_type as data_type \
        \, pg_seq_info.tab as seq_on_tab \
        \, pg_seq_info.col as seq_on_col FROM \
   \(SELECT * FROM information_schema.sequences where sequence_schema = ?) as sequence_schema \
   \LEFT JOIN \
   \(select s.relname as seq, n.nspname as sch, t.relname as tab, a.attname as col \
   \from pg_class s \
     \join pg_depend d on d.objid=s.oid and d.classid='pg_class'::regclass and d.refclassid='pg_class'::regclass \
     \join pg_class t on t.oid=d.refobjid \
     \join pg_namespace n on n.oid=t.relnamespace \
     \join pg_attribute a on a.attrelid=t.oid and a.attnum=d.refobjsubid \
   \where s.relkind='S' and d.deptype='a' and n.nspname = ?) as pg_seq_info \
   \ON pg_seq_info.seq = sequence_schema.sequence_name"
   
schemasQ :: Query
schemasQ =
  "SELECT schema_name from information_schema.schemata where \
   \schema_name <> 'information_schema' AND \
   \schema_name NOT LIKE 'pg_%'"
 
