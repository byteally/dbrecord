{-# LANGUAGE OverloadedStrings
           , DuplicateRecordFields
           , ScopedTypeVariables
           , DeriveGeneric 
           , BangPatterns #-}

module DBRecord.MySQL.Internal.Reify where

{-
module DBRecord.MySQL.Internal.Reify ( getMySQLDbSchemaInfo
                                     ) where

import GHC.Generics
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
import DBRecord.Internal.Schema hiding (Sequence, dbTypeName, DbKeyName (..), SchemaName, DatabaseName)
import qualified DBRecord.Internal.Schema as S
import qualified Data.HashMap.Strict as HM
import DBRecord.Internal.Lens
import Data.Int

import Data.Word
import Database.MySQL.Base
import qualified System.IO.Streams as IOST
import Database.MySQL.Nem.QueryResults
import qualified Database.MySQL.Nem.Result as DBNem
import DBRecord.MySQL.Internal.Sql.Parser
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS

type SchemaName = Text
type DatabaseName = Text

data TableColInfo = TableColInfo { dbTableName  :: Text
                                 , dbColumnName :: Text
                                 , dbPosition :: Int
                                 , dbColDefault :: Maybe Text
                                 , dbIsNullable :: Text
                                 , dbTypeName :: Text
                                 , dbCharacterLength :: Maybe Int
                                 , dbNumericPrecision :: Maybe Int
                                 , dbNumericScale :: Maybe Int
                                 , dbDateTimePrecision :: Maybe Int
                                 , dbIntervalPrecision :: Maybe Int
                                 } deriving (Show, Eq)

instance QueryResults TableColInfo where
  convertResults [fa, fb, fc, fd, fe, ff, fg, fh, fi, fj] [va, vb ,vc ,vd ,ve ,vf ,vg, vh, vi, vj] = 
    TableColInfo dbTabName dbColName dbPos dbDefault dbIsNull dbTyName dbLen dbNumPrecision dbNumScale dbDTPrecision Nothing -- dbIntvlPrecision
    where !dbTabName = DBNem.convert fa va
          !dbColName = DBNem.convert fb vb
          !dbPos = DBNem.convert fc vc
          !dbDefault = DBNem.convert fd vd
          !dbIsNull = DBNem.convert fe ve
          !dbTyName = DBNem.convert ff vf
          !dbLen = DBNem.convert fg vg
          !dbNumPrecision = DBNem.convert fh vh
          !dbNumScale = DBNem.convert fi vi
          !dbDTPrecision = DBNem.convert fj vj
          -- !dbIntvlPrecision = DBNem.convert fk vk
  convertResults fs vs = convertError fs vs 2


data EnumInfo = EnumInfo { enumTypeName :: Text
                         , enumCons     :: Vector Text
                         } deriving (Show, Eq)


data CheckCtx = CheckCtx Text Text Text
               deriving (Show, Eq)

instance QueryResults CheckCtx where
    convertResults [fa,fb, fc] [va,vb, vc] = CheckCtx a b c
        where !a = DBNem.convert fa va
              !b = DBNem.convert fb vb
              !c = DBNem.convert fc vc


type CheckExpr = (Text, Text)

data PrimKey = PrimKey Text Text Text Int
             deriving (Show, Eq, Ord)

instance QueryResults PrimKey where
    convertResults [fa,fb, fc, fd] [va,vb, vc, vd] = PrimKey a b c d
        where !a = DBNem.convert fa va
              !b = DBNem.convert fb vb
              !c = DBNem.convert fc vc
              !d = DBNem.convert fd vd

    convertResults fs vs  = convertError fs vs 2

data UniqKey = UniqKey Text Text Text Int
             deriving (Show, Eq, Ord)

instance QueryResults UniqKey where
  convertResults [fa,fb, fc, fd] [va,vb, vc, vd] = UniqKey a b c d
    where !a = DBNem.convert fa va
          !b = DBNem.convert fb vb
          !c = DBNem.convert fc vc
          !d = DBNem.convert fd vd
  convertResults fs vs = convertError fs vs 2
  

data FKey = FKey Text Text Text Int Text Text Text Int
          deriving (Show, Eq, Ord)

instance QueryResults FKey where
  convertResults [fa,fb, fc, fd, fe, ff, fg, fh] [va, vb, vc, vd, ve, vf, vg, vh] =
    FKey a b c d e f g h
    where !a = DBNem.convert fa va
          !b = DBNem.convert fb vb
          !c = DBNem.convert fc vc
          !d = DBNem.convert fd vd
          !e = DBNem.convert fe ve
          !f = DBNem.convert ff vf
          !g = DBNem.convert fg vg
          !h = DBNem.convert fh vh
  convertResults fs vs = convertError fs vs 2
  


data Seq = Seq Text Text Text Text Text Text Text (Maybe Text) (Maybe Text)
         deriving (Show, Eq, Ord)

instance QueryResults Seq where
  convertResults [fa,fb, fc, fd, fe, ff, fg, fh, fi] [va, vb, vc, vd, ve, vf, vg, vh, vi] =
    Seq a b c d e f g h i
      where !a = DBNem.convert fa va
            !b = DBNem.convert fb vb
            !c = DBNem.convert fc vc
            !d = DBNem.convert fd vd
            !e = DBNem.convert fe ve
            !f = DBNem.convert ff vf
            !g = DBNem.convert fg vg
            !h = DBNem.convert fh vh
            !i = DBNem.convert fi vi
  convertResults fs vs = convertError fs vs 2
  

type TableContent a = HM.HashMap Text [a]


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
--   mySqlConn <- connect dockerConnectInfo
--   ioOK <- ping mySqlConn
--   putStrLn $ "SQL CONN : " ++ (show ioOK)
--   getMySQLDbSchemaInfo "" (Hints HM.empty) dockerConnectInfo

--  where
--   utf8mb4_unicode_ci :: Word8
--   utf8mb4_unicode_ci = 224

dbSchemaNameQueryParams :: Int -> BS.ByteString ->  [MySQLValue]
dbSchemaNameQueryParams n dbNameStr = replicate n $ MySQLText (decodeUtf8 dbNameStr)

getMySQLDbSchemaInfo ::  SchemaName -> Hints -> ConnectInfo -> IO DatabaseInfo 
getMySQLDbSchemaInfo sn hints connInfo = do
  let dbn = ciDatabase connInfo -- run query for this ? 
  conn <- connect connInfo    
  -- enumTs <- query_ conn enumQ
  (tColDefList, iostTCols ) <- query conn tableColQ (dbSchemaNameQueryParams 2 dbn)
  tColInfos::[TableColInfo] <- processStreamAndParseResult iostTCols tColDefList
  -- putStrLn $ show tColInfos
  (primKeysDefList, iostPrims) <- query conn primKeysQ (dbSchemaNameQueryParams 2 dbn)
  tPrimKeys::[PrimKey] <- processStreamAndParseResult iostPrims primKeysDefList
  -- putStrLn $ show tPrimKeys

  -- (checksDefList, iostChks) <- query conn checksQ [MySQLText "byteallytest"]
  -- tChks::[CheckCtx] <- processStreamAndParseResult iostChks checksDefList
  -- putStrLn $ show tChks

  (uqKeysDefList, iostUniques) <- query conn uniqKeysQ (dbSchemaNameQueryParams 2 dbn) 
  tUniques::[UniqKey] <- processStreamAndParseResult iostUniques uqKeysDefList
  -- putStrLn $ show tUniques

  (fkKeysDefList, iostFks) <- query conn foreignKeysQ (dbSchemaNameQueryParams 3 dbn)
  tFKey::[FKey] <- processStreamAndParseResult iostFks fkKeysDefList
  -- putStrLn $ show tFKey

  -- (seqDefList, iostSeqs) <- query conn seqsQ dbSchemaNameQueryParams
  -- tSeqs::[Seq] <- processStreamAndParseResult iostSeqs seqDefList
  -- putStrLn $ show tSeqs

  let tcis = (toTabColInfo hints tColInfos)

  pure $ toDatabaseInfo hints (decodeUtf8 dbn) [] tcis
                         -- (toCheckInfo hints tcis tChks)
                         -- NOTE : checks are left empty for now because we are not if MySQL supports check constraints.
                         HM.empty 
                         (toDefaultInfo hints tColInfos)
                         (toPrimKeyInfo hints tPrimKeys)
                         (toUniqKeyInfo hints tUniques)
                         (toForeignKeyInfo hints tFKey)

 where 
  processStreamAndParseResult :: QueryResults a => IOST.InputStream [MySQLValue] -> [ColumnDef] -> IO [a]
  processStreamAndParseResult ipStream colDefList = do
    mySqlValsList <- handleIOStream [] ipStream
    pure $ fmap (convertResults colDefList) mySqlValsList

  handleIOStream :: [[MySQLValue]] -> IOST.InputStream [MySQLValue] -> IO [[MySQLValue]]
  handleIOStream accSqlValList iostSQLVal = do
    mSqlVal <- IOST.read iostSQLVal
    (resultAcc, isEos) <- writeIOStream accSqlValList mSqlVal
    case isEos of
      True -> pure resultAcc
      False -> handleIOStream resultAcc iostSQLVal
  writeIOStream :: [[MySQLValue]] -> Maybe [MySQLValue] -> IO ([[MySQLValue]], Bool)
  writeIOStream existingList mSqlVal = 
    case mSqlVal of
      Nothing -> pure (existingList, True)
      Just someVal  -> pure (existingList ++ [someVal], False)
        


tableColQ ::  Query
tableColQ =
 "SELECT col.table_name as table_name, \
        \col.column_name, \ 
        \col.ordinal_position as pos, \
        \col.column_default, \ 
        \col.is_nullable, \ 
        \col.data_type, \ 
        \col.character_maximum_length, \
        \col.numeric_precision, \
        \col.numeric_scale, \
        \col.datetime_precision \
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
        \WHERE table_schema = ? \
         \) as col \
  \JOIN \
        \(SELECT table_name \
         \FROM information_schema.tables WHERE table_schema= ? AND table_type='BASE TABLE') as tab \
  \ON col.table_name = tab.table_name \
  \ORDER BY table_name, pos"

-- NOTE : Could not find support for interval precision in MySQL
        -- \col.interval_precision \
        -- \interval_precision \
 

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


-- checksQ :: Query
-- checksQ =
--   "SELECT cc.constraint_name, table_name, check_clause \
--   \FROM information_schema.check_constraints AS cc \
--   \JOIN information_schema.table_constraints AS tc \
--   \ON   cc.constraint_name = tc.constraint_name AND \
--        \cc.constraint_schema = tc.constraint_schema \
--   \WHERE cc.constraint_schema = ?"

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
   

unsafeParseExpr :: Text -> PQ.PrimExpr
unsafeParseExpr t = primExprGen . either parsePanic id . parseOnly sqlExpr $ t
  where parsePanic e = error $ "Panic while parsing: " ++ show e ++ " , " ++ "while parsing " ++ show t


toTabColInfo :: Hints -> [TableColInfo] -> TableContent ColumnInfo
toTabColInfo hints = HM.fromListWith (++) . map colInfo
  where nullable a = case dbIsNullable a of
          "YES" -> True
          "NO"  -> False
          _     -> error "Panic: Invalid nullability information from DB"

        colInfo tci =
          let ci = mkColumnInfo (mkEntityName (mkHaskColumnName (columnNameHints (dbTableName tci) hints) (dbColumnName tci)) (dbColumnName tci))
                                (coerce (parseMySQLType (nullable tci) (sizeInfo tci) (T.unpack (dbTypeName tci))))
                   
          in ( dbTableName tci, [ci])


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

toSchemaInfo :: Hints -> SchemaName -> [EnumInfo] -> TableContent ColumnInfo -> TableContent CheckInfo -> TableContent DefaultInfo -> HM.HashMap Text PrimaryKeyInfo -> TableContent UniqueInfo -> TableContent ForeignKeyInfo -> SchemaInfo
toSchemaInfo hints dbn eis cols chks defs pk uqs fks =
  let tabNs = HM.keys cols
      dbNameHints = databaseNameHints hints
      tabNameHints = tableNameHints hints      
      custTypeNameHints = customTypeNameHints hints      
      dbt = EntityName { _hsName = mkHaskTypeName dbNameHints dbn , _dbName = dbn }
      types = map (\ei ->
                    let et = parseMySQLType False defSizeInfo (T.unpack etn)
                        etn = enumTypeName ei
                    in  mkTypeNameInfo et (EnumTypeNM etn (V.toList (enumCons ei)))
                  ) eis
      tabInfos = L.map (\dbTN ->
                         let tUqs  = HM.lookupDefault [] dbTN uqs
                             tPk   = HM.lookup dbTN pk
                             tFks  = HM.lookupDefault [] dbTN fks
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
  in mkSchemaInfo sct types 0 0 (coerce tabInfos)

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
        isNotNullCk tcis chkOn (PQ.PostfixExpr PQ.OpIsNotNull (PQ.BaseTableAttrExpr coln)) =
          maybe False (const True) $ do
            tcis' <- HM.lookup chkOn tcis
            L.find (\tci -> (tci ^. columnNameInfo . dbName) == coln) tcis'          
        isNotNullCk _ _ _ = True
        chkNameHints = checkKeyNameHints hints

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

toDefaultInfo :: Hints -> [TableColInfo] -> TableContent DefaultInfo
toDefaultInfo hints = HM.fromListWith (++) . map defaultInfo
  where defaultInfo tci =
          ( dbTableName tci
          , [ {-mkDefaultInfo (colName tci)
                            (unsafeParseExpr (fromJust (dbColDefault tci)))-}
            ]
          )
        colName tci = mkHaskColumnName (colNameHints (dbTableName tci)) (dbColumnName tci)
        colNameHints tna = columnNameHints tna hints


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




sizeInfo :: TableColInfo -> SizeInfo
sizeInfo tci =
  SizeInfo { szCharacterLength   = fromIntegral <$> dbCharacterLength tci
           , szNumericPrecision  = fromIntegral <$> dbNumericPrecision tci
           , szNumericScale      = fromIntegral <$> dbNumericScale tci
           , szDateTimePrecision = fromIntegral <$> dbDateTimePrecision tci
           , szIntervalPrecision = fromIntegral <$> dbIntervalPrecision tci
           } 
-}
