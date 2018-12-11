{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, ScopedTypeVariables, DeriveGeneric #-}
module DBRecord.Internal.Migration.Validation
       ( getPostgresDbSchemaInfo
       , defHints
       , columnNameHint
       , tableNameHint
       , databaseNameHint
       , uniqueNameHint
       , foreignKeyNameHint
       , seqNameHint
       , checkNameHint
       ) where

-- import DBRecord.Internal.Migration.Types hiding (CheckExpr)
import GHC.Generics
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple
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
import DBRecord.Internal.Postgres.Parser
import DBRecord.Internal.Postgres.SqlGen (primExprGen)
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
import DBRecord.Internal.Schema hiding (Sequence, dbTypeName)
import qualified DBRecord.Internal.Schema as S
import qualified Data.HashMap.Strict as HM
import DBRecord.Internal.Lens
import Data.Int

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

data FKey = FKey Text Text Text Int Text Text Text Int
          deriving (Show, Eq, Ord)

data Seq = Seq Text Text Text Text Text Text Text (Maybe Text) (Maybe Text)
         deriving (Show, Eq, Ord)

type TableContent a = HM.HashMap Text [a]

toPrimKeyInfo :: Hints -> [PrimKey] -> HM.HashMap HaskName PrimaryKeyInfo
toPrimKeyInfo hints = HM.fromList . map toPrimKeyInfo . groupByTableName
  where groupByTableName = L.groupBy (\(PrimKey _ tna _ _) (PrimKey _ tnb _ _) -> tna == tnb)

        toPrimKeyInfo pks@(PrimKey kna tna _ _ : _) =
          let pki      = PrimaryKeyInfo { _pkeyName = kna
                                        , _pkeyColumns = getPKCols pks
                                        } 
          in  ( tna, pki )
        toPrimKeyInfo []                            = error "impossible: empty group"
        
        getPKCols = map (mkHaskColumnName colNameHints . snd) . L.sortBy cmpByFst . map getPKCol
        getPKCol (PrimKey _ _ col i) = (i, col)
        cmpByFst a b = compare (fst a) (fst b)

        colNameHints = columnNameHints hints

toUniqKeyInfo :: Hints -> [UniqKey] -> TableContent UniqueInfo
toUniqKeyInfo hints = HM.fromListWith (++) . concatMap (map toUniqKeyInfo . groupByKeyName) . groupByTableName
  where groupByTableName = L.groupBy (\(UniqKey _ tna _ _) (UniqKey _ tnb _ _) -> tna == tnb)
        groupByKeyName   = L.groupBy (\(UniqKey kna _ _ _) (UniqKey knb _ _ _) -> kna == knb)

        toUniqKeyInfo uqs@(UniqKey kna tna _ _ : _) =
          let uqi      = UniqueInfo { _uqName = mkEntityName (mkHaskKeyName uniqNameHints kna) kna
                                    , _uqColumns = getUQCols uqs
                                    } 
          in  (tna, [uqi])
        toUniqKeyInfo [] = error "impossible: empty group"
        
        getUQCols = map (mkHaskColumnName colNameHints . snd) . L.sortBy cmpByFst . map getUQCol
        getUQCol (UniqKey _ _ col i) = (i, col)
        cmpByFst a b = compare (fst a) (fst b)

        tabNameHints = tableNameHints hints
        colNameHints = columnNameHints hints
        uniqNameHints = uniqueNameHints hints


toDatabaseInfo :: Hints -> DatabaseName -> [EnumInfo] -> TableContent ColumnInfo -> TableContent CheckInfo -> TableContent DefaultInfo -> HM.HashMap Text PrimaryKeyInfo -> TableContent UniqueInfo -> TableContent ForeignKeyInfo -> DatabaseInfo
toDatabaseInfo hints dbn eis cols chks defs pk uqs fks =
  let tabNs = HM.keys cols
      dbNameHints = databaseNameHints hints
      tabNameHints = tableNameHints hints      
      custTypeNameHints = customTypeNameHints hints      
      dbt = EntityName { _hsName = mkHaskTypeName dbNameHints dbn , _dbName = dbn }
      types = map (\ei ->
                    mkTypeNameInfo (mkHaskTypeName custTypeNameHints (enumTypeName ei)) (EnumTypeNM (enumTypeName ei) (V.toList (enumCons ei)))
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
  in mkDatabaseInfo dbt types 0 0 (coerce tabInfos)

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
        isNotNullCk tcis chkOn (PQ.UnExpr PQ.OpIsNotNull (PQ.BaseTableAttrExpr coln)) =
          maybe False (const True) $ do
            tcis' <- HM.lookup chkOn tcis
            L.find (\tci -> (tci ^. columnNameInfo . dbName) == coln) tcis'          
        isNotNullCk _ _ _ = True
        chkNameHints = checkKeyNameHints hints

toDefaultInfo :: Hints -> [TableColInfo] -> TableContent DefaultInfo
toDefaultInfo hints = HM.fromListWith (++) . map defaultInfo
  where defaultInfo tci =
          ( dbTableName tci
          , [ {-mkDefaultInfo (colName tci)
                            (unsafeParseExpr (fromJust (dbColDefault tci)))-}
            ]
          )
        colName = mkHaskColumnName colNameHints . dbColumnName
        colNameHints = columnNameHints hints

toForeignKeyInfo :: Hints -> [FKey] -> TableContent ForeignKeyInfo
toForeignKeyInfo hints = HM.fromListWith (++) . concatMap (map toForeignKeyInfo . groupByKeyName) . groupByTableName
  where groupByTableName = L.groupBy (\(FKey _ tna _ _ _ _ _ _) (FKey _ tnb _ _ _ _ _ _) -> tna == tnb)
        groupByKeyName   = L.groupBy (\(FKey kna _ _ _ _ _ _ _) (FKey knb _ _ _ _ _ _ _) -> kna == knb)

        toForeignKeyInfo fks@(FKey kna tna _ _ _ rtna _ _ : _) =
          let fki = mkForeignKeyInfo (mkEntityName (mkHaskKeyName fkNameHints kna) kna)
                                     (getFKCols fks)
                                     (mkHaskTypeName tabNameHints rtna)
                                     (getFKRefCols fks)
          in (tna , [fki])
        toForeignKeyInfo []                                        = error "impossible: empty group"
        
        tabNameHints = tableNameHints hints
        getFKCols = map (mkHaskColumnName colNameHints . snd) . L.sortBy cmpByFst . map getFKCol
        getFKCol (FKey _ _ col i _ _ _ _) = (i, col)

        getFKRefCols = map (mkHaskColumnName colNameHints . snd) . L.sortBy cmpByFst . map getFKRefCol
        getFKRefCol (FKey _ _ _ _ _ _ col i) = (i, col)
        
        cmpByFst a b = compare (fst a) (fst b)

        colNameHints = columnNameHints hints
        fkNameHints  = foreignKeyNameHints hints

toTabColInfo :: Hints -> [TableColInfo] -> TableContent ColumnInfo
toTabColInfo hints = HM.fromListWith (++) . map colInfo
  where nullable a = case dbIsNullable a of
          "YES" -> True
          "NO"  -> False
          _     -> error "Panic: Invalid nullability information from DB"

        colInfo tci =
          let ci = mkColumnInfo (nullable tci)
                                (mkEntityName (mkHaskColumnName (columnNameHints hints) (dbColumnName tci)) (dbColumnName tci))
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
  fromRow = FKey <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Seq where
  fromRow = Seq <$> field <*> field <*> field <*> field
                     <*> field <*> field <*> field <*> field
                     <*> field

enumQ :: Query
enumQ =
  "SELECT pg_type.typname AS enumtype, array_agg(pg_enum.enumlabel) AS enumlabel \
   \FROM pg_type \
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
        \col.character_maximum_length \
 \FROM  (SELECT table_name, \ 
               \column_name,\ 
               \ordinal_position, \
               \column_default, \
               \is_nullable, \ 
               \CASE WHEN data_type = 'USER-DEFINED' THEN udt_name ELSE data_type END, \
               \character_maximum_length \
        \FROM information_schema.columns \
        \WHERE table_schema = 'public' \
         \) as col \
  \JOIN \
        \(SELECT table_name \
         \FROM information_schema.tables WHERE table_schema='public' AND table_type='BASE TABLE') as tab \
  \ON col.table_name = tab.table_name \
  \ORDER BY table_name, pos"
  
checksQ :: Query
checksQ =
  "SELECT cc.constraint_name, table_name, check_clause \
  \FROM information_schema.check_constraints AS cc \
  \JOIN information_schema.table_constraints AS tc \
  \ON   cc.constraint_name = tc.constraint_name AND \
       \cc.constraint_schema = tc.constraint_schema \
  \WHERE cc.constraint_schema = 'public'"

primKeysQ :: Query
primKeysQ =
 "SELECT kcu.constraint_name as constraint_name, kcu.table_name, kcu.column_name, kcu.ordinal_position \
 \FROM  (SELECT constraint_name \
              \, table_name \
              \, column_name \
              \, ordinal_position \
        \FROM information_schema.key_column_usage \
        \WHERE constraint_schema = 'public' \
       \) as kcu \
  \JOIN  (SELECT constraint_name \
              \, table_name \
         \FROM information_schema.table_constraints \
         \WHERE constraint_type = 'PRIMARY KEY' AND constraint_schema = 'public' \
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
        \WHERE constraint_schema = 'public' \
       \) as kcu \
  \JOIN  (SELECT constraint_name \
              \, table_name \
         \FROM information_schema.table_constraints \
         \WHERE constraint_type = 'UNIQUE' AND constraint_schema = 'public' \
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
               \WHERE CONSTRAINT_SCHEMA = 'public') AS RC \
 \INNER JOIN (SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE \
                     \WHERE CONSTRAINT_SCHEMA = 'public' \
           \) AS KCU1 \
    \ON KCU1.CONSTRAINT_CATALOG = RC.CONSTRAINT_CATALOG \
    \AND KCU1.CONSTRAINT_SCHEMA = RC.CONSTRAINT_SCHEMA \
    \AND KCU1.CONSTRAINT_NAME = RC.CONSTRAINT_NAME \

 \INNER JOIN (SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE \
                     \WHERE CONSTRAINT_SCHEMA = 'public' \
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
   \(SELECT * FROM information_schema.sequences where sequence_schema = 'public') as sequence_schema \
   \LEFT JOIN \
   \(select s.relname as seq, n.nspname as sch, t.relname as tab, a.attname as col \
   \from pg_class s \
     \join pg_depend d on d.objid=s.oid and d.classid='pg_class'::regclass and d.refclassid='pg_class'::regclass \
     \join pg_class t on t.oid=d.refobjid \
     \join pg_namespace n on n.oid=t.relnamespace \
     \join pg_attribute a on a.attrelid=t.oid and a.attnum=d.refobjsubid \
   \where s.relkind='S' and d.deptype='a' and n.nspname = 'public') as pg_seq_info \
   \ON pg_seq_info.seq = sequence_schema.sequence_name"
   
 
type SchemaName = Text
type DatabaseName = Text

getPostgresDbSchemaInfo ::  SchemaName -> Hints -> ConnectInfo -> IO DatabaseInfo 
getPostgresDbSchemaInfo sn hints connInfo = do
  let dbn = connectDatabase connInfo
  conn <- connect connInfo    
  enumTs <- query_ conn enumQ
  tcols <- query_ conn tableColQ
  tchks <- query_ conn checksQ
  prims <- query_ conn primKeysQ
  uniqs <- query_ conn uniqKeysQ
  fks   <- query_ conn foreignKeysQ
  (seqs :: [Seq])  <- query_ conn seqsQ
  let tcis = (toTabColInfo hints tcols)
  pure $ (toDatabaseInfo hints (T.pack dbn) enumTs tcis
                         (toCheckInfo hints tcis tchks)
                         (toDefaultInfo hints tcols)
                         (toPrimKeyInfo hints prims)
                         (toUniqKeyInfo hints uniqs)
                         (toForeignKeyInfo hints fks)
         )

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

mkHaskKeyName :: HM.HashMap Text Text -> Text -> Text
mkHaskKeyName nameHints dbName = fromMaybe (camelCase dbName) (HM.lookup dbName nameHints)

mkHaskColumnName :: HM.HashMap Text Text -> Text -> Text
mkHaskColumnName nameHints dbName = fromMaybe (camelCase dbName) (HM.lookup dbName nameHints)

mkHaskTypeNameRep :: HM.HashMap Text Text -> Text -> Text
mkHaskTypeNameRep nameHints dbName = fromMaybe (pascalCase dbName) (HM.lookup dbName nameHints)

mkHaskTypeName :: HM.HashMap Text Text -> Text -> S.TypeName Text
mkHaskTypeName typeNameHints dbName =
  mkTypeName "DBPackage" "DBModule" (mkHaskTypeNameRep typeNameHints dbName)
  
camelCase :: Text -> Text
camelCase = mconcat . headLower . splitName . T.toTitle
  where headLower (x : xs) = T.toLower x : xs
        headLower _        = []

pascalCase :: Text -> Text
pascalCase = mconcat . splitName . T.toTitle

splitName :: Text -> [Text]
splitName = filter (\a -> a /= "") . T.split (\x -> x == ' ' || x == '_')

columnNameHint :: DBName -> HaskName -> Hints -> Hints
columnNameHint dbN hsN = Hints . HM.insert (ColumnName dbN) hsN . getHints

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


data HintKey  = ColumnName     DBName
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

columnNameHints :: Hints -> HM.HashMap DBName HaskName
columnNameHints = HM.foldrWithKey colNameHints HM.empty . getHints
  where colNameHints (ColumnName k) v m = HM.insert k v m
        colNameHints _ _ m              = m

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

