{-# LANGUAGE OverloadedStrings 
           , ScopedTypeVariables
           , DeriveGeneric
           , QuasiQuotes
           
#-}

module DBRecord.Sqlite.Internal.Reify where

{-
import           Control.Applicative
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.FromField

import GHC.Generics
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text  (Text)
import qualified Data.Text as T
import qualified Data.List as DL
import Data.Hashable
import Data.Attoparsec.Text (parseOnly)
import Data.Either (either)
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Sql.SqlGen (primExprGen)

import Data.Coerce
import Data.Maybe
import DBRecord.Internal.Schema hiding (Sequence, dbTypeName, DbKeyName (..))
import Data.String.Interpolate

import qualified Data.HashMap.Strict as HM
import DBRecord.Internal.Lens
import DBRecord.Sqlite.Internal.Sql.Parser

data TestField = TestField Int T.Text deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

instance ToRow TestField where
  toRow (TestField id_ str) = toRow (id_, str)

main :: IO ()
main = do
  conn <- open "/Users/kahlil/projects/ByteAlly/tmp/testDatabase.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, str TEXT)"
  execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 2" :: String))
  execute conn "INSERT INTO test (id, str) VALUES (?,?)" (TestField 13 "test string 3")
  rowId <- lastInsertRowId conn
  executeNamed conn "UPDATE test SET str = :str WHERE id = :id" [":str" := ("updated str" :: T.Text), ":id" := rowId]
  r <- query_ conn "SELECT * from test" :: IO [TestField]
  mapM_ print r
  execute conn "DELETE FROM test WHERE id = ?" (Only rowId)
  close conn


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

data SqliteTabColInfo = SqliteTabColInfo { 
                                  colPosition :: Int
                                , colName :: Text
                                , colTypeName :: Text
                                , colIsNullable :: Int
                                , colDefault :: Maybe Text
                                , tabPKIdxVal :: Maybe Int
                                 } deriving (Show, Eq)


data SqliteFKInfo = SqliteFKInfo {
                                   sqlId :: Int
                                 , sqlSeqId :: Int
                                 , sqlRefTable :: Text
                                 , sqlRefCol :: Text
                                 , sqlCol :: Text
                                 , sqlOnUpdate :: Text
                                 , sqlOnDelete :: Text
                                 , sqlMatch :: Text
                                 } deriving (Eq, Show)              

data EnumInfo = EnumInfo { enumTypeName :: Text
                         , enumCons     :: Vector Text
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

data TabName = TabName Text

instance FromRow TabName where
    fromRow = TabName <$> field

-- instance ToRow TabName where 
--     toRow (TabName str) = str


-- instance FromRow EnumInfo where
--   fromRow = EnumInfo <$> field <*> field

instance FromRow SqliteTabColInfo where
  fromRow = SqliteTabColInfo <$> field <*> field <*> field <*> field <*> field <*> field

instance FromRow SqliteFKInfo where
  fromRow = SqliteFKInfo <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field -- <*> field <*> field <*> field

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

-- instance FromField String

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


mainTest = getSqliteDbSchemaInfo "" defHints "/Users/kahlil/projects/ByteAlly/tmp/testDatabase.db"

getSqliteDbSchemaInfo :: SchemaName -> Hints -> String -> IO DatabaseInfo
getSqliteDbSchemaInfo sn hints dbLocation = do
    -- let dbN
    conn <- open dbLocation
    (tableNameList :: [Only Text]) <- query conn getTableNames (Only "table":: Only Text)
    (tColInfos :: [[TableColInfo]] ) <- mapM 
      (\(Only tabName) -> do
        qResult <- query_ conn (tableColQ tabName)
        pure $ (fmap (toTabColInfos tabName) qResult)  ) tableNameList 
    -- (tColInfos ::[TableColInfo]) <- query_ conn tableColQ 
    let tColInfosConcat = DL.concat tColInfos

    pKeyInfos :: [[PrimKey]] <- mapM
      (\(Only tabName) -> do
        qRes <- query_ conn (fKeyQuery tabName)
        pure $ (DL.foldl' (mkPrimKey tabName) [] qRes) ) tableNameList
    let pKeyInfo = DL.concat pKeyInfos

    fKeyInfos :: [[FKey]] <- mapM
      (\(Only tabName) -> do
        qResult <- query_ conn (tableColQ tabName)
        pure $ (fmap (convertSqlFKInfoToFKey tabName) qResult)  ) tableNameList 
    let fKeysFinal = DL.concat fKeyInfos
    -- print pKeyInfo
    -- print fKeysFinal
    -- print $ tableNameList
    close conn
    let tcis = (toTabColInfo hints tColInfosConcat)
    pure $ (toDatabaseInfo hints (T.pack dbLocation) [] tcis
                          HM.empty --  (toCheckInfo hints tcis tchks)
                          HM.empty --  (toDefaultInfo hints tcols)
                           (toPrimKeyInfo hints pKeyInfo)
                           HM.empty --  (toUniqKeyInfo hints uniqs)
                           (toForeignKeyInfo hints fKeysFinal)
           )

mkPrimKey :: Text -> [PrimKey] -> SqliteTabColInfo -> [PrimKey]
mkPrimKey tabName accVal sqliteTabInfo = do
  let pKeyName = T.append tabName "PK"
  let mPKeyIdx = tabPKIdxVal sqliteTabInfo
  case mPKeyIdx of
    Nothing -> accVal
    Just pKeyIdx -> accVal ++ [PrimKey pKeyName tabName (colName sqliteTabInfo) 0] -- TODO : Delete if NA

toTabColInfos :: Text -> SqliteTabColInfo -> TableColInfo
toTabColInfos tableNameTxt sqliteInfo = 
  let tableName = tableNameTxt
      position = colPosition sqliteInfo
      columnName = colName sqliteInfo
      tyName = colTypeName sqliteInfo
      isNullable = 
        case colIsNullable sqliteInfo of
          0 -> "YES"
          1 -> "NO"
          ov -> error $ "Expected either 0 or 1 for Nullable info. Got : " ++ (show ov)
      colDefaultVal = colDefault sqliteInfo
  in TableColInfo {
                    dbTableName = tableName
                  , dbPosition = position
                  , dbColumnName = columnName
                  , dbTypeName = tyName
                  , dbIsNullable = isNullable
                  , dbColDefault = colDefaultVal
                  , dbCharacterLength = Nothing
                  , dbNumericPrecision = Nothing
                  , dbNumericScale = Nothing
                  , dbDateTimePrecision = Nothing
                  , dbIntervalPrecision = Nothing
                  }
      --  = tabPKIdxVal sqliteInfo
-- modify the query to handle the data that is returned.


convertSqlFKInfoToFKey :: Text -> SqliteFKInfo -> FKey
convertSqlFKInfoToFKey tabName singleFk = do
  -- fmap (\singleFk -> do 
      let refTableName = sqlRefTable singleFk
      let refColName = sqlRefCol singleFk
      let colName = sqlCol singleFk
      let seqId = sqlSeqId singleFk
      let fkId = sqlId singleFk
      let fkName = (T.append tabName (T.append "FK" (T.pack $ show seqId)) )
      FKey fkName tabName colName seqId "" refTableName refColName 0 -- )-- 0
      -- sqliteFkInfoList

getTableNames :: Query
getTableNames = 
 "SELECT name FROM sqlite_master WHERE type=? ORDER BY name;"

tableColQ :: Text -> Query
tableColQ tabName = Query $ T.pack [i|PRAGMA table_info(#{T.unpack tabName});|]


fKeyQuery :: Text -> Query
fKeyQuery tabName = Query $ T.pack [i|PRAGMA foreign_key_list(#{T.unpack tabName}) |]


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
                                (coerce (parseSqliteType (nullable tci) (sizeInfo tci) (T.unpack (dbTypeName tci))))
                   
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

toDatabaseInfo :: Hints -> DatabaseName -> [EnumInfo] -> TableContent ColumnInfo -> TableContent CheckInfo -> TableContent DefaultInfo -> HM.HashMap Text PrimaryKeyInfo -> TableContent UniqueInfo -> TableContent ForeignKeyInfo -> DatabaseInfo
toDatabaseInfo hints dbn eis cols chks defs pk uqs fks =
  let tabNs = HM.keys cols
      dbNameHints = databaseNameHints hints
      tabNameHints = tableNameHints hints      
      custTypeNameHints = customTypeNameHints hints      
      dbt = EntityName { _hsName = mkHaskTypeName dbNameHints dbn , _dbName = dbn }
      types = map (\ei ->
                    let et = parseSqliteType False defSizeInfo (T.unpack etn)
                        etn = enumTypeName ei
                    in  mkTypeNameInfo et (EnumTypeNM etn (V.toList (enumCons ei)))
                  ) eis
      tabInfos = DL.map (\dbTN ->
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
        isNotNullCk tcis chkOn (PQ.PostfixExpr PQ.OpIsNotNull (PQ.BaseTableAttrExpr coln)) =
          maybe False (const True) $ do
            tcis' <- HM.lookup chkOn tcis
            DL.find (\tci -> (tci ^. columnNameInfo . dbName) == coln) tcis'          
        isNotNullCk _ _ _ = True
        chkNameHints = checkKeyNameHints hints

toUniqKeyInfo :: Hints -> [UniqKey] -> TableContent UniqueInfo
toUniqKeyInfo hints = HM.fromListWith (++) . concatMap (map toUniqKeyInfo . groupByKeyName) . groupByTableName
  where groupByTableName = DL.groupBy (\(UniqKey _ tna _ _) (UniqKey _ tnb _ _) -> tna == tnb)
        groupByKeyName   = DL.groupBy (\(UniqKey kna _ _ _) (UniqKey knb _ _ _) -> kna == knb)

        toUniqKeyInfo uqs@(UniqKey kna tna _ _ : _) =
          let uqi      = UniqueInfo { _uqName = mkEntityName (mkHaskKeyName uniqNameHints kna) kna
                                    , _uqColumns = getUQCols tna uqs
                                    } 
          in  (tna, [uqi])
        toUniqKeyInfo [] = error "impossible: empty group"
        
        getUQCols tna = map (mkHaskColumnName (colNameHints tna) . snd) . DL.sortBy cmpByFst . map getUQCol
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
  where groupByTableName = DL.groupBy (\(PrimKey _ tna _ _) (PrimKey _ tnb _ _) -> tna == tnb)

        toPrimKeyInfo pks@(PrimKey kna tna _ _ : _) =
          let pki      = PrimaryKeyInfo { _pkeyName = kna
                                        , _pkeyColumns = getPKCols tna pks
                                        }
          in  ( tna, pki )
        toPrimKeyInfo []                            = error "impossible: empty group"
        
        getPKCols tna = map (mkHaskColumnName (colNameHints tna) . snd) . DL.sortBy cmpByFst . map getPKCol
        getPKCol (PrimKey _ _ col i) = (i, col)
        cmpByFst a b = compare (fst a) (fst b)

        colNameHints tna = columnNameHints tna hints


toForeignKeyInfo :: Hints -> [FKey] -> TableContent ForeignKeyInfo
toForeignKeyInfo hints = HM.fromListWith (++) . concatMap (map toForeignKeyInfo . groupByKeyName) . groupByTableName
  where groupByTableName = DL.groupBy (\(FKey _ tna _ _ _ _ _ _) (FKey _ tnb _ _ _ _ _ _) -> tna == tnb)
        groupByKeyName   = DL.groupBy (\(FKey kna _ _ _ _ _ _ _) (FKey knb _ _ _ _ _ _ _) -> kna == knb)

        toForeignKeyInfo fks@(FKey kna tna _ _ _ rtna _ _ : _) =
          let fki = mkForeignKeyInfo (mkEntityName (mkHaskKeyName fkNameHints kna) kna)
                                     (getFKCols tna fks)
                                     (mkHaskTypeName tabNameHints rtna)
                                     (getFKRefCols fks)
          in (tna , [fki])
        toForeignKeyInfo []                                        = error "impossible: empty group"
        getFKCols tna = map (mkHaskColumnName (colNameHints tna) . snd) . DL.sortBy cmpByFst . map getFKCol
        getFKCol (FKey _ _ col i _ _ _ _) = (i, col)

        getFKRefCols = map snd . DL.sortBy cmpByFst . map getFKRefCol
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
