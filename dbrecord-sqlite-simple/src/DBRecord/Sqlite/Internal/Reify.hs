{-# LANGUAGE OverloadedStrings 
           , ScopedTypeVariables
           , DeriveGeneric
           , QuasiQuotes
           
#-}

module DBRecord.Sqlite.Internal.Reify where

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
import qualified Data.List as L
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
                                -- , dbTableName  :: Maybe Text
                                --  , dbCharacterLength :: Maybe Int
                                --  , dbNumericPrecision :: Maybe Int
                                --  , dbNumericScale :: Maybe Int
                                --  , dbDateTimePrecision :: Maybe Int
                                --  , dbIntervalPrecision :: Maybe Int
                                 } deriving (Show, Eq)

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
  fromRow = SqliteTabColInfo <$> field <*> field <*> field <*> field <*> field <*> field -- <*> field -- <*> field <*> field <*> field <*> field

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

getSqliteDbSchemaInfo :: SchemaName -> Hints -> String -> IO ()
getSqliteDbSchemaInfo sn hints dbLocation = do
    -- let dbN
    conn <- open "/Users/kahlil/projects/ByteAlly/tmp/testDatabase.db"
    (tableNameList :: [Only Text]) <- query conn getTableNames (Only "table":: Only Text)
    (tColInfos :: [[TableColInfo]] ) <- mapM 
      (\(Only tabName) -> do
        qResult <- query_ conn (tableColQ tabName)
        pure $ (fmap (toTabColInfos tabName) qResult)  ) tableNameList 
    -- (tColInfos ::[TableColInfo]) <- query_ conn tableColQ 
    print tColInfos
    print $ tableNameList
    close conn


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

getTableNames :: Query
getTableNames = 
 "SELECT name FROM sqlite_master WHERE type=? ORDER BY name;"

tableColQ :: Text -> Query
tableColQ tabName = Query $ T.pack [i|PRAGMA table_info(#{T.unpack tabName});|]
