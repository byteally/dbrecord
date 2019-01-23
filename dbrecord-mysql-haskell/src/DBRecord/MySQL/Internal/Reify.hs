{-# LANGUAGE OverloadedStrings
           , DuplicateRecordFields
           , ScopedTypeVariables
           , DeriveGeneric 
           , BangPatterns #-}

module DBRecord.MySQL.Internal.Reify where



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
import DBRecord.Internal.Schema hiding (Sequence, dbTypeName, DbKeyName (..))
import qualified DBRecord.Internal.Schema as S
import qualified Data.HashMap.Strict as HM
import DBRecord.Internal.Lens
import Data.Int

import Data.Word
import Database.MySQL.Base
import qualified System.IO.Streams as IOST
import Database.MySQL.Nem.QueryResults
import qualified Database.MySQL.Nem.Result as DBNem


type SchemaName = Text
type DatabaseName = Text

-- data EnumInfo = EnumInfo { enumTypeName :: Text
--                          , enumCons     :: Vector Text
--                          } deriving (Show, Eq)


data CheckCtx = CheckCtx Text Text Text
               deriving (Show, Eq)

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

data FKey = FKey Text Text Text Int Text Text Text Int
          deriving (Show, Eq, Ord)

data Seq = Seq Text Text Text Text Text Text Text (Maybe Text) (Maybe Text)
         deriving (Show, Eq, Ord)

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


main :: IO ()
main = do
  let dockerConnectInfo = 
        ConnectInfo {  
                      ciHost = "127.0.0.1"
                    ,  ciPort =  3306
                    ,  ciDatabase = "byteallytest" 
                    ,  ciUser = "root" 
                    ,  ciPassword = "password" 
                    ,  ciCharset = utf8mb4_unicode_ci
                    }
  mySqlConn <- connect dockerConnectInfo
  ioOK <- ping mySqlConn
  putStrLn $ "SQL CONN : " ++ (show ioOK)
  getMySQLDbSchemaInfo "" (Hints HM.empty) dockerConnectInfo

 where
  utf8mb4_unicode_ci :: Word8
  utf8mb4_unicode_ci = 224

dbSchemaNameQueryParams :: [MySQLValue]
dbSchemaNameQueryParams = [MySQLText "byteallytest", MySQLText "byteallytest"]

getMySQLDbSchemaInfo ::  SchemaName -> Hints -> ConnectInfo -> IO () 
getMySQLDbSchemaInfo sn hints connInfo = do
  -- let dbn = connect connInfo
  conn <- connect connInfo    
  -- enumTs <- query_ conn enumQ
  (tColDefList, iostTCols ) <- query conn tableColQ dbSchemaNameQueryParams
  tColInfos::[TableColInfo] <- processStreamAndParseResult iostTCols tColDefList
  putStrLn $ show tColInfos
  (primKeysDefList, iostPrims) <- query conn primKeysQ dbSchemaNameQueryParams
  tPrimKeys::[PrimKey] <- processStreamAndParseResult iostPrims primKeysDefList
  putStrLn $ show tPrimKeys

  putStrLn "Completed!"
  -- tchks <- query_ conn checksQ
  -- uniqs <- query_ conn uniqKeysQ
  -- fks   <- query_ conn foreignKeysQ
  -- (seqs :: [Seq])  <- query_ conn seqsQ
  -- let tcis = (toTabColInfo hints tcols)
  -- pure $ (toDatabaseInfo hints (T.pack dbn) enumTs tcis
  --                        (toCheckInfo hints tcis tchks)
  --                        (toDefaultInfo hints tcols)
  --                        (toPrimKeyInfo hints prims)
  --                        (toUniqKeyInfo hints uniqs)
  --                        (toForeignKeyInfo hints fks)
  --        )

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
      Just someVal  -> do
        putStrLn $ "Next val : " ++ (show someVal)
        pure (existingList ++ [someVal], False)


data User = User { firstName :: String, lastName :: String }

instance QueryResults User where
    convertResults [fa,fb] [va,vb] = User a b
        where !a = DBNem.convert fa va
              !b = DBNem.convert fb vb
    convertResults fs vs  = convertError fs vs 2


data TableColInfo = TableColInfo { dbTableName  :: Text
                                 , dbColumnName :: Text
                                 , dbPosition :: Int
                                 , dbColDefault :: Maybe Text
                                 , dbIsNullable :: Text
                                 , dbTypeName :: Text
                                 , dbLength :: Maybe Int
                                 } deriving (Show, Eq)

instance QueryResults TableColInfo where
  convertResults [fa, fb, fc, fd, fe, ff, fg] [va, vb ,vc ,vd ,ve ,vf ,vg] = TableColInfo dbTabName dbColName dbPos dbDefault dbIsNull dbTyName dbLen 
    where !dbTabName = DBNem.convert fa va
          !dbColName = DBNem.convert fb vb
          !dbPos = DBNem.convert fc vc
          !dbDefault = DBNem.convert fd vd
          !dbIsNull = DBNem.convert fe ve
          !dbTyName = DBNem.convert ff vf
          !dbLen = DBNem.convert fg vg
  convertResults fs vs = convertError fs vs 2


tableColQ ::  Query
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
               \data_type, \
               \character_maximum_length \
        \FROM information_schema.columns \
        \WHERE table_schema = ? \
         \) as col \
  \JOIN \
        \(SELECT table_name \
         \FROM information_schema.tables WHERE table_schema= ? AND table_type='BASE TABLE') as tab \
  \ON col.table_name = tab.table_name \
  \ORDER BY table_name, pos"



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