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
                                         , addCheckExpr
                                         , addDefaultExpr
                                         , addNotNull
                                         , column
                                         , single
                                         )
import qualified DBRecord.Internal.Migration.Types as MT
import Data.Coerce
import Data.Maybe


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


data TableInfo = TableInfo { tableName   :: Text
                           , columns     :: [ColumnInfo]
                           , checks      :: [CheckInfo]
                           , defaults    :: [DefaultInfo]
                           , primaryKeys :: [PrimKeyInfo]
                           , uniqueKeys  :: [UniqKeyInfo]
                           , foreignKeys :: [ForeignKeyInfo]
                           } deriving (Show)

data ColumnInfo = ColumnInfo { columnName :: Text
                             , typeN :: Text
                             , pos  :: Int
                             , colIsNullable :: Bool
                             } deriving (Show)

data CheckCtx = CheckCtx Text Text Text
               deriving (Show, Eq)

type CheckExpr = (Text, Text)

data PrimKey = PrimKey Text Text Text Int
             deriving (Show, Eq)

data UniqKey = UniqKey Text Text Text Int
             deriving (Show, Eq)

data ForeignKey = ForeignKey Text Text Text Int Text Text Int
                deriving (Show, Eq)

data CheckInfo = CheckInfo { checkName     :: Text
                           , checkOnTable  :: Text
                           , check         :: PQ.PrimExpr
                           } deriving (Show)

data DefaultInfo = DefaultInfo { defaultOnColumn  :: Text
                               , defaultOnTable   :: Text
                               , defaultExp       :: PQ.PrimExpr
                               } deriving (Show)

data ForeignKeyInfo = ForeignKeyInfo { fkName :: Text
                                     , fkOnTab :: Text
                                     , fkRefTab :: Text
                                     , fkOnCols  :: [Text]
                                     , fkRefCols :: [Text]
                                     } deriving (Show, Eq)

data PrimKeyInfo = PrimKeyInfo { primKeyName :: Text
                               , ptableName :: Text
                               , primKeyColumns :: [Text]
                               } deriving (Show, Eq)

data UniqKeyInfo = UniqKeyInfo { uniqKeyName :: Text
                               , utableName :: Text
                               , uniqKeyColumns :: [Text]
                               } deriving (Show, Eq)

primKeyInfo :: [PrimKey] -> [PrimKeyInfo]
primKeyInfo = concatMap (map toPrimKeyInfo . groupByKeyName) . groupByTableName
  where groupByTableName = L.groupBy (\(PrimKey _ tna _ _) (PrimKey _ tnb _ _) -> tna == tnb)
        groupByKeyName   = L.groupBy (\(PrimKey kna _ _ _) (PrimKey knb _ _ _) -> kna == knb)

        toPrimKeyInfo pks@(PrimKey kna tna _ _ : _) = PrimKeyInfo kna tna (getPKCols pks)
        toPrimKeyInfo []                            = error "impossible: empty group"
        
        getPKCols = map snd . L.sortBy cmpByFst . map getPKCol
        getPKCol (PrimKey _ _ col i) = (i, col)
        cmpByFst a b = compare a b

uniqKeyInfo :: [UniqKey] -> [UniqKeyInfo]
uniqKeyInfo = concatMap (map toUniqKeyInfo . groupByKeyName) . groupByTableName
  where groupByTableName = L.groupBy (\(UniqKey _ tna _ _) (UniqKey _ tnb _ _) -> tna == tnb)
        groupByKeyName   = L.groupBy (\(UniqKey kna _ _ _) (UniqKey knb _ _ _) -> kna == knb)

        toUniqKeyInfo pks@(UniqKey kna tna _ _ : _) = UniqKeyInfo kna tna (getPKCols pks)
        toUniqKeyInfo []                            = error "impossible: empty group"
        
        getPKCols = map snd . L.sortBy cmpByFst . map getPKCol
        getPKCol (UniqKey _ _ col i) = (i, col)
        cmpByFst a b = compare a b

toTableInfo :: [(Text, [ColumnInfo])] -> [CheckInfo] -> [DefaultInfo] -> [PrimKeyInfo] -> [UniqKeyInfo] -> [ForeignKeyInfo] -> [TableInfo]
toTableInfo tabInfo chks defs pks uqs fks =
  map (\(tN, cInfos) ->
         let tUqs = L.filter (\uq -> utableName uq == tN) uqs
             tPks = L.filter (\pk -> ptableName pk == tN) pks
             tFks = L.filter (\fk -> fkOnTab fk == tN) fks
             tChks = L.filter (\chk -> checkOnTable chk == tN) chks
             tDefs = L.filter (\def -> defaultOnTable def == tN) defs
         in TableInfo tN cInfos tChks tDefs tPks tUqs tFks) tabInfo

checkInfo :: [CheckCtx] -> [CheckInfo]
checkInfo = map chkInfo
  where chkInfo (CheckCtx chkName chkOn chkExp) = 
          CheckInfo { checkName    = chkName
                    , checkOnTable = chkOn
                    , check        = unsafeParseExpr chkExp
                    }

defaultInfo :: [TableColInfo] -> [DefaultInfo]
defaultInfo =
  map (\t -> DefaultInfo { defaultOnColumn = dbColumnName t
                        , defaultOnTable   = dbTableName t
                        , defaultExp       = unsafeParseExpr (fromJust (dbColDefault t))
                        }
      ) . filter (isJust . dbColDefault)

foreignKeyInfo :: [ForeignKey] -> [ForeignKeyInfo]
foreignKeyInfo = concatMap (map toForeignKeyInfo . groupByKeyName) . groupByTableName
  where groupByTableName = L.groupBy (\(ForeignKey _ tna _ _ _ _ _) (ForeignKey _ tnb _ _ _ _ _) -> tna == tnb)
        groupByKeyName   = L.groupBy (\(ForeignKey kna _ _ _ _ _ _) (ForeignKey knb _ _ _ _ _ _) -> kna == knb)

        toForeignKeyInfo fks@(ForeignKey kna tna _ _ rtna _ _ : _) =
          ForeignKeyInfo { fkName    = kna
                         , fkOnTab   = tna
                         , fkRefTab  = rtna
                         , fkOnCols  = (getFKCols fks)
                         , fkRefCols = (getFKRefCols fks)
                         }
        toForeignKeyInfo []                                        = error "impossible: empty group"

        getFKCols = map snd . L.sortBy cmpByFst . map getFKCol
        getFKCol (ForeignKey _ _ col i _ _ _) = (i, col)

        getFKRefCols = map snd . L.sortBy cmpByFst . map getFKRefCol
        getFKRefCol (ForeignKey _ _ _ _ _ col i) = (i, col)
        
        cmpByFst a b = compare a b
  

tabColInfo :: [TableColInfo] -> [(Text, [ColumnInfo])]
tabColInfo = splitTabInfo . L.groupBy (\a b -> dbTableName a == dbTableName b)
  where splitTabInfo = foldr (\as acc -> let cInfos = map (\a -> ColumnInfo (dbColumnName a) (dbTypeName a) (dbPosition a) (nullable a)) as
                                        in case as of
                                             []      -> acc
                                             (a : _) -> ((dbTableName a), cInfos) : acc
                             ) []

        nullable a = case dbIsNullable a of
          "YES" -> True
          "NO"  -> False
          _     -> error "Panic: Invalid nullability information from DB"

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

instance FromRow ForeignKey where
  fromRow = ForeignKey <$> field <*> field <*> field <*> field <*> field <*> field <*> field

enumQ :: Query
enumQ =
  "SELECT pg_type.typname AS enumtype, array_agg(pg_enum.enumlabel) AS enumlabel \
   \FROM pg_type \
   \JOIN pg_enum \ 
     \ON pg_enum.enumtypid = pg_type.oid \
 \GROUP BY enumtype"

-- TODO: parameterize by schema name
tableColQ :: Query
tableColQ =
  "SELECT col.table_name, \
         \col.column_name, \
         \col.ordinal_position, \
         \col.column_default, \
         \col.is_nullable, \
         \col.data_type, \
         \col.character_maximum_length \
  \FROM  (SELECT table_name, \
                \column_name, \
                \ordinal_position, \
                \column_default, \
                \is_nullable, \
                \data_type, \
                \character_maximum_length \
         \FROM information_schema.columns) as col \
  \JOIN\
        \(SELECT table_name \
         \FROM information_schema.tables WHERE table_schema='public' AND table_type='BASE TABLE') as tab \
  \ON col.table_name = tab.table_name"

checksQ :: Query
checksQ =
  "SELECT cc.constraint_name, table_name, check_clause \
  \FROM information_schema.check_constraints AS cc \
  \JOIN information_schema.table_constraints AS tc \
  \ON   cc.constraint_name = tc.constraint_name \
  \WHERE cc.constraint_schema = 'public'"

primKeysQ :: Query
primKeysQ =
  "SELECT kcu.constraint_name, kcu.table_name, kcu.column_name, kcu.ordinal_position \ 
   \FROM  information_schema.key_column_usage as kcu \
   \JOIN  information_schema.table_constraints as tc \
   \ON    kcu.constraint_name = tc.constraint_name \
   \WHERE kcu.constraint_schema = 'public' AND constraint_type = 'PRIMARY KEY'"

uniqKeysQ :: Query
uniqKeysQ =
  "SELECT kcu.constraint_name, kcu.table_name, kcu.column_name, kcu.ordinal_position \
   \FROM   information_schema.key_column_usage as kcu \
   \JOIN   information_schema.table_constraints as tc \
   \ON     kcu.constraint_name = tc.constraint_name \
   \WHERE  kcu.constraint_schema = 'public' AND constraint_type = 'UNIQUE'"

foreignKeysQ :: Query
foreignKeysQ = "SELECT tc.constraint_name, tc.table_name, kcu.column_name, \
      \kcu.ordinal_position AS referencing_fk, \
      \ccu.table_name AS foreign_table_name, \
      \ccu.column_name AS foreign_column_name, \
      \kcu.position_in_unique_constraint AS referred_pk \

\FROM \
    \information_schema.table_constraints AS tc \
    \JOIN information_schema.key_column_usage AS kcu \
      \ON tc.constraint_name = kcu.constraint_name \
    \JOIN information_schema.constraint_column_usage AS ccu \
      \ON ccu.constraint_name = kcu.constraint_name \
    \JOIN (SELECT column_name, \
                 \ordinal_position, \
                 \col.table_name \
          \FROM information_schema.columns as col \
          \JOIN (SELECT * FROM information_schema.tables WHERE table_schema='public' AND table_type='BASE TABLE') as tab \
          \ON col.table_name = tab.table_name \
         \) AS foreign_col \
      \ON foreign_col.ordinal_position = position_in_unique_constraint AND foreign_col.column_name = ccu.column_name AND ccu.table_name = foreign_col.table_name \
\WHERE constraint_type = 'FOREIGN KEY'  AND \
      \kcu.constraint_schema = 'public'"
                              
getSchemaInfo :: Proxy db -> Connection -> IO [PrimDDL]
getSchemaInfo _ conn = do
  enumTs <- query_ conn enumQ
  tcols <- query_ conn tableColQ
  tchks <- query_ conn checksQ
  prims <- query_ conn primKeysQ
  uniqs <- query_ conn uniqKeysQ
  fks   <- query_ conn foreignKeysQ  
  pure $ mkMigrations enumTs (toTableInfo (tabColInfo tcols) (checkInfo tchks) (defaultInfo tcols) (primKeyInfo prims) (uniqKeyInfo uniqs) (foreignKeyInfo fks))

unsafeParseExpr :: Text -> PQ.PrimExpr
unsafeParseExpr t = primExprGen . either parsePanic id . parseOnly sqlExpr $ t
  where parsePanic e = error $ "Panic while parsing: " ++ show e ++ " , " ++ "while parsing " ++ show t

mkMigrations :: [EnumInfo] -> [TableInfo] -> [PrimDDL]
mkMigrations enumInfos tabInfos =
  mkMigrationTypes enumInfos ++ mkMigrationTables tabInfos

-- conversion to migrations

-- Only enums for now
mkMigrationTypes :: [EnumInfo] -> [PrimDDL]
mkMigrationTypes = map mkMigrationType
  where mkMigrationType e =
          let tn = TypeName (enumTypeName e)
              vals = map EnumVal (V.toList (enumCons e))
          in CreateEnum tn vals

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
                  (Just col)  -> addNotNull (coerce tabN) (coerce col)
                  Nothing     -> addCheckExpr (coerce tabN) (coerce chkName) (coerce chkExpr)

        defaultExprs = map defaultExpr (defaults tabInfo)
        defaultExpr defInfo =
          let colName = defaultOnColumn defInfo
              defExp  = defaultExp defInfo
          in addDefaultExpr (coerce tabN) (coerce colName) (coerce defExp)               
        tabN = tableName tabInfo        

        isNotNullExp exp = case exp of
          PQ.UnExpr PQ.OpIsNotNull (PQ.BaseTableAttrExpr coln) -> Just coln
          _                                                    -> Nothing
                                        
