{-# LANGUAGE DataKinds, TypeApplications, DeriveGeneric, KindSignatures, TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleContexts, DuplicateRecordFields, OverloadedLabels #-}
import DBRecord.MSSQL.Internal.Query

import qualified DBRecord.Internal.Types as Type
import DBRecord.Internal.Schema
import DBRecord.Query
import Data.Text (Text)
import DBRecord.Query (getAll, DBM, Driver, runSession, rawClauses, getBaseTable)
import DBRecord.MSSQL.Internal.Query
import GHC.Generics
import Control.Monad.Reader
import DBRecord.Internal.Order
import DBRecord.Internal.Expr
import Data.Proxy
import Data.String
import DBRecord.MSSQL.Internal.Types
import qualified DBRecord.Internal.PrimQuery as PQ
import Data.Bifunctor 
import Prelude hiding (sum)
import DBRecord.Internal.Window
import Data.Functor.Identity
import DBRecord.Internal.Types

import Database.MsSQL as MSSQL hiding (Session (..), runSession)
import Data.Pool


newtype MyDBM (db :: *) a = MyDBM {runDB :: ReaderT (MSSQL Connection) IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (MSSQL Connection))

type instance DBM TestMSDB = MyDBM TestMSDB
type instance Driver (MyDBM TestMSDB) = MSSQL

data Album = Album { albumId  :: Int
                   , title    :: Text
                   , artistId :: Int
                   } deriving (Generic)

data Artist = Artist { artistId  :: Int
                     , name      :: Text
                     } deriving (Generic)

instance FromRow Artist
instance FromRow ()

data TestMSDB deriving (Generic)
data TestMSDB1 deriving (Generic)

instance Database TestMSDB where
  type DB TestMSDB = 'Type.MSSQL
  type DatabaseName TestMSDB = "Chinook"  
  
instance Schema TestMSDB where
  type SchemaName TestMSDB = "dbo"
  type SchemaDB TestMSDB   = TestMSDB
  type Tables TestMSDB = '[ Album
                          , Artist
                          ]

instance Table TestMSDB Album where
  type TableName TestMSDB Album    = "Album"  
  type ColumnNames TestMSDB Album  = '[ '("albumId"  , "AlbumId"  )
                                      , '("title"    , "Title"    )
                                      , '("artistId" , "ArtistId" )
                                      ]

instance Table TestMSDB Artist where
  type TableName TestMSDB Artist    = "Artist"  
  type ColumnNames TestMSDB Artist  = '[ '("name"  , "Name"  )
                                       , '("artistId" , "ArtistId" )
                                       ]
    
constInt    = PQ.ConstExpr . PQ.Integer
constDouble = PQ.ConstExpr . PQ.Double

{-
otherBinOpProjections :: [PQ.Projection]
otherBinOpProjections =
  map (first PQ.symFromText)
                     [ ("mod", PQ.BinExpr PQ.OpMod (constInt 15) (constInt 20))                       
                     , ("bitand", PQ.BinExpr PQ.OpBitAnd (constInt 170) (constInt 75))
                     , ("bitor", PQ.BinExpr PQ.OpBitOr (constInt 170) (constInt 75))
                     , ("bitxor", PQ.BinExpr PQ.OpBitXor (constInt 170) (constInt 75))
                     ]

type LocExpr = Expr () '[]

exprProjections :: [PQ.Projection]
exprProjections =
      [ -- LITERALS
        tup "null"
             (nothing :: LocExpr (Maybe Int))
      , tup "char"
             ((annotateType . coerceExpr . mssqltext) "sized"
                :: LocExpr (Type.CustomType (Sized 5 Text))
             )
      , tup "varchar"
             ((annotateType . coerceExpr . mssqltext) "varsized"
                :: LocExpr (Type.CustomType (Varsized 5 Text))
             )
      , tup "text"
             (annotateType (mssqltext "text")
                :: LocExpr Text
             )
      -- , tup "word"    (5 :: LocExpr Word)
      , tup "int"     (5 :: LocExpr Int)
      -- , tup "integer" (5 :: LocExpr Integer)
      , tup "float"   (5.667 :: LocExpr Float)
      , tup "double"  (5.6678 :: LocExpr Double)                
      , tup "byte"    (bytes "somebytes")
        -- NOTE: Bool literal rewrite 
        -- , ("bool", PQ.Bool True)
        
        -- Num operations
      -- , tup "plusWord"     (5 + 6 :: LocExpr Word)
      , tup "plusInt"      (5 + 6 :: LocExpr Int)
      -- , tup "plusInteger"  (5 + 6 :: LocExpr Integer)
      , tup "plusFloat"    (5.66 + 7.5 :: LocExpr Float)
      , tup "plusDouble"   (5.66 + 7.5 :: LocExpr Double)        

      -- , tup "minusWord"     (5 - 6 :: LocExpr Word)
      , tup "minusInt"      (5 - 6 :: LocExpr Int)
      -- , tup "minusInteger"  (5 - 6 :: LocExpr Integer)
      , tup "minusFloat"    (5.66 - 7.5 :: LocExpr Float)
      , tup "minusDouble"   (5.66 - 7.5 :: LocExpr Double)
        
      -- , tup "prodWord"     (5 * 6 :: LocExpr Word)
      , tup "prodInt"      (5 * 6 :: LocExpr Int)
      -- , tup "prodInteger"  (5 * 6 :: LocExpr Integer)
      , tup "prodFloat"    (5.66 * 7.5 :: LocExpr Float)
      , tup "prodDouble"   (5.66 * 7.5 :: LocExpr Double)        

      -- NOTE: Neg for word does not make sense
      -- , tup "word"    (5 :: LocExpr Word)
      , tup "negInt"     (5 :: LocExpr Int)
      -- , tup "negInteger" (5 :: LocExpr Integer)
      , tup "negFloat"   (5.667 :: LocExpr Float)
      , tup "negDouble"  (5.6678 :: LocExpr Double)                

      -- , tup "absword"    (5 :: LocExpr Word)
      , tup "absint"     ((- 5) :: LocExpr Int)
      -- , tup "absinteger" ((- 5) :: LocExpr Integer)
      , tup "absfloat"   ((- 5.667) :: LocExpr Float)
      , tup "absdouble"  ((- 5.6678) :: LocExpr Double)

       -- IntegralExpr
      -- , tup "quotWord"     (5 `quot_` 6 :: LocExpr Word)
      , tup "quotInt"      (5 `quot_` 6 :: LocExpr Int)
      -- , tup "quotInteger"  (5 `quot_` 6 :: LocExpr Integer)

      -- , tup "remWord"     (5 `rem_` 6 :: LocExpr Word)
      , tup "remInt"      (5 `rem_` 6 :: LocExpr Int)
      -- , tup "remInteger"  (5 `rem_` 6 :: LocExpr Integer)

      -- FractionalExpr
      , tup "divFloat"    (5.66 / 7.5 :: LocExpr Float)
      , tup "divDouble"   (5.66 / 7.5 :: LocExpr Double)

      -- Case expr
      , tup "case"        (case_ [ (5.2 .== (1 :: LocExpr Float), 7.6 :: LocExpr Float)
                                 , (5.2 .== (5.2 :: LocExpr Float), 7.8 :: LocExpr Float)
                                 ] 10.56)
      -- NOTE: in uses literal false
      -- , tup "in"          (in_ ([7.856, 9.23] :: [LocExpr Double]) 8.25)
      ]
  where tup a b = (PQ.symFromText a , getExpr . annotateType $ b)
-}

localConnectionStr :: ConnectionString 
localConnectionStr =
  ConnectionString { database          = "Chinook"
                   , server            = "localhost"
                   , port              = 1433
                   , user              = "sa"
                   , password          = "p@ssw0rd"
                   , odbcDriver        = odbcSQLServer17
                   , connectProperties = mempty
                   }

testConnectInfo :: ConnectInfo
testConnectInfo = connectInfo localConnectionStr

main = do
  pool <- mssqlDefaultPool testConnectInfo
  runSession (MSSQLConfig pool) $ runDB @TestMSDB $ do
         let row = (#artistId 999, #name "Artist!")
         res <- insertRet @TestMSDB
                @Artist
                (withRow row)
                (\t -> #name t :& Nil)
         liftIO $ print res
         {-
         let title = col (Proxy :: Proxy
                         (Type.DBTag TestMSDB Album "title")) 
             artistId = col (Proxy :: Proxy
                            (Type.DBTag TestMSDB Album "artistId"))
             albumId = col (Proxy :: Proxy
                           (Type.DBTag TestMSDB Album "albumId"))
             
         -- Exprs
         _ <- rawClauses @TestMSDB @Album (PQ.clauses { PQ.projections = exprProjections })
         
         -- WHERE & BOOL ops
         let cmpText = mssqltext "title"
             conditions = like title cmpText              .||
                          artistId .== 5                  .&&
                          albumId  .>= 2                  .&&
                          artistId .<= 2                  .&&
                          albumId  .>  2                  .&&
                          artistId .<  2                  .&&
                          artistId ./= 2                  .&&                          
                          isNull (toNullable albumId)           .||
                          not_ (isNotNull (toNullable albumId)) .||
                          isNull nothing                        
                          -- NOTE: In and between missing
         _ <- getAll @Album conditions
                    AnyOrder
                    Nothing

         -- aggregations
         let aggProjs = [ ("count", ctExpr)
                        , ("sum", PQ.FunExpr "sum" [getExpr albumId])
                        , ("avg", PQ.FunExpr "avg" [getExpr albumId])
                        , ("min", PQ.FunExpr "min" [getExpr albumId])
                        , ("max", PQ.FunExpr "max" [getExpr albumId])                                                    
                        ]
             groups   = [getExpr title]
             ctExpr   = PQ.FunExpr "count" [getExpr albumId]
             havs     = [ PQ.BinExpr PQ.OpGt ctExpr (constInt 0)]
         _ <- rawClauses @TestMSDB @Album (PQ.clauses { PQ.projections = aggProjs
                                                     , PQ.groupbys    = groups
                                                     , PQ.havings     = havs
                                                     })
         -- Window
         let winProjs = [ ("sum", getExpr $ windowExpr (partition artistId) (asc albumId) $ sum albumId)
                        , ("title", getExpr title)
                        ]
         _ <- rawClauses @TestMSDB @Album (PQ.clauses { PQ.projections = winProjs
                                                     })
         
        
         -- ordering, limit and offset
         let exp = constInt 10
             ord = PQ.OrderExpr (PQ.OrderOp { PQ.orderDirection = PQ.OpAsc
                                            , PQ.orderNulls = PQ.NullsLast
                                            }
                                ) (getExpr title)
             ord1 = PQ.OrderExpr (PQ.OrderOp { PQ.orderDirection = PQ.OpDesc
                                             , PQ.orderNulls = PQ.NullsLast
                                             }
                                ) (getExpr albumId)
                    
         _ <- rawClauses @TestMSDB @Album (PQ.clauses { PQ.projections = [("null", PQ.ConstExpr PQ.Null)]
                                                     , PQ.limit       = Just exp
                                                     , PQ.orderbys    = [ord]
                                                     }) 

         _ <- rawClauses @TestMSDB @Album (PQ.clauses { PQ.projections = [("null", PQ.ConstExpr PQ.Null)]
                                                     , PQ.offset      = Just exp
                                                     , PQ.orderbys    = [ord1]
                                                     }) 

         _ <- rawClauses @TestMSDB @Album (PQ.clauses { PQ.projections = [("null", PQ.ConstExpr PQ.Null)]
                                                     , PQ.offset      = Just exp
                                                     , PQ.limit       = Just exp
                                                     , PQ.orderbys    = [ord, ord1]
                                                     })
         -- Join

         let album = getBaseTable (Proxy :: Proxy TestMSDB) (Proxy :: Proxy Album)
             artist = getBaseTable (Proxy :: Proxy TestMSDB) (Proxy :: Proxy Artist)
             albumAndArtist1 = rawJoin PQ.LeftJoin True Nothing album artist
             albumAndArtist2 = rawJoin PQ.CrossJoin True Nothing album artist
             albumAndArtist3 = rawJoin PQ.InnerJoin False (Just (1 .== (1 :: LocExpr Int))) album artist
             albumAndArtist4 = rawJoin PQ.LeftJoin False (Just (1 .== (1 :: LocExpr Int))) album artist
             albumAndArtist5 = rawJoin PQ.RightJoin False (Just (1 .== (1 :: LocExpr Int))) album artist
             albumAndArtist6 = rawJoin PQ.FullJoin False (Just (1 .== (1 :: LocExpr Int))) album artist
             albumAndArtist7 = rawJoin PQ.CrossJoin False Nothing album artist                          
         dumpQuery albumAndArtist1
         dumpQuery albumAndArtist2         
         dumpQuery albumAndArtist3         
         dumpQuery albumAndArtist4         
         dumpQuery albumAndArtist5         
         dumpQuery albumAndArtist6
         dumpQuery albumAndArtist7         
         -- Product

         -- CTE
         let ctedExp = rawCte [ PQ.WithExpr "ArtistCTE1" ["x", "y"] artist
                              , PQ.WithExpr "ArtistCTE2" ["x", "y"] artist
                              ] album
         dumpQuery ctedExp

         -- Subquery
         let subqi = PQ.modifyClause 
                    (const (PQ.clauses { PQ.projections =
                                         [ ("sum", PQ.FunExpr "sum" [getExpr albumId]) ]                                                                                                     }
                           )
                    ) (getBaseTable (Proxy @TestMSDB) (Proxy @Album))
             subq = rawClauses @TestMSDB @Album
                    (PQ.clauses { PQ.projections =
                                    [ ("inner", PQ.TableExpr subqi) ]                                                                                                                 }
                    )
         _ <- subq
         -- binary ops
         let unionq = rawBinary PQ.Union artist artist
             interq = rawBinary PQ.Intersection artist artist
             unionallq = rawBinary PQ.UnionAll artist artist
             interallq = rawBinary PQ.IntersectionAll artist artist
             exceptq = rawBinary PQ.Except artist artist
             exceptallq = rawBinary PQ.ExceptAll artist artist
             
         dumpQuery unionq
         dumpQuery unionallq
         dumpQuery interq
         -- dumpQuery interallq         
         dumpQuery exceptq
         -- dumpQuery exceptallq
         pure ()
         -}

dumpQuery :: (MonadIO m) => PQ.PrimQuery -> m ()
dumpQuery = liftIO . putStrLn . renderQuery

{-

- Bool rewrite
- Op ast for sqlgen

-}

{-
rawJoin :: PQ.JoinType -> PQ.Lateral -> Maybe (Expr t sc Bool) -> PQ.PrimQuery -> PQ.PrimQuery -> PQ.PrimQuery
rawJoin jt lt e pql pqr =
  let cls = PQ.clauses { PQ.projections = [] }
  in PQ.Join jt lt (fmap getExpr e) (PQ.modifyClause (\a -> a { PQ.alias = Just "P" }) pql)
                                    (PQ.modifyClause (\a -> a { PQ.alias = Just "Q"}) pqr) cls


rawCte :: [PQ.WithExpr PQ.PrimQuery] -> PQ.PrimQuery -> PQ.PrimQuery
rawCte pqs pq = PQ.CTE pqs pq

rawBinary :: PQ.BinType -> PQ.PrimQuery -> PQ.PrimQuery -> PQ.PrimQuery
rawBinary bt pq1 pq2 = PQ.Binary bt pq1 pq2 PQ.clauses
-}

{-
- With guarentees :
  * should be at top
  * no order by etc.

AST simplification
BaseTable accepting a table expression


- migration
 - name clashes due to camel casing
-}

{-
instance Database TestMSDB1 where
  type DB TestMSDB1 = 'Type.MSSQL
  type Schema TestMSDB1 = "dbo"  
  type Tables TestMSDB1 = '[ 
                          ]

instance Table TestMSDB1 Album where
  type TableName TestMSDB1 Album    = "Album"  
  type ColumnNames TestMSDB1 Album  = '[ '("albumId"  , "AlbumId"  )
                                      , '("title"    , "Title"    )
                                      , '("artistId" , "ArtistId" )
                                      ]
-}
  

{-

| Table TableExpr Clauses
| Product (NEL.NonEmpty TableExpr) Clauses
| Join  JoinType Lateral (Maybe PrimExpr) TableExpr TableExpr Clauses
| Binary BinType PrimQuery PrimQuery
| CTE [WithExpr PrimQuery] PrimQuery


TableExpr =
  PrimQueryExpr PrimQuery
| TableName     TableId
| Table functions
-}
