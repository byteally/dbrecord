{-# LANGUAGE DataKinds, TypeApplications, DeriveGeneric, KindSignatures, TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving, OverloadedStrings #-}
import DBRecord.MSSQL.Internal.Query

import qualified DBRecord.Internal.Types as Type
import DBRecord.Internal.Schema
import Data.Text (Text)
import DBRecord.Query (getAll, DBM, Driver, runSession, rawClauses)
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

newtype MyDBM (db :: *) a = MyDBM {runDB :: ReaderT (MSSQL ()) IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (MSSQL ()))

type instance DBM TestMSDB = MyDBM TestMSDB
type instance Driver (MyDBM TestMSDB) = MSSQL

data Album = Album { albumId  :: Int
                   , title    :: Text
                   , artistId :: Int
                   } deriving (Generic)

data TestMSDB deriving (Generic)

instance Database TestMSDB where
  type DB TestMSDB = 'Type.MSSQL
  type Schema TestMSDB = "dbo"  
  type Tables TestMSDB = '[ Album
                          ]
  
instance Table TestMSDB Album where
  type TableName TestMSDB Album    = "Album"  
  type ColumnNames TestMSDB Album  = '[ '("albumId"  , "AlbumId"  )
                                      , '("title"    , "Title"    )
                                      , '("artistId" , "ArtistId" )
                                      ]

constInt    = PQ.ConstExpr . PQ.Integer
constDouble = PQ.ConstExpr . PQ.Double

                       
otherUnaryOpProjections :: [PQ.Projection]
otherUnaryOpProjections =
  map (first PQ.symFromText)
                     [ ("abs",  PQ.UnExpr PQ.OpAbs (constInt (- 15)) )
                     , ("negate", PQ.UnExpr PQ.OpNegate (constInt 15))
                     ]

otherBinOpProjections :: [PQ.Projection]
otherBinOpProjections =
  map (first PQ.symFromText)
                     [ ("plus",  PQ.BinExpr PQ.OpPlus (constInt 15) (constInt 20))
                     , ("minus", PQ.BinExpr PQ.OpMinus (constInt 15) (constInt 20))
                     , ("mul", PQ.BinExpr PQ.OpMul (constInt 15) (constInt 20))
                     , ("div", PQ.BinExpr PQ.OpDiv (constDouble 15.64) (constDouble 7.8456))
                     , ("mod", PQ.BinExpr PQ.OpMod (constInt 15) (constInt 20))                       
                     , ("bitand", PQ.BinExpr PQ.OpBitAnd (constInt 170) (constInt 75))
                     , ("bitor", PQ.BinExpr PQ.OpBitOr (constInt 170) (constInt 75))
                     , ("bitxor", PQ.BinExpr PQ.OpBitXor (constInt 170) (constInt 75))
                     ]

litProjections :: [PQ.Projection]
litProjections =
  map (bimap PQ.symFromText PQ.ConstExpr)
                     [ ("null",  PQ.Null)
                     , ("string", PQ.String "somestring")
                     , ("byte", PQ.Byte "somebytes")
                     , ("int", PQ.Integer 15)
                     , ("double", PQ.Double 5.6887)
                     -- NOTE: Bool literal rewrite 
                     -- , ("bool", PQ.Bool True)
                     ]

main = runSession (MSSQLConfig ()) $ runDB @TestMSDB $ do
         let title = col (Proxy :: Proxy (Type.DBTag TestMSDB Album "title")) 
             artistId = col (Proxy :: Proxy (Type.DBTag TestMSDB Album "artistId"))
             albumId = col (Proxy :: Proxy (Type.DBTag TestMSDB Album "albumId"))
             
         -- LITERALS
         _ <- rawClauses @TestMSDB @Album (PQ.clauses { PQ.projections = litProjections })
         -- WHERE & BOOL ops
         let cmpText = mssqltext "title"
             conditions = title .== cmpText         .&&
                          title .>  cmpText         .&&
                          title .<  cmpText         .&&
                          title .>= cmpText         .&&
                          title .<= cmpText         .&&
                          title ./= cmpText         .||
                          like title cmpText        .||
                          isNull (unsafeCoerceExpr title) .||
                          isNotNull (unsafeCoerceExpr artistId)
                          -- NOTE: In and between missing
         _ <- getAll @Album conditions
                    AnyOrder
                    Nothing
         -- Other binary ops
         _ <- rawClauses @TestMSDB @Album (PQ.clauses { PQ.projections = otherBinOpProjections })
         -- Other unary ops
         _ <- rawClauses @TestMSDB @Album (PQ.clauses { PQ.projections = otherUnaryOpProjections })

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

         pure ()


