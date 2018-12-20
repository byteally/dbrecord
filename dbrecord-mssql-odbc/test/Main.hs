{-# LANGUAGE DataKinds, TypeApplications, DeriveGeneric, KindSignatures, TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
import DBRecord.MSSQL.Internal.Query

import qualified DBRecord.Internal.Types as Type
import DBRecord.Internal.Schema
import Data.Text
import DBRecord.Query (getAll, DBM, Driver, runSession)
import DBRecord.MSSQL.Internal.Query
import GHC.Generics
import Control.Monad.Reader
import DBRecord.Internal.Order
import DBRecord.Internal.Expr

newtype MyDBM (db :: *) a = MyDBM {runDB :: ReaderT (MSSQL ()) IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (MSSQL ()))

type instance DBM TestMSDB = MyDBM TestMSDB
type instance Driver (MyDBM TestMSDB) = MSSQL

data Album = Album { albumId  :: Int
                   , title    :: Text
                   , artistId :: Int
                   } deriving (Show, Eq, Generic)

data TestMSDB deriving (Generic)

instance Database TestMSDB where
  type DB TestMSDB = 'Type.MSSQL
  type Tables TestMSDB = '[ Album
                          ]
  

instance Table TestMSDB Album where
  type TableName TestMSDB Album    = "Album"
  type ColumnNames TestMSDB Album  = '[ '("albumId"  , "AlbumId"  )
                                      , '("title"    , "Title"    )
                                      , '("artistId" , "ArtistId" )
                                      ]
  

main = runSession (MSSQLConfig ()) $ runDB @TestMSDB $ do
         _ <- getAll @Album true AnyOrder Nothing
         pure ()


