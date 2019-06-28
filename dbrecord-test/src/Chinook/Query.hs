{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE OverloadedStrings       #-}

module Chinook.Query where

import Chinook.Models.Artist
import Chinook.Models.Album
import Chinook.Models.Employee
import Chinook.Models.Track

import DBRecord.Query
import DBRecord.Internal.Schema
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Proxy
import qualified DBRecord.Internal.Types as Type
import GHC.TypeLits
import DBRecord.Internal.Common
import Data.Text
import qualified DBRecord.Internal.PrimQuery as PQ

queries ::
  forall db cfg driver.
  ( Table db Artist
  , MonadIO (DBM db)
  , MonadReader (driver cfg) (DBM db)
  , HasQuery driver
  , FromDBRow driver Artist
  , SingCtx db Artist
  , SingCtxDb db
    
  , Table db Album
  , FromDBRow driver Album
  , SingCtx db Album
  , Database db
  , Type.SingI (DB db)

  , Table db Employee
  , FromDBRow driver Employee
  , SingCtx db Employee
    
  ) => Proxy db -> DBM db ()
queries _ = do
  -- http://homepages.ecs.vuw.ac.nz/~adam/scie201/lec_select.html
  getAll @Artist true AnyOrder Nothing
  
  let artistId = undefined {-col (Proxy :: Proxy (Type.DBTag db Album "artistId"))-}
        :: Expr (OriginalTableFields Album) Int
  getAll @Album (artistId .== 1) AnyOrder Nothing
  -- between & in missing.
  -- getAll @Tracks

  let reportsTo = undefined {-col (Proxy :: Proxy (Type.DBTag db Employee "reportsTo"))-}
        :: Expr (OriginalTableFields Employee) (Maybe Int)
  getAll @Employee (isNull reportsTo) AnyOrder Nothing


  let name = undefined {-col (Proxy :: Proxy (Type.DBTag db Artist "name"))-}
        :: Expr (OriginalTableFields Artist) Text
  getAll @Artist (name `like` "Santana%") AnyOrder Nothing

  let composer = undefined {-col (Proxy :: Proxy (Type.DBTag db Tracks "composer"))-}
        :: Expr (OriginalTableFields Track) (Maybe Text)
      millisecs = undefined {-col (Proxy :: Proxy (Type.DBTag db Tracks "milliseconds"))-}
        :: Expr (OriginalTableFields Track) Int          
  getAll @Track (composer `like` "%Zappa%" .&& millisecs .> 240000) AnyOrder Nothing

  let 
  
  return ()


rawJoin :: PQ.JoinType -> PQ.Lateral -> Maybe (Expr '[] Bool) -> PQ.TableExpr PQ.PrimQuery -> PQ.TableExpr PQ.PrimQuery -> PQ.PrimQuery
rawJoin jt lt e pql pqr =
  let cls = PQ.clauses { PQ.projections = [] }
  in PQ.Join jt lt (fmap getExpr e) pql pqr cls

{-
rawCte :: [PQ.WithExpr PQ.PrimQuery] -> PQ.PrimQuery -> PQ.PrimQuery
rawCte pqs pq = PQ.CTE pqs pq

rawBinary :: PQ.BinType -> PQ.PrimQuery -> PQ.PrimQuery -> PQ.PrimQuery
rawBinary bt pq1 pq2 = PQ.Binary bt pq1 pq2 PQ.clauses
-}
