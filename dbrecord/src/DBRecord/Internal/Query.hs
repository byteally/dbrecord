{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE DeriveGeneric         #-}

module DBRecord.Internal.Query
  ( column
  , getTableProjections
  , getTableProjections_
  , getTableId
  , Tab (..)
  , Columns (..)
  , Nest (..)

  , table
  , project
  , aggregate
  -- , order

  , join

  , convert

  , IsTable
  , Column'
  , UnifyType
  ) where

import DBRecord.Internal.Schema 
import DBRecord.Internal.Common
import DBRecord.Internal.PrimQuery (Expr (..), AggExpr (..), getExpr)
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Types hiding (TableTypes (..))
import GHC.TypeLits
import Data.Proxy
import Data.Kind
import DBRecord.Internal.Lens ((^.))
import Data.Coerce
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.Functor.Identity
import qualified Data.List.NonEmpty as NEL
import GHC.OverloadedLabels
import GHC.Generics

instance ( Column' tab tgt sc a lab (IsTable tab)
         ) => IsLabel lab (Columns tab -> Expr sc a) where
  fromLabel = column @lab @tgt @sc @tab

column :: forall col tgt sc tab a.
         ( Column' tab tgt sc a col (IsTable tab)
         ) => Columns tab -> Expr sc a
column t = column' t (Proxy :: Proxy '(IsTable tab, tgt, col))

type family IsTable (t :: Type) :: Bool where
  IsTable (HList _ _)   = 'False
  IsTable (a, b)        = 'False
  IsTable _             = 'True

class Column' tab tgt sc a (col :: Symbol) (isTab :: Bool) where
  column' :: Columns tab -> Proxy '(isTab, tgt, col) -> Expr sc a

instance ( -- Column_ a (GetDBTypeRep sc a)
           -- , ColumnCtx a (GetDBTypeRep sc a) sc tgt col
           tab ~ tgt
         , UnifyType col (OriginalTableFields tab) ~ a
         , KnownSymbol col
         ) => Column' tab tgt sc a col 'True where
  column' cols _ =
    let mexp = L.find (\(et, _) -> (et ^. hsName) == coln) (getColumns cols)
        coln = T.pack (symbolVal (Proxy @col))
    in  maybe (error $ "Panic: impossible @column': " ++ show coln ++ " : " ++ show (getColumns cols)) (PQ.Expr . snd) mexp
    
type family UnifyType col (xs :: [Type]) where
  UnifyType col (col ::: t ': ts) = t
  UnifyType col (_ ': ts)         = UnifyType col ts

instance ( tgt ~ Alias alias
         , UnifyType col xs ~ a
         , KnownSymbol col
         ) => Column' (HList Identity xs) tgt sc a col 'False where
  column' cols _ =
    let mexp = L.find (\(et, _) -> (et ^. hsName) == coln) (getColumns cols)
        coln = T.pack (symbolVal (Proxy @col))
    in  maybe (error $ "Panic: impossible @column': " ++ show coln ++ " : " ++ show (getColumns cols)) (PQ.Expr . snd) mexp
    
data Alias (als :: Symbol)

newtype Tab sc tab = Tab { getQuery :: Query }

newtype Columns t = Columns { getColumns :: [Projection] }
                  deriving (Show, Eq)

data Nest a b = Nest { left :: a, right :: b }
              deriving (Show, Generic)

data Query = BaseTable PQ.TableId Clauses
           | JoinTable JoinType PQ.PrimExpr Query Query Clauses
           | Product (NEL.NonEmpty Query) Clauses
           deriving (Show, Eq)

data JoinType = IJ | LJ
              deriving (Show, Eq)

data Clauses = Clauses { projections :: [Projection]
                       , criteria    :: [PQ.PrimExpr]
                       , orderbys    :: [PQ.OrderExpr]
                       , groupbys    :: [PQ.PrimExpr]
                       , alias       :: Text
                       }
             deriving (Show, Eq)

clauses :: Clauses
clauses = Clauses { projections = []
                  , criteria = []
                  , orderbys = []
                  , groupbys = []
                  , alias = ""
                  }

type Projection = (EntityNameWithHask, PQ.PrimExpr)

-- coerceTab :: Tab sc tab1 -> Tab sc tab2
-- coerceTab = coerce

-- withTab :: (Query -> Query) -> Tab sc tab -> Tab sc tab
-- withTab f = coerce . f . coerce

modifyClause :: (Clauses -> Clauses) -> Query -> Query
modifyClause f (BaseTable tb cls) = BaseTable tb (f cls)
modifyClause f (JoinTable jt e l r cls) = JoinTable jt e l r (f cls)
modifyClause f (Product pds cls) = Product pds (f cls)

getClause :: Query -> Clauses
getClause (BaseTable _ cls)       = cls
getClause (JoinTable _ _ _ _ cls) = cls
getClause (Product _ cls)       = cls

table :: forall sc tab.
        ( Table sc tab
        , SingCtx sc tab
        , SingCtxSc sc
        ) => Tab sc tab
table =
  let cls = clauses { projections = (getTableProjections_ (Proxy @sc) (Proxy @tab))
                    , alias       = tabn
                    }
      tabId = getTableId (Proxy @sc) (Proxy @tab)
      tabn = PQ.tableName tabId
  in  Tab (BaseTable tabId cls)
         
project :: forall sc tab xs.
            ( SingI (FieldsOf xs)
            , SingE (FieldsOf xs)
            ) => Tab sc tab                                ->
                (Columns tab -> HList (Expr sc) xs) ->
                Maybe (Columns tab -> Expr sc Bool) ->
                Tab sc (HList Identity xs)                
project tab fprjs fcrit =
  let cols  = Columns $ projections (getClause (getQuery tab))
      prjs  = fprjs cols
      mcrit = fmap ($ cols) fcrit
      als   = alias (getClause (getQuery tab))
      flds  = map (\fld -> EntityName { _hsName = fld, _dbName = fld })
               $ fromSing (sing :: Sing (FieldsOf xs))
      
  in  Tab (Product (pure (getQuery tab))
            (clauses { projections = zip flds $ exps prjs
                     , criteria    = case mcrit of
                         (Just crit) -> pure (getExpr crit)
                         Nothing     -> []
                     , alias       = als
                     }
          ))

      where exps :: forall ys. HList (Expr sc) ys -> [PQ.PrimExpr]
            exps (a :& as) = getExpr a : exps as
            exps Nil       = []
  
aggregate :: forall sc tab xs ys.
            ( SingI (FieldsOf ys)
            , SingE (FieldsOf ys)
            ) => Tab sc tab ->
                (Columns tab -> HList (Expr sc) xs) -> 
                (Columns tab -> HList (AggExpr sc) xs -> HList (AggExpr sc) ys) ->
                Tab sc (HList Identity ys)
aggregate tab fgpbys fprjs =
  let cols  = Columns $ projections (getClause (getQuery tab))
      gpbys = fgpbys cols
      prjs  = fprjs cols (hnat coerce gpbys)
      als   = alias (getClause (getQuery tab))
      flds  = map (\fld -> EntityName { _hsName = fld, _dbName = fld })
                $ fromSing (sing :: Sing (FieldsOf ys))
      
  in  Tab (Product (pure (getQuery tab))
            (clauses { projections = zip flds $ exps coerce prjs
                     , groupbys    = exps coerce gpbys
                     , alias       = als
                     }
          ))

      where exps :: forall zs f.(forall a. f a -> PQ.PrimExpr) -> HList f zs -> [PQ.PrimExpr]
            exps f (a :& as) = f a : exps f as
            exps _ Nil       = []
    

-- order = undefined

{-  
tabular :: forall alias sc tab xs.
            ( xs ~ TableFields sc tab
            , KnownSymbol alias
            ) => Tab sc tab -> Tab sc (Tabular alias xs)
tabular (Tab t) = Tab (modifyClause (\(Clauses pjs exps ods _) -> Clauses pjs exps ods als) t)

  where als = T.pack (symbolVal (Proxy @alias))

extend :: forall fld colalias x sc tabalias xs.
          ( KnownSymbol fld
          , KnownSymbol colalias
          ) => (Columns (Tabular tabalias xs) -> Expr sc x) -> Tab sc (Tabular tabalias xs) -> Tab sc (Tabular tabalias (fld ::: x ': xs))
extend e =
  coerceTab . withTab (modifyClause (\cls@(Clauses pjs exps ods tals) -> Clauses (newpj cls : pjs) exps ods tals))
  
  where newpj cls = (colInfo, getExpr $ e $ Columns (projections cls))
        colInfo = EntityName { _hsName = fld
                             , _dbName = als
                             }
        fld = T.pack (symbolVal (Proxy @fld))
        als = T.pack (symbolVal (Proxy @colalias))

-- TODO: validate that column does exist and error out              
contract :: forall fld sc tabalias xs ys.(ys ~ ContractFields fld xs, KnownSymbol fld) => Tab sc (Tabular tabalias xs) -> Tab sc (Tabular tabalias ys)
contract =
  coerceTab . withTab (modifyClause (\(Clauses pjs exps ods tals) -> Clauses (newpjs pjs) exps ods tals))
  
  where newpjs = L.deleteBy (\a b -> _dbName (fst a) == _dbName (fst b)) (EntityName undefined fld, undefined) 
        fld = T.pack (symbolVal (Proxy @fld))
-}

convert :: Query -> PQ.PrimQuery
convert = noalias . go
  where go (BaseTable tid cls) = 
          PQ.baseTable PQ.primQueryFoldDefault tid (convertClause cls)
        go (Product ps cls) =
          PQ.product PQ.primQueryFoldDefault (fmap (PQ.PrimQuery . go) ps) (convertClause cls)
        go (JoinTable jt e l r cls) = case jt of
          IJ -> PQ.innerJoin PQ.primQueryFoldDefault e (PQ.PrimQuery (go l)) (PQ.PrimQuery (go r)) (convertClause cls)
          LJ -> PQ.leftJoin PQ.primQueryFoldDefault e (PQ.PrimQuery (go l)) (PQ.PrimQuery (go r)) (convertClause cls)    

        noalias = PQ.modifyClause (\cls -> cls { PQ.alias = Nothing })
        
convertClause :: Clauses -> PQ.Clauses
convertClause (Clauses pjs crit ord gps als) =
  PQ.clauses { PQ.projections = map (\(c, e) -> (c ^. dbName, e)) pjs
             , PQ.criteria    = crit
             , PQ.orderbys    = ord
             , PQ.groupbys    = gps
             , PQ.alias       = Just als
             }
  
join :: Tab sc tabl -> Tab sc tabr -> (Columns tabl -> Columns tabr -> Expr sc Bool) -> Tab sc (Nest tabl tabr)
join t1@(Tab q1) t2@(Tab q2) f =
  let e = getExpr (f c1 c2)
      c1 = Columns (map (qualify lals) $ projections (getClause (getQuery t1))) 
      c2 = Columns (map (qualify rals) $ projections (getClause (getQuery t2)))
      q2' = modifyClause (\cls' -> cls' { alias = rals }) q2
  in  Tab (JoinTable IJ e q1 q2' cls)

  where cls = Clauses (lpjs ++ rpjs) [] [] [] (lals <> "_" <> rals)
        lpjs = map (\(pn, _) -> ( EntityName { _hsName = rals <> "_" <> (pn ^. hsName)
                                            , _dbName = lals <> "_" <> (pn ^. dbName)
                                            }
                               , PQ.AttrExpr (PQ.unsafeToSym [lals, pn ^. dbName])
                               )) (projections (getClause q1))
        lals = alias (getClause q1) 
        rpjs = map (\(pn, _) -> ( EntityName { _hsName = rals <> "_" <> (pn ^. hsName)
                                            , _dbName = rals <> "_" <> (pn ^. dbName)
                                            }
                               , PQ.AttrExpr (PQ.unsafeToSym [rals, pn ^. dbName])
                               )) (projections (getClause q2))
        rals = let rals' = alias (getClause q2)
               in if lals == rals' then rals' <> "_0" else rals'

        qualify als (etn, (PQ.BaseTableAttrExpr e)) =
          (etn, (PQ.unsafeAttrExpr [als, e]))
        qualify als (etn, (PQ.AttrExpr (PQ.Sym xs e))) =
          (etn, (PQ.unsafeAttrExpr (als : xs ++ [e])))
        qualify _ (etn, e) = (etn, e)

-- column :: Proxy symbol            -> Proxy tab -> PQ.Expr sc a
-- column :: Proxy '(symbol, symbol) -> Proxy tab -> PQ.Expr sc a

getTableProjections_ :: forall sc tab. (SingCtx sc tab) => Proxy sc -> Proxy tab -> [Projection]
getTableProjections_ psc ptab = go (headColInfos psc ptab)
  where go :: [ColumnInfo] -> [Projection]
        go = map mkProj

        mkProj :: ColumnInfo -> Projection
        mkProj ci =
          (ci ^. columnNameInfo, PQ.BaseTableAttrExpr (ci ^. columnNameInfo . dbName))

getTableProjections :: forall sc tab. (SingCtx sc tab) => Proxy sc -> Proxy tab -> [PQ.Projection]
getTableProjections psc ptab = go (headColInfos psc ptab)
  where go :: [ColumnInfo] -> [PQ.Projection]
        go = map mkProj

        mkProj :: ColumnInfo -> PQ.Projection
        mkProj ci =
          let dbn = ci ^. columnNameInfo . dbName
          in  (dbn, PQ.BaseTableAttrExpr dbn)

getTableId :: forall sc tab. (SingCtx sc tab, SingCtxSc sc) => Proxy sc -> Proxy tab -> PQ.TableId
getTableId psc ptab =
  let dbTabName      = headTabNameInfo psc ptab ^. dbName
      dbSchemaName   = headSchemaNameInfo psc ^. dbName
      dbDatabaseName = headDBNameInfo (Proxy :: Proxy (SchemaDB sc)) ^. dbName
  in  PQ.TableId { PQ.schema    = dbSchemaName
                 , PQ.tableName = dbTabName
                 -- TODO: Fit database
                 , PQ.database  = dbDatabaseName
                 }

type family QTableFields sc tab where
  QTableFields sc (a, b) =
    QTableFields sc a :++ QTableFields sc b
  QTableFields _ (HList Identity ys) =
    ys
  QTableFields sc a      =
    OriginalTableFields a


