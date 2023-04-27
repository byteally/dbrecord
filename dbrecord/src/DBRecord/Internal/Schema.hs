{-# OPTIONS_GHC -fno-warn-redundant-constraints -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE LambdaCase                 #-}

module DBRecord.Internal.Schema
  ( module DBRecord.Internal.Schema
  , Database (..)
  , Schema (..)
  ) where

import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import GHC.TypeLits
-- import Data.Type.Equality
import GHC.Generics
import GHC.Exts
-- import GHC.Stack
import GHC.OverloadedLabels
import Data.Kind
import Data.Typeable
import Data.Functor.Identity
import Data.Functor.Const
import DBRecord.Internal.Expr
import DBRecord.Internal.Types
import DBRecord.Internal.Common
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.DBTypes hiding (DBType (..), DBTypeName (..))
import qualified DBRecord.Internal.DBTypes as Type
import qualified Data.List as L
import DBRecord.Internal.Lens ((^.), Lens', coerceL, Traversal', ixBy, view)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.Char
import qualified GHC.Records as R
import Record
import Control.Monad.Trans.State.Strict
-- import Type.Reflection (SomeTypeRep (..), typeRepKind)

data Col (a :: Symbol) = Col
data DefSyms = DefSyms [Symbol]

type ColName  = Text
type ColType  = Text
data Column   = Column !ColName !ColType
  deriving (Show)

type family NoSchema t where
  NoSchema x = TypeError ('Text "No instance for " ':<>: 'ShowType (Schema x))  

class ( Schema sc
      , AssertCxt (Elem (Tables sc) tab) ('Text "Schema " ':<>: 'ShowType sc ':<>: 'Text " does not contain the table: " ':<>: 'ShowType tab)
      , ValidateTableProps sc tab    
      , Generic tab
      , Break0 (NoSchema sc) (SchemaDB sc)
      , DBRepr (DB (SchemaDB sc)) tab
      , ToDBType (DB (SchemaDB sc)) tab ~ 'TableObj
      ) => Table (sc :: Type) (tab :: Type) where
  type PrimaryKey sc tab :: [Symbol]
  type PrimaryKey sc tab = '[]

  type PrimaryKeyName sc tab :: Maybe Symbol
  type PrimaryKeyName sc tab = 'Nothing

  type ForeignKey sc tab :: [ForeignRef Type]
  type ForeignKey sc tab = '[]

  type ForeignKeyNames sc tab :: [(Symbol, Symbol)]
  type ForeignKeyNames sc tab = '[]

  type Unique sc tab     :: [UniqueCT]
  type Unique sc tab = '[]

  type UniqueNames sc tab :: [(Symbol, Symbol)]
  type UniqueNames sc tab = '[]

  type HasDefault sc tab :: [Symbol]
  type HasDefault sc tab = '[]

  type Check sc tab :: [CheckCT]
  type Check sc tab = '[]

  type CheckNames sc tab :: [(Symbol, Symbol)]
  type CheckNames sc tab = '[]
  
  type ColIgnore sc tab :: IgnoredCol
  type ColIgnore sc tab = 'IgnoreNone

  type TableName sc tab :: Symbol
  type TableName sc tab = DefaultTableName tab

  type TableSequence sc tab :: [Sequence]
  type TableSequence sc tab = '[]

  type SequenceNames sc tab :: [(Symbol, Symbol)]
  type SequenceNames sc tab = '[]
  
  type ColumnNames sc tab :: [(Symbol, Symbol)]
  type ColumnNames sc tab = '[]

  type TableType sc tab :: TableTypes
  type TableType sc tab = 'BaseTable

  type DefaultTableCodec sc tab :: Bool
  type DefaultTableCodec sc tab = 'False

  type NewRow sc tab = (r :: Type) | r -> tab

  type TableColumns sc tab :: [(Symbol, Type)]
  type TableColumns sc tab = GGetFields tab (Rep tab)

  defaults :: DBDefaults sc tab
  defaults = DBDefaults Nil

  checks :: DBChecks sc tab
  checks = DBChecks Nil

  checks' :: TableValue sc Identity tab -> [(Text, PQ.Expr sc Bool)]
  checks' _ = []

  rel :: (forall s.Clause s sc tab (TableValue sc Identity o)) -> Query' PlainQ sc o
  default rel ::
    ( GConstructHK tab (HasColumn sc tab) (TypeFields tab)
    , KnownSymbol (TableName sc tab)
    , MkFieldInvIx (TableColumns sc tab)
    ) => (forall s.Clause s sc tab (TableValue sc Identity o)) -> Query' PlainQ sc o
  rel (Clause clau) = Query' (TableValue fsix $ constructHK @(HasColumn sc tab) (ExprF . toExprId . coerceExpr . getCol (Proxy @'(sc, tab))), clau, PQ.Table (Just (PQ.TableName tabId)))
    where tabId = PQ.TableId { PQ.database = "zb"
                             , PQ.schema = "public"
                             , PQ.tableName = defHSNameToDBName $ T.pack $ symbolVal (Proxy @(TableName sc tab))
                             }
          fsix = mkFieldInvIx (Proxy @(TableColumns sc tab)) emptyFieldInvIx
  fromNewRow :: NewRow sc tab -> TableValue sc Identity tab

  default fromNewRow :: (GConstructHK tab (HasConstColumn sc tab (HasDefault sc tab) (NewRow sc tab)) (TypeFields tab), MkFieldInvIx (TableColumns sc tab)) => NewRow sc tab -> TableValue sc Identity tab
  fromNewRow r = TableValue fsix $ constructHK @(HasConstColumn sc tab (HasDefault sc tab) (NewRow sc tab)) (ExprF . toExprId . coerceExpr . getConstCol (Proxy @'(sc, tab, (HasDefault sc tab))) r)
    where fsix = mkFieldInvIx (Proxy @(TableColumns sc tab)) emptyFieldInvIx


getMutQ :: forall o tab sc.(Table sc tab) => (PQ.TableId -> TableValue sc Identity tab -> MQuery sc o) -> MQuery sc o
getMutQ k =
  let
    relq = rel @sc @tab (scoped $ \(clau, (Scoped tabv)) -> (clau, tabv))
    (pq, tabi) = runQuery' relq
    tabId = case pq of
      PQ.Table (Just (PQ.TableName tabId')) _ -> tabId'
      _ -> error "Panic: Internal invariant violated! Expected `Table` con from `rel`"
  in k tabId tabi


class MkFieldInvIx (xs :: [(Symbol, Type)]) where
  mkFieldInvIx :: Proxy xs -> FieldInvIx -> FieldInvIx

instance MkFieldInvIx '[] where
  mkFieldInvIx _ = id

instance (Typeable fn, MkFieldInvIx fs) => MkFieldInvIx ('(fn, ft) ': fs) where
  mkFieldInvIx _ fsix = mkFieldInvIx (Proxy @fs) (indexField (typeRep (Proxy @fn)) fsix)
  
-- newtype 
type family NonDefFields (fs :: [(Symbol, Type)]) (defs :: [Symbol]) :: [(Symbol, Type)] where
  NonDefFields '[] _ = '[]
  NonDefFields ('(fn, ft) ': fs) defs = IsDefFld defs (LookupDefFlds fn defs) ('(fn, ft) ': fs)

type family LookupDefFlds (fn :: Symbol) (defs :: [Symbol]) :: [Symbol] where
  LookupDefFlds fn (fn ': fs) = fs
  LookupDefFlds fn1 (fn ': fs) = fn ': LookupDefFlds fn1 fs
  LookupDefFlds _ '[] = '[]

type family IsDefFld (odefs :: [Symbol]) (ndefs :: [Symbol]) (fs :: [(Symbol, Type)]) :: [(Symbol, Type)] where
  IsDefFld defs defs (f ': fs) = f ': NonDefFields fs defs
  IsDefFld odefs ndefs (f ': fs) = NonDefFields fs ndefs

toExprId :: PQ.Expr sc a -> PQ.Expr sc (Identity a)
toExprId (PQ.Expr e) = PQ.Expr e

class HasConstColumn (sc :: Type) (tab :: Type) (defs :: [Symbol]) (r :: Type) (col :: Symbol) (a :: Type) where
  getConstCol :: Proxy '(sc, tab, defs) -> r -> Proxy '(col, a) -> PQ.Expr sc (Field col a)

instance (HasConstOrDefCol (IsDefCol col defs) sc tab r col a) => HasConstColumn sc tab defs r col a where
  getConstCol _ _r px = getConstOrDefCol (Proxy @'(sc, tab, (IsDefCol col defs))) _r px


class HasConstOrDefCol (isDef :: Bool) (sc :: Type) (tab :: Type) (r :: Type) (col :: Symbol) (a :: Type) where
  getConstOrDefCol :: Proxy '(sc, tab, isDef) -> r -> Proxy '(col, a) -> PQ.Expr sc (Field col a)

instance HasConstOrDefCol 'True sc tab r col a where
  getConstOrDefCol _ _ _ = PQ.Expr PQ.DefaultInsertExpr

instance (R.HasField col r a, ConstExpr sc a) => HasConstOrDefCol 'False sc tab r col a where
  getConstOrDefCol _ r _ = coerceExpr $ constExpr $ R.getField @col r

type family IsDefCol (c :: Symbol) (defs :: [Symbol]) :: Bool where
  IsDefCol c (c ': _) = 'True
  IsDefCol c (_ ': ds) = IsDefCol c ds
  IsDefCol _ '[] = 'False
  

data Sequence = PGSerial Symbol   -- Column
                         Symbol   -- Sequence Name
              | PGOwned  Symbol   -- Column
                         Symbol   -- Sequence Name

type family Serial (cname :: Symbol) (seqname :: Symbol) where
  Serial cname seqname = 'PGSerial cname seqname

type family Owned (cname :: Symbol) (seqname :: Symbol) where
  Owned cname seqname = 'PGOwned cname seqname

data Query' qt sc t = forall i.Query' (TableValue sc Identity i, State (PQ.Clauses, TableValue sc Identity i) (TableValue sc Identity t), PQ.Clauses -> PQ.PrimQuery)

execQuery :: Query' qt sc t -> PQ.PrimQuery
execQuery  = fst . runQuery'
{-# INLINE execQuery #-}

runAliasedQuery :: forall t fn sc qt.KnownSymbol fn => Field fn (Query' qt sc t) -> (PQ.PrimQuery, TableValue sc Identity t)
runAliasedQuery = runQuery'' (Just $ T.pack $ symbolVal (Proxy @fn)) . val

runQuery' :: Query' qt sc t -> (PQ.PrimQuery, TableValue sc Identity t)
runQuery' = runQuery'' Nothing
  
runQuery'' :: Maybe Text -> Query' qt sc t -> (PQ.PrimQuery, TableValue sc Identity t)
runQuery'' asMay (Query' (exprs, st, mkPQ)) =
  let
    (tv, (clau', _)) = runState st (PQ.clauses, exprs)
    clau = clau' { PQ.projections = case tableToProjections tv of
                     [] -> [("unit", unitExpr)]
                     ps -> ps
                 , PQ.alias = asMay
                 }
  in (mkPQ clau, tv)
{-# INLINE runQuery' #-}

execMQuery ::
  (PQ.InsertQuery -> r)
  -> (PQ.UpdateQuery -> r)
  -> (PQ.DeleteQuery -> r)
  -> r
  -> MQuery sc t
  -> r
execMQuery i u d nop = fst . runMQuery i u d nop

runMQuery ::
  (PQ.InsertQuery -> r)
  -> (PQ.UpdateQuery -> r)
  -> (PQ.DeleteQuery -> r)
  -> r
  -> MQuery sc t
  -> (r, TableValue sc Identity t)
runMQuery i u d nop = \case
  (InsertMQuery (exprs, st, mkPQ)) ->
    let
      (tv, (clau', _)) = runState st ([], exprs)
      iq' = mkPQ clau'
      setRet rets (PQ.InsertQuery tid atrs vs confMay _) = PQ.InsertQuery tid atrs vs confMay rets
      iq = case tableToProjections tv of
        [] -> iq'
        ps -> setRet (fmap snd ps) iq'
    in (i iq, tv)
  (UpdateMQuery (exprs, st, mkPQ)) ->
    let
      (tv, (clau', _)) = runState st (PQ.clauses, exprs)
      uq' = mkPQ clau'
      setRet rets (PQ.UpdateQuery tid conds sets _) = PQ.UpdateQuery tid conds sets rets
      uq = case tableToProjections tv of
        [] -> uq'
        ps -> setRet (fmap snd ps) uq'
    in (u uq, tv)
  (DeleteMQuery (exprs, st, mkPQ)) ->
    let
      (tv, (clau', _)) = runState st (PQ.clauses, exprs)
      dq' = mkPQ clau'
      setRet rets (PQ.DeleteQuery tid conds _) = PQ.DeleteQuery tid conds rets
      dq = case tableToProjections tv of
        [] -> dq'
        ps -> setRet (fmap snd ps) dq'
    in (d dq, tv)
  MQueryNoOp -> (nop, EmptyTable)

getColumnAliasMap :: forall sc tab.(Table sc tab, AllF SingE (ColumnNames sc tab), SingI (ColumnNames sc tab)) => HM.HashMap Text Text
getColumnAliasMap = HM.fromList $ fromSing (sing :: Sing (ColumnNames sc tab))
{-# INLINE getColumnAliasMap #-}

-- Clause should be opaque
-- o should never be `Expr`
newtype Clause (s :: Type) sc i o = Clause (State (PQ.Clauses, TableValue sc Identity i) o)
  deriving newtype (Functor, Applicative, Monad)

instance Semigroup (Clause s sc i o) where
  clau1 <> clau2 = clau1 *> clau2

data MQuery sc t where
  MQueryNoOp :: MQuery sc ()
  InsertMQuery :: (TableValue sc Identity i, State ([PQ.PrimExpr], TableValue sc Identity i) (TableValue sc Identity t), [PQ.PrimExpr] -> PQ.InsertQuery) -> MQuery sc t
  UpdateMQuery :: (TableValue sc Identity i, State (PQ.Clauses, TableValue sc Identity i) (TableValue sc Identity t), PQ.Clauses -> PQ.UpdateQuery) -> MQuery sc t
  DeleteMQuery :: (TableValue sc Identity i, State (PQ.Clauses, TableValue sc Identity i) (TableValue sc Identity t), PQ.Clauses -> PQ.DeleteQuery) -> MQuery sc t
  
newtype InsertClause s sc i o = InsertClause (Clause s sc i o)
  deriving newtype (Functor, Applicative, Monad, Semigroup)

-- runClause :: forall i o sc s.
--   Clause s sc i o
--   -> PQ.Clauses
-- runClause (Clause _clau) = undefined

-- Unsafe
scoped :: forall i o sc s.
  ((PQ.Clauses, Scoped s sc i) -> (PQ.Clauses, o))
  -> Clause s sc i o
scoped fn = Clause $ state (\(c,es) -> let (c', o) = fn (c, Scoped es) in (o, (c', es)))

-- scopeToListWith :: (forall g (a :: Type).Typeable a => ExprF sc g a -> r) -> Scoped s sc i -> [r]
-- scopeToListWith fn (Scoped tab) = tableToListWith (const fn) tab

tableToListWith :: (forall g (a :: Type).Typeable a => [Text] -> SomeSymbol -> ExprF sc g a -> r) -> TableValue sc f i -> [r]
tableToListWith = tableToListWith' []

tableToListWith' :: [Text] -> (forall g (a :: Type).Typeable a => [Text] -> SomeSymbol -> ExprF sc g a -> r) -> TableValue sc f i -> [r]
tableToListWith' pfxs fn (TableValue fsix hk) = fmap snd $ L.sortOn fst $ hkToListWithTag (\ssym ex -> (fromMaybe (error $ "Panic: Invariant violated! " <> (show $ typeRepOfSomeSym ssym) <> (show $ hkToListWith (\fa -> show $ typeRep fa) hk)) $ lookupFieldIx (typeRepOfSomeSym ssym) fsix, fn pfxs ssym ex)) hk
tableToListWith' pfxs fn (CrossTable tv1 tv2) = tableToListWith' (getFnName tv1 : pfxs) fn (val tv1) ++ tableToListWith' (getFnName tv2 : pfxs) fn (val tv2)
tableToListWith' pfxs fn (LeftJoinTable tv1 tv2) = tableToListWith' (getFnName tv1 : pfxs) fn (val tv1) ++ tableToListWith' (getFnName tv2 : pfxs) fn (val tv2)
tableToListWith' pfxs fn (RightJoinTable tv1 tv2) = tableToListWith' (getFnName tv1 : pfxs) fn (val tv1) ++ tableToListWith' (getFnName tv2 : pfxs) fn (val tv2)
tableToListWith' _pfxs _ (ConsTable _ _) = undefined
tableToListWith' _pfxs _ (ConsOptTable _ _ _) = undefined
tableToListWith' _pfxs _ EmptyTable = []
--tableToListWith' pfxs fn (NestedTable hk) = concat $ hkToListWith (tableToListWith' pfxs fn) hk

-- mapScope :: (forall a.Typeable a => PQ.Expr sc a -> PQ.Expr sc a) -> Scoped s sc i -> Scoped s sc i
-- mapScope fn (Scoped hk) = Scoped $ hoistWithKeyHK fn hk

tableToProjections :: TableValue sc f a -> [PQ.Projection]
tableToProjections = tableToListWith getPrjs
{-# INLINE tableToProjections #-}

getPrjs :: forall a f sc.(Typeable a) => [Text] -> SomeSymbol -> ExprF sc f a -> (T.Text, PQ.PrimExpr)
getPrjs pfxs ssym e = (T.intercalate "_" ((reverse pfxs) ++ [aliasedExprName ssym]), PQ.getExpr $ getExprF e)

toIdExpr :: PQ.Expr sc x -> PQ.Expr sc (Identity x)
toIdExpr = coerceExpr
{-# INLINE toIdExpr #-}

fromIdExpr :: PQ.Expr sc (Identity x) -> PQ.Expr sc x
fromIdExpr = coerceExpr
{-# INLINE fromIdExpr #-}


crossRel :: (KnownSymbol n1, KnownSymbol n2, Typeable r1, Typeable r2) => Field n1 (TableValue sc f r1) -> Field n2 (TableValue sc f r2) -> TableValue sc f (Rec '[ '(n1, r1), '(n2, r2)])
crossRel q1 q2 = CrossTable q1 q2
-- crossRel q1 q2 = NestedTable $ hrecToHKOfRec $ q1 .& q2 .& Record.end

ljRel :: (KnownSymbol n1, KnownSymbol n2, Typeable r1, Typeable r2) => Field n1 (TableValue sc f r1) -> Field n2 (TableValue sc Maybe r2) -> TableValue sc f (Rec '[ '(n1, r1), '(n2, Maybe r2)])
ljRel q1 q2 = LeftJoinTable q1 q2
-- ljRel q1 q2 = NestedTable $ hrecToHKOfRec $ q1 .& (R.getField <$> q2) .& Record.end

rjRel :: (KnownSymbol n1, KnownSymbol n2, Typeable r1, Typeable r2) => Field n1 (TableValue sc Maybe r1) -> Field n2 (TableValue sc f r2) -> TableValue sc f (Rec '[ '(n1, Maybe r1), '(n2, r2)])
rjRel q1 q2 = RightJoinTable q1 q2

nextStage :: TableValue sc f i -> TableValue sc f i
nextStage = nextStage' []

nextStage' :: [Text] -> TableValue sc f i -> TableValue sc f i
nextStage' pfxs (TableValue fsix hk) = TableValue fsix $ hoistWithKeyAndTagHK (aliasedExprWithPrefix pfxs) hk
nextStage' pfxs (CrossTable ntv1 ntv2) = CrossTable (fmap (nextStage' (getFnName ntv1 : pfxs)) ntv1) (fmap (nextStage' (getFnName ntv2 : pfxs)) ntv2)
nextStage' pfxs (LeftJoinTable ntv1 ntv2) = LeftJoinTable (fmap (nextStage' (getFnName ntv1 : pfxs)) ntv1) (fmap (nextStage' (getFnName ntv2 : pfxs)) ntv2)
nextStage' pfxs (RightJoinTable ntv1 ntv2) = RightJoinTable (fmap (nextStage' (getFnName ntv1 : pfxs)) ntv1) (fmap (nextStage' (getFnName ntv2 : pfxs)) ntv2)
nextStage' _pfxs (ConsTable _ _) = undefined
nextStage' _pfxs (ConsOptTable _ _ _) = undefined
nextStage' _pfxs EmptyTable = EmptyTable
--nextStage' (NestedTable tabhk) = NestedTable $ hoistWithKeyHK nextStage' tabhk

hoistMaybeTable :: (forall g x. ExprF sc g x -> ExprF sc Maybe x) -> TableValue sc f i -> TableValue sc Maybe i
hoistMaybeTable fn (TableValue fsix hk) = TableValue fsix $ hoistHK fn hk
hoistMaybeTable fn (CrossTable ntv1 ntv2) = CrossTable (fmap (hoistMaybeTable fn) ntv1) (fmap (hoistMaybeTable fn) ntv2)
hoistMaybeTable fn (LeftJoinTable ntv1 ntv2) = LeftJoinTable (fmap (hoistMaybeTable fn) ntv1) (fmap (hoistMaybeTable fn) ntv2)
hoistMaybeTable fn (RightJoinTable ntv1 ntv2) = RightJoinTable (fmap (hoistMaybeTable fn) ntv1) (fmap (hoistMaybeTable fn) ntv2)
hoistMaybeTable _ (ConsTable _ _) = undefined
hoistMaybeTable _ (ConsOptTable _ _ _) = undefined
hoistMaybeTable _ EmptyTable = EmptyTable

toNullExprF :: ExprF sc g x -> ExprF sc Maybe x
toNullExprF (ExprF (PQ.Expr x)) = (ExprF (PQ.Expr x))


getScopeOfTable :: TableValue sc Identity i -> Scoped s sc i
getScopeOfTable tab = Scoped tab

maybeExprsToExpr :: HK (ExprF sc Maybe) r -> PQ.Expr sc (Maybe r)
maybeExprsToExpr hk = PQ.Expr $ PQ.FlatComposite $ hkToListWithTag (\ssym e -> (aliasedExprName ssym, PQ.getExpr $ getExprF e)) hk

maybeTableToExpr :: TableValue sc Maybe i -> PQ.Expr sc (Maybe i)
maybeTableToExpr = unsafeTableToExpr

idTableToExpr :: TableValue sc Identity i -> PQ.Expr sc i
idTableToExpr = unsafeTableToExpr

unsafeTableToExpr :: TableValue sc f i -> PQ.Expr sc o
unsafeTableToExpr (TableValue _ hk) = PQ.Expr $ PQ.FlatComposite $ hkToListWithTag (\ssym e -> (aliasedExprName ssym, PQ.getExpr $ getExprF e)) hk
-- unsafeTableToExpr (NestedTable hk) = PQ.Expr $ PQ.FlatComposite $ hkToListWith (\texp -> (aliasedTableName texp, PQ.getExpr $ unsafeTableToExpr texp)) hk
unsafeTableToExpr (CrossTable tv1 tv2) = PQ.Expr $ PQ.FlatComposite $ [ (getFnName tv1, PQ.getExpr $ unsafeTableToExpr $ val tv1), (getFnName tv2, PQ.getExpr $ unsafeTableToExpr $ val tv2)]
unsafeTableToExpr (LeftJoinTable tv1 tv2) = PQ.Expr $ PQ.FlatComposite $ [ (getFnName tv1, PQ.getExpr $ unsafeTableToExpr $ val tv1), (getFnName tv2, PQ.getExpr $ unsafeTableToExpr $ val tv2)]
unsafeTableToExpr (RightJoinTable tv1 tv2) = PQ.Expr $ PQ.FlatComposite $ [ (getFnName tv1, PQ.getExpr $ unsafeTableToExpr $ val tv1), (getFnName tv2, PQ.getExpr $ unsafeTableToExpr $ val tv2)]
unsafeTableToExpr (ConsTable _ _) = undefined
unsafeTableToExpr (ConsOptTable _ _ _) = undefined
unsafeTableToExpr EmptyTable = PQ.Expr unitExpr

unitExpr :: PQ.PrimExpr
unitExpr = PQ.ConstExpr $ PQ.Integer 1
{-# INLINE unitExpr #-}

getFnName :: forall n a.KnownSymbol n => Field n a -> Text
getFnName _ = T.pack $ symbolVal (Proxy @n)
{-# INLINE getFnName #-}

typeRepOfSomeSym :: SomeSymbol -> TypeRep
typeRepOfSomeSym (SomeSymbol pxn) = typeRep pxn
{-# INLINE typeRepOfSomeSym #-}

aliasedExprWithPrefix :: forall a f sc.[Text] -> SomeSymbol -> ExprF sc f a -> ExprF sc f a
aliasedExprWithPrefix pfxs ssym _ = ExprF $ PQ.unsafeCol ((reverse pfxs) ++ [aliasedExprName ssym])
{-# INLINE aliasedExprWithPrefix #-}

aliasedExpr :: forall a f sc.SomeSymbol -> ExprF sc f a
aliasedExpr ssym = ExprF $ PQ.unsafeCol [aliasedExprName ssym]
{-# INLINE aliasedExpr #-}

aliasedExprName :: SomeSymbol -> Text
aliasedExprName ssym = symStrToText $ show $ typeRepOfSomeSym ssym
{-# INLINE aliasedExprName #-}
 
symStrToText :: String -> Text
symStrToText [] = ""
symStrToText s@(_ : []) = T.pack s
symStrToText s = T.pack $ init $ tail s
{-# INLINE symStrToText #-}
          
newtype Scoped s sc t = Scoped (TableValue sc Identity t)

newtype ExprF sc f t = ExprF (PQ.Expr sc (f t))

getExprF :: ExprF sc f t -> PQ.Expr sc (f t)
getExprF (ExprF e) = e

data FieldInvIx = FieldInvIx !Int !(Map.Map Data.Typeable.TypeRep Int)
  deriving Show

emptyFieldInvIx :: FieldInvIx
emptyFieldInvIx = FieldInvIx 0 mempty
{-# INLINE emptyFieldInvIx #-}

fromListToFieldInvIx :: [TypeRep] -> FieldInvIx
fromListToFieldInvIx = L.foldl' (\fsix trep -> indexField trep fsix) emptyFieldInvIx
{-# INLINE fromListToFieldInvIx #-}

indexField :: TypeRep -> FieldInvIx -> FieldInvIx
indexField trep (FieldInvIx prev ixMap) =
  let newIx = prev + 1
  in FieldInvIx newIx (Map.insert trep newIx ixMap)
{-# INLINE indexField #-}  

lookupFieldIx :: TypeRep -> FieldInvIx -> Maybe Int
lookupFieldIx trep (FieldInvIx _ ixMap) = Map.lookup trep ixMap
{-# INLINE lookupFieldIx #-}

deleteFieldIx :: TypeRep -> FieldInvIx -> FieldInvIx
deleteFieldIx trep (FieldInvIx prev ixMap) = FieldInvIx prev $ Map.delete trep ixMap
{-# INLINE deleteFieldIx #-}

data TableValue sc f t where
  TableValue :: !FieldInvIx -> (HK (ExprF sc f) t) -> TableValue sc f t
--  NestedTable :: (HK (TableValue sc f) t) -> TableValue sc f t
  CrossTable :: (KnownSymbol n1, KnownSymbol n2, Typeable r1, Typeable r2) => Field n1 (TableValue sc f r1) -> Field n2 (TableValue sc f r2) -> TableValue sc f (Rec '[ '(n1, r1), '(n2, r2)])
  LeftJoinTable :: (KnownSymbol n1, KnownSymbol n2, Typeable r1, Typeable r2) => Field n1 (TableValue sc f r1) -> Field n2 (TableValue sc Maybe r2) -> TableValue sc f (Rec '[ '(n1, r1), '(n2, Maybe r2)])
  RightJoinTable :: (KnownSymbol n1, KnownSymbol n2, Typeable r1, Typeable r2) => Field n1 (TableValue sc Maybe r1) -> Field n2 (TableValue sc f r2) -> TableValue sc f (Rec '[ '(n1, Maybe r1), '(n2, r2)])
  ConsTable :: (KnownSymbol n, Typeable r) => Field n (TableValue sc f r) -> TableValue sc f (Rec xs) -> TableValue sc f (Rec ('(n, r) ': xs))
  ConsOptTable :: (KnownSymbol n, Typeable r) => PQ.JoinType -> Field n (TableValue sc Maybe r) -> TableValue sc f (Rec xs) -> TableValue sc f (Rec ('(n, Maybe r) ': xs))
  EmptyTable :: TableValue sc f ()

tableRecAsType :: ValidateRecToType os t => TableValue sc f (Rec os) -> TableValue sc f t
tableRecAsType (TableValue fsix hk) = TableValue fsix (fromHKOfRec hk)
tableRecAsType _tv@CrossTable{} = undefined -- coerce tv
tableRecAsType _tv@LeftJoinTable{} = undefined -- coerce tv
tableRecAsType _tv@RightJoinTable{} = undefined -- coerce tv
tableRecAsType _tv@ConsTable{} = undefined -- coerce tv
tableRecAsType _tv@ConsOptTable{} = undefined -- coerce tv
  
--  NullableTable :: (TableValue sc Maybe x -> PQ.Expr sc (Maybe t)) -> TableValue sc f t
--  TableExpr :: (TableValue sc g x -> PQ.Expr sc (Maybe t)) -> TableValue sc f t
  
newtype Scalar sc t = Scalar (PQ.Expr sc t)

data PlainQ
data AggQ agglist
data WindowQ
data SubQ
data MutatingQ
data AsQ (n :: Symbol) q

instance (R.HasField f i t, KnownSymbol f, Typeable t) => R.HasField (f :: Symbol) (Scoped s sc i) (PQ.Expr sc t) where
  getField (Scoped hk) = R.getField @f hk

instance (R.HasField f i t, KnownSymbol f, Typeable t) => R.HasField (f :: Symbol) (TableValue sc Identity i) (PQ.Expr sc t) where
  getField (TableValue _ hk) = fromIdExpr $ getExprF $ R.getField @f hk
--  getField (NestedTable hk) = undefined -- idTableToExpr $ R.getField @f hk
  getField (CrossTable tv1 tv2) = idTableToExpr $ getMat tv1 tv2
    where getMat :: forall n1 n2 r1 r2.(Typeable r1, Typeable r2) => Field n1 (TableValue sc Identity r1) -> Field n2 (TableValue sc Identity r2) -> TableValue sc Identity t
          getMat ntv1 ntv2 = case (eqT @t @r1, eqT @t @r2) of
            (Just Refl, Nothing) -> val ntv1
            (Nothing, Just Refl) -> val ntv2
            (Nothing, Nothing) -> error "Panic"
            (Just _, Just _) -> error "Panic"

  getField (LeftJoinTable tv1 tv2) = getMat tv1 tv2
    where getMat :: forall n1 n2 r1 r2.(Typeable r1, Typeable r2) => Field n1 (TableValue sc Identity r1) -> Field n2 (TableValue sc Maybe r2) -> PQ.Expr sc t
          getMat ntv1 ntv2 = case (eqT @t @r1, eqT @t @(Maybe r2)) of
            (Just Refl, Nothing) -> idTableToExpr $ val ntv1
            (Nothing, Just Refl) -> maybeTableToExpr $ val ntv2              
            (Nothing, Nothing) -> error "Panic"
            (Just _, Just _) -> error "Panic"

  getField (RightJoinTable tv1 tv2) = getMat tv1 tv2
    where getMat :: forall n1 n2 r1 r2.(Typeable r1, Typeable r2) => Field n1 (TableValue sc Maybe r1) -> Field n2 (TableValue sc Identity r2) -> PQ.Expr sc t
          getMat ntv1 ntv2 = case (eqT @t @(Maybe r1), eqT @t @r2) of
            (Just Refl, Nothing) -> maybeTableToExpr $ val ntv1
            (Nothing, Just Refl) -> idTableToExpr $ val ntv2              
            (Nothing, Nothing) -> error "Panic"
            (Just _, Just _) -> error "Panic"
  getField (ConsTable _ _) = undefined
  getField (ConsOptTable _ _ _) = undefined
  getField EmptyTable = PQ.Expr unitExpr

class HasColumn sc tab (col :: Symbol) (a :: Type) where
  getCol :: Proxy '(sc, tab) -> Proxy '(col, a) -> PQ.Expr sc (Field col a)

instance
  ( HasColumnByDBType sc tab col a dbTypeRep
  , R.HasField col tab ct
  , ct ~ a
  , Table sc tab
  , KnownSymbol col
  , dbTypeRep ~ GetDBTypeRep sc a
  ) => HasColumn sc tab col a where
  getCol _ _ = PQ.Expr $ PQ.getExpr $ getColByDBTypeRep @sc @tab @col @a @dbTypeRep Proxy

class HasColumnByDBType sc tab (col :: Symbol) (a :: Type) (atrep :: DBTypeK) where
  getColByDBTypeRep :: Proxy '(sc, tab, col, a, atrep) -> PQ.Expr sc a

instance {-# OVERLAPPING #-}
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  , UDType sc a
  , AllF SingE (ColumnNames sc tab)
  , SingI (ColumnNames sc tab)
  ) => HasColumnByDBType sc tab col a ('DBCustomType scn a tn) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance {-# OVERLAPPING #-}
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  , UDType sc a
  , AllF SingE (ColumnNames sc tab)
  , SingI (ColumnNames sc tab)
  ) => HasColumnByDBType sc tab col a (f ('DBCustomType scn a tn)) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col  

instance {-# OVERLAPPABLE #-}
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  , AllF SingE (ColumnNames sc tab)
  , SingI (ColumnNames sc tab)
  ) => HasColumnByDBType sc tab col a atrep where
  getColByDBTypeRep _ = getColumnName @sc @tab @col  



type family GetTypeMappings (db :: Type) where
  GetTypeMappings db = GetTypeMappings' db (Types db)

type family GetTypeMappings' (db :: Type) (ts :: [Type]) where
  GetTypeMappings' db (t ': ts) = '(GetPMT (Rep t), TypeMappings db t) ': GetTypeMappings' db ts
  GetTypeMappings' db '[]       = '[]

data TagHK b a = Tag b a

type family TagEach (db :: tk) (ent :: [k]) :: [TagHK tk k] where
  TagEach db (ent ': ents) = 'Tag db ent ': TagEach db ents
  TagEach db '[]           = '[]

type family UnTag (t :: TagHK k1 k) :: k where
  UnTag ('Tag _ a) = a

data instance Sing (a :: TagHK tk k) where
  STag :: Sing tag -> Sing a -> Sing ('Tag tag a)

data instance Sing (uq :: UniqueCT) where
  SUniqueOn :: Sing uniqFlds -> Sing uniqOn -> Sing ('UniqueOn uniqFlds uniqOn)

data instance Sing (fk :: ForeignRef a) where
  SRefBy :: Sing cols -> Sing reft -> Sing refCols -> Sing fkname -> Sing ('RefBy cols reft refCols fkname)
  SRef   :: Sing col -> Sing reft -> Sing fkname -> Sing ('Ref col reft fkname)

data instance Sing (ch :: CheckCT) where
  SCheck :: Sing cols -> Sing cname -> Sing ('CheckOn cols cname)

data instance Sing (uq :: Sequence) where
  SPGSerial :: Sing col -> Sing seqn -> Sing ('PGSerial col seqn)
  SPGOwned  :: Sing col -> Sing seqn -> Sing ('PGOwned col seqn)

data instance Sing (t :: TypeName Symbol) where
  STypeName :: Sing (pkgN :: Symbol) -> Sing (modN :: Symbol) -> Sing (typN :: Symbol) -> Sing ('TypeName pkgN modN typN)

instance ( SingI a
         , SingI tag
         ) => SingI ('Tag tag a)  where
  sing = STag sing sing

instance (SingI uniqFlds, SingI uniqOn) => SingI ('UniqueOn uniqFlds uniqOn) where
  sing = SUniqueOn sing sing 

instance (SingI cols, SingI reft, SingI refcols, SingI fkname) => SingI ('RefBy cols reft refcols fkname) where
  sing = SRefBy sing sing sing sing

instance (SingI col, SingI reft, SingI fkname) => SingI ('Ref col reft fkname) where
  sing = SRef sing sing sing

instance (SingI col, SingI seqn) => SingI ('PGSerial col seqn)  where
  sing = SPGSerial sing sing

instance (SingI col, SingI seqn) => SingI ('PGOwned col seqn)  where
  sing = SPGOwned sing sing

instance ( SingI pkgN
         , SingI modN
         , SingI typN
         ) => SingI ('TypeName (pkgN :: Symbol) (modN :: Symbol) (typN :: Symbol)) where
  sing = STypeName sing sing sing

instance ( SingI cols
         , KnownSymbol cname
         , All SingE cols
         ) => SingI ('CheckOn cols cname) where
  sing = SCheck sing sing

type family UqCtx (ctx :: Symbol -> Constraint) (uq :: UniqueCT) :: Constraint where
  UqCtx ctx ('UniqueOn uniqFlds uniqOn) = (All ctx uniqFlds, ctx uniqOn)

type family CkCtx (ctx :: Symbol -> Constraint) (uq :: CheckCT) :: Constraint where
  CkCtx ctx ('CheckOn ckFlds ckn) = (All ctx ckFlds, ctx ckn)

type family FKCtxTy (ctx :: Symbol -> Constraint) (fk :: ForeignRef Type) :: Constraint where
  FKCtxTy ctx ('RefBy cols reft refcols name) = (All ctx cols, All ctx refcols, ctx name, SingE (GetPMT (Rep reft)), Generic reft
                                                , SingI (GetPMT (Rep reft))
                                              )
  FKCtxTy ctx ('Ref col reft name)            = ( ctx col, ctx name, SingE (GetPMT (Rep reft)), Generic reft
                                                , SingI (GetPMT (Rep reft))
                                              )

type family FKCtxTyN (ctx :: Symbol -> Constraint) (fk :: ForeignRef (TypeName Symbol)) :: Constraint where
  FKCtxTyN ctx ('RefBy cols reft refcols name) = ( All ctx cols, All ctx refcols, SingE reft, ctx name
                                                 )
  FKCtxTyN ctx ('Ref col reft name)            = ( ctx col, SingE reft, ctx name
                                                 )

type family TagDBTypeCtx (taggedDbt :: TagHK DbK DBTypeK) where
  TagDBTypeCtx ('Tag dbT dbTy) = (SingE dbTy)

instance (TagDBTypeCtx taggedDbt) => SingE (taggedDbt :: TagHK DbK DBTypeK) where
  type Demote taggedDbt     = Type.DBType
  fromSing (STag _ stype) = fromSing stype

showDBTypeSing :: forall db dbTy.
                   ( DBTypeCtx dbTy
                   ) => Sing (db :: DbK) -> Sing (dbTy :: DBTypeK) -> Type.DBType
showDBTypeSing _ dbT = fromSing dbT

instance (UqCtx SingE uq) => SingE (uq :: UniqueCT) where
  type Demote (uq :: UniqueCT)         = (Demote (Any :: [Symbol]), Demote (Any :: Symbol))
  fromSing (SUniqueOn uniqFlds uniqOn) = (fromSing uniqFlds, fromSing uniqOn)

instance (CkCtx SingE uq) => SingE (uq :: CheckCT) where
  type Demote (uq :: CheckCT)         = (Demote (Any :: [Symbol]), Demote (Any :: Symbol))
  fromSing (SCheck chkFlds chkn) = (fromSing chkFlds, fromSing chkn)

instance ( FKCtxTy SingE fk
         ) => SingE (fk :: ForeignRef Type) where
  type Demote (fk :: (ForeignRef Type)) = ForeignRefD
  fromSing (SRefBy cols reft refcols fkname) =
    RefByD (fromSing fkname) (fromSing cols)
           (fromSing (singTypeName reft)) (fromSing refcols)
  fromSing (SRef coln reft fkname) =
    let cols = [fromSing coln]
    in RefByD (fromSing fkname) cols (fromSing (singTypeName reft)) cols

instance ( FKCtxTyN SingE fk
         ) => SingE (fk :: ForeignRef (TypeName Symbol)) where
  type Demote (fk :: ForeignRef (TypeName Symbol)) = ForeignRefD
  fromSing (SRefBy cols reft refcols fkname) =
    RefByD (fromSing fkname) (fromSing cols)
           (fromSing reft) (fromSing refcols)
  fromSing (SRef coln reft fkname) =
    let cols = [fromSing coln]
    in RefByD (fromSing fkname) cols (fromSing reft) cols

type family GetPMT (rep :: Type -> Type) :: TypeName Symbol where
  GetPMT (D1 ('MetaData tyName modName pkgName _) _) =
    'TypeName pkgName modName tyName

singTypeName :: forall t.
                 ( Generic t
                 , SingI (GetPMT (Rep t))
                 ) => Sing t -> Sing (GetPMT (Rep t))
singTypeName _ = sing                 

fromSingDefTabName :: forall reft.
                      ( SingI (DefaultTableName reft)
                      , SingE (DefaultTableName reft)
                      ) => Sing reft -> Demote (DefaultTableName reft :: Symbol)
fromSingDefTabName _ = fromSing (sing :: Sing (DefaultTableName reft))
                      
tabName :: forall sc t proxy.
  KnownSymbol (TableName sc t) => proxy sc -> proxy t -> String
tabName _ _ = symbolVal (Proxy :: Proxy (TableName sc t))

instance SingE (seq :: Sequence) where
  type Demote seq = (Text, Text, SequenceType)
  fromSing (SPGSerial coln seqn) = (fromSing coln, fromSing seqn, SeqSerial)
  fromSing (SPGOwned coln seqn)  = (fromSing coln, fromSing seqn, SeqOwned)

instance SingE (t :: TypeName Symbol) where
  type Demote t = TypeName Text
  fromSing (STypeName p m f) = TypeName (fromSing p)
                                        (fromSing m)
                                        (fromSing f)

type family ValidateTableProps (sc :: Type) (tab :: Type) :: Constraint where
  ValidateTableProps sc tab =
    ( MissingField tab (ElemFields1 (OriginalTableFields tab) (PrimaryKey sc tab))
    , MissingField tab (ElemFields1 (OriginalTableFields tab) (HasDefault sc tab))
    , MissingField tab (ElemUniqs (OriginalTableFields tab) (Unique sc tab))
    , ValidateTabFk sc tab (ForeignKey sc tab)
    , ValidateTabCk tab (Check sc tab)
    , ValidateTabIx tab
{- TODO: Enable Name Validations    
    , ValidateNames tab ('Text "at the usage of ColumnNames") (OriginalTableFields tab) (ColumnNames sc tab)
    , ValidateNames tab ('Text "at the usage of ForeignKeyNames") (OriginalFKNames sc tab) (ForeignKeyNames sc tab)
    , ValidateNames tab ('Text "at the usage of UniqueNames") (OriginalUQNames sc tab) (UniqueNames sc tab)
    , ValidateNames tab ('Text "at the usage of CheckNames") (OriginalCheckNames sc tab) (CheckNames sc tab)
    , ValidateNames tab ('Text "at the usage of SequenceNames") (OriginalSequenceNames sc tab) (SequenceNames sc tab)
-}
    )

type family OriginalFKNames sc tab :: [Symbol] where
  OriginalFKNames sc tab = GetFKNames (ForeignKey sc tab)

type family GetFKNames (fks :: [ForeignRef a]) :: [Symbol] where
  GetFKNames ('RefBy _ _ _ n ': refs) = n ': GetFKNames refs
  GetFKNames ('Ref _ _ n ': refs)     = n ': GetFKNames refs
  GetFKNames '[]                      = '[]

type family OriginalUQNames db tab :: [Symbol] where
  OriginalUQNames db tab = GetUQNames (Unique db tab)

type family GetUQNames (uqs :: [UniqueCT]) :: [Symbol] where
  GetUQNames ('UniqueOn _ n ': uqs) = n ': GetUQNames uqs
  GetUQNames '[]                    = '[]

type family OriginalCheckNames sc tab :: [Symbol] where
  OriginalCheckNames sc tab = GetCheckNames (Check sc tab)

type family GetCheckNames (chks :: [CheckCT]) :: [Symbol] where
  GetCheckNames ('CheckOn _ n ': chks) = n ': GetCheckNames chks
  GetCheckNames '[]                   = '[]

type family OriginalSequenceNames sc tab :: [Symbol] where
  OriginalSequenceNames sc tab = GetSequenceNames (TableSequence sc tab)

type family GetSequenceNames (seqs :: [Sequence]) :: [Symbol] where
  GetSequenceNames ('PGSerial _ n ': seqs) = n ': GetSequenceNames seqs
  GetSequenceNames ('PGOwned _ n ': seqs)  = n ': GetSequenceNames seqs
  GetSequenceNames '[]                     = '[]

{-
type family ValidateNames (tab :: Type) (msg :: ErrorMessage) (flds :: [k]) (map :: [(Symbol, Symbol)]) :: Constraint where
  ValidateNames tab msg flds ('(fn, _) ': maps) = (ValidateAlias' tab msg flds fn, ValidateNames tab msg flds maps)
  ValidateNames _ _ flds '[] = ()

type family ValidateAlias' (tab :: Type) (msg :: ErrorMessage) (flds :: [k]) (aliased :: Symbol) :: Constraint where
  ValidateAlias' _ _ (fn ::: _ ': flds) fn     = ()
  ValidateAlias' _ _ (fn ': flds) fn           = ()  
  ValidateAlias' tab msg (fn ': flds) cn       = ValidateAlias' tab msg flds cn  
  ValidateAlias' tab msg '[] cn                = TypeError ('Text "column " ':<>: ('ShowType cn) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab) ':$$: msg)
-}
  
type family ValidateTabPk (tab :: Type) (pks :: [Symbol]) :: Constraint where
  ValidateTabPk tab (p ': ps) = If (ElemField (OriginalTableFields tab) p) (ValidateTabPk tab ps) (TypeError ('Text "column " ':<>: ('ShowType p) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab)))
  ValidateTabPk tab '[]       = ()

type family ValidateTabFk sc tab (fks :: [ForeignRef Type]) :: Constraint where
  ValidateTabFk fk tab ('Ref fn reft _ ': fks)
    = ( MatchFkFields fk tab reft (FindFields (OriginalTableFields tab) '[fn]) (FindFields (OriginalTableFields reft) '[fn])
--        MatchFkRefFld tab reft fn (FindField (OriginalTableFields tab) fn) (FindField (OriginalTableFields reft) (HeadPk reft (PrimaryKey fk reft)))
      , ValidateRefPk reft '[fn] (PrimaryKey fk reft)
      , ValidateTabFk fk tab fks
      )
  ValidateTabFk fk tab ('RefBy fkeys reft rkeys _ ': fks)
    = ( MatchFkFields fk tab reft (FindFields (OriginalTableFields tab) fkeys) (FindFields (OriginalTableFields reft) rkeys)
      , ValidateRefPk reft rkeys (PrimaryKey fk reft)
      , ValidateTabFk fk tab fks
      )
  ValidateTabFk fk tab '[]         = ()

type family ValidateRefPk (reft :: Type) (rkeys :: [Symbol]) (pkeys :: [Symbol]) :: Constraint where
  ValidateRefPk reft keys keys = ()
  ValidateRefPk reft rpkeys pkeys = TypeError ('Text "In foreign key declaration:" ':$$: 'ShowType rpkeys ':<>: 'Text " is not a primary key of table " ':<>: 'ShowType reft)

type family MatchFkFields fk tab reft (fkeys :: [Either Symbol Type]) (rkeys :: [Either Symbol Type]) :: Constraint where
  MatchFkFields fk tab reft ('Right (fn1 ::: t) ': fkeys) ('Right (fn2 ::: t) ': rkeys)
    = MatchFkFields fk tab reft fkeys rkeys
  -- NOTE: only to handle nullable foreignkeys
  MatchFkFields fk tab reft ('Right (fn1 ::: Maybe t) ': fkeys) ('Right (fn2 ::: t) ': rkeys)
    = MatchFkFields fk tab reft fkeys rkeys      
  MatchFkFields fk tab reft ('Right (fn1 ::: t1) ': fkeys) ('Right (fn2 ::: t2) ': rkeys)
    = ( TypeError ('Text "Type mismatch between foreign key and primary key"
                  ':$$: ('ShowType fn1) ':<>: 'Text ": " ':<>: ('ShowType t1)
                  ':$$: ('ShowType fn2) ':<>: 'Text ": " ':<>: ('ShowType t2)
                  )
      , MatchFkFields fk tab reft fkeys rkeys
      )
  MatchFkFields fk tab reft ('Left fn1 ': fkeys) ('Right _ ': rkeys)
    = (TypeError ('Text "column " ':<>: ('ShowType fn1) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab)), MatchFkFields fk tab reft fkeys rkeys)
  MatchFkFields fk tab reft ('Right _ ': fkeys) ('Left fn2 ': rkeys)
    = (TypeError ('Text "column " ':<>: ('ShowType fn2) ':<>: 'Text " does not exist in table " ':<>: ('ShowType reft)), MatchFkFields fk tab reft fkeys rkeys)
  MatchFkFields fk tab reft ('Left fn1 ': fkeys) ('Left fn2 ': rkeys)
    = ( TypeError ('Text "column " ':<>: ('ShowType fn1) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab))
      , TypeError ('Text "column " ':<>: ('ShowType fn2) ':<>: 'Text " does not exist in table " ':<>: ('ShowType reft))
      , MatchFkFields fk tab reft fkeys rkeys
      )
  MatchFkFields fk tab reft '[] (_ ': _)
    = TypeError ('Text "Number of foreign key column is less than that of its referenced primary keys")
  MatchFkFields fk tab reft (_ ': _) '[]
    = TypeError ('Text "Number of foreign key column is greater than that of its referenced primary keys")
  MatchFkFields _ _ _ '[] '[] = ()
  
type family ValidateTabCk tab (chks :: [CheckCT]) :: Constraint where
  ValidateTabCk tab ('CheckOn fs cn ': chks) = ValidateTabCk' (ElemFields1 (OriginalTableFields tab) fs) tab cn chks
  ValidateTabCk tab '[] = ()

type family ValidateTabCk' (mis :: Maybe Symbol) (tab :: Type) (cn :: Symbol) (chks :: [CheckCT]) where
  ValidateTabCk' ('Just fn) tab cn chks = (TypeError ('Text "column " ':<>: ('ShowType fn) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab)))
  ValidateTabCk' 'Nothing tab cn chks   = ValidateTabCk tab chks

type family ValidateTabIx tab :: Constraint where
  ValidateTabIx tab = ()

type family ElemFields1 (flds :: [Type]) (fs :: [Symbol]) :: Maybe Symbol where
  ElemFields1 flds (f :fs) = If (ElemField flds f) (ElemFields1 flds fs) ('Just f)
  ElemFields1 flds '[]     = 'Nothing

type family ElemFields2 (flds :: [Type]) (fss :: [[Symbol]]) :: Maybe Symbol where
  ElemFields2 flds (fs :fss) = ElemFields2' (ElemFields1 flds fs) flds fss
  ElemFields2 flds '[]       = 'Nothing

type family ElemFields2' (may :: Maybe Symbol) (flds :: [Type]) (fss :: [[Symbol]])  :: Maybe Symbol where
  ElemFields2' ('Just fn) flds fss = ('Just fn)
  ElemFields2' 'Nothing flds fss   = ElemFields2 flds fss

type family ElemUniqs (flds :: [Type]) (uniqs :: [UniqueCT]) :: Maybe Symbol where
  ElemUniqs flds ('UniqueOn fs _ : uqs) = ElemUniqs' (ElemFields1 flds fs) flds uqs
  ElemUniqs flds '[]                  = 'Nothing

type family ElemUniqs' (may :: Maybe Symbol) (flds :: [Type]) (fss :: [UniqueCT])  :: Maybe Symbol where
  ElemUniqs' ('Just fn) flds uqs = 'Just fn
  ElemUniqs' 'Nothing flds uqs    = ElemUniqs flds uqs

type family MissingField (tab :: Type) (fn :: Maybe Symbol) :: Constraint where
  MissingField tab ('Just fn) = TypeError ('Text "column " ':<>: ('ShowType fn) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab))
  MissingField tab 'Nothing   = ()

type family MissingDefault (tab :: Type) (fn :: Symbol) (isElem :: Bool) :: Constraint where
  MissingDefault _ _ 'True     = ()
  MissingDefault tab fn 'False = TypeError ('Text "Default for column " ':<>: ('ShowType fn) ':<>: 'Text " is not set in table " ':<>: ('ShowType tab))

type family MissingCheck (tab :: Type) (fn :: Symbol) (isElem :: Bool) :: Constraint where
  MissingCheck _ _ 'True     = ()
  MissingCheck tab fn 'False = TypeError ('Text "Check expression for column " ':<>: ('ShowType fn) ':<>: 'Text " is not set in table " ':<>: ('ShowType tab))

type family GetAllUniqs (uqs :: [UniqueCT]) (colMap :: [(Symbol, Symbol)]) :: [[Symbol]] where
   GetAllUniqs ('UniqueOn fs _ : uqs) colMap = (MapAliasedCol fs colMap) ': GetAllUniqs uqs colMap
   GetAllUniqs '[]                        _  = '[]

type family GetUniqBy (un :: Symbol) (uqs :: [UniqueCT]) :: Maybe [Symbol] where
  GetUniqBy un ('UniqueOn fs un : uqs)   = 'Just fs
  GetUniqBy un1 ('UniqueOn fs un2 : uqs) = GetUniqBy un1 uqs
  GetUniqBy _ '[]                        = 'Nothing

data ForeignRef a
  = RefBy [Symbol] a [Symbol] Symbol
  | Ref Symbol a Symbol

data UniqueCT = UniqueOn [Symbol] Symbol
data Uq sc (un :: Symbol) = Uq

instance un ~ uqn => IsLabel un (Uq sc uqn) where
#if __GLASGOW_HASKELL__ > 800
  fromLabel = Uq
#else
  fromLabel _ = Uq
#endif

data CheckCT = CheckOn [Symbol] Symbol

data Ix = Ix Symbol

data IgnoredCol
  = IgnoreRest
  | IgnoreOnly [Symbol]
  | IgnoreExcept [Symbol]
  | IgnoreNone

data Def (sc :: Type) (tab :: k) (fn :: Symbol) = forall v.Def (PQ.Expr sc v)

def :: forall (fn :: Symbol) (tab :: Type) (sc :: Type) v.(ValidateDBFld tab fn v) => PQ.Expr sc v -> Def sc tab fn
def = Def

instance ( ValidateDBFld tab un a
         , un ~ fn
         , v ~ PQ.Expr sc a
         ) => IsLabel un (v -> Def sc (tab :: Type) fn) where
#if __GLASGOW_HASKELL__ > 800
  fromLabel v = def @un @tab @sc v
#else
  fromLabel _ v = def @un @tab @sc v
#endif

data DBDefaults (sc :: Type) tab = forall xs. (All KnownSymbol xs) => DBDefaults (HList (Def sc tab) xs)

end :: HList f '[]
end = Nil

dbDefaults :: forall tab sc xs.
              ( All KnownSymbol xs
              , ValidateDefExprs sc tab (HasDefault sc tab) xs
              ) => HList (Def sc tab) xs -> DBDefaults sc tab
dbDefaults = DBDefaults

type family ValidateDefExprs (sc :: Type) (tab :: Type) (defs :: [Symbol]) (setDefs :: [Symbol]) :: Constraint where
  ValidateDefExprs sc tab (def ': defs) setDefs =
    ( MissingDefault tab def (Elem setDefs def)
    , ValidateDefExprs sc tab defs setDefs
    )
  ValidateDefExprs _ _ '[] _ = ()

data Chk (sc :: Type) (tab :: k) (chk :: CheckCT) = forall val.(ApCheckOnExpr chk val) => Chk val

data DBChecks (sc :: Type) tab = forall chks. (All CheckExpr chks) => DBChecks (HList (Chk sc tab) chks)

getColumnName :: forall sc tab (fn :: Symbol) a.
  ( Table sc tab
  , AllF SingE (ColumnNames sc tab)
  , SingI (ColumnNames sc tab)
  , R.HasField fn tab a
  , KnownSymbol fn
  ) => PQ.Expr sc a
getColumnName =
  let
    caliases = getColumnAliasMap @sc @tab
    fname = T.pack $ symbolVal (Proxy @fn)
    cname = maybe (defHSNameToDBName fname) id $ HM.lookup fname caliases
    cexpr = PQ.BaseTableAttrExpr $ cname
  in PQ.Expr cexpr
{-# INLINE getColumnName #-}
                    

type family ApCheckOnExpr chk val where
  ApCheckOnExpr ('CheckOn cols name) v = ApCheckExpr cols name v

type family LookupCheck (chks :: [CheckCT]) (cn :: Symbol) :: Maybe [Symbol] where
  LookupCheck ('CheckOn args cn ': chks) cn  = 'Just args
  LookupCheck ('CheckOn args cn1 ': chks) cn = LookupCheck chks cn
  LookupCheck '[] cn                         = 'Nothing

type family UnifyCheck (sc :: Type) (tab :: Type) (cn :: Symbol) (flds :: [Type]) (args :: Maybe [Symbol]) (val :: Type) :: Constraint where
  UnifyCheck _ tab cn flds 'Nothing val = TypeError ('Text "check constraint " ':<>: 'ShowType cn ':<>: 'Text " does not exist on table " ':<>: 'ShowType tab)
  UnifyCheck sc tab cn flds ('Just args) val = UnifyOrErr (SeqEither (MkCheckFn sc tab args val flds)) val

-- TODO: flds are dummied out  
type family MkCheckFn (sc :: Type) (tab :: Type) (args :: [Symbol]) (val :: Type) (flds :: [Type]) :: [Either ErrorMessage Type] where
  MkCheckFn sc tab (fn ': fs) chkFun flds = Note (ColNotFoundMsg fn tab) (FMapMaybe (PQ.Expr sc) (FindField flds fn)) ': MkCheckFn sc tab fs chkFun flds
  MkCheckFn sc tab '[] r flds = '[ 'Right (PQ.Expr sc Bool)]


check :: forall (cn :: Symbol) (sc :: Type) (tab :: Type) val args.
        ( args ~ LookupCheck (Check sc tab) cn
        , UnifyCheck sc tab cn (OriginalTableFields tab) args val
        , ApCheckExpr (PartialJust args) cn val
        ) => val -> Chk sc tab ('CheckOn (PartialJust args) cn)
check = Chk

dbChecks :: forall tab (sc :: Type) chks.
            ( All CheckExpr chks
            , ValidateCheckExpr sc tab (Check sc tab) chks
            ) => HList (Chk sc tab) chks -> DBChecks sc tab
dbChecks = DBChecks

type family ValidateCheckExpr sc tab (chks :: [CheckCT]) (setChks :: [CheckCT]) :: Constraint where
  ValidateCheckExpr sc tab ('CheckOn _ chkName ': chks) setChks =
    ( MissingCheck tab chkName (ElemCheck chkName setChks)
    , ValidateCheckExpr sc tab chks setChks
    )
  ValidateCheckExpr _ _ _ _ = ()

type family ElemCheck (chkName :: Symbol) (setChks :: [CheckCT]) where
  ElemCheck cn (('CheckOn _ cn) ': chks) = 'True
  ElemCheck cn (_ ': chks)               = ElemCheck cn chks
  ElemCheck _  '[]                       = 'False

type family ValidateDBFld tab (fn :: Symbol) t :: Constraint where
  ValidateDBFld tab fn t = UnifyField (OriginalTableFields tab) fn t ('Text "column " ':<>: ('ShowType fn) ':<>: 'Text " does not exist in table " ':<>: ('ShowType tab))

type DefaultTableName t    = DefaultTypeName t
type DefaultDatabaseName t = DefaultTypeName t

type family DefaultTypeName (t :: Type) :: Symbol where
  DefaultTypeName t = GenTyCon (Rep t)

{-
type family GetSchemaName (t :: Type) :: Symbol where
  GetSchemaName ()   = Schema ()
  GetSchemaName db  = Schema db
-}

type OriginalTableFields t = GenTabFields (Rep t)

type family TableFields (sc :: Type) (t :: Type) :: [Type] where
  TableFields sc t = TableFields' (GenTabFields (Rep t)) (ColumnNames sc t)

type family TableFields' (flds :: [Type]) (colMap :: [(Symbol, Symbol)]) :: [Type] where
  TableFields' ((fn ::: ft) ': flds) colMap = (AliasedCol fn colMap ::: ft) ': (TableFields' flds colMap)
  TableFields' '[] colMap = '[]

type family MapAliasedCol (fns :: [Symbol]) (colMap :: [(Symbol, Symbol)]) :: [Symbol] where
  MapAliasedCol (fn ': fns) colMap = AliasedCol fn colMap ': MapAliasedCol fns colMap
  MapAliasedCol '[]         _      = '[]

type family AliasedCol (fn :: Symbol) (colMap :: [(Symbol, Symbol)]) :: Symbol where
  AliasedCol fn ('(fn, alias) ': colMap) = alias
  AliasedCol fn (_ ': colMap)            = AliasedCol fn colMap
  AliasedCol fn '[]                      = fn
  
type family GetTypeFields (t :: Type) :: [(Symbol, [Type])] where
  GetTypeFields t              = GenTyFields (Rep t)

-- newtype EnumType a = EnumType a
-- newtype SumType a = SumType a

recordToList :: HList (Const a) rs -> [a]
recordToList Nil = []
recordToList (x :& xs) = getConst x : recordToList xs

class SingCols (sc :: Type) (cols :: [Type]) (colMap :: [(Symbol, Symbol)]) where
  singCols :: Proxy sc -> Proxy cols -> Proxy colMap -> HList (Const Column) cols

newtype DConAttr = DConAttr (ColName, [Column])

class SingAttrs (sc :: Type) (attrs :: [(Symbol, [Type])]) where
  singAttrs :: Proxy sc -> Proxy attrs -> HList (Const DConAttr) attrs

instance ( SingAttrs sc cons
         , SingCols sc flds '[] -- TODO: composite type alias
         , KnownSymbol c
         ) => SingAttrs sc ('(c, flds) ': cons) where
  singAttrs pxyDB _ =
    let colHLists = singCols (Proxy @sc) (Proxy @flds) (Proxy @('[]))
        cn = T.pack $ symbolVal (Proxy @c)
    in Const (DConAttr (cn, recordToList colHLists)) :& singAttrs pxyDB (Proxy @cons)

instance SingAttrs db '[] where
  singAttrs _ _ = Nil

happlyChkExpr :: (All CheckExpr chks) => [ColumnInfo] -> [(T.Text, T.Text)] -> HList (Chk sc tab) chks -> [(T.Text, PQ.PrimExpr)]
happlyChkExpr cis chkMap (v :& vs) = checkExpr cis chkMap v : happlyChkExpr cis chkMap vs
happlyChkExpr _cis _chkMap Nil     = []

happlyDefExprs :: (All KnownSymbol xs) => [ColumnInfo] -> HList (Def sc tab) xs -> [(T.Text, PQ.PrimExpr)]
happlyDefExprs cis (v :& vs) = defExpr cis v : happlyDefExprs cis vs
happlyDefExprs _ Nil         = []

class CheckExpr (chk :: CheckCT) where
  checkExpr :: [ColumnInfo] -> [(T.Text, T.Text)] -> Chk sc tab chk -> (T.Text, PQ.PrimExpr)

instance CheckExpr ('CheckOn chkOns chkName) where
  checkExpr cis chkMaps (Chk val') = apCheckExpr (Proxy @chkOns) (Proxy @chkName) cis chkMaps val'

class ApCheckExpr (chkOns :: [Symbol]) (chkName :: Symbol) val where
  apCheckExpr :: Proxy chkOns -> Proxy chkName -> [ColumnInfo] -> [(T.Text, T.Text)] -> val -> (T.Text, PQ.PrimExpr)

instance ( ApCheckExpr chkOns chkName b
         , KnownSymbol chkOn
         ) => ApCheckExpr (chkOn ': chkOns) chkName (PQ.Expr sc a -> b) where
  apCheckExpr _ pChkN cis chkMaps v =
    let colE = PQ.unsafeCol [dbColN]
        colN = T.pack (symbolVal (Proxy @chkOn))
        dbColN = getDbColumnName cis colN
    in  apCheckExpr (Proxy @chkOns) pChkN cis chkMaps (v colE)

instance (KnownSymbol chkName) => ApCheckExpr '[] chkName (PQ.Expr sc a) where
  apCheckExpr _ _ _ chkMaps e = (dbChkName, PQ.getExpr e)
    where dbChkName = getDbCheckName chkMaps (T.pack (symbolVal (Proxy @chkName)))

defExpr :: forall db tab fld. (KnownSymbol fld) => [ColumnInfo] -> Def db tab fld -> (T.Text, PQ.PrimExpr)
defExpr cis (Def (PQ.Expr e)) = (dbColN, e)
  where dbColN = getDbColumnName cis (T.pack (symbolVal (Proxy @fld)))

-- Value level counterparts
type HaskName = Text
type DBName   = Text
-- NOTE: Step count start from 0
--       If there are there are 3 steps, then there will be
--       corresponding 2 changesets [0 -> 1, 1 -> 2]
type Step = Int

mkTypeName :: Text -> Text -> Text -> TypeName Text
mkTypeName p m t =
  TypeName { _packageName = p
           , _moduleName  = m
           , _typeName    = t
           }

data TypeName a = TypeName { _packageName :: a
                           , _moduleName  :: a
                           , _typeName    :: a
                           }
                deriving (Show, Eq)

packageName :: Lens' (TypeName a) a
packageName k t = fmap (\a -> t { _packageName = a }) (k (_packageName t))

moduleName :: Lens' (TypeName a) a
moduleName k t = fmap (\a -> t { _moduleName = a }) (k (_moduleName t))

typeName :: Lens' (TypeName a) a
typeName k t = fmap (\a -> t { _typeName = a }) (k (_typeName t))

newtype DBType = DBType { _dbType :: Type.DBType }
               deriving (Show, Eq)

dbType :: Lens' DBType Type.DBType
dbType k t = fmap coerce (k (coerce t))

data EntityName a = EntityName { _hsName :: a
                               , _dbName :: DBName
                               } deriving (Show, Eq)

mkEntityName :: a -> DBName -> EntityName a
mkEntityName hsn dbn = EntityName { _hsName = hsn
                                   , _dbName = dbn
                                   }

hsName :: Lens' (EntityName a) a
hsName k t = fmap (\a -> t { _hsName = a }) (k (_hsName t))

dbName :: Lens' (EntityName a) DBName
dbName k t = fmap (\a -> t { _dbName = a }) (k (_dbName t))

type EntityNameWithHask = EntityName HaskName
type EntityNameWithType = EntityName (TypeName Text)

eqBy :: (Eq a) => Lens' s a -> s -> s -> Bool
eqBy l old new = (old ^. l) == (new ^. l)

mkTypeNameInfo :: Type.DBType -> TypeNameMap -> TypeNameInfo
mkTypeNameInfo pgt tnm =
  TypeNameInfo { _typeNameVal = pgt
               , _typeNameMap = tnm
               }

data TypeNameInfo = TypeNameInfo { _typeNameVal   :: Type.DBType 
                                 , _typeNameMap   :: TypeNameMap
                                 } deriving (Show, Eq)

{-
addEnumValAfter :: Text -> Text -> TypeNameMap -> TypeNameMap
addEnumValAfter eVal eAfter (EnumTypeNM et es) =
  EnumTypeNM et (go es)

  where go (ev : evs) = case ev == eAfter of
          True  -> ev : eVal : evs
          False -> ev : go evs
        go [] =  error "Panic: unexpected empty list @addEnumValAfter"
  

addEnumValBefore :: Text -> Text -> TypeNameMap -> TypeNameMap
addEnumValBefore eVal eBefore (EnumTypeNM et es) =
  EnumTypeNM et (go es)

  where go (ev : evs) = case ev == eBefore of
          True  -> eVal : ev : evs
          False -> ev : go evs
        go _ = error "Panic: unexpected empty list @addEnumValBefore"

addEnumVal :: Text -> TypeNameMap -> TypeNameMap
addEnumVal eVal (EnumTypeNM et evs) =
  EnumTypeNM et (evs ++ [eVal])
-}

typeNameVal :: Lens' TypeNameInfo Type.DBType 
typeNameVal k t = fmap (\a -> t { _typeNameVal = a }) (k (_typeNameVal t))

typeNameMap :: Lens' TypeNameInfo TypeNameMap
typeNameMap k t = fmap (\a -> t { _typeNameMap = a }) (k (_typeNameMap t))

mkSchemaInfo :: EntityNameWithType -> [TypeNameInfo] -> Step -> Step -> TableInfos -> EntityNameWithType -> DbK -> SchemaInfo
mkSchemaInfo n tnis b v tis dbn dbk =
  SchemaInfo { _schemaName    = n
             , _typeNameInfos = tnis
             , _ignoredTabs   = ()
             , _baseline      = b
             , _version       = v
             , _tableInfos    = tis
             , _databaseName  = dbn
             , _dbKind        = dbk
             } 
             
data SchemaInfo = SchemaInfo { _schemaName     :: EntityNameWithType
                             , _typeNameInfos  :: [TypeNameInfo]
                             , _ignoredTabs    :: ()
                             , _baseline       :: Step
                             , _version        :: Step
                             , _tableInfos     :: TableInfos
                             , _databaseName   :: EntityNameWithType
                             , _dbKind         :: DbK
                             } deriving (Show, Eq)

{-
mkDatabaseInfo :: EntityNameWithType -> [SchemaInfo] -> DbK -> DatabaseInfo
mkDatabaseInfo et sis dbk =
  DatabaseInfo { _name = et
               , _schemaInfos = sis
               , _dbKind = dbk 
               }

data DatabaseInfo = DatabaseInfo { _name :: EntityNameWithType
                                 , _schemaInfos :: [SchemaInfo]
                                 , _dbKind :: DbK
                                 } deriving (Show, Eq)
-}

newtype TableInfos = TableInfos { _getTableInfos :: [TableInfo] }
                   deriving (Show, Eq)

databaseName :: Lens' SchemaInfo EntityNameWithType 
databaseName k t = fmap (\a -> t { _databaseName = a }) (k (_databaseName t))

schemaName :: Lens' SchemaInfo EntityNameWithType
schemaName k t = fmap (\a -> t { _schemaName = a }) (k (_schemaName t))

typeNameInfos :: Lens' SchemaInfo [TypeNameInfo]
typeNameInfos k t = fmap (\a -> t { _typeNameInfos = a }) (k (_typeNameInfos t))

typeNameInfoAt :: Type.DBType -> Traversal' SchemaInfo TypeNameInfo
typeNameInfoAt pgt = typeNameInfos . ixBy pgt _typeNameVal

baseline :: Lens' SchemaInfo Step
baseline k t = fmap (\a -> t { _baseline = a }) (k (_baseline t))

version :: Lens' SchemaInfo Step
version k t = fmap (\a -> t { _version = a }) (k (_version t))

tableInfos :: Lens' SchemaInfo TableInfos
tableInfos k t = fmap (\a -> t { _tableInfos = a }) (k (_tableInfos t))

dbKind :: Lens' SchemaInfo DbK
dbKind k t = fmap (\a -> t { _dbKind = a }) (k (_dbKind t))

tableInfoAt :: TypeName T.Text -> Traversal' SchemaInfo TableInfo
tableInfoAt hsN = tableInfos . coerceL . ixBy hsN (_hsName . _tableName)

mkTableInfo :: Maybe PrimaryKeyInfo -> [ForeignKeyInfo] -> [DefaultInfo] -> [CheckInfo] -> [UniqueInfo] -> [SequenceInfo] -> EntityNameWithType -> [ColumnInfo] -> TableTypes -> TableInfo
mkTableInfo pki fki di cki uqi sqi tn ci ttyp =
  TableInfo { _primaryKeyInfo = pki
            , _foreignKeyInfo = fki
            , _defaultInfo    = di
            , _checkInfo      = cki
            , _uniqueInfo     = uqi
            , _sequenceInfo   = sqi
            , _tableName      = tn
            , _columnInfo     = ci
            , _ignoredCols    = ()
            , _tableType      = ttyp
            }

data TableInfo = TableInfo { _primaryKeyInfo   :: Maybe PrimaryKeyInfo
                           , _foreignKeyInfo   :: [ForeignKeyInfo]
                           , _defaultInfo      :: [DefaultInfo]
                           , _checkInfo        :: [CheckInfo]
                           , _uniqueInfo       :: [UniqueInfo]
                           , _sequenceInfo     :: [SequenceInfo]
                           , _tableName        :: EntityNameWithType
                           , _columnInfo       :: [ColumnInfo]
                           , _ignoredCols      :: ()
                           , _tableType        :: TableTypes
                           } deriving (Show, Eq)

primaryKeyInfo :: Lens' TableInfo (Maybe PrimaryKeyInfo)
primaryKeyInfo k t = fmap (\a -> t { _primaryKeyInfo = a }) (k (_primaryKeyInfo t))

foreignKeyInfoAt :: HaskName -> Traversal' TableInfo ForeignKeyInfo
foreignKeyInfoAt hsN = foreignKeyInfo . ixBy hsN (_hsName . _fkeyName)

foreignKeyInfoAtDb :: DBName -> Traversal' TableInfo ForeignKeyInfo
foreignKeyInfoAtDb dbN = foreignKeyInfo . ixBy dbN (_dbName . _fkeyName)

foreignKeyInfo :: Lens' TableInfo [ForeignKeyInfo] 
foreignKeyInfo k t = fmap (\a -> t { _foreignKeyInfo = a }) (k (_foreignKeyInfo t))

defaultInfo :: Lens' TableInfo [DefaultInfo]
defaultInfo k t = fmap (\a -> t { _defaultInfo = a }) (k (_defaultInfo t))

checkInfoAt :: HaskName -> Traversal' TableInfo CheckInfo
checkInfoAt hsN = checkInfo . ixBy hsN (_hsName . _checkName)

checkInfoAtDb :: DBName -> Traversal' TableInfo CheckInfo
checkInfoAtDb dbN = checkInfo . ixBy dbN (_dbName . _checkName)

checkInfo :: Lens' TableInfo [CheckInfo]
checkInfo k t = fmap (\a -> t { _checkInfo = a }) (k (_checkInfo t))

uniqueInfo :: Lens' TableInfo [UniqueInfo]
uniqueInfo k t = fmap (\a -> t { _uniqueInfo = a }) (k (_uniqueInfo t))

uniqueInfoAt :: HaskName -> Traversal' TableInfo UniqueInfo
uniqueInfoAt hsN = uniqueInfo . ixBy hsN (_hsName . _uqName)

uniqueInfoAtDb :: DBName -> Traversal' TableInfo UniqueInfo
uniqueInfoAtDb dbN = uniqueInfo . ixBy dbN (_dbName . _uqName)

sequenceInfo :: Lens' TableInfo [SequenceInfo]
sequenceInfo k t = fmap (\a -> t { _sequenceInfo = a }) (k (_sequenceInfo t))

tableName :: Lens' TableInfo EntityNameWithType
tableName k t = fmap (\a -> t { _tableName = a }) (k (_tableName t))

columnInfo :: Lens' TableInfo [ColumnInfo]
columnInfo k t = fmap (\a -> t { _columnInfo = a }) (k (_columnInfo t))

tableType :: Lens' TableInfo TableTypes
tableType k t = fmap (\a -> t { _tableType = a }) (k (_tableType t))

columnInfoAt :: HaskName -> Traversal' TableInfo ColumnInfo
columnInfoAt hsN = columnInfo . ixBy hsN (_hsName . _columnNameInfo)

columnInfoAtDb :: DBName -> Traversal' TableInfo ColumnInfo 
columnInfoAtDb dbN = columnInfo . ixBy dbN (_dbName . _columnNameInfo)

mkColumnInfo :: EntityNameWithHask -> DBType -> ColumnInfo
mkColumnInfo cni ctn =
  ColumnInfo { _columnNameInfo = cni
             , _columnTypeName = ctn
             } 

data ColumnInfo = ColumnInfo { _columnNameInfo :: EntityNameWithHask
                             , _columnTypeName :: DBType
                             } deriving (Show, Eq)

columnNameInfo :: Lens' ColumnInfo EntityNameWithHask
columnNameInfo k t = fmap (\a -> t { _columnNameInfo = a }) (k (_columnNameInfo t))

columnTypeName :: Lens' ColumnInfo DBType
columnTypeName k t = fmap (\a -> t { _columnTypeName = a }) (k (_columnTypeName t))

mkPrimaryKeyInfo :: Text -> [HaskName] -> PrimaryKeyInfo
mkPrimaryKeyInfo pkn pkcols =
  PrimaryKeyInfo { _pkeyName    = pkn
                 , _pkeyColumns = pkcols
                 }

data PrimaryKeyInfo = PrimaryKeyInfo { _pkeyName    :: Text
                                     , _pkeyColumns :: [HaskName]
                                     } deriving (Eq, Show)

pkeyName :: Lens' PrimaryKeyInfo Text 
pkeyName k t = fmap (\a -> t { _pkeyName = a }) (k (_pkeyName t))

pkeyColumns :: Lens' PrimaryKeyInfo [HaskName]
pkeyColumns k t = fmap (\a -> t { _pkeyColumns = a }) (k (_pkeyColumns t))

mkForeignKeyInfo :: EntityNameWithHask -> [HaskName] -> TypeName Text -> [HaskName] -> ForeignKeyInfo
mkForeignKeyInfo n cols reft refCols =
  ForeignKeyInfo { _fkeyName = n
                 , _fkeyColumns = cols
                 , _fkeyRefTable = reft
                 , _fkeyRefColumns = refCols
                 }

data ForeignKeyInfo = ForeignKeyInfo { _fkeyName       :: EntityNameWithHask
                                     , _fkeyColumns    :: [HaskName]
                                     , _fkeyRefTable   :: TypeName Text
                                     , _fkeyRefColumns :: [HaskName]
                                     } deriving (Show, Eq)

fkeyName :: Lens' ForeignKeyInfo EntityNameWithHask 
fkeyName k t = fmap (\a -> t { _fkeyName = a }) (k (_fkeyName t))

fkeyColumns :: Lens' ForeignKeyInfo [HaskName]
fkeyColumns k t = fmap (\a -> t { _fkeyColumns = a }) (k (_fkeyColumns t))

fkeyRefTable :: Functor f => (TypeName Text -> f (TypeName Text)) -> ForeignKeyInfo -> f ForeignKeyInfo
fkeyRefTable k t = fmap (\a -> t { _fkeyRefTable = a }) (k (_fkeyRefTable t))

fkeyRefColumns :: Functor f => ([HaskName] -> f [HaskName]) -> ForeignKeyInfo -> f ForeignKeyInfo
fkeyRefColumns k t = fmap (\a -> t { _fkeyRefColumns = a }) (k (_fkeyRefColumns t))

mkUniqueInfo :: EntityNameWithHask -> [HaskName] -> UniqueInfo
mkUniqueInfo n cols =
  UniqueInfo { _uqName = n
             , _uqColumns = cols
             } 
                             
data UniqueInfo = UniqueInfo { _uqName    :: EntityNameWithHask
                             , _uqColumns :: [HaskName]
                             } deriving (Show, Eq)

uqName :: Lens' UniqueInfo EntityNameWithHask
uqName k t = fmap (\a -> t { _uqName = a }) (k (_uqName t))

uqColumns :: Lens' UniqueInfo [HaskName]
uqColumns k t = fmap (\a -> t { _uqColumns = a }) (k (_uqColumns t))

mkDefaultInfo :: HaskName -> PQ.PrimExpr -> DefaultInfo
mkDefaultInfo n e =
  DefaultInfo { _defaultOn  = n
              , _defaultExp = e
              } 

data DefaultInfo = DefaultInfo { _defaultOn  :: HaskName
                               , _defaultExp :: PQ.PrimExpr
                               } deriving (Show, Eq)

defaultOn :: Lens' DefaultInfo HaskName
defaultOn k t = fmap (\a -> t { _defaultOn = a }) (k (_defaultOn t))

defaultExp :: Lens' DefaultInfo PQ.PrimExpr
defaultExp k t = fmap (\a -> t { _defaultExp = a }) (k (_defaultExp t))

mkCheckInfo :: EntityNameWithHask -> PQ.PrimExpr -> CheckInfo
mkCheckInfo et e =
  CheckInfo { _checkExp  = e
            , _checkName = et
            }

data CheckInfo = CheckInfo { _checkExp  :: PQ.PrimExpr
                           , _checkName :: EntityNameWithHask
                           } deriving (Show, Eq)

checkName :: Lens' CheckInfo EntityNameWithHask
checkName k t = fmap (\a -> t { _checkName = a }) (k (_checkName t))

checkExp :: Lens' CheckInfo PQ.PrimExpr
checkExp k t = fmap (\a -> t { _checkExp = a }) (k (_checkExp t))

mkSequenceInfo :: EntityNameWithHask -> HaskName -> SequenceType -> SequenceInfo
mkSequenceInfo n coln ty =
  SequenceInfo { _seqName = n
               , _seqOn   = coln
               , _seqType = ty
               }

data SequenceInfo = SequenceInfo { _seqName   :: EntityNameWithHask
                                 , _seqOn     :: HaskName
                                 , _seqType   :: SequenceType
                                 } deriving (Show, Eq)

seqName :: Lens' SequenceInfo EntityNameWithHask
seqName k t = fmap (\a -> t { _seqName = a }) (k (_seqName t))

seqOn :: Lens' SequenceInfo HaskName
seqOn k t = fmap (\a -> t { _seqOn = a }) (k (_seqOn t))

seqType :: Lens' SequenceInfo SequenceType
seqType k t = fmap (\a -> t { _seqType = a }) (k (_seqType t))

data SequenceType = SeqOwned | SeqSerial
                  deriving (Show, Eq)

data ForeignRefD = RefByD Text   --  fk name
                          [Text] --  cols
                          (TypeName Text) --  ref tab name
                          [Text] --  ref cols
                 | RefD   Text   --  fk name
                          Text   --  col
                          (TypeName Text) --  ref tab name

headSchemaInfo :: forall sc.
                ( SingCtxSc sc
                ) => Proxy sc -> SchemaInfo
headSchemaInfo psc =
  mkSchemaInfo (headSchemaNameInfo psc) (headTypeInfo psc) 0 0 (coerce (headTableInfos psc (sing :: Sing (Tables sc)))) (headDBNameInfo (Proxy :: Proxy (SchemaDB sc))) (fromSing (sing :: Sing (DB (SchemaDB sc))))

headTypeInfo :: forall sc.
  ( AllUDCtx sc (Types sc)
  , SingI (Types sc)
  ) => Proxy sc -> [TypeNameInfo]
headTypeInfo psc = headTypeNameInfos psc (sing :: Sing (Types sc))

headTypeNameInfos :: (AllUDCtx sc xs) => Proxy sc -> Sing (xs :: [Type]) -> [TypeNameInfo]
headTypeNameInfos pdb (SCons x xs) =
  headTypeNameInfo pdb x : headTypeNameInfos pdb xs
headTypeNameInfos _ SNil =
  []

headTypeNameInfo :: forall sc ty.
                      ( UDType sc ty
                      , SingI (TypeMappings sc ty)
                      , UDTCtx (TypeMappings sc ty)
                      , Generic ty
                      , DBTypeCtx (GetDBTypeRep sc ty)
                      , SingI (GetDBTypeRep sc ty)
                      ) => Proxy sc -> Sing (ty :: Type) -> TypeNameInfo
headTypeNameInfo _ _ =
  let _tnm = error "Panic: TODO Sing TypeMappings"
      -- fromSing (sing :: Sing (TypeMappings sc ty))
      tnv = fromSing (sing :: Sing (GetDBTypeRep sc ty))
  in  mkTypeNameInfo tnv _tnm

type family AllUDCtx sc tys :: Constraint where
  AllUDCtx sc (ty ': tys) = ( UDType sc ty
                            , SingI (TypeMappings sc ty)
                            , UDTCtx (TypeMappings sc ty)
                            , AllUDCtx sc tys
                            , Generic ty
                            , DBTypeCtx (GetDBTypeRep sc ty)
                            , SingI (GetDBTypeRep sc ty)
                            )
  AllUDCtx sc '[]         = ()                                  

{-
dbTypeName :: TypeNameMap -> Text
dbTypeName tyMap =
  case tyMap of
    EnumTypeNM tyN _ -> mkDbTypeName tyN

dbConstructors :: TypeNameMap -> [Text]
dbConstructors tyMap =
  case tyMap of
    EnumTypeNM _ ctors -> ctors
-}

headTableInfos :: (All (SingCtx sc) xs) => Proxy (sc :: Type) -> Sing (xs :: [Type]) -> [TableInfo]
headTableInfos psc (SCons st sxs) =
  headTableInfo psc st : headTableInfos psc sxs
headTableInfos _ _ = []  

headTableInfo :: forall sc tab.
             ( SingCtx sc tab               
             ) => Proxy sc -> Sing tab -> TableInfo

headTableInfo db _stab =
  let hti = headTabNameInfo db tab 
      hci = headColInfos db tab
      ttyp = fromSing (sing :: Sing (TableType sc tab))
      tab = Proxy :: Proxy tab
  in mkTableInfo (headPkInfo db tab hti)
                 (headFkInfo db tab hti)
                 (headDefInfo db tab hti hci)
                 (headCksInfo db tab hti hci)
                 (headUqInfo db tab hti)
                 (headSeqsInfo db tab hti)
                 hti
                 hci
                 ttyp

headPkInfo :: forall sc tab.
          ( Table sc tab
          , SingE (PrimaryKeyName sc tab)
          , SingI (PrimaryKeyName sc tab)
          , SingE (PrimaryKey sc tab)
          , SingI (PrimaryKey sc tab)
          ) => Proxy sc -> Proxy tab -> EntityNameWithType -> Maybe PrimaryKeyInfo
headPkInfo _ _ et =
  let pkDefN = let hsn = et ^. hsName . typeName
               in  mkDbKeyName (PkName hsn pkCols)
      pkCols = fromSing (sing :: Sing (PrimaryKey sc tab))
  in  case pkCols of
    [] -> Nothing
    _  -> let dbn = maybe pkDefN id (fromSing (sing :: Sing (PrimaryKeyName sc tab)))
          in Just $ mkPrimaryKeyInfo dbn pkCols

headFkInfo :: forall sc tab.
          ( Table sc tab
          , SingE (ForeignKey sc tab)
          , SingI (ForeignKey sc tab)
          , SingE (ForeignKeyNames sc tab)
          , SingI (ForeignKeyNames sc tab)
          ) => Proxy sc -> Proxy tab -> EntityNameWithType -> [ForeignKeyInfo]
headFkInfo _ _ et = 
  let fkds = fromSing (sing :: Sing (ForeignKey sc tab))
      fkNameMappings = fromSing (sing :: Sing (ForeignKeyNames sc tab))
  in map (fkInfoOne et fkNameMappings) fkds

fkInfoOne ::  EntityNameWithType -> [(Text, Text)] -> ForeignRefD -> ForeignKeyInfo
fkInfoOne et fkMappings ref =
  case ref of
    (RefByD fkname hsCols refHsTab hsRefCols) -> 
             let etName = mkEntityName fkname (getDbFkName hsCols refHsTab fkname)
             in  mkForeignKeyInfo etName hsCols refHsTab hsRefCols
    (RefD fkname hsCol refHsTab) ->
             let etName = mkEntityName fkname (getDbFkName [hsCol] refHsTab fkname)
                 hsCols = [hsCol]
             in  mkForeignKeyInfo etName hsCols refHsTab hsCols
                 
  where getDbFkName hsCols refHsTab fkname =
          case L.lookup fkname fkMappings of
            Just fkmapped -> fkmapped
            Nothing       -> let hsn    = et ^. hsName . typeName
                                 refn   = refHsTab ^. typeName
                            in  mkDbKeyName (FkName hsn hsCols refn)
                                                                         
headUqInfo :: forall sc tab.
          ( Table sc tab
          , SingE (Unique sc tab)
          , SingE (UniqueNames sc tab)
          , SingI (Unique sc tab)
          , SingI (UniqueNames sc tab)
          ) => Proxy sc -> Proxy tab -> EntityNameWithType -> [UniqueInfo]
headUqInfo _ _ et =
  let uniqs = fromSing (sing :: Sing (Unique sc tab))
      uniqNameMappings = fromSing (sing :: Sing (UniqueNames sc tab))
  in  map (uniqWithMapping et uniqNameMappings) uniqs
  
uniqWithMapping :: EntityNameWithType -> [(HaskName, DBName)] -> ([HaskName], HaskName) -> UniqueInfo
uniqWithMapping et uniqMaps (uniqFlds, uniqHsName) =
  let etName = mkEntityName uniqHsName (lookupUniqMapping uniqFlds)
  in  mkUniqueInfo etName uniqFlds

  where lookupUniqMapping hsCols = 
          case L.lookup uniqHsName uniqMaps of
            Just uniqDbName -> uniqDbName
            _               -> let hstn = et ^. hsName . typeName
                               in  mkDbKeyName (UqName hstn hsCols)

headDefInfo :: forall sc tab.
           ( Table sc tab
           ) => Proxy sc -> Proxy tab -> EntityNameWithType -> [ColumnInfo] -> [DefaultInfo]
headDefInfo _ _ _et cis = case (defaults :: DBDefaults sc tab) of
  DBDefaults hl -> map mkDefInfo (happlyDefExprs cis hl)

  where mkDefInfo (n, expr) = mkDefaultInfo n expr

headCksInfo :: forall sc tab.
           ( Table sc tab
           , SingE (CheckNames sc tab)
           , SingI (CheckNames sc tab)
           ) => Proxy sc -> Proxy tab -> EntityNameWithType -> [ColumnInfo] -> [CheckInfo]
headCksInfo _ _ et cis =  case (checks :: DBChecks sc tab) of
  DBChecks hls -> map (checkInfoOne et chkNameMaps) (happlyChkExpr cis chkNameMaps hls)
  where chkNameMaps = fromSing (sing :: Sing (CheckNames sc tab))

checkInfoOne :: EntityNameWithType -> [(Text, Text)] -> (Text, PQ.PrimExpr) -> CheckInfo
checkInfoOne et chkNameMaps (n, expr) =
  let etName = mkEntityName n (lookupchkMappings n chkNameMaps)
  in mkCheckInfo etName expr

  where lookupchkMappings checkHsName chkMaps =  
          case L.lookup checkHsName chkMaps of
            Just checkDbName -> checkDbName
            _                -> let hstn = et ^. hsName . typeName
                               in  mkDbKeyName (CkName hstn checkHsName) 

headSeqsInfo :: forall sc tab.
            ( Table sc tab
            , SingI (TableSequence sc tab)
            , SingE (TableSequence sc tab)
            , SingI (SequenceNames sc tab)
            , SingE (SequenceNames sc tab)
            ) => Proxy sc -> Proxy tab -> EntityNameWithType -> [SequenceInfo]
headSeqsInfo _ _ et =
  let seqs = fromSing (sing :: Sing (TableSequence sc tab))
      seqNameMappings = fromSing (sing :: Sing (SequenceNames sc tab))
  in  map (mkSeqInfoOne et seqNameMappings) seqs

mkSeqInfoOne :: EntityNameWithType -> [(Text, Text)] -> (Text, Text, SequenceType) -> SequenceInfo
mkSeqInfoOne et seqNameMaps (seqcol, seqHsn, st) =
  let etName = mkEntityName seqHsn (lookupSeqMapping seqcol seqHsn seqNameMaps)
  in  mkSequenceInfo etName seqcol st
      
  where lookupSeqMapping hsCol seqHsName seqMaps =  
          case L.lookup seqHsName seqMaps of
            Just seqDbName -> seqDbName
            _              -> let hstn    = et ^. hsName . typeName
                              in  mkDbKeyName (SeqName hstn hsCol Nothing)

headSchemaNameInfo :: forall sc.
               ( Schema sc
               , SingE (SchemaName sc)
               , SingI (SchemaName sc)
               , SingE (GetPMT (Rep sc))
               , SingI (GetPMT (Rep sc))
               ) => Proxy (sc :: Type) -> EntityNameWithType
headSchemaNameInfo _ =
  (mkEntityName (coerce (fromSing (sing :: Sing (GetPMT (Rep sc)))))
                               (fromSing (sing :: Sing (SchemaName sc)))
                 )

headDBNameInfo :: forall db.
               ( Database db
               , SingE (DatabaseName db)
               , SingI (DatabaseName db)
               , SingE (GetPMT (Rep db))
               , SingI (GetPMT (Rep db))
               ) => Proxy (db :: Type) -> EntityNameWithType
headDBNameInfo _ =
  (mkEntityName (coerce (fromSing (sing :: Sing (GetPMT (Rep db)))))
                        (fromSing (sing :: Sing (DatabaseName db)))
                 )


headTabNameInfo :: forall tab sc.
               ( Table sc tab
               , KnownSymbol (TableName sc tab)
               , SingE (GetPMT (Rep tab))
               , SingI (GetPMT (Rep tab))
               ) => Proxy (sc :: Type) -> Proxy (tab :: Type) -> EntityNameWithType
headTabNameInfo _ _ =
  mkEntityName (coerce (fromSing (sing :: Sing (GetPMT (Rep tab)))))
                       (fromSing (sing :: Sing (TableName sc tab)))

headColInfos :: forall tab sc.
            ( Table sc tab
            , SingE (ColumnNames sc tab)
            , SingI (ColumnNames sc tab)
            , SingE (OriginalTableFieldInfo sc tab)
            , SingI (OriginalTableFieldInfo sc tab)
            ) => Proxy sc -> Proxy tab -> [ColumnInfo]
headColInfos _ _ =
  let colMap = fromSing (sing :: Sing (ColumnNames sc tab))
      hsns   = fromSing (sing :: Sing (OriginalTableFieldInfo sc tab))
  in  map (colInfoOne colMap) hsns
                         
colInfoOne :: [(Text, Text)] -> ((Type.DBType, Bool), Text) -> ColumnInfo
colInfoOne cMap ((typN, _), hsn) =
  let dbn = case L.lookup hsn cMap of
        Just dbn' -> dbn'
        _         -> hsn
      etName = mkEntityName hsn dbn
  in mkColumnInfo etName (coerce typN)

getDbColumnName :: [ColumnInfo] -> HaskName -> DBName
getDbColumnName cis n = (getColumnInfo cis n) ^. columnNameInfo . dbName

getBothColumnName :: [ColumnInfo] -> HaskName -> (HaskName, DBName)
getBothColumnName cis n = (n, (getColumnInfo cis n ^. columnNameInfo . dbName))

getDbColumnNames :: [ColumnInfo] -> [HaskName] -> [DBName]
getDbColumnNames cis = map (getDbColumnName cis)

getBothColumnNames :: [ColumnInfo] -> [HaskName] -> [(HaskName, DBName)]
getBothColumnNames cis = map (getBothColumnName cis)

getDbCheckName :: [(T.Text, T.Text)] -> T.Text -> T.Text
getDbCheckName chkMap k = fromJust (L.lookup k chkMap)

filterColumns :: [Text] -> [ColumnInfo] -> [ColumnInfo]
filterColumns hsns cis = map (getColumnInfo cis) hsns

getNullableColumns :: [ColumnInfo] -> [ColumnInfo]
getNullableColumns = filter (isNullable . view (columnTypeName . dbType))

getNonNullableColumns :: [ColumnInfo] -> [ColumnInfo]
getNonNullableColumns = filter (not . isNullable . view (columnTypeName . dbType))

getColumnInfo :: [ColumnInfo] -> Text -> ColumnInfo
getColumnInfo cis hsn = 
  let mci = L.find (\ci -> _hsName (_columnNameInfo ci) == hsn) cis
  in case mci of
       Just ci -> ci
       Nothing  -> error $ "Panic: invalid column name lookup for (hs)column: " ++ show hsn


class ( Table sc tab
      , KnownSymbol (TableName sc tab)
      , KnownSymbol (DefaultTableName tab)
        
      , SingE (ColumnNames sc tab)
      , SingI (ColumnNames sc tab)                  
      , SingE (OriginalTableFieldInfo sc tab)
      , SingI (OriginalTableFieldInfo sc tab)

      , SingE (PrimaryKeyName sc tab)
      , SingI (PrimaryKeyName sc tab)
      , SingE (PrimaryKey sc tab)
      , SingI (PrimaryKey sc tab)

      , SingE (Unique sc tab)
      , SingE (UniqueNames sc tab)
      , SingI (Unique sc tab)
      , SingI (UniqueNames sc tab)

      , SingE (ForeignKey sc tab)
      , SingI (ForeignKey sc tab)
      , SingE (ForeignKeyNames sc tab)
      , SingI (ForeignKeyNames sc tab)

      , SingI (TableSequence sc tab)
      , SingE (TableSequence sc tab)
      , SingI (SequenceNames sc tab)
      , SingE (SequenceNames sc tab)

      , SingE (CheckNames sc tab)
      , SingI (CheckNames sc tab)

      , SingI (GetPMT (Rep tab))
      , SingE (GetPMT (Rep tab))

      , SingI (TableType sc tab)
      , SingE (TableType sc tab)            
      ) => SingCtx sc tab where

instance ( Table sc tab
      , KnownSymbol (TableName sc tab)
      , KnownSymbol (DefaultTableName tab)
        
      , SingE (ColumnNames sc tab)
      , SingI (ColumnNames sc tab)                  
      , SingE (OriginalTableFieldInfo sc tab)
      , SingI (OriginalTableFieldInfo sc tab)

      , SingE (PrimaryKeyName sc tab)
      , SingI (PrimaryKeyName sc tab)
      , SingE (PrimaryKey sc tab)
      , SingI (PrimaryKey sc tab)

      , SingE (Unique sc tab)
      , SingE (UniqueNames sc tab)
      , SingI (Unique sc tab)
      , SingI (UniqueNames sc tab)

      , SingE (ForeignKey sc tab)
      , SingI (ForeignKey sc tab)
      , SingE (ForeignKeyNames sc tab)
      , SingI (ForeignKeyNames sc tab)

      , SingI (TableSequence sc tab)
      , SingE (TableSequence sc tab)
      , SingI (SequenceNames sc tab)
      , SingE (SequenceNames sc tab)

      , SingE (CheckNames sc tab)
      , SingI (CheckNames sc tab)

      , SingI (GetPMT (Rep tab))
      , SingE (GetPMT (Rep tab))

      , SingI (TableType sc tab)
      , SingE (TableType sc tab)      
      ) => SingCtx sc tab

class ( Schema sc
      , SingE (SchemaName sc)
      , SingI (SchemaName sc)
      , SingI (GetPMT (Rep sc))
      , SingE (GetPMT (Rep sc))
      , All (SingCtx sc) (Tables sc)
      , SingI (Tables sc)
      , AllUDCtx sc (Types sc)
      , SingI (Types sc)
      , SingI (DB (SchemaDB sc))
      , SingE (DB (SchemaDB sc))
      , Database (SchemaDB sc)
      , SingE (DatabaseName (SchemaDB sc))
      , SingI (DatabaseName (SchemaDB sc))
      , SingE (GetPMT (Rep (SchemaDB sc)))
      , SingI (GetPMT (Rep (SchemaDB sc)))
      ) => SingCtxSc sc where

instance ( Schema sc
         , SingE (DefaultDatabaseName sc)
         , SingE (SchemaName sc)
         , SingI (DefaultDatabaseName sc)
         , SingI (SchemaName sc)
         , SingI (GetPMT (Rep sc))
         , SingE (GetPMT (Rep sc))
         , All (SingCtx sc) (Tables sc)
         , SingI (Tables sc)
         , AllUDCtx sc (Types sc)
         , SingI (Types sc)
         , SingI (DB (SchemaDB sc))
         , SingE (DB (SchemaDB sc))
         , Database (SchemaDB sc)
         , SingE (DatabaseName (SchemaDB sc))
         , SingI (DatabaseName (SchemaDB sc))
         , SingE (GetPMT (Rep (SchemaDB sc)))
         , SingI (GetPMT (Rep (SchemaDB sc)))         
         ) => SingCtxSc sc where  

type family OriginalTableFieldInfo (sc :: Type) (tab :: Type) :: [((TagHK DbK DBTypeK, Bool), Symbol)] where
  OriginalTableFieldInfo sc tab = GetFieldInfo sc (DB (SchemaDB sc)) (OriginalTableFields tab)

type family GetFieldInfo (sc :: Type) (db :: DbK) (xs :: [Type]) :: [((TagHK DbK DBTypeK, Bool), Symbol)] where
  GetFieldInfo sc db (fld ::: x ': xs) = '(TagTypeInfo db (GetDBTypeRep sc x), fld) ': GetFieldInfo sc db xs
  GetFieldInfo _ _ '[]                 = '[]

type family TagTypeInfo (db :: DbK) (dbt :: DBTypeK) :: (TagHK DbK DBTypeK, Bool) where
  TagTypeInfo db t              =  '( 'Tag db t, (IsNullable t))

type family IsNullable (dbt :: DBTypeK) where
  IsNullable ('DBNullable t) = 'True
  IsNullable _               = 'False

reproxy :: proxy a -> Proxy a
reproxy _ = Proxy

col :: forall (sc :: Type) (tab :: Type) (col :: Symbol) (a :: Type).
  ( PlainColumnCtx sc tab col a
  ) => Proxy (DBTag sc tab col) -> PQ.Expr sc a
col _ = PQ.Expr (PQ.AttrExpr sym)
  where sym = maybe (error "Panic: Empty col @col_") id (PQ.toSym [dbColN])
        dbColN = _dbName (_columnNameInfo (getColumnInfo (headColInfos (Proxy @sc) (Proxy @tab)) fld))
        fld = T.pack (symbolVal (Proxy @col))

type PlainColumnCtx sc tab col a =
  ( KnownSymbol col
  , UnifyField (OriginalTableFields tab) col a ('Text "Unable to find column " ':<>: 'ShowType col)
  , Table sc tab
  , Schema sc
  , SingE (ColumnNames sc tab)
  , SingI (ColumnNames sc tab)
  , SingE (OriginalTableFieldInfo sc tab)
  , SingI (OriginalTableFieldInfo sc tab)
  )  

class (PlainColumnCtx sc tab col a) => PlainColumnCtx_ a rep sc tab col where
instance (PlainColumnCtx sc tab col a) => PlainColumnCtx_ a rep sc tab col where
  
class Column_ a (rep :: DBTypeK) where
  type ColumnCtx a rep :: Type -> Type -> Symbol -> Constraint
  column_ :: (ColumnCtx a rep sc tab col) => Proxy (DBTag sc tab col) -> Proxy rep -> PQ.Expr sc a

instance Column_ a 'DBInt2 where
  type ColumnCtx a 'DBInt2 =
    PlainColumnCtx_ a 'DBInt2
  column_ tag _ = col tag

instance Column_ a 'DBInt4 where
  type ColumnCtx a 'DBInt4 =
    PlainColumnCtx_ a 'DBInt4
  column_ tag _ = col tag

instance Column_ a 'DBInt8 where
  type ColumnCtx a 'DBInt8 =
    PlainColumnCtx_ a 'DBInt8
  column_ tag _ = col tag

instance Column_ a ('DBFloat i) where
  type ColumnCtx a ('DBFloat i) =
    PlainColumnCtx_ a ('DBFloat i)
  column_ tag _ = col tag

instance Column_ a ('DBNumeric i j) where
  type ColumnCtx a ('DBNumeric i j) =
    PlainColumnCtx_ a ('DBNumeric i j)
  column_ tag _ = col tag

instance Column_ a ('DBChar c) where
  type ColumnCtx a ('DBChar c) =
    PlainColumnCtx_ a ('DBChar c)
  column_ tag _ = col tag

instance Column_ a ('DBVarchar e) where
  type ColumnCtx a ('DBVarchar e) =
    PlainColumnCtx_ a ('DBVarchar e)
  column_ tag _ = col tag

instance Column_ a 'DBBool where
  type ColumnCtx a 'DBBool =
    PlainColumnCtx_ a 'DBBool
  column_ tag _ = col tag

instance Column_ a 'DBDate where
  type ColumnCtx a 'DBDate =
    PlainColumnCtx_ a 'DBDate
  column_ tag _ = col tag

instance Column_ a ('DBTime i) where
  type ColumnCtx a ('DBTime i) =
    PlainColumnCtx_ a ('DBTime i)
  column_ tag _ = col tag

instance Column_ a ('DBTimetz i) where
  type ColumnCtx a ('DBTimetz i) =
    PlainColumnCtx_ a ('DBTimetz i)
  column_ tag _ = col tag

instance Column_ a ('DBTimestamp i) where
  type ColumnCtx a ('DBTimestamp i) =
    PlainColumnCtx_ a ('DBTimestamp i)
  column_ tag _ = col tag

instance Column_ a ('DBTimestamptz i) where
  type ColumnCtx a ('DBTimestamptz i) =
    PlainColumnCtx_ a ('DBTimestamptz i)
  column_ tag _ = col tag

instance Column_ a ('DBInterval m i) where
  type ColumnCtx a ('DBInterval m i) =
    PlainColumnCtx_ a ('DBInterval m i)
  column_ tag _ = col tag

instance Column_ a ('DBNullable t) where
  type ColumnCtx a ('DBNullable t) =
    PlainColumnCtx_ a ('DBNullable t)
  column_ tag _ = col tag

instance Column_ a 'DBXml where
  type ColumnCtx a 'DBXml =
    PlainColumnCtx_ a 'DBXml
  column_ tag _ = col tag

instance Column_ a 'DBJson where
  type ColumnCtx a 'DBJson =
    PlainColumnCtx_ a 'DBJson
  column_ tag _ = col tag

instance Column_ a ('DBBinary b) where
  type ColumnCtx a ('DBBinary b) =
    PlainColumnCtx_ a ('DBBinary b)
  column_ tag _ = col tag

instance Column_ a ('DBVarbinary b) where
  type ColumnCtx a ('DBVarbinary b) =
    PlainColumnCtx_ a ('DBVarbinary b)
  column_ tag _ = col tag

instance Column_ a 'DBText where
  type ColumnCtx a 'DBText =
    PlainColumnCtx_ a 'DBText
  column_ tag _ = col tag

instance Column_ a 'DBCiText where
  type ColumnCtx a 'DBCiText =
    PlainColumnCtx_ a 'DBCiText
  column_ tag _ = col tag

instance Column_ a 'DBUuid where
  type ColumnCtx a 'DBUuid =
    PlainColumnCtx_ a 'DBUuid
  column_ tag _ = col tag

instance Column_ a ('DBBit i) where
  type ColumnCtx a ('DBBit i) =
    PlainColumnCtx_ a ('DBBit i)
  column_ tag _ = col tag

instance Column_ a ('DBVarbit i) where
  type ColumnCtx a ('DBVarbit i) =
    PlainColumnCtx_ a ('DBVarbit i)
  column_ tag _ = col tag

instance Column_ a 'DBJsonB where
  type ColumnCtx a 'DBJsonB =
    PlainColumnCtx_ a 'DBJsonB
  column_ tag _ = col tag

instance Column_ a ('DBArray t) where
  type ColumnCtx a ('DBArray t) =
    PlainColumnCtx_ a ('DBArray t)
  column_ tag _ = col tag

instance Column_ a ('DBCustomType scn (typ :: Type) ('DBTypeName name args ('EnumType en es))) where
  type ColumnCtx a ('DBCustomType scn typ ('DBTypeName name args ('EnumType en es))) =
    PlainColumnCtx_ a ('DBCustomType scn typ ('DBTypeName name args ('EnumType en es)))
  column_ tag _ = col tag 

instance Column_ a ('DBCustomType scn (typ :: Type) ('DBTypeName name args ('Composite en es))) where
  type ColumnCtx a ('DBCustomType scn typ ('DBTypeName name args ('Composite en es))) =
    PlainColumnCtx_ a ('DBCustomType scn typ ('DBTypeName name args ('Composite en es)))
  column_ tag _ = col tag

class CustomColumnCtx_ sc tab col where
instance CustomColumnCtx_ sc tab col where

{-  
instance (SingI es, SingE es) => Column_ a ('DBCustomType typ ('DBTypeName name args ('Flat es))) where
  type ColumnCtx a ('DBCustomType typ ('DBTypeName name args ('Flat es))) = CustomColumnCtx_
  column_ _tag _ =
    let cols = map fst $ fromSing (sing :: Sing es)
        exprs = map (PQ.unsafeAttrExpr . pure) $ cols
    in  PQ.Expr (PQ.FlatComposite (zip cols exprs))

instance (SingI ess, SingE ess) => Column_ a ('DBCustomType typ ('DBTypeName name args ('Sum tn ess))) where
  type ColumnCtx a ('DBCustomType typ ('DBTypeName name args ('Sum ess))) = CustomColumnCtx_
  column_ _tag _ =
    let sumCols = fromSing (sing :: Sing ess)
        sumExprs = concatMap (\(_tag, es) ->
                                let exprs = map (\s -> caseExpr s . PQ.unsafeAttrExpr . pure $ s) (map fst es)
                                    caseExpr s _e = (s, PQ.CaseExpr [(PQ.BinExpr PQ.OpEq undefined undefined, undefined)] undefined)
                                in  exprs
                             ) sumCols
    in  PQ.Expr (PQ.FlatComposite sumExprs)
-}
  
-- project :: (UDType sc a) => PQ.Expr sc scope a -> 

-- class Project sc a field where

instance (UDType sc a, UDTargetType (TypeMappings sc a) x r a) => HasField x (PQ.Expr sc a) (PQ.Expr sc r) where
  hasField = udTargetType (Proxy @'(x, (TypeMappings sc a)))


insert :: a -> [a] -> [a]
insert = (:)

-- NOTE : fail delete if element not found?
delete :: (Eq b) => b -> (a -> b) -> [a] -> [a]
delete b f (x : xs)
 | f x == b  = delete b f xs
 | otherwise = x : delete b f xs
delete _ _ [] = []


-- Naming strategies

-- hs to db
data DbKeyName
  = PkName T.Text [T.Text]
  | FkName T.Text [T.Text] T.Text
  | UqName T.Text [T.Text]
  | CkName T.Text T.Text
  | SeqName T.Text T.Text (Maybe T.Text)
  deriving (Show, Eq)

mkDbKeyName :: DbKeyName -> T.Text
mkDbKeyName (PkName tab _cols)          = T.intercalate "_" ("pk":tab:[])
mkDbKeyName (FkName tab cols reft)      = T.intercalate "_" (("fk":tab:cols) ++ [reft])
mkDbKeyName (UqName tab cols)           = T.intercalate "_" ("uq":tab:cols)
mkDbKeyName (CkName tab cn)             = T.intercalate "_" ["ck",tab,cn]
mkDbKeyName (SeqName tab cn Nothing)    = T.intercalate "_" ["seq",tab,cn]
mkDbKeyName (SeqName tab cn (Just n))   = T.intercalate "_" ["seq",tab, cn, n]

mkDbTabName :: TypeName T.Text -> T.Text
mkDbTabName tn = tn ^. typeName

mkDbColumnName :: T.Text -> T.Text
mkDbColumnName hsn = hsn

mkDbTypeName :: T.Text -> T.Text
mkDbTypeName tn = tn

-- db to hs
mkHaskKeyName :: HM.HashMap Text Text -> Text -> Text
mkHaskKeyName nameHints dbn = fromMaybe (camelCase dbn) (HM.lookup dbn nameHints)

mkHaskColumnName :: HM.HashMap Text Text -> Text -> Text
mkHaskColumnName nameHints dbn = fromMaybe (camelCase dbn) (HM.lookup dbn nameHints)

mkHaskTypeNameRep :: HM.HashMap Text Text -> Text -> Text
mkHaskTypeNameRep nameHints dbn =
  fromMaybe (pascalCase dbn) (HM.lookup dbn nameHints)

mkHaskTypeName :: HM.HashMap Text Text -> Text -> TypeName Text
mkHaskTypeName typeNameHints =
  mkTypeName "DBPackage" "DBModule" . mkHaskTypeNameRep typeNameHints

-- 
camelCase :: Text -> Text
camelCase = keywordMap . validId . repInvalid . mconcat . headLower . splitName
  where headLower (x : xs) = uncapitalizeHead x : map capitalizeHead xs
        headLower _        = []

        validId xs | xs == "" = xs
                   | isDigit (T.head xs) = "_" <> xs
                   | otherwise = xs

        repInvalid = T.map go
          where go x
                  | isAlphaNum x = x
                  | x == '\''     = x
                  | otherwise    = '_'


        keywordMap x
          | x == "type" = "type_"
          | otherwise  = x
                       
pascalCase :: Text -> Text
pascalCase = digitAppend . mconcat . map capitalizeHead . splitName
  where digitAppend xs | xs == "" = xs
                       | isDigit (T.head xs) = "T" <> xs
                       | otherwise = xs

defHSNameToDBName :: Text -> Text
defHSNameToDBName = LT.toStrict . LTB.toLazyText .  T.foldl'
  (\b c ->
     if
       | c == '\'' -> b <> LTB.singleton '_' <> LTB.singleton c
       | isUpper c -> if b == mempty
                      then LTB.singleton (toLower c)
                      else b <> LTB.singleton '_' <> LTB.singleton (toLower c)
       | otherwise -> b <> LTB.singleton c
  ) mempty  

splitName :: Text -> [Text]
splitName = filter (\a -> a /= "") . T.split (\x -> x == ' ')

capitalizeHead :: Text -> Text
capitalizeHead txt = case T.uncons txt of
  Just (h, rest) -> T.toUpper (T.singleton h) <> rest
  Nothing        -> txt

uncapitalizeHead :: Text -> Text
uncapitalizeHead txt = case T.uncons txt of
  Just (h, rest) -> T.toLower (T.singleton h) <> rest
  Nothing        -> txt

columnExpr :: ColumnInfo -> PQ.PrimExpr
columnExpr ci = PQ.unsafeAttrExpr (pure (ci ^. columnNameInfo . dbName))

{-
ppDatabaseInfo :: DatabaseInfo -> String
ppDatabaseInfo di =
  "Database Name: " <> ppEntityName (di ^. databaseName) <>
  "Tables 
-}  

{-

number {

int      - DBInt4
bigint   - DBInt8
smallint - DBInt2

real             - DBFloat4
double precision - DBFloat8

float(1 - 24)  - real
float(25 - 53) - double precision
float          - double precision

-- precision > 0, scale >= 0
NUMERIC(precision, scale)
NUMERIC(precision)
NUMERIC

decimal & numeric are equivalent.

money - 8 byte
}

char {

character varying(n), varchar(n)
character(n), char(n)
text
}

binary : bytea

datetime {

timestamp [p] / timestamp without time zone [p] { p in 0 - 6 }
timestamptz [p] / timestamp with time zone [p] { 0 - 6 }

date

time [ (p) ] [ without time zone ]
time [ (p) ] with time zone

interval [ fields ] [ (p) ] , fields in interval

//

geometry
network
bit string
text search
enum types
composite types
range types
domain types
oid ?
pg_lsn 

--
r-}
