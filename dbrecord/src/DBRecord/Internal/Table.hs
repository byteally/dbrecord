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
module DBRecord.Internal.Table
  ( module DBRecord.Internal.Table
  ) where

import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits
import GHC.Generics
import Data.String
import Data.Coerce
import GHC.OverloadedLabels
import Data.Kind
import Data.Typeable
import Data.Functor.Identity
import Data.Functor.Const
import DBRecord.Internal.Schema
import DBRecord.Internal.UDType
import DBRecord.Internal.Expr
import DBRecord.Internal.Types
import DBRecord.Internal.Common
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.DBTypes
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
-- import Data.Aeson (ToJSON (..))
import qualified GHC.Records as R
import Record
import Control.Monad.Trans.State.Strict

type family NoSchema t where
  NoSchema x = TypeError ('Text "No instance for " ':<>: 'ShowType (Schema x))

-- type Table :: forall (tab :: Type) (sc :: Type). tab -> sc -> Constraint
class ( Schema sc
      , ValidateTableProps sc tab
      , Generic tab
      , Break0 (NoSchema sc) (SchemaDB sc)
      , DBRepr (DB (SchemaDB sc)) tab
      , ToDBType (DB (SchemaDB sc)) tab ~ 'TableObj
      , Fst (TableId sc tab) ~ sc
      ) => Table (sc :: Type) (tab :: Type) where
  type TableId sc tab = (oid :: (Type, Nat)) | oid -> tab

  type PrimaryKey sc tab :: [Symbol]
  type PrimaryKey sc tab = '[]

  type ForeignKey sc tab :: [ForeignRef Type]
  type ForeignKey sc tab = '[]

  type Unique sc tab     :: [UniqueCT]
  type Unique sc tab = '[]

  type HasDefault sc tab :: [Symbol]
  type HasDefault sc tab = '[]

  type Generated sc tab :: [(Symbol, GenerationType)]
  type Generated sc tab = '[]

  type TableSequence sc tab :: [Sequence]
  type TableSequence sc tab = '[]

  type NewRow sc tab = (r :: Type) | r -> tab

  type Extension sc tab :: Type
  type Extension sc tab = ()

  tableName :: TableName sc tab
  default tableName :: (Generic tab, KnownSymbol (GenTyCon (Rep tab))) => TableName sc tab
  tableName = TableName $ defHSNameToDBName $ T.pack (symbolVal (Proxy @(GenTyCon (Rep tab))))

  primaryKeyName :: PrimaryKeyName sc tab
  default primaryKeyName :: (Generic tab, KnownSymbol (GenTyCon (Rep tab))) => PrimaryKeyName sc tab
  primaryKeyName = PrimaryKeyName $ defPkNameFromHsName $ T.pack (symbolVal (Proxy @(GenTyCon (Rep tab))))

  checks :: TableValue sc Identity tab -> [(Text, Expr sc Bool)]
  checks _ = []

  rel :: (forall s.Clause s sc tab (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o
  -- (Sub tab (GetGenIdCols (Generated sc tab)))
  default rel ::
    ( GConstructHK tab (HasColumn sc tab) (TypeFields tab)
    , MkFieldInvIx (Fields tab)
    ) => (forall s.Clause s sc tab (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o
  rel (Clause clau) = Query' (TableValue fsix $ constructHK @(HasColumn sc tab) (ExprF . toExprId . coerceExpr . getCol (Proxy @'(sc, tab))), clau, PQ.Table (Just (PQ.TableName tabId Nothing)), ReadQType ManyR)
    where tabId = getTableId @sc @tab Proxy Proxy
          fsix = mkFieldInvIx (Proxy @(Fields tab)) emptyFieldInvIx

  fromNewRow :: NewRow sc tab -> TableValue sc Identity tab
  default fromNewRow ::
    ( GConstructHK tab (HasConstColumn sc tab (HasDefault sc tab) (Generated sc tab) (NewRow sc tab)) (TypeFields tab)
    , MkFieldInvIx (Fields tab)
    ) => NewRow sc tab -> TableValue sc Identity tab
  fromNewRow r = TableValue fsix $ constructHK @(HasConstColumn sc tab (HasDefault sc tab) (Generated sc tab) (NewRow sc tab)) (ExprF . toExprId . coerceExpr . getConstCol (Proxy @'(sc, tab, (HasDefault sc tab), (Generated sc tab))) r)
    where fsix = mkFieldInvIx (Proxy @(Fields tab)) emptyFieldInvIx

newtype TableName sc ty = TableName Text

unTableName :: TableName sc tab -> Text
unTableName = coerce

newtype PrimaryKeyName sc tab = PrimaryKeyName Text

_getPrimaryKeyName :: PrimaryKeyName sc tab -> Text
_getPrimaryKeyName = coerce

getDatabaseName :: forall sc.
               ( Database (SchemaDB sc)
               , Schema sc
               ) => Const Text sc
getDatabaseName = Const $ _getDatabaseName $ databaseName @(SchemaDB sc)

getSchemaName :: forall sc.
               ( Schema sc
               ) => Const Text sc
getSchemaName = Const $ _getSchemaName $ schemaName @sc

getTableName :: forall sc tab.
               ( Table sc tab
               ) => Const Text (sc,tab)
getTableName = Const $ unTableName $ tableName @sc @tab

getTableId :: forall sc tab.
               ( Schema sc
               , Table sc tab
               ) => Proxy sc -> Proxy tab -> PQ.TableId
getTableId _ _ = tab
  where tab = PQ.TableId { PQ.schema    = getConst (getSchemaName :: Const Text sc)
                         , PQ.tableName = getConst (getTableName  :: Const Text (sc, tab))
                         , PQ.database  = getConst (getDatabaseName :: Const Text sc)
                         }

instance IsString (TableName sc ty) where
  fromString s = TableName $ T.pack s

newtype Defaulted (t :: Type) = Defaulted {getDefaulted :: Maybe t}

defaulted :: Defaulted t
defaulted = Defaulted Nothing

override :: t -> Defaulted t
override t = Defaulted (Just t)

type family GetGenIdCols (fs :: [(Symbol, GenerationType)]) :: [Symbol] where
  GetGenIdCols ('(c, 'GenAsId 'GenAlways _) ': fs) = c ': GetGenIdCols fs
  GetGenIdCols (_ ': fs) = GetGenIdCols fs
  GetGenIdCols '[] = '[]


getMutQ :: forall o tab sc.(Table sc tab) => (PQ.TableId -> TableValue sc Identity tab -> MQuery sc o) -> MQuery sc o
getMutQ k =
  let
    relq = rel @sc @tab (scoped $ \(clau, (Scoped tabv)) -> (clau, tabv))
    (pq, tabi) = runQuery' relq
    tabId = case pq of
      PQ.Table (Just (PQ.TableName tabId' _talias)) _ -> tabId'
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

toExprId :: Expr sc a -> Expr sc (Identity a)
toExprId (Expr e) = Expr e

class HasConstColumn (sc :: Type) (tab :: Type) (defs :: [Symbol]) (gens :: [(Symbol, GenerationType)]) (r :: Type) (col :: Symbol) (a :: Type) where
  getConstCol :: Proxy '(sc, tab, defs, gens) -> r -> Proxy '(col, a) -> Expr sc (Field col a)

instance (HasConstOrDefCol (IsDefCol col defs gens) sc tab r col a) => HasConstColumn sc tab defs gens r col a where
  getConstCol _ _r px = getConstOrDefCol (Proxy @'(sc, tab, (IsDefCol col defs gens))) _r px


class HasConstOrDefCol (isDef :: Either Bool GenerationType) (sc :: Type) (tab :: Type) (r :: Type) (col :: Symbol) (a :: Type) where
  getConstOrDefCol :: Proxy '(sc, tab, isDef) -> r -> Proxy '(col, a) -> Expr sc (Field col a)

instance HasConstOrDefCol ('Left 'True) sc tab r col a where
  getConstOrDefCol _ _ _ = Expr PQ.DefaultInsertExpr

instance (R.HasField col r a, DBRepr (DB (SchemaDB sc)) a, AutoConstExpr sc a (ToDBType (DB (SchemaDB sc)) a) (AutoCodec (DB (SchemaDB sc)) a)) => HasConstOrDefCol ('Left 'False) sc tab r col a where
  getConstOrDefCol _ r _ = coerceExpr $ constExpr $ R.getField @col r


type family IsDefCol (c :: Symbol) (defs :: [Symbol]) (gens :: [(Symbol, GenerationType)]) :: Either Bool GenerationType where
  IsDefCol c (c ': _) gs = 'Left 'True
  IsDefCol c (_ ': ds) gs = IsDefCol c ds gs
  IsDefCol c '[] ('(c, gt) ': _) = 'Right gt
  IsDefCol c '[] (_ ': gs)  = IsDefCol c '[] gs
  IsDefCol _ '[] '[] = 'Left 'False

data GenerationType
  = GenAsId GenerationClause (Maybe SequenceOption)
  | GenAsExpr Bool
  deriving (Show, Eq)

data GenerationClause
  = GenAlways
  | GenByDef
  deriving (Show, Eq)

data SequenceOption = SequenceOption
  deriving (Show, Eq)


data Sequence = PGSerial Symbol   -- Column
                         Symbol   -- Sequence Name
              | PGOwned  Symbol   -- Column
                         Symbol   -- Sequence Name

type family Serial (cname :: Symbol) (seqname :: Symbol) where
  Serial cname seqname = 'PGSerial cname seqname

type family Owned (cname :: Symbol) (seqname :: Symbol) where
  Owned cname seqname = 'PGOwned cname seqname

data Multiplicity
  = OneRow
  | ManyRow
  | SomeRow
  | OptionRow

data QueryType
  = ReadQ Multiplicity
  | MutQ Multiplicity

data MultiplicityW (mul :: Multiplicity) where
  OneR :: MultiplicityW 'OneRow
  ManyR :: MultiplicityW 'ManyRow
  SomeR :: MultiplicityW 'SomeRow
  OptionR :: MultiplicityW 'OptionRow

data QueryTypeW (qty :: QueryType) where
  ReadQType :: MultiplicityW mul -> QueryTypeW ('ReadQ mul)
  MutQType :: MultiplicityW mul -> QueryTypeW ('MutQ mul)


data Query' qt sc t = forall i.Query' (TableValue sc Identity i, State (PQ.Clauses, TableValue sc Identity i) (TableValue sc Identity t), PQ.Clauses -> PQ.PrimQuery, QueryTypeW qt)

execQuery :: Query' qt sc t -> PQ.PrimQuery
execQuery  = fst . runQuery'
{-# INLINE execQuery #-}

runAliasedQuery :: forall t fn sc qt.KnownSymbol fn => Field fn (Query' qt sc t) -> (PQ.PrimQuery, TableValue sc Identity t)
runAliasedQuery = runQuery'' (Just $ T.pack $ symbolVal (Proxy @fn)) . val

runQuery' :: Query' qt sc t -> (PQ.PrimQuery, TableValue sc Identity t)
runQuery' = runQuery'' Nothing

runQuery'' :: Maybe Text -> Query' qt sc t -> (PQ.PrimQuery, TableValue sc Identity t)
runQuery'' asMay (Query' (exprs, st, mkPQ, _)) =
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


data ClauseType
  = SelectClause
  | AggregateClause
  | InsertClause
  | UpdateClause
  | DeleteClause

data ClauseTypeW (cty :: ClauseType) where
  SelectClauseW :: ClauseTypeW 'SelectClause
  AggregateClauseW :: ClauseTypeW 'AggregateClause
  InsertClauseW :: ClauseTypeW 'InsertClause
  UpdateClauseW :: ClauseTypeW 'UpdateClause
  DeleteClauseW :: ClauseTypeW 'DeleteClause


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

newtype InsertClause s sc i o = InsertClause_ (Clause s sc i o)
  deriving newtype (Functor, Applicative, Monad, Semigroup)

-- Unsafe
scoped :: forall i o sc s.
  ((PQ.Clauses, Scoped s sc i) -> (PQ.Clauses, o))
  -> Clause s sc i o
scoped fn = Clause $ state (\(c,es) -> let (c', o) = fn (c, Scoped es) in (o, (c', es)))

promapClause ::
  (TableValue sc Identity j -> TableValue sc Identity i) ->
  (TableValue sc Identity i -> TableValue sc Identity j) ->
  Clause s sc i o -> Clause s sc j o
promapClause bw fw (Clause st) = Clause $ StateT $ \s -> (fmap . fmap . fmap) fw $ runStateT st (bw <$> s)

tableToListWith :: (forall g (a :: Type).Typeable a => [Text] -> SomeSymbol -> ExprF sc g a -> r) -> TableValue sc f i -> [r]
tableToListWith = tableToListWith' []

tableToListWith' :: [Text] -> (forall g (a :: Type).Typeable a => [Text] -> SomeSymbol -> ExprF sc g a -> r) -> TableValue sc f i -> [r]
tableToListWith' pfxs fn (TableValue fsix hk) = fmap snd $ L.sortOn fst $ hkToListWithTag (\ssym ex -> (fromMaybe (error $ "Panic: Invariant violated! " <> (show $ typeRepOfSomeSym ssym) <> (show $ hkToListWith (\fa -> show $ typeRep fa) hk)) $ lookupFieldIx (typeRepOfSomeSym ssym) fsix, fn pfxs ssym ex)) hk
tableToListWith' pfxs fn (JoinedTables fsix tvals) = concatMap snd $ L.sortOn fst $ hkToListWithTag
  (\ssym tv ->
     let
       ssymTRep = typeRepOfSomeSym ssym
       tagToPfx (SomeSymbol s) = T.pack $ symbolVal s
       fnix = case lookupFieldIx ssymTRep fsix of
         Nothing -> error $ "Panic: Invariant violated! " <> (show ssymTRep) <> (show fsix)
         Just ix -> ix
     in (fnix, tableToListWith' (tagToPfx ssym:pfxs) fn tv)
  ) tvals
tableToListWith' pfxs fn (OptTable tv) = tableToListWith' pfxs fn tv
tableToListWith' pfxs fn (PairTable t1 t2) = tableToListWith' pfxs fn t1 ++ tableToListWith' pfxs fn t2
tableToListWith' _pfxs _ EmptyTable = []

tableToProjections :: TableValue sc f a -> [PQ.Projection]
tableToProjections = tableToListWith getPrjs
{-# INLINE tableToProjections #-}

getPrjs :: forall a f sc.(Typeable a) => [Text] -> SomeSymbol -> ExprF sc f a -> (T.Text, PQ.PrimExpr)
getPrjs pfxs ssym e = (T.intercalate "_" ((reverse pfxs) ++ [aliasedExprName ssym]), getExpr $ getExprF e)

toIdExpr :: Expr sc x -> Expr sc (Identity x)
toIdExpr = coerceExpr
{-# INLINE toIdExpr #-}

fromIdExpr :: Expr sc (Identity x) -> Expr sc x
fromIdExpr = coerceExpr
{-# INLINE fromIdExpr #-}

aliasTableValue :: forall (n :: Symbol) r sc f. (KnownSymbol n, Typeable r) =>TableValue sc f r -> TableValue sc f (Rec '[ '(n, r)])
aliasTableValue tv = JoinedTables (fromListToFieldInvIx [typeRep (Proxy @n)]
                              ) ( hrecToHKOfRec ( fromLabel @n .= tv
                                                  .& Record.end))

unaliasTableValue :: forall (n :: Symbol) r sc f. (KnownSymbol n, Typeable r) => TableValue sc f (Rec '[ '(n, r)]) -> TableValue sc f r
unaliasTableValue (JoinedTables _ hkTabs) = R.getField @n hkTabs
unaliasTableValue (_) = error "TODO:"

crossRel :: forall r1 r2 n1 n2 f sc.(KnownSymbol n1, KnownSymbol n2, Typeable r1, Typeable r2) => Field n1 (TableValue sc f r1) -> Field n2 (TableValue sc f r2) -> TableValue sc f (Rec '[ '(n1, r1), '(n2, r2)])
crossRel q1 q2 = JoinedTables (fromListToFieldInvIx [typeRep (Proxy @n1)
                                                    , typeRep (Proxy @n2)]
                              ) ( hrecToHKOfRec ( fromLabel @n1 .= val q1
                                                  .& fromLabel @n2 .= val q2
                                                  .& Record.end))

ljRel :: forall r1 r2 n1 n2 f sc.(KnownSymbol n1, KnownSymbol n2, Typeable r1, Typeable r2) => Field n1 (TableValue sc f r1) -> Field n2 (TableValue sc f (Maybe r2)) -> TableValue sc f (Rec '[ '(n1, r1), '(n2, Maybe r2)])
ljRel q1 q2 = JoinedTables (fromListToFieldInvIx [typeRep (Proxy @n1), typeRep (Proxy @n2)]
                           ) ( hrecToHKOfRec ( fromLabel @n1 .= val q1
                                               .& fromLabel @n2 .= val q2
                                               .& Record.end))

rjRel :: forall r1 r2 n1 n2 f sc.(KnownSymbol n1, KnownSymbol n2, Typeable r1, Typeable r2) => Field n1 (TableValue sc f (Maybe r1)) -> Field n2 (TableValue sc f r2) -> TableValue sc f (Rec '[ '(n1, Maybe r1), '(n2, r2)])
rjRel q1 q2 = JoinedTables (fromListToFieldInvIx [typeRep (Proxy @n1), typeRep (Proxy @n2)]
                           ) ( hrecToHKOfRec ( fromLabel @n1 .= val q1
                                               .& fromLabel @n2 .= val q2
                                               .& Record.end))

nextStage :: TableValue sc f i -> TableValue sc f i
nextStage = nextStage' []

nextStage' :: [Text] -> TableValue sc f i -> TableValue sc f i
nextStage' pfxs (TableValue fsix hk) = TableValue fsix $ hoistWithKeyAndTagHK (aliasedExprWithPrefix pfxs) hk
nextStage' pfxs (JoinedTables fsix tvals) = JoinedTables fsix $ hoistWithKeyAndTagHK (\(SomeSymbol ssym) -> nextStage' ((T.pack $ symbolVal ssym) : pfxs)) tvals
nextStage' pfxs (OptTable tv) = OptTable (nextStage' pfxs tv)
nextStage' pfxs (PairTable t1 t2) = PairTable (nextStage' pfxs t1) (nextStage' pfxs t2)
nextStage' _pfxs EmptyTable = EmptyTable


hoistMaybeTable :: (forall g x. ExprF sc g x -> ExprF sc Maybe x) -> TableValue sc f i -> TableValue sc Maybe i
hoistMaybeTable fn (TableValue fsix hk) = TableValue fsix $ hoistHK fn hk
hoistMaybeTable fn (JoinedTables fsix tvals) = JoinedTables fsix $ hoistHK (hoistMaybeTable fn) tvals
hoistMaybeTable fn (OptTable tv) = OptTable (hoistMaybeTable fn tv)
hoistMaybeTable fn (PairTable t1 t2) = PairTable (hoistMaybeTable fn t1) (hoistMaybeTable fn t2)
hoistMaybeTable _ EmptyTable = EmptyTable

toNullExprF :: ExprF sc g x -> ExprF sc Maybe x
toNullExprF (ExprF (Expr x)) = (ExprF (Expr x))


getScopeOfTable :: TableValue sc Identity i -> Scoped s sc i
getScopeOfTable tab = Scoped tab

maybeExprsToExpr :: HK (ExprF sc Maybe) r -> Expr sc (Maybe r)
maybeExprsToExpr hk = Expr $ PQ.FlatComposite $ hkToListWithTag (\ssym e -> (aliasedExprName ssym, getExpr $ getExprF e)) hk

maybeTableToExpr :: TableValue sc Maybe i -> Expr sc (Maybe i)
maybeTableToExpr = unsafeTableToExpr

idTableToExpr :: TableValue sc Identity i -> Expr sc i
idTableToExpr = unsafeTableToExpr

unsafeTableToExpr :: TableValue sc f i -> Expr sc o
unsafeTableToExpr (TableValue _ hk) = Expr $ PQ.FlatComposite $ hkToListWithTag (\ssym e -> (aliasedExprName ssym, getExpr $ getExprF e)) hk
unsafeTableToExpr (JoinedTables _ tvals) = Expr $ PQ.FlatComposite $ hkToListWithTag (\ssym tv -> (aliasedExprName ssym, getExpr $ unsafeTableToExpr tv)) tvals
unsafeTableToExpr (OptTable tv) = unsafeTableToExpr tv
unsafeTableToExpr (PairTable _t1 _t2) = error "TODO:"
unsafeTableToExpr EmptyTable = Expr unitExpr

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
aliasedExprWithPrefix pfxs ssym _ = ExprF $ unsafeCol ((reverse pfxs) ++ [aliasedExprName ssym])
{-# INLINE aliasedExprWithPrefix #-}

aliasedExpr :: forall a f sc.SomeSymbol -> ExprF sc f a
aliasedExpr ssym = ExprF $ unsafeCol [aliasedExprName ssym]
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

newtype ExprF sc f t = ExprF (Expr sc (f t))

getExprF :: ExprF sc f t -> Expr sc (f t)
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

  JoinedTables :: !FieldInvIx -> HK (TableValue sc f) t -> TableValue sc f t
  OptTable :: Typeable t => TableValue sc f t -> TableValue sc f (Maybe t)

  PairTable :: (Typeable t1, Typeable t2) => TableValue sc f t1 -> TableValue sc f t2 -> TableValue sc f (t1, t2)

  EmptyTable :: TableValue sc f ()

tableRecAsType :: ValidateRecToType os t => TableValue sc f (Rec os) -> TableValue sc f t
tableRecAsType (TableValue fsix hk) = TableValue fsix (fromHKOfRec hk)
tableRecAsType (JoinedTables fsix hk) = JoinedTables fsix (fromHKOfRec hk)

fstTable :: TableValue sc f (t1, t2) -> TableValue sc f t1
fstTable (PairTable t1 _) = t1
fstTable _ = error "Expecting only PairTable"

sndTable :: TableValue sc f (t1, t2) -> TableValue sc f t2
sndTable (PairTable _ t2) = t2
sndTable _ = error "Expecting only PairTable"

fstOf :: Scoped s sc (a, b) -> Scoped s sc a
fstOf s = Scoped $ fstTable $ coerce s

sndOf :: Scoped s sc (a, b) -> Scoped s sc b
sndOf s = Scoped $ sndTable $ coerce s 

newtype Scalar sc t = Scalar (Expr sc t)

instance (R.HasField f i t, KnownSymbol f, Typeable t) => R.HasField (f :: Symbol) (Scoped s sc i) (Expr sc t) where
  getField (Scoped hk) = R.getField @f hk

instance (R.HasField f i t, KnownSymbol f, Typeable t) => R.HasField (f :: Symbol) (TableValue sc Identity i) (Expr sc t) where
  getField (TableValue _ hk) = fromIdExpr $ getExprF $ R.getField @f hk
  getField EmptyTable = Expr unitExpr
  getField (PairTable {}) = error "TODO"
  getField optv@(OptTable tv) = go tv optv
    where
      go :: forall r.(Typeable r) => TableValue sc Identity r -> TableValue sc Identity (Maybe r) -> Expr sc t
      go tv' _ = case eqT @t @(Maybe r) of
        Just Refl -> DBRecord.Internal.Expr.toNullable $ idTableToExpr tv'
        Nothing -> error "Panic"
  getField (JoinedTables _ tvals) = idTableToExpr $ R.getField @f tvals

class HasColumn sc tab (col :: Symbol) (a :: Type) where
  getCol :: Proxy '(sc, tab) -> Proxy '(col, a) -> Expr sc (Field col a)

instance
  ( Table sc tab
  , R.HasField col tab ct
  , ct ~ a
  , KnownSymbol col
  , HasColumnByDBType sc tab col a dbTypeRep
  , dbTypeRep ~ ToDBType (DB (SchemaDB sc)) a
  ) => HasColumn sc tab col a where
  getCol _ _ = Expr $ getExpr $ getColByDBTypeRep @sc @tab @col @a @dbTypeRep Proxy


class HasColumnByDBType sc tab (col :: Symbol) (a :: Type) (dbObj :: DBObjK) where
  getColByDBTypeRep :: Proxy '(sc, tab, col, a, atrep) -> Expr sc a


instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  , TypeError ('Text "TODO: Should this be allowed?")
  ) => HasColumnByDBType sc tab col a 'TableObj where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  ) => HasColumnByDBType sc tab col a ('NativeTypeObj dbk) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  ) => HasColumnByDBType sc tab col a ('NullableObjOf e edbk) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  ) => HasColumnByDBType sc tab col a ('ArrayObjOf e edbk) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('SerializedBlob ct)) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  , UDType sc a
  , SynTypeToExpr (GetUDTypeKind (DB (SchemaDB sc)) a (ToDBType (DB (SchemaDB sc)) a)) sc a (Fields a)
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('UDRec 'FlatRec)) where
  getColByDBTypeRep _ = udtypeToExpr (Proxy @'(sc, a))

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('UDRec 'CompositeRec)) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('UDRec 'JsonRec)) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  , UDType sc a
  , Matcher (DB (SchemaDB sc)) a ~ 'SumMatcher (DB (SchemaDB sc)) pfx a m
  , Generic (m sc)
  , GenHasSumRepr (DB (SchemaDB sc)) a m sc (Rep (m sc))
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('TaggedSum enk 'FlatRec)) where
  getColByDBTypeRep _ =
    let
      allCons = getPatArgs (Proxy @'(Matcher (DB (SchemaDB sc)) a, sc , a)) $ sumRepr (Proxy @'((DB (SchemaDB sc)), a))
      discTag = _getDiscriminatorTagName $ discriminatorTagName @(DB (SchemaDB sc)) @a
      -- colBE = getColumnName @sc @tab @col
      -- cn = T.pack $ symbolVal (Proxy @col)
    in Expr $ PQ.FlatComposite $ (discTag, PQ.BaseTableAttrExpr discTag) :
       (catMaybes $ fmap (\(cn, hasArg, _) ->
                            if hasArg
                            then Just (cn, PQ.BaseTableAttrExpr $ defHSNameToDBName cn)
                            else Nothing
                         ) allCons)

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  , UDType sc a
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('TaggedSum enk 'CompositeRec)) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  , UDType sc a
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('TaggedSum enk 'JsonRec)) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  , UDType sc a
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('TaggedSumMono enk cty 'FlatRec)) where
  getColByDBTypeRep _ =
    let
      discTag = _getDiscriminatorTagName $ discriminatorTagName @(DB (SchemaDB sc)) @a
      colBE = getColumnName @sc @tab @col
      -- cn = T.pack $ symbolVal (Proxy @col)
      tyN = _getTypeName $ typeName @(DB (SchemaDB sc)) @a
    in Expr $ PQ.FlatComposite [ ("tag", PQ.BaseTableAttrExpr discTag)
                               , (tyN, getExpr colBE)
                               ]

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  , UDType sc a
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('TaggedSumMono enk cty 'CompositeRec)) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  , UDType sc a
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('TaggedSumMono enk cty 'JsonRec)) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  , UDType sc a
  , Matcher (DB (SchemaDB sc)) a ~ 'SumMatcher (DB (SchemaDB sc)) pfx a m
  , Generic (m sc)
  , GenHasSumRepr (DB (SchemaDB sc)) a m sc (Rep (m sc))
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('SumOfCol 'FlatRec)) where
  getColByDBTypeRep _ =
    let
      allCons = getPatArgs (Proxy @'(Matcher (DB (SchemaDB sc)) a, sc , a)) $ sumRepr (Proxy @'((DB (SchemaDB sc)), a))
      -- colBE = getColumnName @sc @tab @col
      -- cn = T.pack $ symbolVal (Proxy @col)
    in Expr $ PQ.FlatComposite $
       (catMaybes $ fmap (\(cn, hasArg, _) ->
                            if hasArg
                            then Just (cn, PQ.BaseTableAttrExpr $ defHSNameToDBName cn)
                            else Nothing
                         ) allCons)

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  -- , UDType sc a
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('SumOfCol 'CompositeRec)) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  -- , UDType sc a
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('SumOfCol 'JsonRec)) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  -- , UDType sc a
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('UDEnum enk)) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

getColumnName :: forall sc tab (fn :: Symbol) a.
  ( Table sc tab
  , R.HasField fn tab a
  , KnownSymbol fn
  ) => Expr sc a
getColumnName =
  let
    FieldAliases caliases = fieldAliases @(DB (SchemaDB sc)) @tab
    fname = T.pack $ symbolVal (Proxy @fn)
    cname = maybe (defHSNameToDBName fname) id $ HM.lookup fname caliases
    cexpr = PQ.BaseTableAttrExpr $ cname
  in Expr cexpr
{-# INLINE getColumnName #-}

getColumnNameText :: forall sc tab (fn :: Symbol).
  ( Table sc tab
  , KnownSymbol fn
  ) => Const Text (sc,tab)
getColumnNameText =
  let
    FieldAliases caliases = fieldAliases @(DB (SchemaDB sc)) @tab
    fname = T.pack $ symbolVal (Proxy @fn)
    cname = maybe (defHSNameToDBName fname) id $ HM.lookup fname caliases
  in Const cname
{-# INLINE getColumnNameText #-}

getPrimaryKeysText :: forall sc tab.
  ( Table sc tab
  , SingI (PrimaryKey sc tab)
  , All KnownSymbol (PrimaryKey sc tab)
  ) => Proxy '(sc, tab) -> [Text]
getPrimaryKeysText p = getPrimaryKeysText' p (sing :: Sing (PrimaryKey sc tab))

getUniquesText :: forall sc tab.
  ( Table sc tab
  , SingI (Unique sc tab)
  , AllUniqCxt (Unique sc tab)
  ) => Proxy '(sc, tab) -> [(Text, [Text])]
getUniquesText p = getUniquesText' p (sing :: Sing (Unique sc tab))

-- getUniquesText :: forall sc tab.
--   ( Table sc tab
--   , SingI (Unique sc tab)
--   , AllUniqCxt (Unique sc tab)
--   ) => Proxy '(sc, tab) -> [(Text, [Text])]
-- getUniquesText p = getUniquesText' p (sing :: Sing (Unique sc tab))

getForeignKeys :: forall sc tab.
 ( Table sc tab
 , SingI (ForeignKey sc tab)
 , AllFkCxt (ForeignKey sc tab)
 ) => Proxy '(sc, tab) -> [(Text, Either (Text, PQ.TableId) ([Text], PQ.TableId, [Text]))]
getForeignKeys p = getForeignKeys' p (sing :: Sing (ForeignKey sc tab))

getPrimaryKeysText' :: forall sc tab xs.
  ( Table sc tab
  , AllF KnownSymbol xs
  ) => Proxy '(sc, tab) -> Sing (xs :: [Symbol]) -> [Text]
getPrimaryKeysText' p = \case
  SNil -> []
  SCons s pks -> getColN p s : getPrimaryKeysText' (Proxy @'(sc,tab)) pks

getColN :: forall sc tab fn.(Table sc tab, KnownSymbol fn) => Proxy '(sc, tab) -> Sing (fn :: Symbol) -> Text
getColN _ _ = getConst $ getColumnNameText @sc @tab @fn

getUniquesText' :: forall sc tab uqs.
  ( Table sc tab
  , AllUniqCxt uqs
  ) => Proxy '(sc, tab) -> Sing (uqs :: [UniqueCT]) -> [(Text, [Text])]
getUniquesText' p = \case
  SNil -> []
  SCons (SUniqueOn flds uqn) pks -> (T.pack $ symbolVal uqn, (getPrimaryKeysText' p flds)) : (getUniquesText' (Proxy @'(sc,tab)) pks)

getForeignKeys' :: forall sc tab fks.
 ( Table sc tab
 , AllFkCxt fks
 ) => Proxy '(sc, tab) -> Sing (fks :: [ForeignRef Type]) -> [(Text, Either (Text, PQ.TableId) ([Text], PQ.TableId, [Text]))]
getForeignKeys' p = \case
  SNil -> []
  SCons fk fks -> case fk of
    SRef scol refsc reft sfkn ->
      let
        fkn = defFkNameFromHsName (T.pack $ symbolVal sfkn)
        col = getColN p scol
        reftn = getTableIdFromSing refsc reft 
      in (fkn, Left (col, reftn)) : getForeignKeys' p fks
    SRefBy scols refsc reft srcols sfkn ->
      let
        fkn = T.pack $ symbolVal sfkn
        cols = getPrimaryKeysText' p scols
        rcols = getPrimaryKeysText' p srcols
        reftn = getTableIdFromSing refsc reft 
      in (fkn, Right (cols, reftn, rcols)) : getForeignKeys' p fks
  

getTableIdFromSing :: forall sc tab.(Schema sc, Table sc tab) => Sing sc -> Sing tab -> PQ.TableId
getTableIdFromSing _ _ = getTableId @sc @tab Proxy Proxy

type family AllUniqCxt (uqs :: [UniqueCT]) :: Constraint where
  AllUniqCxt '[] = ()
  AllUniqCxt (( 'UniqueOn uniqFlds uniqOn) ': uqs) = ((AllF KnownSymbol uniqFlds, KnownSymbol uniqOn), AllUniqCxt uqs)

type family AllFkCxt (fkss :: [ForeignRef Type]) :: Constraint where
  AllFkCxt '[] = ()
  AllFkCxt (('Ref col refsc reft fkn) ': fks) = (KnownSymbol col, KnownSymbol fkn, Table refsc reft, AllFkCxt fks)
  AllFkCxt (('RefBy cols refsc reft refCols fkn) ': fks) = (AllF KnownSymbol cols, KnownSymbol fkn, Table refsc reft, AllF KnownSymbol refCols, AllFkCxt fks)

type family ValidateTableProps (sc :: Type) (tab :: Type) :: Constraint where
  ValidateTableProps sc tab =
    (
    )

data ForeignRef a
  = RefBy [Symbol] a a [Symbol] Symbol
  | Ref Symbol a a Symbol

data UniqueCT = UniqueOn [Symbol] Symbol
data Uq sc (un :: Symbol) = Uq

data instance Sing (uq :: UniqueCT) where
  SUniqueOn :: Sing uniqFlds -> Sing uniqOn -> Sing ('UniqueOn uniqFlds uniqOn)

instance (SingI uniqFlds, SingI uniqOn) => SingI ('UniqueOn uniqFlds uniqOn) where
  sing = SUniqueOn sing sing

data instance Sing (fk :: ForeignRef a) where
  SRefBy :: Sing cols -> Sing refsc -> Sing reft -> Sing refCols -> Sing fkname -> Sing ('RefBy cols refsc reft refCols fkname)
  SRef   :: Sing col -> Sing refsc -> Sing reft -> Sing fkname -> Sing ('Ref col refsc reft fkname)

instance (SingI cols, SingI refsc, SingI reft, SingI refcols, SingI fkname) => SingI ('RefBy cols refsc reft refcols fkname) where
  sing = SRefBy sing sing sing sing sing

instance (SingI col, SingI refsc, SingI reft, SingI fkname) => SingI ('Ref col refsc reft fkname) where
  sing = SRef sing sing sing sing
  
