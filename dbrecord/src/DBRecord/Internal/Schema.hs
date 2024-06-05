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
-- import Data.Functor.Const
import DBRecord.Internal.Expr
import DBRecord.Internal.Types
import DBRecord.Internal.Common
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.DBTypes hiding (DBType (..), DBTypeName (..))
-- import qualified DBRecord.Internal.DBTypes as Type
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.Char
import qualified GHC.Records as R
import Record
import Control.Monad.Trans.State.Strict
-- import Type.Reflection (SomeTypeRep (..), typeRepKind)

-- data Col (a :: Symbol) = Col
-- data DefSyms = DefSyms [Symbol]

-- type ColName  = Text
-- type ColType  = Text
-- data Column   = Column !ColName !ColType
--   deriving (Show)

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
  type TableId sc tab = (oid :: Nat) | oid -> tab

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

  type TableColumns sc tab :: [(Symbol, Type)]
  type TableColumns sc tab = GGetFields tab (Rep tab)

  type Extension sc tab :: Type
  type Extension sc tab = ()

  tableName :: TableName sc tab
  default tableName :: (Generic tab) => TableName sc tab
  tableName = ""

  columnAliases :: FieldAliases sc tab
  columnAliases = mempty

  checks :: TableValue sc Identity tab -> [(Text, PQ.Expr sc Bool)]
  checks _ = []

  rel :: (forall s.Clause s sc tab (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o
  -- (Sub tab (GetGenIdCols (Generated sc tab)))
  default rel ::
    ( GConstructHK tab (HasColumn sc tab) (TypeFields tab)
    , KnownSymbol (SchemaName sc)
    , MkFieldInvIx (TableColumns sc tab)
    ) => (forall s.Clause s sc tab (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o
  rel (Clause clau) = Query' (TableValue fsix $ constructHK @(HasColumn sc tab) (ExprF . toExprId . coerceExpr . getCol (Proxy @'(sc, tab))), clau, PQ.Table (Just (PQ.TableName tabId)), ReadQType ManyR)
    where tabId = PQ.TableId { PQ.database = "zb"
                             , PQ.schema = schName
                             , PQ.tableName = defHSNameToDBName $ unTableName $ tableName @sc @tab
                             }
          schName = T.pack $ symbolVal (Proxy @(SchemaName sc))
          fsix = mkFieldInvIx (Proxy @(TableColumns sc tab)) emptyFieldInvIx

  fromNewRow :: NewRow sc tab -> TableValue sc Identity tab
  default fromNewRow ::
    ( GConstructHK tab (HasConstColumn sc tab (HasDefault sc tab) (Generated sc tab) (NewRow sc tab)) (TypeFields tab)
    , MkFieldInvIx (TableColumns sc tab)
    ) => NewRow sc tab -> TableValue sc Identity tab
  fromNewRow r = TableValue fsix $ constructHK @(HasConstColumn sc tab (HasDefault sc tab) (Generated sc tab) (NewRow sc tab)) (ExprF . toExprId . coerceExpr . getConstCol (Proxy @'(sc, tab, (HasDefault sc tab), (Generated sc tab))) r)
    where fsix = mkFieldInvIx (Proxy @(TableColumns sc tab)) emptyFieldInvIx

newtype TableName sc ty = TableName Text

unTableName :: TableName sc tab -> Text
unTableName = coerce

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

class HasConstColumn (sc :: Type) (tab :: Type) (defs :: [Symbol]) (gens :: [(Symbol, GenerationType)]) (r :: Type) (col :: Symbol) (a :: Type) where
  getConstCol :: Proxy '(sc, tab, defs, gens) -> r -> Proxy '(col, a) -> PQ.Expr sc (Field col a)

instance (HasConstOrDefCol (IsDefCol col defs gens) sc tab r col a) => HasConstColumn sc tab defs gens r col a where
  getConstCol _ _r px = getConstOrDefCol (Proxy @'(sc, tab, (IsDefCol col defs gens))) _r px


class HasConstOrDefCol (isDef :: Either Bool GenerationType) (sc :: Type) (tab :: Type) (r :: Type) (col :: Symbol) (a :: Type) where
  getConstOrDefCol :: Proxy '(sc, tab, isDef) -> r -> Proxy '(col, a) -> PQ.Expr sc (Field col a)

instance HasConstOrDefCol ('Left 'True) sc tab r col a where
  getConstOrDefCol _ _ _ = PQ.Expr PQ.DefaultInsertExpr

instance (R.HasField col r a, AutoConstExpr sc a (ToDBType (DB (SchemaDB sc)) a) (AutoCodec (DB (SchemaDB sc)) a)) => HasConstOrDefCol ('Left 'False) sc tab r col a where
  getConstOrDefCol _ r _ = coerceExpr $ autoConstExpr (Proxy @'(ToDBType (DB (SchemaDB sc)) a, AutoCodec (DB (SchemaDB sc)) a)) $ R.getField @col r

class AutoConstExpr sc t (dbObj :: DBObjK) (isAuto :: Bool) where
  autoConstExpr :: Proxy '(dbObj, isAuto) -> t -> PQ.Expr sc t

-- TODO: Add TypeError for `'TableObj`
instance ConstExpr sc t => AutoConstExpr sc t dbObj 'False where
  autoConstExpr _ = constExpr
  
instance TypeError ('Text "Unexpected Table in place of Type" ':<>: 'ShowType t) => AutoConstExpr sc t 'TableObj 'True where
  autoConstExpr = error "Panic: Unreachable code"

instance ConstExpr sc t => AutoConstExpr sc t ('NativeTypeObj nat) 'True where
  autoConstExpr _ = constExpr

instance AutoConstExpr sc t ('UDTypeObj ('TaggedUnionUnary colty enum)) 'True where
  autoConstExpr _ = undefined

instance (Generic t, UDType sc t, GenEnumExpr sc t (Rep t) (GetTagEnumK (ToDBType (DB (SchemaDB sc)) t))) => AutoConstExpr sc t ('UDTypeObj ('UDEnum enum)) 'True where
  autoConstExpr _ t = genEnumExpr t

instance (t ~ ety, AutoConstExpr sc ety edbk 'True) => AutoConstExpr sc (Maybe t) ('NullableObjOf ety edbk) 'True where
  autoConstExpr _ = \case
    Nothing -> nothing
    Just t -> toNullable $ autoConstExpr (Proxy @'(edbk, 'True)) t

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

-- runClause :: forall i o sc s.
--   Clause s sc i o
--   -> PQ.Clauses
-- runClause (Clause _clau) = undefined

-- Unsafe
scoped :: forall i o sc s.
  ((PQ.Clauses, Scoped s sc i) -> (PQ.Clauses, o))
  -> Clause s sc i o
scoped fn = Clause $ state (\(c,es) -> let (c', o) = fn (c, Scoped es) in (o, (c', es)))

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
tableToListWith' _pfxs _ EmptyTable = []

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
nextStage' _pfxs EmptyTable = EmptyTable


hoistMaybeTable :: (forall g x. ExprF sc g x -> ExprF sc Maybe x) -> TableValue sc f i -> TableValue sc Maybe i
hoistMaybeTable fn (TableValue fsix hk) = TableValue fsix $ hoistHK fn hk
hoistMaybeTable fn (JoinedTables fsix tvals) = JoinedTables fsix $ hoistHK (hoistMaybeTable fn) tvals
hoistMaybeTable fn (OptTable tv) = OptTable (hoistMaybeTable fn tv)
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
unsafeTableToExpr (JoinedTables _ tvals) = PQ.Expr $ PQ.FlatComposite $ hkToListWithTag (\ssym tv -> (aliasedExprName ssym, PQ.getExpr $ unsafeTableToExpr tv)) tvals
unsafeTableToExpr (OptTable tv) = unsafeTableToExpr tv
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
  
  JoinedTables :: !FieldInvIx -> HK (TableValue sc f) t -> TableValue sc f t
  OptTable :: Typeable t => TableValue sc f t -> TableValue sc f (Maybe t)

  EmptyTable :: TableValue sc f ()

tableRecAsType :: ValidateRecToType os t => TableValue sc f (Rec os) -> TableValue sc f t
tableRecAsType (TableValue fsix hk) = TableValue fsix (fromHKOfRec hk)
tableRecAsType (JoinedTables fsix hk) = JoinedTables fsix (fromHKOfRec hk)
  
newtype Scalar sc t = Scalar (PQ.Expr sc t)

instance (R.HasField f i t, KnownSymbol f, Typeable t) => R.HasField (f :: Symbol) (Scoped s sc i) (PQ.Expr sc t) where
  getField (Scoped hk) = R.getField @f hk

instance (R.HasField f i t, KnownSymbol f, Typeable t) => R.HasField (f :: Symbol) (TableValue sc Identity i) (PQ.Expr sc t) where
  getField (TableValue _ hk) = fromIdExpr $ getExprF $ R.getField @f hk
  getField EmptyTable = PQ.Expr unitExpr
  getField optv@(OptTable tv) = go tv optv
    where
      go :: forall r.(Typeable r) => TableValue sc Identity r -> TableValue sc Identity (Maybe r) -> PQ.Expr sc t
      go tv' _ = case eqT @t @(Maybe r) of
        Just Refl -> DBRecord.Internal.Expr.toNullable $ idTableToExpr tv'
        Nothing -> error "Panic"
  getField (JoinedTables _ tvals) = idTableToExpr $ R.getField @f tvals

class HasColumn sc tab (col :: Symbol) (a :: Type) where
  getCol :: Proxy '(sc, tab) -> Proxy '(col, a) -> PQ.Expr sc (Field col a)

instance
  ( Table sc tab
  , R.HasField col tab ct
  , ct ~ a
  , KnownSymbol col
  , HasColumnByDBType sc tab col a dbTypeRep
  , dbTypeRep ~ ToDBType (DB (SchemaDB sc)) a
  ) => HasColumn sc tab col a where
  getCol _ _ = PQ.Expr $ PQ.getExpr $ getColByDBTypeRep @sc @tab @col @a @dbTypeRep Proxy


class HasColumnByDBType sc tab (col :: Symbol) (a :: Type) (dbObj :: DBObjK) where
  getColByDBTypeRep :: Proxy '(sc, tab, col, a, atrep) -> PQ.Expr sc a


instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  -- , UDType sc a
  ) => HasColumnByDBType sc tab col a 'TableObj where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  -- , UDType sc a
  ) => HasColumnByDBType sc tab col a ('NativeTypeObj dbk) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  -- , UDType sc a
  ) => HasColumnByDBType sc tab col a ('NullableObjOf e edbk) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  -- , UDType sc a
  ) => HasColumnByDBType sc tab col a ('ArrayObjOf e edbk) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col  
  
instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  -- , UDType sc a
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('SerializedBlob ct)) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col  
  
instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  -- , UDType sc a
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('UDRec 'FlatRec)) where
  getColByDBTypeRep _ = undefined -- TODO: Make FlatComposite

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  -- , UDType sc a
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('UDRec 'CompositeRec)) where
  getColByDBTypeRep _ = undefined -- TODO: Make Composite

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  -- , UDType sc a
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('UDRec 'JsonRec)) where
  getColByDBTypeRep _ = undefined -- TODO: Make JSON

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  -- , UDType sc a
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('TaggedUnionRec dis r)) where
  getColByDBTypeRep _ = undefined -- TODO:

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  , UDType sc a
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('TaggedUnionUnary dis r)) where
  getColByDBTypeRep _ = undefined -- TODO:

instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  -- , UDType sc a
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('TypedUnion ty)) where
  getColByDBTypeRep _ = undefined -- TODO:  
  
instance
  ( Table sc tab
  , R.HasField col tab a
  , KnownSymbol col
  -- , UDType sc a
  ) => HasColumnByDBType sc tab col a ('UDTypeObj ('UDEnum et)) where
  getColByDBTypeRep _ = getColumnName @sc @tab @col  

getColumnName :: forall sc tab (fn :: Symbol) a.
  ( Table sc tab
  , R.HasField fn tab a
  , KnownSymbol fn
  ) => PQ.Expr sc a
getColumnName =
  let
    FieldAliases caliases = columnAliases @sc @tab
    fname = T.pack $ symbolVal (Proxy @fn)
    cname = maybe (defHSNameToDBName fname) id $ HM.lookup fname caliases
    cexpr = PQ.BaseTableAttrExpr $ cname
  in PQ.Expr cexpr
{-# INLINE getColumnName #-}


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

type family ValidateTableProps (sc :: Type) (tab :: Type) :: Constraint where
  ValidateTableProps sc tab =
    ( 
    )
    
data ForeignRef a
  = RefBy [Symbol] a [Symbol] Symbol
  | Ref Symbol a Symbol

data UniqueCT = UniqueOn [Symbol] Symbol
data Uq sc (un :: Symbol) = Uq

  
