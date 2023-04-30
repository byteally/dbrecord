{-# OPTIONS_GHC -Wwarn -Wno-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE ExplicitForAll                #-}
{-# LANGUAGE KindSignatures                #-}
{-# LANGUAGE TypeFamilyDependencies        #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE ConstraintKinds               #-}
{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE TypeOperators                 #-}
{-# LANGUAGE UndecidableInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE TypeApplications              #-}
{-# LANGUAGE PatternSynonyms               #-}
{-# LANGUAGE ViewPatterns                  #-}
{-# LANGUAGE GADTs                         #-}
{-# LANGUAGE FlexibleInstances             #-}
{-# LANGUAGE MultiParamTypeClasses         #-}
{-# LANGUAGE FunctionalDependencies        #-}
{-# LANGUAGE RankNTypes                    #-}
{-# LANGUAGE DerivingStrategies            #-}

-- TODO: Clean up
{-# LANGUAGE DuplicateRecordFields         #-}
{-# LANGUAGE OverloadedLabels              #-}
{-# LANGUAGE DeriveGeneric                 #-}

module DBRecord.Query2
  ( module DBRecord.Internal.Order
  , module DBRecord.Internal.Expr
  , module DBRecord.Internal.Window
  , module DBRecord.Internal.Predicate
  , module DBRecord.Query2
  , module Record
  -- Schema Internal Reexports
  , Query' (..)
  , Query
  , PlainQ
  , MQuery
  --
  ) where


import DBRecord.Old.Schema

import DBRecord.Internal.Order
import DBRecord.Internal.Expr hiding (Alias)
import DBRecord.Internal.Predicate
import DBRecord.Internal.Common
import DBRecord.Internal.Window
import DBRecord.Internal.Schema hiding (insert, delete)
import DBRecord.Internal.PrimQuery  hiding (insertQ, updateQ, deleteQ, alias, Join)
import DBRecord.Internal.Query (getTableId, getTableProjections)
import           DBRecord.Internal.DBTypes (GetDBTypeRep)
import qualified DBRecord.Internal.Query as Q
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Types
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Int
import GHC.Exts
import GHC.TypeLits
import Data.Typeable
import Data.Type.Equality
import Data.Functor.Identity
import Control.Monad.IO.Class
import Control.Monad.Reader
import DBRecord.Internal.Lens ((^.))
import Control.Exception hiding (TypeError)
import Data.Kind 
import Record
import Record.Setter
import GHC.OverloadedLabels
import GHC.Records as R
-- import qualified GHC.Records.Compat as R
import GHC.Generics

-- TODO: Clean up
import Debug.Trace
import DBRecord.Driver
import GHC.Records

type Query sc t = Query' PlainQ sc t
type As = Field

-- * Joins
-- FROM T1 CROSS JOIN T2 is equivalent to FROM T1 INNER JOIN T2 ON TRUE. It is also equivalent to FROM T1, T2.

-- newtype Rel (n :: Symbol) r = Rel (
data Join (n1 :: Symbol) r1 (n2 :: Symbol) r2 = Join (Field n1 r1) (Field n2 r2)
  deriving (Show, Eq)

data JoinScope s sc (n1 :: Symbol) r1 (n2 :: Symbol) r2 = JoinScope (Scoped s sc r1) (Scoped s sc r2)

instance
  ( GHC.Records.HasField '(fn, fn == n1, fn == n2) (Join n1 r1 n2 r2) t
  ) => R.HasField (fn :: Symbol) (Join n1 r1 n2 r2) t where
  getField v = R.getField @'(fn, fn == n1, fn == n2) v

instance (t ~ r1) => R.HasField '(fn :: Symbol, 'True, 'False) (Join n1 r1 n2 r2) (Field n1 t) where
  getField (Join r1 r2) = r1

instance (t ~ r2) => R.HasField '(fn :: Symbol, 'False, 'True) (Join n1 r1 n2 r2) (Field n2 t) where
  getField (Join r1 r2) = r2

instance
  ( GHC.Records.HasField '(fn, fn == n1, fn == n2) (JoinScope s sc n1 r1 n2 r2) t
  ) => R.HasField (fn :: Symbol) (JoinScope s sc n1 r1 n2 r2) t where
  getField v = R.getField @'(fn, fn == n1, fn == n2) v

instance (t ~ Scoped s sc r1) => R.HasField '(fn :: Symbol, 'True, 'False) (JoinScope s sc n1 r1 n2 r2) (Field n1 t) where
  getField (JoinScope r1 r2) = fromLabel @n1 .= r1

instance (t ~ Scoped s sc r2) => R.HasField '(fn :: Symbol, 'False, 'True) (JoinScope s sc n1 r1 n2 r2) (Field n2 t) where
  getField (JoinScope r1 r2) = fromLabel @n2 .= r2  

{-
crossJoin :: forall o1 o2 n1 r1 n2 r2 sc.
  As n1 (Query sc r1)
  -> As n2 (Query sc r2)
  -> (forall s.Clause s sc (Join n1 r1 n2 r2) (Join n1 o1 n2 o2))
  -> Query sc (Join n1 o1 n2 o2)
crossJoin _ _ _ = undefined

-}

newtype Joins sc qs = Joins (HRec (Query' PlainQ sc) qs)
  deriving newtype (AnonRec)

{-
select ...
from [tabA a
  left join tabB b on (b.aid = a.id)]
  left join tabC c on (c.aid = a.id)
  left join tabD d on (d.aid = a.id)
...

some => One or more.
many => Zero or more.
optional => One or none.

join $  #foo .= q1
      .& #bar .= q2 `on`
      
-}
joins :: forall o qs sc.
  () =>
  Joins sc qs -> ()
joins (DBRecord.Query2.Joins js) = undefined
  -- where
  --   toTabVal :: Joins sc qs -> TableValue sc Identity (Rec qs)
  --   toTabVal js = let (fval, rst) = unconsRec js in consRec fval (toTabVal rst)

innerJoin :: forall o n1 r1 n2 r2 sc.
  (KnownSymbol n1, KnownSymbol n2, Typeable r1, Typeable r2) =>
    As n1 (Query sc r1)
  -> As n2 (Query sc r2)
  -> (forall s.Scoped s sc (Rec '[ '(n1, r1), '(n2, r2)]) -> Expr sc Bool)
  -> (forall s.Clause s sc (Rec '[ '(n1, r1), '(n2, r2)]) (TableValue sc Identity o))
  -> Query sc o
innerJoin q1 q2 on (Clause clau) =
  let
    (q1PQ, q1Res) = runAliasedQuery q1
    (q2PQ, q2Res) = runAliasedQuery q2
    joinTabVal = nextStage $ crossRel (fromLabel @n1 .= q1Res) (fromLabel @n2 .= q2Res)
    onCond = getExpr $ on $ getScopeOfTable $ joinTabVal
  in Query' ( joinTabVal
            , clau
            , PQ.Join PQ.InnerJoin False (Just onCond) (PQ.PrimQuery q1PQ) (PQ.PrimQuery q2PQ)
            )

class (KnownSymbol fn, Typeable t) => FieldCxt (fn :: Symbol) (t :: Type)
instance (KnownSymbol fn, Typeable t) => FieldCxt fn t

leftJoin :: forall o n1 r1 n2 r2 sc.
  (KnownSymbol n1, KnownSymbol n2, Typeable r1, Typeable r2) =>
    As n1 (Query sc r1)
  -> As n2 (Query sc r2)
  -> (forall s.Scoped s sc (Rec '[ '(n1, r1), '(n2, r2)]) -> Expr sc Bool)
  -> (forall s.Clause s sc (Rec '[ '(n1, r1), '(n2, Maybe r2)]) (TableValue sc Identity o))
  -> Query sc o
leftJoin q1 q2 on (Clause clau) =
  let
    (q1PQ, q1Res) = runAliasedQuery q1
    (q2PQ, q2Res) = runAliasedQuery q2
    crossTableVal = nextStage $ crossRel (fromLabel @n1 .= q1Res) (fromLabel @n2 .= q2Res)
    ljjoinTabVal = nextStage $ ljRel (fromLabel @n1 .= q1Res) (fromLabel @n2 .= hoistMaybeTable toNullExprF q2Res)
    onCond = getExpr $ on $ getScopeOfTable $ crossTableVal
  in Query' ( ljjoinTabVal
            , clau
            , PQ.Join PQ.LeftJoin False (Just onCond) (PQ.PrimQuery q1PQ) (PQ.PrimQuery q2PQ)
            )

rightJoin :: forall o n1 r1 n2 r2 sc.
  (KnownSymbol n1, KnownSymbol n2, Typeable r1, Typeable r2) =>
    As n1 (Query sc r1)
  -> As n2 (Query sc r2)
  -> (forall s.Scoped s sc (Rec '[ '(n1, r1), '(n2, r2)]) -> Expr sc Bool)
  -> (forall s.Clause s sc (Rec '[ '(n1, Maybe r1), '(n2, r2)]) (TableValue sc Identity o))
  -> Query sc o
rightJoin q1 q2 on (Clause clau) =
  let
    (q1PQ, q1Res) = runAliasedQuery q1
    (q2PQ, q2Res) = runAliasedQuery q2
    crossTableVal = nextStage $ crossRel (fromLabel @n1 .= q1Res) (fromLabel @n2 .= q2Res)
    rjoinTabVal = nextStage $ rjRel (fromLabel @n1 .= hoistMaybeTable toNullExprF q1Res) (fromLabel @n2 .= q2Res)
    onCond = getExpr $ on $ getScopeOfTable $ crossTableVal
  in Query' ( rjoinTabVal
            , clau
            , PQ.Join PQ.RightJoin False (Just onCond) (PQ.PrimQuery q1PQ) (PQ.PrimQuery q2PQ)
            )

{-
fullJoin :: () -> As n1 (Query sc r1) -> As n2 (Query sc r2) -> Query sc (Join n1 (HK Maybe r1) n2 (HK Maybe r2))
fullJoin = undefined
-}

-- lateralInnerJoin :: As n1 (Query sc r1) -> As n2 (Query sc (Scoped sr2)) -> Query sc ()
-- lateralInnerJoin = undefined

-- Emulated (https://learn.microsoft.com/en-gb/archive/blogs/craigfr/introduction-to-joins)
-- semiJoin
-- antiSemiJoin

-- * Query Combination

-- In order to calculate the union, intersection, or difference of two queries, the two queries must be “union compatible”, which means that they return the same number of columns and the corresponding columns have compatible data types.
-- REF: https://www.postgresql.org/docs/13/typeconv-union-case.html
union :: Query sc r -> Query sc r -> Query sc r
union = binQ Union

unionAll :: Query sc r -> Query sc r -> Query sc r
unionAll = binQ UnionAll
  
intersect :: Query sc r -> Query sc r -> Query sc r
intersect = binQ Intersection

intersectAll :: Query sc r -> Query sc r -> Query sc r
intersectAll = binQ IntersectionAll

except :: Query sc r -> Query sc r -> Query sc r
except = binQ Except

exceptAll :: Query sc r -> Query sc r -> Query sc r
exceptAll = binQ ExceptAll

binQ :: BinType -> Query sc r -> Query sc r -> Query sc r
binQ binType q1 q2 = Query'
  ( undefined
  , let (Clause clau) = selectAll in clau
  , const $ Binary Union ((execQuery q1)) ((execQuery q2)) Nothing
  )

-- * Ordering

-- ORDER BY can be applied to the result of a UNION, INTERSECT, or EXCEPT combination, but in this case it is only permitted to sort by output column names or numbers, not by expressions.

sort :: forall i sc s.(Scoped s sc i -> Order sc) -> Clause s sc i ()
sort ordFn = scoped $ \(clau, inp) -> (clau {orderbys = orderbys clau <> getOrder (ordFn inp)}, ())

-- * Where
restrict :: forall i sc s.(Scoped s sc i -> Expr sc Bool) -> Clause s sc i ()
restrict filtFn = scoped $ \(clau, inp) -> (clau {criteria = criteria clau <> [PQ.getExpr (filtFn inp)]}, ())


data SelectList sc os = SelectList [TypeRep] (HRec (PQ.Expr sc) os)
--  deriving newtype (AnonRec)

instance AnonRec (SelectList sc) where
  type FieldKind (SelectList sc) = FieldKind (HRec (PQ.Expr sc))
  type IsHKRec (SelectList sc) = IsHKRec (HRec (PQ.Expr sc))
  type FieldNameConstraint (SelectList sc) = FieldNameConstraint (HRec (PQ.Expr sc))
  type FieldConstraint (SelectList sc) = FieldConstraint (HRec (PQ.Expr sc))
  endRec = SelectList [] endRec
  {-# INLINE endRec #-}
  consRec fld (SelectList fsix r) =
    let nfsix = (fldTyTRep fld) : fsix
        fldTyTRep :: forall fn a.(KnownSymbol fn) => Field fn a -> TypeRep
        fldTyTRep _ = typeRep (Proxy @fn)
    in SelectList nfsix $ consRec fld r
  {-# INLINE consRec #-}
  unconsRec (SelectList fsix r) =
    let (fld, r') = unconsRec r
        fldTyTRep :: forall fn a.(KnownSymbol fn) => Field fn a -> TypeRep
        fldTyTRep _ = typeRep (Proxy @fn)
    in (fld, SelectList (L.delete (fldTyTRep fld) fsix) r')
  

data AggSelectList sc os = AggSelectList [TypeRep] (HRec (PQ.AggExpr sc) os)

instance AnonRec (AggSelectList sc) where
  type FieldKind (AggSelectList sc) = FieldKind (HRec (PQ.AggExpr sc))
  type IsHKRec (AggSelectList sc) = IsHKRec (HRec (PQ.AggExpr sc))
  type FieldNameConstraint (AggSelectList sc) = FieldNameConstraint (HRec (PQ.AggExpr sc))
  type FieldConstraint (AggSelectList sc) = FieldConstraint (HRec (PQ.AggExpr sc))
  endRec = AggSelectList [] endRec
  {-# INLINE endRec #-}
  consRec fld (AggSelectList fsix r) =
    let nfsix = (fldTyTRep fld) : fsix
        fldTyTRep :: forall fn a.(KnownSymbol fn) => Field fn a -> TypeRep
        fldTyTRep _ = typeRep (Proxy @fn)
    in AggSelectList nfsix $ consRec fld r
  {-# INLINE consRec #-}
  unconsRec (AggSelectList fsix r) =
    let (fld, r') = unconsRec r
        fldTyTRep :: forall fn a.(KnownSymbol fn) => Field fn a -> TypeRep
        fldTyTRep _ = typeRep (Proxy @fn)
    in (fld, AggSelectList (L.delete (fldTyTRep fld) fsix) r')

selectListToTable :: SelectList sc os -> TableValue sc Identity (Rec os)
selectListToTable (SelectList fsix selRec) = TableValue (fromListToFieldInvIx fsix) $ hoistWithKeyHK (ExprF . toIdExpr) $ hrecToHKOfRec selRec

aggSelectListToTable :: AggSelectList sc os -> TableValue sc Aggregated (Rec os)
aggSelectListToTable (AggSelectList fsix selRec) = TableValue (fromListToFieldInvIx fsix) $ hoistWithKeyHK (ExprF . coerceExpr . getAggExpr) $ hrecToHKOfRec selRec

-- TODO: Bugy. Fix the FieldIxs w.r.t target type
selectListToType :: (ValidateRecToType os t) => SelectList sc os -> TableValue sc Identity t
selectListToType (SelectList fsix selRec) = TableValue (fromListToFieldInvIx fsix) $ hoistWithKeyHK (ExprF . toIdExpr) $ fromHRec selRec

  
newtype SelectScope s sc i = SelectScope (Scoped s sc i)

instance (R.HasField fn i t, KnownSymbol fn, Typeable t) => R.HasField (fn :: Symbol) (SelectScope s sc i) (Field fn (PQ.Expr sc t)) where
  getField (SelectScope scope) = fromLabel @fn .= R.getField @fn scope

instance R.HasField fn (HRec (PQ.Expr sc) os) (PQ.Expr sc t) => R.HasField (fn :: Symbol) (SelectList sc os) (PQ.Expr sc t) where
  getField (SelectList _ r) = R.getField @fn r
  
  
-- * Select List

selectAll :: forall i sc s.Clause s sc i (TableValue sc Identity i)
selectAll = scoped $ \(clau, scopes@(Scoped tabv)) -> (clau, tabv)

select :: forall i os sc s.
  (SelectScope s sc i -> SelectList sc os) -> Clause s sc i (TableValue sc Identity (Rec os))
select selFn = scoped $ \(clau, scopes) -> let selCols@(SelectList _ selRec) = selFn (SelectScope scopes)
  in (clau, selectListToTable selCols)

selectAgg :: forall i os sc s.
  ((forall a.Grouped s sc a -> AggExpr sc a) -> SelectScope s sc i -> AggSelectList sc os) -> Clause s sc i (TableValue sc Aggregated (Rec os))
selectAgg selFn = scoped $ \(clau, scopes) -> let selCols@(AggSelectList _ selRec) = selFn unGroup (SelectScope scopes)
  in (clau, aggSelectListToTable selCols)  

selectUsing :: forall o i os sc s.ValidateRecToType os o => (SelectScope s sc i -> SelectList sc os) -> Clause s sc i (TableValue sc Identity o)
selectUsing selFn = scoped $ \(clau, scopes) -> let selCols@(SelectList _ selRec) = selFn (SelectScope scopes)
  in (clau, selectListToType selCols)

using :: forall o i os sc s.ValidateRecToType os o => Clause s sc i (TableValue sc Identity (Rec os)) -> Clause s sc i (TableValue sc Identity o)
using clau = clau >>= pure . tableRecAsType

-- TODO: Consider the alt strategy of having index representing Plain | Agg | Insert | Update | Delete  in `Clause` which will let us reuse the combinators
newtype Aggregated a = Aggregated { unAgg :: Identity a}

{- Variants
SELECT DISTINCT select_list ...
SELECT DISTINCT ON (expression [, expression ...]) select_list ...
-}

selectDistinct :: forall i os sc s.
  ( 
  ) => (Scoped s sc i -> SelectList sc os) -> Clause s sc i (SelectList sc os)
selectDistinct = undefined

selectNone :: forall i sc s.Clause s sc i (TableValue sc Identity ())
selectNone = scoped $ \(clau, _) -> (clau, EmptyTable)

-- * Grouping
aggregate :: forall o i sc.
  ((forall s.Clause s sc i (TableValue sc Identity o)) -> Query' PlainQ sc o)
  -> (forall s1.Clause s1 sc i (TableValue sc Aggregated o))
  -> Query' PlainQ sc o
aggregate fn clauM = fn (go <$> clauM)
  where
    go :: TableValue sc Aggregated o -> TableValue sc Identity o
    go (TableValue fsix v) = TableValue fsix $ hoistWithKeyHK (\(ExprF e) -> ExprF (coerceExpr e)) v
    go _ = undefined

newtype Grouped s sc a = Grouped {unGroup :: AggExpr sc a}

fromGroup :: Grouped s sc a -> (AggExpr sc a -> AggSelectList sc os) -> AggSelectList sc os
fromGroup (Grouped g) f = f g

groupBy :: forall a i sc s.(Scoped s sc i -> Expr sc a) -> Clause s sc i (Grouped s sc a)
groupBy grpFn = scoped $ \(clau, inp) ->
  let gpVal = grpFn inp
  in (clau {groupbys = groupbys clau <> [PQ.getExpr gpVal]}, Grouped (AggExpr gpVal))

having :: forall i sc s.(Scoped s sc i -> AggExpr sc Bool) -> Clause s sc i ()
having filtFn = scoped $ \(clau, inp) -> (clau {havings = havings clau <> [PQ.getExpr (getAggExpr (filtFn inp))]}, ())

-- * LIMIT & OFFSET
{-
SELECT select_list
    FROM table_expression
    [ ORDER BY ... ]
    [ LIMIT { number | ALL } ] [ OFFSET number ]
-}

limit :: forall i sc s.Maybe Word -> Clause s sc i ()
limit lmtMay = scoped $ \(clau, _) -> (clau {limit = getExpr . constExpr <$> lmtMay}, ())

offset :: forall i sc s.Maybe Word -> Clause s sc i ()
offset osMay = scoped $ \(clau, _) -> (clau {offset = getExpr . constExpr <$> osMay}, ())


-- * CTE
with :: As fn (Query sc r)
     -> (Query sc r -> Query sc res)
     -> Query sc res
with = undefined

with2 ::
  As fn (Query sc r1)
  -> As fn (Query sc r2)
  -> (Query sc r1 -> Query sc r2 -> Query sc res)
  -> Query sc res
with2 = undefined

-- Subquery
from :: forall o r fn sc.Field fn (Query sc r) -> (forall s.Clause s sc r (TableValue sc Identity o)) -> Query' PlainQ sc o
from _ _ = undefined

{-
4.2.11. Scalar Subqueries
A scalar subquery is an ordinary SELECT query in parentheses that returns exactly one row with one column. (See Chapter 7 for information about writing queries.) The SELECT query is executed and the single returned value is used in the surrounding value expression. It is an error to use a query that returns more than one row or more than one column as a scalar subquery. (But if, during a particular execution, the subquery returns no rows, there is no error; the scalar result is taken to be null.) The subquery can refer to variables from the surrounding query, which will act as constants during any one evaluation of the subquery. See also Section 9.23 for other expressions involving subqueries.
SELECT name, (SELECT max(pop) FROM cities WHERE cities.state = states.name)
    FROM states;
-}
scalarSubQuery :: forall t par r sc ps.((forall s.Clause s sc r (TableValue sc Identity t)) -> Query sc t) -> (forall s1.Clause s1 sc r {- + par -} (Scalar sc t)) -> Clause ps sc par (Scalar sc t)
scalarSubQuery _ _ = undefined

runQueryAsList :: forall r m sc driver.
  ( MonadIO m
  , HasQuery driver
  , FromDBRow driver r
  , MonadReader driver m
  ) => Query sc r
    -> m [r]
runQueryAsList q = do
  driver <- ask
  liftIO $ dbQuery driver (execQuery q)

-- MutationQ

data InsertSetting

newtype ViaTraversable f a = ViaTraversable {getTraverseable :: f a}

insert' :: forall o f tab sc.(Table sc tab) => InsertValues -> (TableValue sc Identity tab -> TableValue sc Identity o) -> MQuery sc o
insert' ivals retFn = getMutQ @o @tab @sc $ \tabId basetab ->
  let attrs = fmap peToAttr $ tableToProjections basetab
      peToAttr (_, PQ.BaseTableAttrExpr a) = a
      peToAttr (_, e) = error $ "Panic: Invariant violated! Expected only `BaseTableAttrExpr` " <> (show e)
  in InsertMQuery (basetab, pure $ retFn basetab, InsertQuery tabId attrs ivals Nothing)

class Insertable (f :: Type -> Type) where
  insert :: forall o tab sc.(Table sc tab) => f (NewRow sc tab) -> (TableValue sc Identity tab -> TableValue sc Identity o) -> MQuery sc o

{-
instance Insertable [] where
  insert :: forall o tab sc.(Table sc tab) => [NewRow sc tab] -> (TableValue sc Identity tab -> TableValue sc Identity o) -> MQuery sc o
  insert [] retFn = MQueryNoOp
  insert (v : vs) retFn = insert (v NE.:| vs) retFn
-}

ivals :: TableValue sc f i -> [PQ.PrimExpr]
ivals tabv = fmap snd $ tableToProjections tabv

instance Insertable NE.NonEmpty where
  insert :: forall o tab sc.(Table sc tab) => NE.NonEmpty (NewRow sc tab) -> (TableValue sc Identity tab -> TableValue sc Identity o) -> MQuery sc o
  insert vs retFn = insert' (InsertValues $ fmap (ivals . fromNewRow @sc @tab) vs) retFn
  
instance Insertable Identity where
  insert :: forall o tab sc.(Table sc tab) => Identity (NewRow sc tab) -> (TableValue sc Identity tab -> TableValue sc Identity o) -> MQuery sc o
  insert (Identity v) retFn = insert' (InsertValues (ivals (fromNewRow @sc @tab v) NE.:| [])) retFn

instance Insertable (Query' ct sc) where
--  insert = insert'


data ISubQuery sc t where
  ISubQuery :: Query sc t -> ISubQuery sc (NewRow sc t)
  
insertOne :: forall o tab sc.(Table sc tab) => NewRow sc tab -> (TableValue sc Identity tab -> TableValue sc Identity o) -> MQuery sc o
insertOne v ret = insert @Identity (Identity v) ret

insertMany :: forall o tab f sc.(Insertable f, Traversable f, Table sc tab) => f (NewRow sc tab) -> (TableValue sc Identity tab -> TableValue sc Identity o) -> MQuery sc o
insertMany vs ret = insert @f vs ret

insertFrom :: forall o tab sc.(Table sc tab) => Query sc (NewRow sc tab) -> (TableValue sc Identity tab -> TableValue sc Identity o) -> MQuery sc o
insertFrom qs ret = insert @(Query' PlainQ sc) qs ret

data UpdatingRow sc tab = UpdatingRow (TableValue sc Identity tab) (TableValue sc Identity tab -> PQ.Assoc)

runUpdatingRow :: UpdatingRow sc tab -> PQ.Assoc
runUpdatingRow (UpdatingRow tv updr) = updr tv

instance (R.HasField fn (TableValue sc Identity tab) t) => R.HasField (fn :: Symbol) (UpdatingRow sc tab) t where
  getField (UpdatingRow tv _) = R.getField @fn tv

instance (R.HasField fn tab a, KnownSymbol fn, Typeable a) => SetField (fn :: Symbol) (UpdatingRow sc tab) (Expr sc a) where
  modifyField upd (UpdatingRow tv@(TableValue _ _) updr) =
    let
      newUpdr = \btv ->
        let
          colE = R.getField @fn tv
          colN = case colE of
            PQ.Expr (PQ.BaseTableAttrExpr a) -> a
            PQ.Expr e -> error $ "Panic: Invariant violated! Expected only `BaseTableAttrExpr` " <> (show e)
        in (colN, PQ.getExpr (upd colE)) : updr btv
    in UpdatingRow tv newUpdr
  modifyField _ (UpdatingRow _ _) = error "Panic: Invariant of UpdatingRow violated! Expected only base table"

set :: forall tab sc s.(UpdatingRow sc tab -> UpdatingRow sc tab) -> Clause s sc tab ()
set updFn = scoped $ \(clau, Scoped inp) ->
  let
    updAssoc = runUpdatingRow $ updFn (UpdatingRow inp (const []))
  in (clau, ())

-- TODO: Make `set` a combinator instead of HOF.
update :: forall tab o sc.(Table sc tab) => (UpdatingRow sc tab -> UpdatingRow sc tab) -> (forall s.Clause s sc tab (TableValue sc Identity o)) -> MQuery sc o
update updFn (Clause clau) = getMutQ @o @tab @sc $ \tabId basetab ->
  let
    updAssoc = runUpdatingRow $ updFn (UpdatingRow basetab (const []))
    mkUpdatePQ PQ.Clauses {criteria} = UpdateQuery tabId criteria updAssoc []
  in UpdateMQuery (basetab, clau, mkUpdatePQ)

delete :: forall tab o sc.(Table sc tab) => (forall s.Clause s sc tab (TableValue sc Identity o)) -> MQuery sc o
delete (Clause clau) = getMutQ @o @tab @sc $ \tabId basetab ->
  let
    mkDeletePQ PQ.Clauses {criteria} = PQ.DeleteQuery tabId criteria []
  in DeleteMQuery (basetab, clau, mkDeletePQ)

runMQueryAsList :: forall r m sc driver.
  ( MonadIO m
  , HasInsertRet driver
  , HasUpdateRet driver
  , HasDeleteRet driver
  , FromDBRow driver r
  , MonadReader driver m
  ) => MQuery sc r
    -> m [r]
runMQueryAsList q = do
  driver <- ask
  let
    runInsert iq = do
      liftIO $ dbInsertRet driver iq
    runUpdate uq = do
      liftIO $ dbUpdateRet driver uq
    runDelete dq = do
      liftIO $ dbDeleteRet driver dq
  execMQuery runInsert runUpdate runDelete (pure []) q

runMQuery_ :: forall m sc driver.
  ( MonadIO m
  , HasInsert driver
  , HasUpdate driver
  , HasDelete driver  
  , MonadReader driver m
  ) => MQuery sc ()
    -> m Int64
runMQuery_ q = do
  driver <- ask
  let
    runInsert iq = do
      liftIO $ dbInsert driver iq
    runUpdate uq = do
      liftIO $ dbUpdate driver uq
    runDelete dq = do
      liftIO $ dbDelete driver dq
  execMQuery runInsert runUpdate runDelete (pure 0) q  
  
-- class Updatable (f :: Type -> Type) where
--   updatable :: forall o tab sc.(Table sc tab) => (TableValue sc Identity tab -> TableValue sc Identity o) -> (forall s.Clause s sc tab o) -> MQuery sc o

-- instance Updatable [] where
--   updatable = undefined
  
-- instance Updatable Identity where
--   updatable = undefined

-- instance Updatable (Query' ct sc) where
--   updatable = undefined  

-- class Deletable (f :: Type -> Type) where
--   deletable :: f ()

-- instance Deletable [] where
--   deletable = undefined
  
-- instance Deletable Identity where
--   deletable = undefined

-- instance Deletable (Query' ct sc) where
--   deletable = undefined  

-- TEST CODE
{-
-- testClau1 :: _ -- Clause s sc User2 User2
testClau1 = do
  let ce :: Scoped s ZB User2 -> Expr ZB Int
      ce usr2 = R.getField @"age" usr2 - 1
  restrict $ \i -> R.getField @"age" i .> (ce i)
  testClau2
  sort $ \i -> asc $ R.getField @"age" i
  DBRecord.Query2.limit $ Just 10
  DBRecord.Query2.offset $ Just 11
  selectAll

-- testClau2 :: _
testClau2 = restrict @User2 $ \i -> R.getField @"age" i .> 10

testClau3 :: (R.HasField "isVerified" i Bool) => Clause s sc i ()
testClau3 = restrict $ \i -> R.getField @"isVerified" i

-- testQ1 :: _
testQ1 = rel @ZB @User2 $ testClau1

testBinQ1 = testQ1 `union` testQ1
-}


{-
usrQ :: As "user" (Query ZB User1)
usrQ = #user .= rel @ZB @User1

usr2Q :: As "user" (Query ZB User2)
usr2Q = #user .= rel @ZB @User2

cteQ :: Query ZB User1
cteQ = with (#user .= rel @ZB @User1) $ id

cteQ1 :: Query ZB User1
cteQ1 = with usrQ id

trecdot = R.getField @"city" $ R.getField @"address" (rel @ZB @User2) :: _
-}

