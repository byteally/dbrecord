{-# LANGUAGE OverloadedStrings          #-}
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
  ( crossJoin
  , innerJoin
  , leftJoin
  , rightJoin
  , fullJoin
  , union
  , unionAll
  , intersect
  , intersectAll
  , except
  , exceptAll
  , distinctOrder
  , distinct  
  , order
  , restrict
  , selectAll
  , select
  , selectAgg -- TODO: Get rid off
  , using
  , selectNone
  , selectExpr
  , selectList
  , aggregate
  , fromGroup -- TODO: 
  , groupBy
  , having
  , limit
  , offset
  , with
  , with2
  , DBRecord.Query2.from -- TODO: clashing with Generics. Revisit!
  , fstOf
  , sndOf
  , joins
  -- , joinsL
  -- , joinsR
  , one
  , oneOn
  , many
  , manyOn
  , some
  , someOn
  , option
  , optionOn
  , lateral
  , crossJoins
  , alias
  , subSelect
  , exists
  , insertOne
  , insertMany
  , insertFrom
  , update
  , set
  , delete

  , TableExpr
  , As
  , Grouped
  , SelectList
  , Insertable (..)
  , UpdatingRow
  -- Schema Internal Reexports
  , Query' (..)
  , Query
  , MQuery
  , JQuery
  , Joins
  , XJoins
  --
  , runQuery
  , runQueryMaybe
  , runQueryWithSession
  , runQueryWithTransaction
  , runMQuery
  , runMQueryMaybe  
  , runMQueryWithTransaction
  , runMQueryWithSession
  , runMQuery_
  , runMQueryWithTransaction_
  , runMQueryWithSession_
  , runRawQuery
  , execRawQuery
  , runSession
  , runTransaction
  , getQueryShow
  , getMQueryShow

  , TransactionM (..)
  , SessionM (..)
  --
  , module DBRecord.Internal.Order
  , module DBRecord.Internal.Expr
  , module DBRecord.Internal.Window
  , module DBRecord.Internal.Predicate
  , module Record
  ) where


import DBRecord.Internal.Order hiding (order)
import DBRecord.Internal.Expr
import DBRecord.Internal.Predicate
import DBRecord.Internal.Window
import DBRecord.Internal.Schema
import DBRecord.Internal.Table hiding (runMQuery)
import qualified DBRecord.Internal.PrimQuery as PQ
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Int
import GHC.TypeLits
import Data.Typeable
import Data.Functor.Identity
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Kind 
import Record
import Record.Setter
import GHC.OverloadedLabels
import GHC.Records as R
import Control.Monad.Catch

-- TODO: Clean up
import DBRecord.Driver
import qualified UnliftIO as U
import qualified Control.Monad.Trans.Control as U
import qualified Data.Vector as V
import Data.Vector (Vector)

type Query sc t = Query' ('ReadQ 'ManyRow) sc t
type As = Field
type TableExpr sc tab o = (forall s. Clause s sc tab (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o


-- * Joins
-- FROM T1 CROSS JOIN T2 is equivalent to FROM T1 INNER JOIN T2 ON TRUE. It is also equivalent to FROM T1, T2.


data XJoins sc qs = XJoins [TypeRep] (HRec (Query' ('ReadQ 'ManyRow) sc) qs)

instance AnonRec (XJoins sc) where
  type FieldKind (XJoins sc) = FieldKind (HRec (Query' ('ReadQ 'ManyRow) sc))
  type IsHKRec (XJoins sc) = IsHKRec (HRec (Query' ('ReadQ 'ManyRow) sc))
  type FieldNameConstraint (XJoins sc) = FieldNameConstraint (HRec (Query' ('ReadQ 'ManyRow) sc))
  type FieldConstraint (XJoins sc) = FieldConstraint (HRec (Query' ('ReadQ 'ManyRow) sc))
  endRec = XJoins [] endRec
  {-# INLINE endRec #-}
  consRec fld (XJoins fsix r) =
    let nfsix = (fldTyTRep fld) : fsix
        fldTyTRep :: forall fn a.(KnownSymbol fn) => Field fn a -> TypeRep
        fldTyTRep _ = typeRep (Proxy @fn)
    in XJoins nfsix $ consRec fld r
  {-# INLINE consRec #-}
  unconsRec (XJoins fsix r) =
    let (fld, r') = unconsRec r
        fldTyTRep :: forall fn a.(KnownSymbol fn) => Field fn a -> TypeRep
        fldTyTRep _ = typeRep (Proxy @fn)
    in (fld, XJoins (L.delete (fldTyTRep fld) fsix) r')

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

-- join $  #foo .= q1
--       .& #bar .= q2 `on`
      
-- -}
-- joinsL :: forall o qs sc.
--   XJoins sc qs
--   -> (forall s.Scoped s sc (Rec qs) -> Expr sc Bool)
--   -> (forall s.Clause s sc (Rec qs) (TableValue sc Identity o))
--   -> Query sc o
-- joinsL (XJoins treps jsHRec) _ (Clause clau) = Query'
--   ( nextStage joinedTabs 
--   , clau
--   , PQ.Joins pqJoinsL
--   )
--   where
--     pqJoinsL = case joinedPQs of
--       [] -> error "Panic: Invariant violated! `Joins` list cannot be empty"
--       (hpq : rstpqs) -> L.foldl' (\acc q -> PQ.InlineJoinL acc PQ.LeftJoin False (PQ.PrimQuery q) (PQ.ConstExpr $ PQ.Bool True)) (PQ.InlineJoinBase $ PQ.PrimQuery hpq) rstpqs
--     joinedPQs = fmap snd $ L.sortOn fst $ hrecToListWithTag
--       (\ssym q ->
--           let
--             ssymTRep = typeRepOfSomeSym ssym
--             fnix = case lookupFieldIx ssymTRep fsix of
--               Nothing -> error $ "Panic: Invariant violated! " <> (show ssymTRep) <> (show fsix)
--               Just ix -> ix
--             pq = fst $ runQuery'' (Just $ tagToPfx ssym) q
--           in (fnix, pq)
--       ) jsHRec
--     joinedTabs = JoinedTables fsix $ hrecToHKOfRec $ hoistWithKeyAndTagHRec (\tag -> snd . runQuery'' (Just $ tagToPfx tag)) jsHRec
--     fsix = fromListToFieldInvIx treps
--     tagToPfx (SomeSymbol s) = T.pack $ symbolVal s

-- joinsR :: forall o qs sc.
--   XJoins sc qs
--   -> (forall s.Scoped s sc (Rec qs) -> Expr sc Bool)
--   -> (forall s.Clause s sc (Rec qs) (TableValue sc Identity o))
--   -> Query sc o
-- joinsR (XJoins treps jsHRec) _ (Clause clau) = Query'
--   ( nextStage joinedTabs 
--   , clau
--   , PQ.Joins pqJoinsR
--   )
--   where
--     pqJoinsR = case joinedPQs of
--       [] -> error "Panic: Invariant violated! `Joins` list cannot be empty"
--       (hpq : rstpqs) -> L.foldr (\q acc -> PQ.InlineJoinR (PQ.PrimQuery q) PQ.LeftJoin False acc (PQ.ConstExpr $ PQ.Bool True)) (PQ.InlineJoinBase $ PQ.PrimQuery hpq) rstpqs
--     joinedPQs = fmap snd $ L.sortOn fst $ hrecToListWithTag
--       (\ssym q ->
--           let
--             ssymTRep = typeRepOfSomeSym ssym
--             fnix = case lookupFieldIx ssymTRep fsix of
--               Nothing -> error $ "Panic: Invariant violated! " <> (show ssymTRep) <> (show fsix)
--               Just ix -> ix
--             pq = fst $ runQuery'' (Just $ tagToPfx ssym) q
--           in (fnix, pq)
--       ) jsHRec
--     joinedTabs = JoinedTables fsix $ hrecToHKOfRec $ hoistWithKeyAndTagHRec (\tag -> snd . runQuery'' (Just $ tagToPfx tag)) jsHRec
--     fsix = fromListToFieldInvIx treps
--     tagToPfx (SomeSymbol s) = T.pack $ symbolVal s

crossJoins :: forall o qs sc.
  XJoins sc qs
  -> (forall s.Clause s sc (Rec qs) (TableValue sc Identity o))
  -> Query sc o
crossJoins (XJoins treps jsHRec) (Clause clau) = Query'
  ( nextStage joinedTabs 
  , clau
  , PQ.Joins pqJoinsR
  , ReadQType ManyR
  )
  where
    pqJoinsR = case joinedPQs of
      [] -> error "Panic: Invariant violated! `Joins` list cannot be empty"
      (hpq : rstpqs) -> L.foldr (\q acc -> PQ.InlineJoinR (PQ.PrimQuery q) PQ.CrossJoin False acc (PQ.ConstExpr $ PQ.Bool True)) (PQ.InlineJoinBase $ PQ.PrimQuery hpq) rstpqs
    joinedPQs = fmap snd $ L.sortOn fst $ hrecToListWithTag
      (\ssym q ->
          let
            ssymTRep = typeRepOfSomeSym ssym
            fnix = case lookupFieldIx ssymTRep fsix of
              Nothing -> error $ "Panic: Invariant violated! " <> (show ssymTRep) <> (show fsix)
              Just ix -> ix
            pq = fst $ runQuery'' (Just $ tagToPfx ssym) q
          in (fnix, pq)
      ) jsHRec
    joinedTabs = JoinedTables fsix $ hrecToHKOfRec $ hoistWithKeyAndTagHRec (\tag -> snd . runQuery'' (Just $ tagToPfx tag)) jsHRec
    fsix = fromListToFieldInvIx treps
    tagToPfx (SomeSymbol s) = T.pack $ symbolVal s    

-- data JoinPrec
--   = JoinPrecL
-- --  | JoinPrecR
  
-- class JoinOn (jp :: JoinPrec) qs r where
--   on' :: Proxy jp -> XJoins sc qs -> r

-- instance ( KnownSymbol fn1
--          , Typeable q1
--          , KnownSymbol fn2
--          , Typeable q2
--          , JoinOn 'JoinPrecL qs r
--          ) => JoinOn 'JoinPrecL ('(fn1, q1) ': '(fn2, q2) ': qs) ((Scoped s sc (Rec '[ '(f1, q1), '(f2, q2)]) -> Expr sc Bool) -> r) where
--   on' pjp js = \_f ->
--     let
--       (_fval1, rst') = unconsRec js
--       (_fval2, rst) = unconsRec rst'
--     in on' pjp rst


data Extend base ext = Extend
  { base :: base
  , ext  :: ext
  } deriving (Show)

{-

extend (rel @Sc @Tab) $
 #tab1 .= one rel .&
 #tab2 .= some rel .&
 #tab3 .= many rel .&
 #tab4 .= optional rel .&
 end
-}

data JQuery tab tabPrj sc o = JQuery JQueryOpt (Query sc o)

data JQueryOpt = JQueryOpt
  { joinType :: PQ.JoinType
  , isLateral :: Bool
  } deriving (Show)

unJQuery :: JQuery tab tabPrj sc o -> Query sc o
unJQuery (JQuery _ q) = q

data Joins (tab :: Type) (tabPrj :: Type) sc qs = Joins [TypeRep] (HRec (JQuery tab tabPrj sc) qs)

instance AnonRec (Joins tab tabPrj sc) where
  type FieldKind (Joins tab tabPrj sc) = FieldKind (HRec (JQuery tab tabPrj sc))
  type IsHKRec (Joins tab tabPrj sc) = IsHKRec (HRec (JQuery tab tabPrj sc))
  type FieldNameConstraint (Joins tab tabPrj sc) = FieldNameConstraint (HRec (JQuery tab tabPrj sc))
  type FieldConstraint (Joins tab tabPrj sc) = FieldConstraint (HRec (JQuery tab tabPrj sc))
  endRec = Joins [] endRec
  {-# INLINE endRec #-}
  consRec fld (Joins fsix r) =
    let nfsix = (fldTyTRep fld) : fsix
        fldTyTRep :: forall fn a.(KnownSymbol fn) => Field fn a -> TypeRep
        fldTyTRep _ = typeRep (Proxy @fn)
    in Joins nfsix $ consRec fld r
  {-# INLINE consRec #-}
  unconsRec (Joins fsix r) =
    let (fld, r') = unconsRec r
        fldTyTRep :: forall fn a.(KnownSymbol fn) => Field fn a -> TypeRep
        fldTyTRep _ = typeRep (Proxy @fn)
    in (fld, Joins (L.delete (fldTyTRep fld) fsix) r')

joins :: forall o o1 tab qs sc.
  Table sc tab
  => ((forall s.Clause s sc tab (TableValue sc Identity o1)) -> Query' ('ReadQ 'ManyRow) sc o1)
  -> (forall s.Clause s sc tab (TableValue sc Identity o1))
  -> Joins tab o1 sc qs
  -> (forall s.Clause s sc (Rec ('("self", o1) ': qs)) (TableValue sc Identity o))
  -> Query sc o
joins self selfClau js jClau = joins' @"self" self selfClau js jClau
{-# INLINE joins #-}

joins' :: forall (self :: Symbol) o o1 tab qs sc.
  Table sc tab
  => ((forall s.Clause s sc tab (TableValue sc Identity o1)) -> Query' ('ReadQ 'ManyRow) sc o1)
  -> (forall s.Clause s sc tab (TableValue sc Identity o1))
  -> Joins tab o1 sc qs
  -> (forall s.Clause s sc (Rec ('(self, o1) ': qs)) (TableValue sc Identity o))
  -> Query sc o
joins' self selfClau (Joins treps jsHRec) (Clause _clau) = Query'
  ( nextStage joinedTabs
  , undefined -- _clau
  , PQ.Joins pqJoins
  , ReadQType ManyR
  )
  where
    _selfQ = self selfClau
    joinedPQs = fmap snd $ L.sortOn fst $ hrecToListWithTag
      (\ssym q ->
          let
            ssymTRep = typeRepOfSomeSym ssym
            fnix = case lookupFieldIx ssymTRep fsix of
              Nothing -> error $ "Panic: Invariant violated! " <> (show ssymTRep) <> (show fsix)
              Just ix -> ix
            pq = fst $ runQuery'' (Just $ tagToPfx ssym) $ unJQuery q
          in (fnix, pq)
      ) jsHRec
    pqJoins = case joinedPQs of
      [] -> error "Panic: Invariant violated! `Joins` list cannot be empty"
      (hpq : rstpqs) -> L.foldl' (\acc q -> PQ.InlineJoinL acc PQ.LeftJoin False (PQ.PrimQuery q) (PQ.ConstExpr $ PQ.Bool True)) (PQ.InlineJoinBase $ PQ.PrimQuery hpq) rstpqs
    joinedTabs = JoinedTables fsix $ hrecToHKOfRec $ hoistWithKeyAndTagHRec (\tag -> snd . runQuery'' (Just $ tagToPfx tag) . unJQuery) jsHRec
    fsix = fromListToFieldInvIx treps
    tagToPfx (SomeSymbol s) = T.pack $ symbolVal s

{- ^ Performs inner join between tabL and tabR producing exactly one row of tabR for each tabL
-}
oneOn :: forall o tabR tabL tabLPrj sc.
  (Table sc tabL, Table sc tabR)
  => (forall s.Scoped s sc (tabL {- + tabR | o1 -}) -> Expr sc Bool) -- ^ join-on
  -> ((forall s.Clause s sc tabR (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o)
  -> (forall s.Clause s sc tabR (TableValue sc Identity o))
  -> JQuery tabL tabLPrj sc o
oneOn _ _ _ =
  let
    jopt = JQueryOpt { joinType = PQ.InnerJoin, isLateral = False }
  in JQuery jopt undefined

one :: forall o tabR tabL tabLPrj sc.
  (Table sc tabL, Table sc tabR)
  => ((forall s.Clause s sc tabR (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o)
  -> (forall s.Clause s sc tabR (TableValue sc Identity o))
  -> JQuery tabL tabLPrj sc o
one tvFn clau = oneOn (const true) tvFn clau

{- ^ Performs left join between tabL and tabR producing zero or more row of tabR for each tabL
-}
manyOn :: forall o tabR tabL tabLPrj sc.
  (Table sc tabL, Table sc tabR)
  => (forall s.Scoped s sc (tabL {- + tabR | o1 -}) -> Expr sc Bool) -- ^ join-on
  -> ((forall s.Clause s sc tabR (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o)
  -> (forall s.Clause s sc tabR (TableValue sc Identity o))
  -> JQuery tabL tabLPrj sc (Vector o)
manyOn _ _ _ =
  let
    jopt = JQueryOpt { joinType = PQ.LeftJoin, isLateral = False }
  in JQuery jopt undefined

many :: forall o tabR tabL tabLPrj sc.
  (Table sc tabL, Table sc tabR)
  => ((forall s.Clause s sc tabR (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o)
  -> (forall s.Clause s sc tabR (TableValue sc Identity o))
  -> JQuery tabL tabLPrj sc (Vector o)
many tvFn clau = manyOn (const true) tvFn clau

{- ^ Performs inner join between tabL and tabR producing one or more row of tabR for each tabL
-}
someOn :: forall o tabR tabL tabLPrj sc.
  (Table sc tabL, Table sc tabR)
  => (forall s.Scoped s sc (tabL {- + tabR | o1 -}) -> Expr sc Bool) -- ^ join-on
  -> ((forall s.Clause s sc tabR (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o)
  -> (forall s.Clause s sc tabR (TableValue sc Identity o))
  -> JQuery tabL tabLPrj sc (NE.NonEmpty o)
someOn _ _ _ =
  let
    jopt = JQueryOpt { joinType = PQ.InnerJoin, isLateral = False }
  in JQuery jopt undefined

some :: forall o tabR tabL tabLPrj sc.
  (Table sc tabL, Table sc tabR)
  => ((forall s.Clause s sc tabR (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o)
  -> (forall s.Clause s sc tabR (TableValue sc Identity o))
  -> JQuery tabL tabLPrj sc (NE.NonEmpty o)
some tvFn clau = someOn (const true) tvFn clau  

{- ^ Performs left join between tabL and tabR producing zero or one row of tabR for each tabL
-}
optionOn :: forall o tabR tabL tabLPrj sc.
  (Table sc tabL, Table sc tabR)
  => (forall s.Scoped s sc (tabL {- + tabR | o1 -}) -> Expr sc Bool) -- ^ join-on
  -> ((forall s.Clause s sc tabR (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o)
  -> (forall s.Clause s sc tabR (TableValue sc Identity o))
  -> JQuery tabL tabLPrj sc (Maybe o)
optionOn _ _ _ =
  let
    jopt = JQueryOpt { joinType = PQ.LeftJoin, isLateral = False }
  in JQuery jopt undefined

option :: forall o tabR tabL tabLPrj sc.
  (Table sc tabL, Table sc tabR)
  => ((forall s.Clause s sc tabR (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o)
  -> (forall s.Clause s sc tabR (TableValue sc Identity o))
  -> JQuery tabL tabLPrj sc (Maybe o)
option tvFn clau = optionOn (const true) tvFn clau

{- ^ lateral is modifier which converts normal join function into lateral join function
-}
lateral :: forall o tabR tabL tabLPrj sc.
  (Table sc tabL, Table sc tabR)
  => (((forall s.Clause s sc tabR (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o)
       -> (forall s.Clause s sc tabR (TableValue sc Identity o))
       -> JQuery tabL tabLPrj sc o) -- ^ Join Function
  -> ((forall s.Clause s sc tabR (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o) -- ^ R.H.S
  -> (forall s.Clause s sc (tabR {-+ tabLPrj-}) (TableValue sc Identity o))
  -> JQuery tabL tabLPrj sc o
lateral jFn tvFnR clauR =
  let
    JQuery jopt q = jFn tvFnR clauR
  in JQuery (jopt {isLateral = True}) q

crossJoin :: forall o n1 r1 n2 r2 sc.
  (KnownSymbol n1, KnownSymbol n2, Typeable r1, Typeable r2) =>
    As n1 (Query sc r1)
  -> As n2 (Query sc r2)
  -> (forall s.Clause s sc (Rec '[ '(n1, r1), '(n2, r2)]) (TableValue sc Identity o))
  -> Query sc o
crossJoin q1 q2 (Clause clau) =
  let
    (q1PQ, q1Res) = runAliasedQuery q1
    (q2PQ, q2Res) = runAliasedQuery q2
    joinTabVal = nextStage $ crossRel (fromLabel @n1 .= q1Res) (fromLabel @n2 .= q2Res)
  in Query' ( joinTabVal
            , clau
            , PQ.Join PQ.CrossJoin False Nothing (PQ.PrimQuery q1PQ) (PQ.PrimQuery q2PQ)
            , ReadQType ManyR
            )

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
            , ReadQType ManyR
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
    ljjoinTabVal = nextStage $ ljRel (fromLabel @n1 .= q1Res) (fromLabel @n2 .= OptTable q2Res)
    onCond = getExpr $ on $ getScopeOfTable $ crossTableVal
  in Query' ( ljjoinTabVal
            , clau
            , PQ.Join PQ.LeftJoin False (Just onCond) (PQ.PrimQuery q1PQ) (PQ.PrimQuery q2PQ)
            , ReadQType ManyR
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
    rjoinTabVal = nextStage $ rjRel (fromLabel @n1 .= OptTable q1Res) (fromLabel @n2 .= q2Res)
    onCond = getExpr $ on $ getScopeOfTable $ crossTableVal
  in Query' ( rjoinTabVal
            , clau
            , PQ.Join PQ.RightJoin False (Just onCond) (PQ.PrimQuery q1PQ) (PQ.PrimQuery q2PQ)
            , ReadQType ManyR
            )

fullJoin :: forall o n1 r1 n2 r2 sc.
  (KnownSymbol n1, KnownSymbol n2, Typeable r1, Typeable r2) =>
    As n1 (Query sc r1)
  -> As n2 (Query sc r2)
  -> (forall s.Scoped s sc (Rec '[ '(n1, r1), '(n2, r2)]) -> Expr sc Bool)
  -> (forall s.Clause s sc (Rec '[ '(n1, Maybe r1), '(n2, Maybe r2)]) (TableValue sc Identity o))
  -> Query sc o
fullJoin q1 q2 on (Clause clau) =
  let
    (q1PQ, q1Res) = runAliasedQuery q1
    (q2PQ, q2Res) = runAliasedQuery q2
    crossTableVal = nextStage $ crossRel (fromLabel @n1 .= q1Res) (fromLabel @n2 .= q2Res)
    fulljoinTabVal = nextStage $ rjRel (fromLabel @n1 .= OptTable q1Res) (fromLabel @n2 .= OptTable q2Res)
    onCond = getExpr $ on $ getScopeOfTable $ crossTableVal
  in Query' ( fulljoinTabVal
            , clau
            , PQ.Join PQ.FullJoin False (Just onCond) (PQ.PrimQuery q1PQ) (PQ.PrimQuery q2PQ)
            , ReadQType ManyR
            )


-- lateralInnerJoin :: As n1 (Query sc r1) -> As n2 (Query sc (Scoped sr2)) -> Query sc ()
-- lateralInnerJoin = undefined

-- Emulated (https://learn.microsoft.com/en-gb/archive/blogs/craigfr/introduction-to-joins)
-- semiJoin
-- antiSemiJoin

-- * Query Combination

-- In order to calculate the union, intersection, or difference of two queries, the two queries must be “union compatible”, which means that they return the same number of columns and the corresponding columns have compatible data types.
-- REF: https://www.postgresql.org/docs/13/typeconv-union-case.html
union :: Query sc r -> Query sc r -> Query sc r
union = binQ PQ.Union

unionAll :: Query sc r -> Query sc r -> Query sc r
unionAll = binQ PQ.UnionAll
  
intersect :: Query sc r -> Query sc r -> Query sc r
intersect = binQ PQ.Intersection

intersectAll :: Query sc r -> Query sc r -> Query sc r
intersectAll = binQ PQ.IntersectionAll

except :: Query sc r -> Query sc r -> Query sc r
except = binQ PQ.Except

exceptAll :: Query sc r -> Query sc r -> Query sc r
exceptAll = binQ PQ.ExceptAll

binQ :: PQ.BinType -> Query sc r -> Query sc r -> Query sc r
binQ binType q1 q2 =
  let
    (pq1, tv) = runQuery' q1
    pq2 = execQuery q2
    in Query' ( nextStage tv
              , let (Clause clau) = selectAll in clau
              , const $ PQ.Binary binType pq1 pq2 Nothing
              , ReadQType ManyR
              )

-- Distinct order applies order by as usual along with adding a DISTINCT ON on the select with the ordered columns
distinct :: forall i sc s.Clause s sc i ()
distinct = scoped $ \(clau, _) ->
  (clau {PQ.options = pure (PQ.Distinct Nothing)}, ())

-- * Ordering

-- Distinct order applies order by as usual along with adding a DISTINCT ON on the select with the ordered columns
distinctOrder :: forall i sc s.(Scoped s sc i -> Order sc) -> Clause s sc i ()
distinctOrder ordFn = scoped $ \(clau, inp) ->
  let ords = getOrder (ordFn inp)
      exps = NE.nonEmpty $ map go ords
  in (clau {PQ.orderbys = ords, PQ.options = fmap (PQ.Distinct . Just) exps}, ())

  where
    go (PQ.OrderExpr _ e) = e

-- ORDER BY can be applied to the result of a UNION, INTERSECT, or EXCEPT combination, but in this case it is only permitted to sort by output column names or numbers, not by expressions.

order :: forall i sc s.(Scoped s sc i -> Order sc) -> Clause s sc i ()
order ordFn = scoped $ \(clau, inp) -> (clau {PQ.orderbys = PQ.orderbys clau <> getOrder (ordFn inp)}, ())

-- * Where
restrict :: forall i sc s.(Scoped s sc i -> Expr sc Bool) -> Clause s sc i ()
restrict filtFn = scoped $ \(clau, inp) -> (clau {PQ.criteria = PQ.criteria clau <> [getExpr (filtFn inp)]}, ())

-- byKey :: forall tab sc s. (Table sc tab) => Clause s sc tab ()
-- byKey = undefined

-- byUniq :: forall tab sc s. (Table sc tab) => Clause s sc tab ()
-- byUniq = undefined

-- type family PkType sc tab (pks :: [Symbol]) where
--   PkType sc tab '[kn] = Expr sc (Field kn tab)

data SelectList sc os = SelectList [TypeRep] (HRec (Expr sc) os)

instance AnonRec (SelectList sc) where
  type FieldKind (SelectList sc) = FieldKind (HRec (Expr sc))
  type IsHKRec (SelectList sc) = IsHKRec (HRec (Expr sc))
  type FieldNameConstraint (SelectList sc) = FieldNameConstraint (HRec (Expr sc))
  type FieldConstraint (SelectList sc) = FieldConstraint (HRec (Expr sc))
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
  

data AggSelectList sc os = AggSelectList [TypeRep] (HRec (AggExpr sc) os)

instance AnonRec (AggSelectList sc) where
  type FieldKind (AggSelectList sc) = FieldKind (HRec (AggExpr sc))
  type IsHKRec (AggSelectList sc) = IsHKRec (HRec (AggExpr sc))
  type FieldNameConstraint (AggSelectList sc) = FieldNameConstraint (HRec (AggExpr sc))
  type FieldConstraint (AggSelectList sc) = FieldConstraint (HRec (AggExpr sc))
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

  
newtype SelectScope s sc i = SelectScope (Scoped s sc i)

instance (R.HasField fn i t, KnownSymbol fn, Typeable t) => R.HasField (fn :: Symbol) (SelectScope s sc i) (Field fn (Expr sc t)) where
  getField (SelectScope scope) = fromLabel @fn .= R.getField @fn scope

instance R.HasField fn (HRec (Expr sc) os) (Expr sc t) => R.HasField (fn :: Symbol) (SelectList sc os) (Expr sc t) where
  getField (SelectList _ r) = R.getField @fn r

{-
distinct :: forall i os sc s. Clause s sc i (SelectList sc os) -> Clause s sc i (SelectList sc os)

distinctOn :: forall i os sc s.
  ( 
  ) => (Scoped s sc i -> SelectList sc os) -> Clause s sc i (SelectList sc os) -> Clause s sc i (SelectList sc os)
distinctOn distinctFn clau =
  scoped $ \(clau, scopes) -> let selCols = selFn (SelectScope scopes)
                                  distCols = distinctFn (SelectScope scopes)
                              in (clau, selectListToTable selCols)  
-}

-- * Select List

selectAll :: forall i sc s.Clause s sc i (TableValue sc Identity i)
selectAll = scoped $ \(clau, (Scoped tabv)) -> (clau, tabv)

select :: forall i os sc s.
  (SelectScope s sc i -> SelectList sc os) -> Clause s sc i (TableValue sc Identity (Rec os))
select selFn = scoped $ \(clau, scopes) -> let selCols = selFn (SelectScope scopes)
                                           in (clau, selectListToTable selCols)

selectAgg :: forall i os sc s.
  ((forall a.Grouped s sc a -> AggExpr sc a) -> SelectScope s sc i -> AggSelectList sc os) -> Clause s sc i (TableValue sc Aggregated (Rec os))
selectAgg selFn = scoped $ \(clau, scopes) -> let selCols = selFn unGroup (SelectScope scopes)
                                              in (clau, aggSelectListToTable selCols)

using :: forall o i os sc s f.ValidateRecToType os o => Clause s sc i (TableValue sc f (Rec os)) -> Clause s sc i (TableValue sc f o)
using clau = clau >>= pure . tableRecAsType

-- TODO: Consider the alt strategy of having index representing Plain | Agg | Insert | Update | Delete  in `Clause` which will let us reuse the combinators
newtype Aggregated a = Aggregated { _unAgg :: Identity a}

{- Variants
SELECT DISTINCT select_list ...
SELECT DISTINCT ON (expression [, expression ...]) select_list ...
-}

selectNone :: forall i sc s.Clause s sc i (TableValue sc Identity ())
selectNone = scoped $ \(clau, _) -> (clau, EmptyTable)

selectExpr :: forall a n sc.(Typeable a, KnownSymbol n) => Field n (Expr sc a) -> Query sc (Rec '[ '(n, a)])
selectExpr e =
  let
    tabV = TableValue
      (fromListToFieldInvIx [typeRep (Proxy @n)])
      (hrecToHKOfRec ( fromLabel @n .= (ExprF $ toIdExpr $ val e) .& end))
  in Query' (tabV, state (\s -> (snd s, s)), PQ.Table Nothing, ReadQType ManyR)

selectList :: forall xs sc.() => (SelectList sc xs) -> Query sc (Rec xs)
selectList sels =
  let
    tabV = selectListToTable sels
  in Query' (tabV, state (\s -> (snd s, s)), PQ.Table Nothing, ReadQType ManyR)

-- * Grouping
aggregate :: forall o i sc.
  ((forall s.Clause s sc i (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o)
  -> (forall s1.Clause s1 sc i (TableValue sc Aggregated o))
  -> Query' ('ReadQ 'ManyRow) sc o
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
  in (clau {PQ.groupbys = PQ.groupbys clau <> [getExpr gpVal]}, Grouped (AggExpr gpVal))

having :: forall i sc s.(Scoped s sc i -> AggExpr sc Bool) -> Clause s sc i ()
having filtFn = scoped $ \(clau, inp) -> (clau {PQ.havings = PQ.havings clau <> [getExpr (getAggExpr (filtFn inp))]}, ())

-- * LIMIT & OFFSET
{-
SELECT select_list
    FROM table_expression
    [ ORDER BY ... ]
    [ LIMIT { number | ALL } ] [ OFFSET number ]
-}

limit :: forall i sc s.Maybe Word -> Clause s sc i ()
limit lmtMay = scoped $ \(clau, _) -> (clau {PQ.limit = getExpr . constExpr @Int64 . fromIntegral <$> lmtMay}, ())

offset :: forall i sc s.Maybe Word -> Clause s sc i ()
offset osMay = scoped $ \(clau, _) -> (clau {PQ.offset = getExpr . constExpr @Int64 . fromIntegral <$> osMay}, ())


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
from :: forall o r fn sc.(KnownSymbol fn) => Field fn (Query sc r) -> (forall s.Clause s sc r (TableValue sc Identity o)) -> Query' ('ReadQ 'ManyRow) sc o
from q (Clause clau) =
  let
    (pq, tv) = runAliasedQuery q
  in Query' (nextStage tv, clau, PQ.Table (Just (PQ.PrimQuery pq)), ReadQType ManyR)

{-
4.2.11. Scalar Subqueries
A scalar subquery is an ordinary SELECT query in parentheses that returns exactly one row with one column. (See Chapter 7 for information about writing queries.) The SELECT query is executed and the single returned value is used in the surrounding value expression. It is an error to use a query that returns more than one row or more than one column as a scalar subquery. (But if, during a particular execution, the subquery returns no rows, there is no error; the scalar result is taken to be null.) The subquery can refer to variables from the surrounding query, which will act as constants during any one evaluation of the subquery. See also Section 9.23 for other expressions involving subqueries.
SELECT name, (SELECT max(pop) FROM cities WHERE cities.state = states.name)
    FROM states;
-}
-- scalarSubQuery :: forall t par r sc ps.((forall s.Clause s sc r (TableValue sc Identity t)) -> Query sc t) -> (forall s1.Clause s1 sc r {- + par -} (Scalar sc t)) -> Clause ps sc par (Scalar sc t)


{-


-}

alias :: forall as tab sc o. (KnownSymbol as, Table sc tab, Typeable tab) => TableExpr sc tab o -> TableExpr sc (Rec '[ '(as, tab)]) o
alias teF = \c -> case teF (promapClause (aliasTableValue @as) (unaliasTableValue @as) c) of
  Query' (tv, st, pqF, qt) -> Query' (tv, st, \clau -> case pqF clau of
                                         PQ.Table (Just (PQ.TableName tid Nothing)) cla -> PQ.Table (Just (PQ.TableName tid $ Just $ T.pack $ symbolVal (Proxy @as))) cla
                                         pq -> pq
                                         , qt)

subSelect ::
  forall t r n f sc ps.
  ( Typeable t
  , KnownSymbol n
  ) =>
  ((forall s.Clause s sc r (TableValue sc f (Rec '[ '(n, t)]))) -> Query sc (Rec '[ '(n, t)])) ->
  (forall s1.Clause s1 sc r (TableValue sc f (Rec '[ '(n, t)]))) ->
  Clause ps sc r (Expr sc t)
subSelect f cls = do
  let tabExpr = Expr (PQ.TableExpr (execQuery $ f cls))
  
  pure tabExpr

-- Exists, Any, All
-- exists1 :: forall t r n f sc.
--   ( Typeable t
--   , KnownSymbol n
--   , Schema sc
--   ) =>
--   ((forall s.Clause s sc r (TableValue sc f (Rec '[ '(n, t)]))) -> Query sc (Rec '[ '(n, t)])) ->
--   (forall s1.Clause s1 sc r (TableValue sc f (Rec '[ '(n, t)]))) ->
--   (Expr sc Bool)
-- exists1 f cls =
--   let tabPExpr = PQ.TableExpr (execQuery $ f cls)
--   in Expr (PQ.PrefixExpr (PQ.OpOtherFun "EXISTS") tabPExpr)

exists :: forall o r pn par f sc ps.
  ( Schema sc
  , Typeable r
  , Typeable par
  , KnownSymbol pn
  ) =>
  ((forall s.Clause s sc r (TableValue sc f o)) -> Query sc o) ->
  (forall s1.Clause s1 sc (r, Rec '[ '(pn, par)]) (TableValue sc f o)) ->
  Clause ps sc (Rec '[ '(pn, par)]) (Expr sc Bool)
exists f cls = scoped $ \(clau, Scoped tpar) -> 
  let
    _pname = T.pack $ symbolVal (Proxy @pn)
    tabPExpr = PQ.TableExpr (execQuery $ f (promapClause (\tr -> (PairTable tr (nextStage tpar))) fstTable cls))
  in (clau, Expr (PQ.PrefixExpr (PQ.OpOtherFun "EXISTS") tabPExpr))

-- MutationQ

-- data InsertSetting

-- newtype ViaTraversable f a = ViaTraversable {getTraverseable :: f a}

insert' :: forall o tab sc.(Table sc tab) => PQ.InsertValues -> (TableValue sc Identity tab -> TableValue sc Identity o) -> MQuery sc o
insert' ivs retFn = getMutQ @o @tab @sc $ \tabId basetab ->
  let attrs = fmap peToAttr $ tableToProjections basetab -- TODO: filter Identity Prj
      peToAttr (_, PQ.BaseTableAttrExpr a) = a
      peToAttr (_, e) = error $ "Panic: Invariant violated! Expected only `BaseTableAttrExpr` " <> (show e)
  in InsertMQuery (basetab, pure $ retFn basetab, PQ.InsertQuery tabId attrs ivs Nothing)

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
  insert vs retFn = insert' (PQ.InsertValues $ fmap (ivals . fromNewRow @sc @tab) vs) retFn
  
instance Insertable Identity where
  insert :: forall o tab sc.(Table sc tab) => Identity (NewRow sc tab) -> (TableValue sc Identity tab -> TableValue sc Identity o) -> MQuery sc o
  insert (Identity v) retFn = insert' (PQ.InsertValues (ivals (fromNewRow @sc @tab v) NE.:| [])) retFn

instance Insertable (Query' ct sc) where
  insert = undefined

-- data ISubQuery sc t where
--   ISubQuery :: Query sc t -> ISubQuery sc (NewRow sc t)
  
insertOne :: forall o tab sc.(Table sc tab) => NewRow sc tab -> (TableValue sc Identity tab -> TableValue sc Identity o) -> MQuery sc o
insertOne v ret = insert @Identity (Identity v) ret

insertMany :: forall o tab f sc.(Insertable f, Traversable f, Table sc tab) => f (NewRow sc tab) -> (TableValue sc Identity tab -> TableValue sc Identity o) -> MQuery sc o
insertMany vs ret = insert @f vs ret

insertFrom :: forall o tab sc.(Table sc tab) => Query sc (NewRow sc tab) -> (TableValue sc Identity tab -> TableValue sc Identity o) -> MQuery sc o
insertFrom qs ret = insert @(Query' ('ReadQ 'ManyRow) sc) qs ret

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
            Expr (PQ.BaseTableAttrExpr a) -> a
            Expr e -> error $ "Panic: Invariant violated! Expected only `BaseTableAttrExpr` " <> (show e)
        in (colN, getExpr (upd colE)) : updr btv
    in UpdatingRow tv newUpdr
  modifyField _ (UpdatingRow _ _) = error "Panic: Invariant of UpdatingRow violated! Expected only base table"

set :: forall tab sc s.(UpdatingRow sc tab -> UpdatingRow sc tab) -> Clause s sc tab ()
set updFn = scoped $ \(clau, Scoped inp) ->
  let
    _updAssoc = runUpdatingRow $ updFn (UpdatingRow inp (const []))
  in (clau, ())

-- TODO: Make `set` a combinator instead of HOF.
update :: forall tab o sc.(Table sc tab) => (UpdatingRow sc tab -> UpdatingRow sc tab) -> (forall s.Clause s sc tab (TableValue sc Identity o)) -> MQuery sc o
update updFn (Clause clau) = getMutQ @o @tab @sc $ \tabId basetab ->
  let
    updAssoc = runUpdatingRow $ updFn (UpdatingRow basetab (const []))
    mkUpdatePQ PQ.Clauses {criteria} = PQ.UpdateQuery tabId criteria updAssoc []
  in UpdateMQuery (basetab, clau, mkUpdatePQ)

delete :: forall tab o sc.(Table sc tab) => (forall s.Clause s sc tab (TableValue sc Identity o)) -> MQuery sc o
delete (Clause clau) = getMutQ @o @tab @sc $ \tabId basetab ->
  let
    mkDeletePQ PQ.Clauses {criteria} = PQ.DeleteQuery tabId criteria []
  in DeleteMQuery (basetab, clau, mkDeletePQ)

-----


newtype TransactionM r m a =
  TransactionM { runTransactionM :: ReaderT r m a }
  deriving (Functor, Applicative, Monad, MonadReader r, U.MonadUnliftIO, MonadIO, MonadThrow, MonadFail, MonadTrans)

newtype SessionM r m a =
  SessionM { runSessionM :: ReaderT r m a }
  deriving (Functor, Applicative, Monad, MonadReader r, U.MonadUnliftIO, MonadIO, MonadThrow, MonadFail, MonadTrans)

runQuery :: forall sc driver m a.
  ( MonadIO m
  , HasQuery driver
  , FromDBRow driver a
  , MonadReader driver m
  ) => Query sc a -> m (Vector a)
runQuery q = do
  driver <- ask
  liftIO $ dbQuery driver (execQuery q)

runQueryWithSession ::
  forall sc m a env driver.
  ( MonadReader env m
  , HasSessionConfig env driver
  , Session driver
  , U.MonadUnliftIO m
  , U.MonadBaseControl IO m
  , HasQuery driver
  , FromDBRow driver a
  ) => Query sc a -> m (Vector a)
runQueryWithSession q = do
  scfg <- reader getSessionConfig
  runSession_ scfg (runQuery q) (flip const)

runQueryWithTransaction ::
  forall sc m a env driver.
  ( MonadReader env m
  , HasSessionConfig env driver
  , Session driver
  , U.MonadUnliftIO m
  , U.MonadBaseControl IO m
  , HasQuery driver
  , FromDBRow driver a
  , HasTransaction driver
  ) => Query sc a -> m (Vector a)
runQueryWithTransaction q = do
  scfg <- reader getSessionConfig
  runSession_ scfg (runQuery q) withTransaction

runMQuery :: forall sc m a driver.
  ( MonadIO m
  , HasInsertRet driver
  , HasUpdateRet driver
  , HasDeleteRet driver
  , FromDBRow driver a
  , MonadReader driver m
  ) => MQuery sc a
    -> m (Vector a)
runMQuery q = do
  driver <- ask
  let
    runInsert iq = do
      liftIO $ dbInsertRet driver iq
    runUpdate uq = do
      liftIO $ dbUpdateRet driver uq
    runDelete dq = do
      liftIO $ dbDeleteRet driver dq
  execMQuery runInsert runUpdate runDelete (pure mempty) q

runMQueryWithSession :: forall sc m a env driver.
  ( MonadReader env m
  , HasSessionConfig env driver
  , Session driver
  , U.MonadUnliftIO m
  , U.MonadBaseControl IO m
  , HasInsertRet driver
  , HasUpdateRet driver
  , HasDeleteRet driver
  , FromDBRow driver a
  ) => MQuery sc a -> m (Vector a)
runMQueryWithSession q = do
  scfg <- reader getSessionConfig
  runSession_ scfg (runMQuery q) (flip const)  

runMQueryWithTransaction :: forall sc m a env driver.
  ( MonadReader env m
  , HasSessionConfig env driver
  , Session driver
  , U.MonadUnliftIO m
  , U.MonadBaseControl IO m
  , HasInsertRet driver
  , HasUpdateRet driver
  , HasDeleteRet driver
  , FromDBRow driver a
  , HasTransaction driver
  ) => MQuery sc a -> m (Vector a)
runMQueryWithTransaction q = do
  scfg <- reader getSessionConfig
  runSession_ scfg (runMQuery q) withTransaction  

runMQuery_ :: forall sc m driver.
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

runMQueryWithSession_ :: forall sc m env driver.
  ( MonadReader env m
  , HasSessionConfig env driver
  , Session driver
  , U.MonadUnliftIO m
  , U.MonadBaseControl IO m
  , HasInsert driver
  , HasUpdate driver
  , HasDelete driver
  ) => MQuery sc () -> m Int64
runMQueryWithSession_ q = do
  scfg <- reader getSessionConfig
  runSession_ scfg (runMQuery_ q) (flip const)  

runMQueryWithTransaction_ :: forall sc m env driver.
  ( MonadReader env m
  , HasSessionConfig env driver
  , Session driver
  , U.MonadUnliftIO m
  , U.MonadBaseControl IO m
  , HasInsert driver
  , HasUpdate driver
  , HasDelete driver
  , HasTransaction driver
  ) => MQuery sc () -> m Int64
runMQueryWithTransaction_ q = do
  scfg <- reader getSessionConfig
  runSession_ scfg (runMQuery_ q) withTransaction

-- runMQueryMaybe returns 'Just' constructor only if
-- the response contains a single value.
-- for other cases, it returns Nothing.
runMQueryMaybe :: forall sc m a driver.
  ( FromDBRow driver a
  , MonadReader driver m
  , MonadIO m
  , HasInsertRet driver
  , HasUpdateRet driver
  , HasDeleteRet driver
  ) => MQuery sc a -> m (Maybe a)
runMQueryMaybe q = do
  r <- runMQuery q
  case V.uncons r of
    Just (r0, rs)
      | V.null rs -> pure $ pure r0
      | otherwise -> pure Nothing
    _ -> pure Nothing

-- runQueryMaybe returns 'Just' constructor only if
-- the response contains a single value.
-- for other cases, it returns Nothing.
runQueryMaybe :: forall sc m a driver.
  ( HasQuery driver
  , FromDBRow driver a
  , MonadReader driver m
  , MonadIO m
  ) => Query sc a -> m (Maybe a)
runQueryMaybe q = do
  r <- runQuery q
  case V.uncons r of
    Just (r0, rs)
      | V.null rs -> pure $ pure r0
      | otherwise -> pure Nothing
    _ -> pure Nothing

runRawQuery :: forall m a driver.
  ( MonadIO m
  , HasRawQuery driver
  , FromDBRow driver a
  , MonadReader driver m
  ) => T.Text -> m (Vector a)
runRawQuery q = do
  driver <- ask
  liftIO $ dbRawQuery driver q

execRawQuery :: forall m driver.
  ( MonadIO m
  , HasRawQuery driver
  , MonadReader driver m
  ) => T.Text -> m Int64
execRawQuery q = do
  driver <- ask
  liftIO $ dbRawQuery_ driver q
  
runSession :: forall m a env driver.
  ( MonadReader env m
  , HasSessionConfig env driver
  , Session driver
  , U.MonadUnliftIO m
  , U.MonadBaseControl IO m
  ) => SessionM driver m a -> m a
runSession dbQ = do
  scfg <- reader getSessionConfig
  runSession_ scfg (runSessionM dbQ) (flip const)

runTransaction :: forall m a env driver.
  ( MonadReader env m
  , HasSessionConfig env driver
  , Session driver
  , HasTransaction driver
  , U.MonadUnliftIO m
  , U.MonadBaseControl IO m
  ) => TransactionM driver m a -> m a
runTransaction dbQ = do
  scfg <- reader getSessionConfig
  runSession_ scfg (runTransactionM dbQ) withTransaction

getQueryShow :: forall sc m a env driver.
  ( MonadReader env m
  , HasSessionConfig env driver
  , ShowQuery driver
  ) => m (Query sc a -> T.Text)
getQueryShow = do
  scfg <- reader getSessionConfig
  pure $ \q -> showQuery scfg (execQuery q)

getMQueryShow :: forall sc m a env driver.
  ( MonadReader env m
  , HasSessionConfig env driver
  , ShowQuery driver
  ) => m (MQuery sc a -> T.Text)
getMQueryShow = do
  scfg <- reader getSessionConfig
  pure $ \q -> execMQuery
               (showInsertQuery scfg)
               (showUpdateQuery scfg)
               (showDeleteQuery scfg)
               "" q  
