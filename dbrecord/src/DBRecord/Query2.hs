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
  --
  ) where


import DBRecord.Old.Schema

import DBRecord.Internal.Order
import DBRecord.Internal.Expr hiding (Alias)
import DBRecord.Internal.Predicate
import DBRecord.Internal.Common
import DBRecord.Internal.Window
import DBRecord.Internal.Schema hiding (insert, delete)
import DBRecord.Internal.PrimQuery  hiding (insertQ, updateQ, deleteQ, alias)
import DBRecord.Internal.Query (getTableId, getTableProjections)
import           DBRecord.Internal.DBTypes (GetDBTypeRep)
import qualified DBRecord.Internal.Query as Q
import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.Types
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Int
import GHC.Exts
import GHC.TypeLits
import Data.Typeable
import Data.Functor.Identity
import Control.Monad.IO.Class
import Control.Monad.Reader
import DBRecord.Internal.Lens ((^.))
import Control.Exception hiding (TypeError)
import Data.Kind 
import Record
import GHC.OverloadedLabels
import GHC.Records as R
-- import qualified GHC.Records.Compat as R
import GHC.Generics

-- TODO: Clean up
import DBRecord.Driver
import GHC.Records

type Query sc t = Query' PlainQ sc t
newtype As (fn :: Symbol) t = As t
  deriving newtype (Show, Eq, Read)



{-
(.=) :: Alias f => Label n -> f t -> f (As n t)
(.=) = alias


-- Label n -> Query' qt sc t -> Query' (AsQ n qt) sc t
class Alias (f :: Type -> Type) where
  alias :: Label n -> f t -> f (As n t)

instance Alias (Expr sc) where
  alias _ (Expr _e) = undefined

data Label (n :: Symbol) = Label

instance (fn ~ n) => IsLabel fn (Label n) where
  fromLabel = Label
-}

-- * Joins
-- FROM T1 CROSS JOIN T2 is equivalent to FROM T1 INNER JOIN T2 ON TRUE. It is also equivalent to FROM T1, T2.

data Join (n1 :: Symbol) r1 (n2 :: Symbol) r2 = Join {left :: r1, right :: r2}
  deriving (Show, Eq)

{-
crossJoin :: forall o1 o2 n1 r1 n2 r2 sc.
  As n1 (Query sc r1)
  -> As n2 (Query sc r2)
  -> (forall s.Clause s sc (Join n1 r1 n2 r2) (Join n1 o1 n2 o2))
  -> Query sc (Join n1 o1 n2 o2)
crossJoin _ _ _ = undefined

innerJoin :: forall o1 o2 n1 r1 n2 r2 sc.
  As n1 (Query sc r1)
  -> As n2 (Query sc r2)
  -> (forall s.Scoped s sc (Join n1 r1 n2 r2) -> Expr sc Bool)
  -> (forall s.Clause s sc (Join n1 r1 n2 r2) (Join n1 o1 n2 o2))
  -> Query sc (Join n1 o1 n2 o2)
innerJoin _ _ _ _ = undefined
-}

leftJoin :: forall o n1 r1 n2 r2 sc.
    As n1 (Query sc r1)
  -> As n2 (Query sc r2)
  -> (forall s.Scoped s sc (Join n1 r1 n2 r2) -> Expr sc Bool)
  -> (forall s.Clause s sc (Join n1 r1 n2 (HK Maybe r2)) o)
  -> Query sc o
leftJoin (As q1) (As q2) on (Clause clau) =
  Query' ( undefined
         , clau
         , PQ.Join PQ.LeftJoin False Nothing (PQ.PrimQuery (execQuery q1)) (PQ.PrimQuery (execQuery q2))
         )

{-
rightJoin :: () -> As n1 (Query sc r1) -> As n2 (Query sc r2) -> Query sc (Join n1 (HK Maybe r1) n2 r2)
rightJoin = undefined

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

{- Variants
query1 UNION [ALL] query2
query1 INTERSECT [ALL] query2
query1 EXCEPT [ALL] query2
-}

-- * Ordering

-- ORDER BY can be applied to the result of a UNION, INTERSECT, or EXCEPT combination, but in this case it is only permitted to sort by output column names or numbers, not by expressions.

sort :: forall i sc s.(Scoped s sc i -> Order sc) -> Clause s sc i ()
sort ordFn = scoped $ \(clau, inp) -> (clau {orderbys = orderbys clau <> getOrder (ordFn inp)}, ())

-- * Where
restrict :: forall i sc s.(Scoped s sc i -> Expr sc Bool) -> Clause s sc i ()
restrict filtFn = scoped $ \(clau, inp) -> (clau {criteria = criteria clau <> [PQ.getExpr (filtFn inp)]}, ())

selectAll :: forall i sc s.Clause s sc i i
selectAll = scoped $ \(clau, scopes) -> (clau {projections = zip [T.pack "A",T.pack "B",T.pack "C",T.pack "D",T.pack "E"] $ scopeToListWith (getExpr) scopes}, undefined)

newtype SelectList sc os = SelectList (HRec (PQ.Expr sc) os)
  deriving newtype (AnonRec)

newtype SelectScope s sc i = SelectScope (Scoped s sc i)

instance (R.HasField fn i t, KnownSymbol fn, Typeable t) => R.HasField (fn :: Symbol) (SelectScope s sc i) (Field fn (PQ.Expr sc t)) where
  getField (SelectScope scope) = fromLabel @fn .= R.getField @fn scope

instance R.HasField fn (HRec (PQ.Expr sc) os) (PQ.Expr sc t) => R.HasField (fn :: Symbol) (SelectList sc os) (PQ.Expr sc t) where
  getField (SelectList r) = R.getField @fn r
  
-- * Select List
select :: forall i os sc s.
  ( 
  ) => (SelectScope s sc i -> SelectList sc os) -> Clause s sc i (SelectList sc os)
select selFn = scoped $ \(clau, scopes) -> let selCols@(SelectList selRec) = selFn (SelectScope scopes)
  in (clau {projections = zip [T.pack "A",T.pack "B",T.pack "C",T.pack "D",T.pack "E"] $ hrecToListWith (getExpr) selRec},selCols)

{- Variants
SELECT DISTINCT select_list ...
SELECT DISTINCT ON (expression [, expression ...]) select_list ...
-}

selectDistinct :: forall i os sc s.
  ( 
  ) => (Scoped s sc i -> SelectList sc os) -> Clause s sc i (SelectList sc os)
selectDistinct = undefined

-- * Grouping
group :: () -> Query sc r -> Query' (AggQ ()) sc ()
group = undefined

having :: () -> Query' (AggQ ()) sc r -> ()
having = undefined

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

{-
Eg1.
usrQ
 & select (\usr -> usr.foo :& usr.bar :& end)
 & filter (\usr -> usr.foo .> 2)
 & aggregate (\usr -> sum usr.foo :& count usr.bar :& end)
 & group (\usr -> usr.a)
 & sort
 & page

Eg2.
usrQ = rel @ZB @User
addrQ = rel @ZB @Addr
innerjoin (#user .= usrQ) (#addr .= addrQ) (\u a -> u.uid .== a.uid)
 & filter (\ua -> ua.user.age .> 21)
 & select (\ua -> ua.user.userId :& ua.addr.zipCode :& end)
 & toHask @Foo

Eg3.
(rel @ZB EdiSet)
 & select (\set -> set.ediSetSeqNo :& #setIds .= array set.ediSetId :& end)
 & group (\set -> set.ediSetSeqNo)
 & alias #setBySeqNo
 & filter $ \aggSet -> length aggSet.setIds .> 2

Eg4.
let setBySeqNoQ =
  #setBySeqNo .= (rel @ZB EdiSet)
    & select (\set -> set.ediSetSeqNo :& #setIds .= array set.ediSetId :& end)
    & group (\set -> set.ediSetSeqNo)
in setBySeqNoQ
     & filter $ \aggSet -> length aggSet.setIds .> 2

Eg5.
rel @ZB @Addr $ do
 filter (\addr -> addr.city .== "Chennai")
 order (\addr -> asc (addr.zipCode) <> desc (1))
 r <- select (\addr -> #cities .= arr_agg (addr.city.val) :& addr.zipCode :& end)
 addrGrouping <- group (\addr -> addr.zipCode :& end)
 aggregate addrGrouping (\groupedAddr -> groupedAddr.zipCode :& groupedAddr.foo :& end)
-}


{-
data ZB

instance Database ZB where
  type DatabaseName ZB = "zb"
  type DB ZB = 'Postgres
instance Schema ZB where
  type SchemaDB ZB = ZB
  type Tables ZB = '[ User1
                    , User2
                    ]
instance Table ZB User1 where

instance Table ZB User2 where

instance UDType ZB Address where  

data User1 = User1
  { name :: String
  , age :: Int
  } deriving (Show, Generic)

data User2 = User2
  { name :: String
  , age :: Int
  , isVerified :: Bool
  , address :: Address
  } deriving (Show, Generic)

data Address = Address
  { city :: String
  , pincode :: String
  } deriving (Show, Generic)

-}
