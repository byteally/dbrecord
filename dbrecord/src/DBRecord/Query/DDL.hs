{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase           #-}
module DBRecord.Query.DDL
  ( module DBRecord.Query.DDL
  ) where

-- import DBRecord.Internal.Common
import DBRecord.Internal.Types
import DBRecord.Internal.Schema
import DBRecord.Internal.DBTypes
import DBRecord.Internal.Table
import DBRecord.Internal.UDType
-- import DBRecord.Internal.Expr
-- import qualified DBRecord.Internal.PrimQuery as PQ
import DBRecord.Internal.DDL
import Data.Text (Text)
import Data.Proxy
import Data.Kind
import qualified Data.Graph as G
import Data.Functor.Const
import Data.Typeable
-- import Data.List (mapAccumL)
-- import Data.IntMap (IntMap)
import GHC.TypeLits
import GHC.Records
import GHC.Generics
-- import DBRecord.Driver


withMigration ::
  Maybe (UpDDLGraph scc, DownDDLGraph scc)
  -> Char
withMigration _ = 'a'

migrationCheck ::
  (UpDDLGraph scc, DownDDLGraph scc)
  -> Maybe ()
migrationCheck = undefined


newtype DDLGraph = DDLGraph (G.Graph, G.Vertex -> (BaseLinePrimDDL, SomeDDLInsSetId, [SomeDDLInsSetId]), SomeDDLInsSetId -> Maybe G.Vertex)

newtype UpDDLGraph scc = UpDDLGraph DDLGraph
newtype DownDDLGraph scc = DownDDLGraph DDLGraph

upDDLHashSHA256 :: UpDDLGraph scc -> Text
upDDLHashSHA256 _ = mempty

downDDLHashSHA256 :: UpDDLGraph scc -> Text
downDDLHashSHA256 _ = mempty

withUpDDL :: UpDDLGraph scc -> ([(BaseLinePrimDDL, SomeDDLInsSetId, [SomeDDLInsSetId])] -> r) -> r
withUpDDL (UpDDLGraph (DDLGraph (g, lkpFn, _))) f = f (fmap lkpFn $ G.reverseTopSort g)

withDownDDL :: DownDDLGraph scc -> ([(BaseLinePrimDDL, SomeDDLInsSetId, [SomeDDLInsSetId])] -> r) -> r
withDownDDL (DownDDLGraph (DDLGraph (g, lkpFn, _))) f = f (fmap lkpFn $ G.reverseTopSort g)

getDDLForSchema :: forall scc.
  ( SchemaCatalog scc
  , SingI (Tables scc)
  , SingI (Types scc)
  , AllMigCxt (SchemaOf scc) (Tables scc)
  , AllMigCxt (SchemaOf scc) (Types scc)
  ) => Proxy scc -> Maybe (UpDDLGraph scc, DownDDLGraph scc) -> (UpDDLGraph scc, DownDDLGraph scc)
getDDLForSchema _ base =
  let
    dstate = fmap (\(UpDDLGraph ds, _) -> ds) base
    getUpDDL :: forall (a :: Type) .
      ( AutoMigrationDDL (SchemaOf scc) a (ToDBType (DB (SchemaDB (SchemaOf scc))) a)
      ) => Sing a -> [(BaseLinePrimDDL, SomeDDLInsSetId, [SomeDDLInsSetId])]
    getUpDDL _ = autoGetUpDLL (Proxy @'(a, SchemaOf scc, ToDBType (DB (SchemaDB (SchemaOf scc))) a)) dstate

    getDownDDL :: forall (a :: Type) .
      ( AutoMigrationDDL (SchemaOf scc) a (ToDBType (DB (SchemaDB (SchemaOf scc))) a)
      ) => Sing a -> [(BaseLinePrimDDL, SomeDDLInsSetId, [SomeDDLInsSetId])]
    getDownDDL _ = autoGetDownDLL (Proxy @'(a, SchemaOf scc, ToDBType (DB (SchemaDB (SchemaOf scc))) a)) dstate

    getAllUpDDL :: AllMigCxt (SchemaOf scc) tabs => Sing (tabs :: [Type]) -> [[(BaseLinePrimDDL, SomeDDLInsSetId, [SomeDDLInsSetId])]]
    getAllUpDDL = \case
      SNil -> []
      SCons strep stabs -> getUpDDL strep : getAllUpDDL stabs

    getAllDownDDL :: AllMigCxt (SchemaOf scc) tabs => Sing (tabs :: [Type]) -> [[(BaseLinePrimDDL, SomeDDLInsSetId, [SomeDDLInsSetId])]]
    getAllDownDDL = \case
      SNil -> []
      SCons strep stabs -> getDownDDL strep : getAllDownDDL stabs

    tabsUpDDL = concat $ getAllUpDDL ((sing :: Sing (Tables scc)))
    tabsDownDDL = concat $ getAllDownDDL ((sing :: Sing (Tables scc)))
    tysUpDDL = concat $ getAllUpDDL ((sing :: Sing (Types scc)))
    tysDownDDL = concat $ getAllDownDDL ((sing :: Sing (Types scc)))
  in ( UpDDLGraph $ DDLGraph $ G.graphFromEdges $ concat [tysUpDDL, tabsUpDDL]
     , DownDDLGraph $ DDLGraph $ G.graphFromEdges $ concat [tabsDownDDL, tysDownDDL]
     )

type family AllMigCxt (sc :: Type) (ts :: [Type]) :: Constraint where
  AllMigCxt sc '[] = ()
  AllMigCxt sc (ty ': ts) = (AutoMigrationDDL sc ty (ToDBType (DB (SchemaDB sc)) ty), AllMigCxt sc ts)

class AutoMigrationDDL (sc :: Type) (t :: Type) (dbObj :: DBObjK) where
  autoGetUpDLL :: Proxy '(t, sc, dbObj) -> Maybe DDLGraph -> [(BaseLinePrimDDL, SomeDDLInsSetId, [SomeDDLInsSetId])]
  autoGetDownDLL :: Proxy '(t, sc, dbObj) -> Maybe DDLGraph -> [(BaseLinePrimDDL, SomeDDLInsSetId, [SomeDDLInsSetId])]

instance ( Table sc t
         , KnownNat (Snd (TableId sc t))
         , SingI (PrimaryKey sc t)
         , SingI (Unique sc t)
         , SingI (ForeignKey sc t)
         , AllFkCxt (ForeignKey sc t)
         , All KnownSymbol (PrimaryKey sc t)
         , AllUniqCxt (Unique sc t)
         , AutoTableMigrationDDL sc t (Fields t)
         ) => AutoMigrationDDL sc t 'TableObj where
  autoGetUpDLL _ dbstate = concat
                   [ [ mkRootNode rootISId
                     , (CreateTable tabId Proxy, oid, [rootISId])
                     ]
                   , [(AlterTable tabId $ AddConstraint pkn $ AddPrimaryKey pks, oid, [])]
                   , addUqs
                   , addFks
                   , addCols
                   ]
    where
      tabOid = TableOid_ $ fromInteger @Int $ natVal (Proxy @(Snd (TableId sc t)))
      rootISId = mkSomeRootISId (Proxy @sc) tabOid
      oid = SomeDDLInsSetId $ CreateTableISId tabOid
      tabId = getTableId @sc @t Proxy Proxy
      pks = fmap ColName $ getPrimaryKeysText (Proxy @'(sc,t))
      pkn = ConstraintName $ _getPrimaryKeyName $ primaryKeyName @sc @t
      addUq (uqn, uqs) = (AlterTable tabId $ AddConstraint (ConstraintName uqn) $ AddUnique (fmap ColName uqs), oid, [])
      addUqs = fmap addUq $ getUniquesText (Proxy @'(sc,t))
      addFk (fkn, Left (col, rtab)) = (AlterTable tabId $ AddConstraint (ConstraintName fkn) $ AddForeignKey [ColName col] rtab [ColName col], oid, [])
      addFk (fkn, Right (cols, rtab, rcols)) = (AlterTable tabId $ AddConstraint (ConstraintName fkn) $ AddForeignKey (ColName <$> cols) rtab (ColName <$> rcols), oid, [])
      addFks = fmap addFk $ getForeignKeys (Proxy @'(sc,t))
      addCols = autoGetTableUpDLL (Proxy @'(t, sc, Fields t)) dbstate

  autoGetDownDLL _ _ = concat
                     [ [(AlterTable tabId $ DropConstraint $ DropPrimaryKey pkn, oid, [])]
                     , dropFks
                     , dropUqs
                     , [(DropTable tabId, oid, [])]
                     ]
    where
      oid = SomeDDLInsSetId $ undefined $ TableOid_ $ fromInteger $ natVal (Proxy @(Snd (TableId sc t)))
      tabId = getTableId @sc @t Proxy Proxy
      pkn = ConstraintName $ _getPrimaryKeyName $ primaryKeyName @sc @t
      dropUq (uqn, _) = (AlterTable tabId $ DropConstraint $ DropUnique (ConstraintName uqn), oid, [])
      dropUqs = fmap dropUq $ getUniquesText (Proxy @'(sc,t))
      dropFk (fkn, _) = (AlterTable tabId $ DropConstraint $ DropForeignKey (ConstraintName fkn), oid, [])
      dropFks = fmap dropFk $ getForeignKeys (Proxy @'(sc,t))

instance AutoMigrationDDL sc t ('NativeTypeObj nat) where
  autoGetUpDLL _ _ = []
  autoGetDownDLL _ _ = []

instance (UDType sc t, KnownNat (Snd (TypeId sc t))) => AutoMigrationDDL sc t ('UDTypeObj ('UDRec 'CompositeRec)) where
  autoGetUpDLL _ _ = [(CreateType tyName Proxy, oid, [])
                   ]
    where
      oid = SomeDDLInsSetId $ undefined $ TypeOid_ $ fromInteger $ natVal (Proxy @(Snd (TypeId sc t)))
      tyName = undefined -- typeName @(DB (SchemaDB sc)) @t
  autoGetDownDLL _ _ = [(DropType tyName, oid, [])
                     ]
    where
      oid = SomeDDLInsSetId $ undefined $ TypeOid_ $ fromInteger $ natVal (Proxy @(Snd (TypeId sc t)))
      tyName = undefined

instance (UDType sc t, KnownNat (Snd (TypeId sc t))) => AutoMigrationDDL sc t ('UDTypeObj ('UDRec 'FlatRec)) where
  autoGetUpDLL _ _dbstate = [ (AlterTable undefined $ AddColumn undefined, attrOid, [])
                   ]
    where
      attrOid = SomeDDLInsSetId $ undefined $ AttrOid_ undefined undefined undefined
      _oid = SomeDDLInsSetId $ undefined $ TypeOid_ $ fromInteger $ natVal (Proxy @(Snd (TypeId sc t)))
  autoGetDownDLL _ _ = [ (AlterTable undefined $ DropColumn undefined, attrOid, [])
                       ]
    where
      attrOid = SomeDDLInsSetId $ undefined $ AttrOid_ undefined undefined undefined
      _oid = SomeDDLInsSetId $ undefined $ TypeOid_ $ fromInteger $ natVal (Proxy @(Snd (TypeId sc t)))

instance (TypeError ('Text "TODO: @AutoMigrationDDL UDRec 'JsonRec")) => AutoMigrationDDL sc t ('UDTypeObj ('UDRec 'JsonRec)) where
  autoGetUpDLL = error "Panic: TODO"
  autoGetDownDLL = error "Panic: TODO"

instance
  ( DBRepr (DB (SchemaDB sc)) t
  , HasDiscriminatorEnumDDL enk
  , KnownNat (Snd (TypeId sc t))
  , Typeable t
  , Matcher (DB (SchemaDB sc)) t ~ 'EnumMatcher m
  ) => AutoMigrationDDL sc t ('UDTypeObj ('UDEnum enk)) where
  autoGetUpDLL _ _dbstate =
    let
      EnumMatchRep { ctors = ectors } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      cnames = fmap (\(cn, _) -> case lookupConName cn Nothing (conAliases @(DB (SchemaDB sc)) @t) of
                        Left cn' -> cn'
                        Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @t))) ectors
      discTyN = undefined $ discriminatorTypeName @(DB (SchemaDB sc)) @t
      _oid = SomeDDLInsSetId $ undefined $ TypeOid_ $ fromInteger $ natVal (Proxy @(Snd (TypeId sc t)))
    in getDiscriminatorEnumUpDDL (Proxy @enk) discTyN cnames
  autoGetDownDLL _ _ =
    let
      tyN = undefined $ discriminatorTypeName @(DB (SchemaDB sc)) @t
    in concat
       [ getDiscriminatorEnumDownDDL (Proxy @enk) tyN
       ]

instance ( DBRepr (DB (SchemaDB sc)) t
         , HasDiscriminatorEnumDDL enk
         , KnownNat (Snd (TypeId sc t))
         , Generic (m sc)
         , Typeable t
         , GenHasSumRepr (DB (SchemaDB sc)) t m sc (Rep (m sc))
         , Matcher (DB (SchemaDB sc)) t ~ 'SumMatcher (DB (SchemaDB sc)) pfx t m
         ) => AutoMigrationDDL sc t ('UDTypeObj ('TaggedSum enk 'FlatRec)) where
  autoGetUpDLL _ _dbstate =
    let
      SumMatchRep { ctors = sctors } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      cnames = fmap (\(cn, _, _) -> case lookupConName cn Nothing (conAliases @(DB (SchemaDB sc)) @t) of
                        Left cn' -> cn'
                        Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @t))) (sctors @sc)
      discTyN = undefined $ discriminatorTypeName @(DB (SchemaDB sc)) @t
      _oid = SomeDDLInsSetId $ undefined $ TypeOid_ $ fromInteger $ natVal (Proxy @(Snd (TypeId sc t)))
    in getDiscriminatorEnumUpDDL (Proxy @enk) discTyN cnames
  autoGetDownDLL _ _ =
    let
      tyN = undefined $ discriminatorTypeName @(DB (SchemaDB sc)) @t
    in concat
       [ getDiscriminatorEnumDownDDL (Proxy @enk) tyN
       ]

instance
  ( DBRepr (DB (SchemaDB sc)) t
  , HasDiscriminatorEnumDDL enk
  , Generic (m sc)
  , Typeable t
  , GenHasSumRepr (DB (SchemaDB sc)) t m sc (Rep (m sc))
  , Matcher (DB (SchemaDB sc)) t ~ 'SumMatcher (DB (SchemaDB sc)) pfx t m
  ) => AutoMigrationDDL sc t ('UDTypeObj ('TaggedSum enk 'CompositeRec)) where
  autoGetUpDLL _ _dbstate =
    let
      SumMatchRep { ctors = sctors } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      cnames = fmap (\(cn, _, _) -> case lookupConName cn Nothing (conAliases @(DB (SchemaDB sc)) @t) of
                        Left cn' -> cn'
                        Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @t))) (sctors @sc)
      tyN = undefined $ discriminatorTypeName @(DB (SchemaDB sc)) @t
    in concat
       [ getDiscriminatorEnumUpDDL (Proxy @enk) tyN cnames
       ]
  autoGetDownDLL _ _ =
    let
      tyN = undefined $ discriminatorTypeName @(DB (SchemaDB sc)) @t
    in concat
       [ getDiscriminatorEnumDownDDL (Proxy @enk) tyN
       ]

instance (TypeError ('Text "TODO: @AutoMigrationDDL TaggedSum 'JsonRec")) => AutoMigrationDDL sc t ('UDTypeObj ('TaggedSum enk 'JsonRec)) where
  autoGetUpDLL = error "Panic: TODO"
  autoGetDownDLL = error "Panic: TODO"

instance
  ( DBRepr (DB (SchemaDB sc)) t
  , HasDiscriminatorEnumDDL enk
  , Generic (m sc)
  , Typeable t
  , GenHasSumRepr (DB (SchemaDB sc)) t m sc (Rep (m sc))
  , Matcher (DB (SchemaDB sc)) t ~ 'SumMatcher (DB (SchemaDB sc)) pfx t m
  ) => AutoMigrationDDL sc t ('UDTypeObj ('TaggedSumMono enk cty 'FlatRec)) where
  autoGetUpDLL _ _dbstate =
    let
      SumMatchRep { ctors = sctors } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      cnames = fmap (\(cn, _, _) -> case lookupConName cn Nothing (conAliases @(DB (SchemaDB sc)) @t) of
                        Left cn' -> cn'
                        Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @t))) (sctors @sc)
      tyN = undefined $ discriminatorTypeName @(DB (SchemaDB sc)) @t
    in concat
       [ getDiscriminatorEnumUpDDL (Proxy @enk) tyN cnames
       ]
  autoGetDownDLL _ _ =
    let
      tyN = undefined $ discriminatorTypeName @(DB (SchemaDB sc)) @t
    in concat
       [ getDiscriminatorEnumDownDDL (Proxy @enk) tyN
       ]

instance
  ( DBRepr (DB (SchemaDB sc)) t
  , HasDiscriminatorEnumDDL enk
  , Generic (m sc)
  , Typeable t
  , GenHasSumRepr (DB (SchemaDB sc)) t m sc (Rep (m sc))
  , Matcher (DB (SchemaDB sc)) t ~ 'SumMatcher (DB (SchemaDB sc)) pfx t m
  ) => AutoMigrationDDL sc t ('UDTypeObj ('TaggedSumMono enk cty 'CompositeRec)) where
  autoGetUpDLL _ _dbstate =
    let
      SumMatchRep { ctors = sctors } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      cnames = fmap (\(cn, _, _) -> case lookupConName cn Nothing (conAliases @(DB (SchemaDB sc)) @t) of
                        Left cn' -> cn'
                        Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @t))) (sctors @sc)
      tyN = undefined $ discriminatorTypeName @(DB (SchemaDB sc)) @t
    in concat
       [ getDiscriminatorEnumUpDDL (Proxy @enk) tyN cnames
       ]
  autoGetDownDLL _ _ =
    let
      tyN = undefined $ discriminatorTypeName @(DB (SchemaDB sc)) @t
    in concat
       [ getDiscriminatorEnumDownDDL (Proxy @enk) tyN
       ]

instance (TypeError ('Text "TODO: @AutoMigrationDDL TaggedSumMono 'JsonRec")) => AutoMigrationDDL sc t ('UDTypeObj ('TaggedSumMono enk cty 'JsonRec)) where
  autoGetUpDLL = error "Panic: TODO"
  autoGetDownDLL = error "Panic: TODO"

instance AutoMigrationDDL sc t ('UDTypeObj ('SumOfCol 'FlatRec)) where
  autoGetUpDLL = undefined
  autoGetDownDLL = undefined

instance AutoMigrationDDL sc t ('UDTypeObj ('SumOfCol 'CompositeRec)) where
  autoGetUpDLL = undefined
  autoGetDownDLL = undefined

instance (TypeError ('Text "TODO: @AutoMigrationDDL SumOfCol 'JsonRec")) => AutoMigrationDDL sc t ('UDTypeObj ('SumOfCol 'JsonRec)) where
  autoGetUpDLL = error "Panic: TODO"
  autoGetDownDLL = error "Panic: TODO"

instance AutoMigrationDDL sc t ('UDTypeObj ('SerializedBlob ('JsonContent 'Nothing))) where
  autoGetUpDLL _ _ = []
  autoGetDownDLL _ _ = []

instance AutoMigrationDDL sc t ('UDTypeObj ('SerializedBlob ('TextContent 'Nothing))) where
  autoGetUpDLL _ _ = []
  autoGetDownDLL _ _ = []

instance AutoMigrationDDL sc t ('NullableObjOf ety edbobj) where
  autoGetUpDLL _ _ = [(AlterTable undefined $ AlterColumn (ColName undefined) DropNotNull, undefined, [])]
  autoGetDownDLL _ _ = [(AlterTable undefined $ AlterColumn (ColName undefined) SetNotNull, undefined, [])]

instance AutoMigrationDDL sc t ('ArrayObjOf ety edbobj) where
  autoGetUpDLL = undefined
  autoGetDownDLL = undefined


class HasDiscriminatorEnumDDL (enk :: UDEnumK) where
  getDiscriminatorEnumUpDDL :: Proxy enk -> DBTypeName -> [Text] -> [(BaseLinePrimDDL, SomeDDLInsSetId, [SomeDDLInsSetId])]
  getDiscriminatorEnumDownDDL :: Proxy enk -> DBTypeName -> [(BaseLinePrimDDL, SomeDDLInsSetId, [SomeDDLInsSetId])]

instance HasDiscriminatorEnumDDL 'EnumType where
  getDiscriminatorEnumUpDDL _ tyN cons =
    let
      addCon cname =
        (AlterType tyN (AddEnumVal $ EnumVal cname), undefined, [])
      addedCons = fmap addCon cons
    in (CreateEnum tyN Proxy, undefined, [])
       : addedCons
  getDiscriminatorEnumDownDDL _ tyN =
    [(DropType tyN, undefined, [])]

instance HasDiscriminatorEnumDDL 'EnumText where
  getDiscriminatorEnumUpDDL _ _ _ = []
  getDiscriminatorEnumDownDDL _ _ = []

instance HasDiscriminatorEnumDDL 'EnumNum where
  getDiscriminatorEnumUpDDL _ _ _ = []
  getDiscriminatorEnumDownDDL _ _ = []

-- instance (DBRepr dbk ty, Typeable ty, Generic (m ())) => HasDiscriminatorEnumDDL dbk ty ('SumMatcher dbk pfx ty m) 'EnumType where
--   getDiscriminatorEnumUpDDL _ SumMatchRep {ctors = sctors} =
--     let
--       addCon (cn, _, _) =
--         let
--           cname = case lookupConName cn Nothing (conAliases @dbk @ty) of
--                 Left cn' -> cn'
--                 Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @ty))
--         in (AlterType undefined (AddEnumVal $ EnumVal cname), undefined, [])
--       addedCons = fmap addCon (sctors @())
--     in (CreateEnum undefined Proxy, undefined, [])
--        : addedCons
--   getDiscriminatorEnumDownDDL _ SumMatchRep {} =
--     [(DropType undefined, undefined, [])]

class AutoTableMigrationDDL (sc :: Type) (t :: Type) (flds :: [(Symbol, Type)]) where
  autoGetTableUpDLL :: Proxy '(t, sc, flds) -> Maybe DDLGraph -> [(BaseLinePrimDDL, SomeDDLInsSetId, [SomeDDLInsSetId])]
  autoGetTableDownDLL :: Proxy '(t, sc, flds) -> Maybe DDLGraph -> [(BaseLinePrimDDL, SomeDDLInsSetId, [SomeDDLInsSetId])]

instance ( HasField cn t cty
         , Table sc t
         , KnownSymbol cn
         , DBTypeOf sc cty
         , KnownNat (Snd (TableId sc t))
         , AutoMigrationDDL sc cty (ToDBType (DB (SchemaDB sc)) cty)
         ) => AutoTableMigrationDDL sc t ( '(cn, cty) ': cols) where
  autoGetTableUpDLL _ dbstate
    =
    let
      colN = getConst $ getColumnNameText @sc @t @cn
      tabId = getTableId @sc @t Proxy Proxy
      tabOid = TableOid_ $ fromInteger @Int $ natVal (Proxy @(Snd (TableId sc t)))
      rootTabISId = mkSomeRootISId (Proxy @sc) tabOid
      attrOid = AttrOid_ tabOid 0 (TypeOid_ 1)
      addAttrISId = SomeDDLInsSetId $ AlterTableISId tabOid $ AddColumnISId attrOid
      colDDL = autoGetUpDLL @sc @cty @(ToDBType (DB (SchemaDB sc)) cty) Proxy dbstate
    in [(AlterTable tabId $ AddColumn (Column (ColName colN) (ColType $ dbTypeOf (Proxy @(sc, cty)))), addAttrISId, [rootTabISId])] ++ colDDL
  autoGetTableDownDLL _ _ = undefined


class AutoProdTypeMigrationDDL (sc :: Type) (t :: Type) (flds :: [(Symbol, Type)]) where
  autoGetProdTypeUpDLL :: Proxy '(t, sc, flds) -> Maybe DDLGraph -> [(BaseLinePrimDDL, SomeDDLInsSetId, [SomeDDLInsSetId])]
  autoGetProdTypeDownDLL :: Proxy '(t, sc, flds) -> Maybe DDLGraph -> [(BaseLinePrimDDL, SomeDDLInsSetId, [SomeDDLInsSetId])]

class AutoSumTypeMigrationDDL (sc :: Type) (t :: Type) (ctors :: [(Symbol, Maybe Type)]) where
