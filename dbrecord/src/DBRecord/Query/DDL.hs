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


newtype DDLGraph = DDLGraph (G.Graph, G.Vertex -> (BaseLinePrimDDL, SomeOid, [SomeOid]), SomeOid -> Maybe G.Vertex)

getUpDDLForSchema :: forall scc.
  ( SchemaCatalog scc
  ) => Proxy scc -> DDLGraph
getUpDDLForSchema = undefined

getUpDDL :: forall a sc.
  ( DBRepr (DB (SchemaDB sc)) a
  , Schema sc
  , Database (SchemaDB sc)
  , AutoMigrationDDL sc a (ToDBType (DB (SchemaDB sc)) a)
  ) => Proxy '(a, sc) -> DDLGraph
getUpDDL _ = DDLGraph $ G.graphFromEdges $ autoGetUpDLL (Proxy @'(a, sc, ToDBType (DB (SchemaDB sc)) a))

getDownDDL :: forall a sc.
  ( DBRepr (DB (SchemaDB sc)) a
  , Schema sc
  , Database (SchemaDB sc)
  , AutoMigrationDDL sc a (ToDBType (DB (SchemaDB sc)) a)
  ) => Proxy '(a, sc) -> DDLGraph
getDownDDL _ = DDLGraph $ G.graphFromEdges $ autoGetUpDLL (Proxy @'(a, sc, ToDBType (DB (SchemaDB sc)) a))

class AutoMigrationDDL (sc :: Type) (t :: Type) (dbObj :: DBObjK) where
  autoGetUpDLL :: Proxy '(t, sc, dbObj) -> [(BaseLinePrimDDL, SomeOid, [SomeOid])]
  autoGetDownDLL :: Proxy '(t, sc, dbObj) -> [(BaseLinePrimDDL, SomeOid, [SomeOid])]

instance ( Table sc t
         , KnownNat (TableId sc t)
         , SingI (PrimaryKey sc t)
         , SingI (Unique sc t)
         , SingI (ForeignKey sc t)
         , AllFkCxt (ForeignKey sc t)
         , All KnownSymbol (PrimaryKey sc t)
         , AllUniqCxt (Unique sc t)
         ) => AutoMigrationDDL sc t 'TableObj where
  autoGetUpDLL _ = concat
                   [ [(CreateTable tabId Proxy, oid, [])]
                   , [(AlterTable tabId $ AddConstraint pkn $ AddPrimaryKey pks, oid, [])]
                   , addUqs
                   , addFks
                   ]
    where
      oid = SomeOid $ TableOid_ $ fromInteger $ natVal (Proxy @(TableId sc t))
      tabId = getTableId @sc @t Proxy Proxy
      pks = fmap ColName $ getPrimaryKeysText (Proxy @'(sc,t))
      pkn = ConstraintName $ _getPrimaryKeyName $ primaryKeyName @sc @t
      addUq (uqn, uqs) = (AlterTable tabId $ AddConstraint (ConstraintName uqn) $ AddUnique (fmap ColName uqs), oid, [])
      addUqs = fmap addUq $ getUniquesText (Proxy @'(sc,t))
      addFk (fkn, Left (col, rtab)) = (AlterTable tabId $ AddConstraint (ConstraintName fkn) $ AddForeignKey [ColName col] rtab [ColName col], oid, [])
      addFk (fkn, Right (cols, rtab, rcols)) = (AlterTable tabId $ AddConstraint (ConstraintName fkn) $ AddForeignKey (ColName <$> cols) rtab (ColName <$> rcols), oid, [])
      addFks = fmap addFk $ getForeignKeys (Proxy @'(sc,t))

  autoGetDownDLL _ = concat
                     [ [(AlterTable tabId $ DropConstraint $ DropPrimaryKey pkn, oid, [])]
                     , dropFks
                     , dropUqs
                     , [(DropTable tabId, oid, [])]
                     ]
    where
      oid = SomeOid $ TableOid_ $ fromInteger $ natVal (Proxy @(TableId sc t))
      tabId = getTableId @sc @t Proxy Proxy
      pkn = ConstraintName $ _getPrimaryKeyName $ primaryKeyName @sc @t
      dropUq (uqn, _) = (AlterTable tabId $ DropConstraint $ DropUnique (ConstraintName uqn), oid, [])
      dropUqs = fmap dropUq $ getUniquesText (Proxy @'(sc,t))
      dropFk (fkn, _) = (AlterTable tabId $ DropConstraint $ DropForeignKey (ConstraintName fkn), oid, [])
      dropFks = fmap dropFk $ getForeignKeys (Proxy @'(sc,t))

instance AutoMigrationDDL sc t ('NativeTypeObj nat) where
  autoGetUpDLL _ = []
  autoGetDownDLL _ = []

instance (UDType sc t, KnownNat (TypeId sc t)) => AutoMigrationDDL sc t ('UDTypeObj ('UDRec 'CompositeRec)) where
  autoGetUpDLL _ = [(CreateType tyName Proxy, oid, [])
                   ]
    where
      oid = SomeOid $ TypeOid_ $ fromInteger $ natVal (Proxy @(TypeId sc t))
      tyName = undefined -- typeName @(DB (SchemaDB sc)) @t
  autoGetDownDLL _ = [(DropType tyName, oid, [])
                     ]
    where
      oid = SomeOid $ TypeOid_ $ fromInteger $ natVal (Proxy @(TypeId sc t))
      tyName = undefined

instance (UDType sc t, KnownNat (TypeId sc t)) => AutoMigrationDDL sc t ('UDTypeObj ('UDRec 'FlatRec)) where
  autoGetUpDLL _ = [ (AlterTable undefined $ AddColumn undefined, attrOid, [])
                   ]
    where
      attrOid = SomeOid $ AttrOid_ undefined undefined undefined
      _oid = SomeOid $ TypeOid_ $ fromInteger $ natVal (Proxy @(TypeId sc t))
  autoGetDownDLL _ = [ (AlterTable undefined $ DropColumn undefined, attrOid, [])
                     ]
    where
      attrOid = SomeOid $ AttrOid_ undefined undefined undefined
      _oid = SomeOid $ TypeOid_ $ fromInteger $ natVal (Proxy @(TypeId sc t))

instance (TypeError ('Text "TODO: @AutoMigrationDDL UDRec 'JsonRec")) => AutoMigrationDDL sc t ('UDTypeObj ('UDRec 'JsonRec)) where
  autoGetUpDLL = error "Panic: TODO"
  autoGetDownDLL = error "Panic: TODO"

instance
  ( DBRepr (DB (SchemaDB sc)) t
  , HasDiscriminatorEnumDDL enk
  , KnownNat (TypeId sc t)
  , Typeable t
  , Matcher (DB (SchemaDB sc)) t ~ 'EnumMatcher m
  ) => AutoMigrationDDL sc t ('UDTypeObj ('UDEnum enk)) where
  autoGetUpDLL _ =
    let
      EnumMatchRep { ctors = ectors } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      cnames = fmap (\(cn, _) -> case lookupConName cn Nothing (conAliases @(DB (SchemaDB sc)) @t) of
                        Left cn' -> cn'
                        Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @t))) ectors
      discTyN = undefined $ discriminatorTypeName @(DB (SchemaDB sc)) @t
      _oid = SomeOid $ TypeOid_ $ fromInteger $ natVal (Proxy @(TypeId sc t))
    in getDiscriminatorEnumUpDDL (Proxy @enk) discTyN cnames
  autoGetDownDLL _ =
    let
      tyN = undefined $ discriminatorTypeName @(DB (SchemaDB sc)) @t
    in concat
       [ getDiscriminatorEnumDownDDL (Proxy @enk) tyN
       ]

instance ( DBRepr (DB (SchemaDB sc)) t
         , HasDiscriminatorEnumDDL enk
         , KnownNat (TypeId sc t)
         , Generic (m sc)
         , Typeable t
         , GenHasSumRepr (DB (SchemaDB sc)) t m sc (Rep (m sc))
         , Matcher (DB (SchemaDB sc)) t ~ 'SumMatcher (DB (SchemaDB sc)) pfx t m
         ) => AutoMigrationDDL sc t ('UDTypeObj ('TaggedSum enk 'FlatRec)) where
  autoGetUpDLL _ =
    let
      SumMatchRep { ctors = sctors } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      cnames = fmap (\(cn, _, _) -> case lookupConName cn Nothing (conAliases @(DB (SchemaDB sc)) @t) of
                        Left cn' -> cn'
                        Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @t))) (sctors @sc)
      discTyN = undefined $ discriminatorTypeName @(DB (SchemaDB sc)) @t
      _oid = SomeOid $ TypeOid_ $ fromInteger $ natVal (Proxy @(TypeId sc t))
    in getDiscriminatorEnumUpDDL (Proxy @enk) discTyN cnames
  autoGetDownDLL _ =
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
  autoGetUpDLL _ =
    let
      SumMatchRep { ctors = sctors } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      cnames = fmap (\(cn, _, _) -> case lookupConName cn Nothing (conAliases @(DB (SchemaDB sc)) @t) of
                        Left cn' -> cn'
                        Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @t))) (sctors @sc)
      tyN = undefined $ discriminatorTypeName @(DB (SchemaDB sc)) @t
    in concat
       [ getDiscriminatorEnumUpDDL (Proxy @enk) tyN cnames
       ]
  autoGetDownDLL _ =
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
  autoGetUpDLL _ =
    let
      SumMatchRep { ctors = sctors } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      cnames = fmap (\(cn, _, _) -> case lookupConName cn Nothing (conAliases @(DB (SchemaDB sc)) @t) of
                        Left cn' -> cn'
                        Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @t))) (sctors @sc)
      tyN = undefined $ discriminatorTypeName @(DB (SchemaDB sc)) @t
    in concat
       [ getDiscriminatorEnumUpDDL (Proxy @enk) tyN cnames
       ]
  autoGetDownDLL _ =
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
  autoGetUpDLL _ =
    let
      SumMatchRep { ctors = sctors } = sumRepr (Proxy @'((DB (SchemaDB sc)), t))
      cnames = fmap (\(cn, _, _) -> case lookupConName cn Nothing (conAliases @(DB (SchemaDB sc)) @t) of
                        Left cn' -> cn'
                        Right _ -> error $ "Panic: Expecting only Text, not Int64 as tag for: " ++ (show $ typeRep (Proxy @t))) (sctors @sc)
      tyN = undefined $ discriminatorTypeName @(DB (SchemaDB sc)) @t
    in concat
       [ getDiscriminatorEnumUpDDL (Proxy @enk) tyN cnames
       ]
  autoGetDownDLL _ =
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
  autoGetUpDLL _ = []
  autoGetDownDLL _ = []

instance AutoMigrationDDL sc t ('UDTypeObj ('SerializedBlob ('TextContent 'Nothing))) where
  autoGetUpDLL _ = []
  autoGetDownDLL _ = []

instance AutoMigrationDDL sc t ('NullableObjOf ety edbobj) where
  autoGetUpDLL _ = [(AlterTable undefined $ AlterColumn (ColName undefined) DropNotNull, undefined, [])]
  autoGetDownDLL _ = [(AlterTable undefined $ AlterColumn (ColName undefined) SetNotNull, undefined, [])]

instance AutoMigrationDDL sc t ('ArrayObjOf ety edbobj) where
  autoGetUpDLL = undefined
  autoGetDownDLL = undefined


class HasDiscriminatorEnumDDL (enk :: UDEnumK) where
  getDiscriminatorEnumUpDDL :: Proxy enk -> DBTypeName -> [Text] -> [(BaseLinePrimDDL, SomeOid, [SomeOid])]
  getDiscriminatorEnumDownDDL :: Proxy enk -> DBTypeName -> [(BaseLinePrimDDL, SomeOid, [SomeOid])]

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
  autoGetTableUpDLL :: Proxy '(t, sc, flds) -> [(BaseLinePrimDDL, SomeOid, [SomeOid])]
  autoGetTableDownDLL :: Proxy '(t, sc, flds) -> [(BaseLinePrimDDL, SomeOid, [SomeOid])]

instance ( HasField cn t cty
         , Table sc t
         , KnownSymbol cn
         , DBTypeOf sc cty
         , KnownNat (TableId sc t)
         , AutoMigrationDDL sc cty (ToDBType (DB (SchemaDB sc)) cty)
         ) => AutoTableMigrationDDL sc t ( '(cn, cty) ': cols) where
  autoGetTableUpDLL _ =
    let
      colN = getConst $ getColumnNameText @sc @t @cn
      tabId = getTableId @sc @t Proxy Proxy
      tabOid = TableOid_ $ fromInteger $ natVal (Proxy @(TableId sc t))
      attrOid = SomeOid  $ AttrOid_ tabOid 0 undefined
      colDDL = autoGetUpDLL @sc @cty @(ToDBType (DB (SchemaDB sc)) cty) Proxy
    in [(AlterTable tabId $ AddColumn (Column (ColName colN) (ColType $ dbTypeOf (Proxy @(sc, cty)))), attrOid, [])] ++ colDDL
  autoGetTableDownDLL _ = undefined


class AutoProdTypeMigrationDDL (sc :: Type) (t :: Type) (flds :: [(Symbol, Type)]) where
  autoGetProdTypeUpDLL :: Proxy '(t, sc, flds) -> [(BaseLinePrimDDL, SomeOid, [SomeOid])]
  autoGetProdTypeDownDLL :: Proxy '(t, sc, flds) -> [(BaseLinePrimDDL, SomeOid, [SomeOid])]

class AutoSumTypeMigrationDDL (sc :: Type) (t :: Type) (ctors :: [(Symbol, Maybe Type)]) where
