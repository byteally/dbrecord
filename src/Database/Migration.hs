{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}

module Database.Migration where

import Database.Schema
import Data.Type.Equality
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Identity
import Data.Proxy
import GHC.TypeLits
import Data.ByteString (ByteString)
import GHC.Exts
import Data.Functor.Const

data TypeAttr
  = SumAttr [(ColName, [Column])]
  | ProdAttr [Column]
  | EnumAttr [ColName]

toTypeAttr :: HList (Const DConAttr) xs -> TypeAttr
toTypeAttr hlist =
  let consAttrs = recordToList hlist
      isUnary (DConAttr (_cn, [])) = True
      isUnary _                    = False
  in case consAttrs of
    [DConAttr (_cn, cols)]   -> ProdAttr cols
    [] -> error "@toTypeAttr: DB Type cannot be of Void type"
    cons | all isUnary cons -> EnumAttr $ fmap (\(DConAttr cattr) -> fst cattr) cons
         | otherwise        -> SumAttr $ fmap (\(DConAttr cattr) -> cattr) cons

mkMigration :: forall db schema tables types.
  ( Database db
  , schema ~ Schema db
  , tables ~ Tables db
  , types  ~ Types db
  , All (Table db) tables
  , TyCxts db types
  , SingI tables
  , SingI types
  ) => Proxy db -> [Migration]
mkMigration pxyDB = mkMigrationTables pxyDB (sing :: Sing tables)
  ++ mkMigrationTypes pxyDB (sing :: Sing types)


mkMigrationTables :: forall db tabs.
                    ( All (Table db) tabs
                    ) => Proxy (db :: *) -> Sing (tabs :: [*]) -> [Migration]
mkMigrationTables _ SNil             = []
mkMigrationTables pxyDB (SCons tab tabs) = mkMigrationTable pxyDB tab ++ mkMigrationTables pxyDB tabs

mkMigrationTable :: forall db tab pks fks chks uqs defs.
                   ( Table db tab
                   , pks ~ PrimaryKey tab
                   , fks ~ ForeignKey tab
                   , chks ~ Check tab
                   , uqs ~ Unique tab
                   , defs ~ HasDefault tab
                   ) => Proxy (db :: *) -> Sing (tab :: *) -> [Migration]
mkMigrationTable _ _
  = let addPks = AlterTable tabN $ AddConstraint "pk_" $ AddPrimaryKey $ fmap T.pack $ fromSing (sing :: Sing pks)
        addUqs = let addUq fs = AlterTable tabN $ AddConstraint "uq_" $ AddUnique $ fmap T.pack fs
                 in fmap addUq $ fromSing (sing :: Sing uqs)
        addFks = let addFk (fcols, reft, rcols) = AlterTable tabN $ AddConstraint "fk_" $ AddForeignKey fcols reft rcols
                 in fmap addFk $ fromSing (sing :: Sing fks)
        addChks = let addChk chExpr = AlterTable tabN $ AddConstraint "ch_" $ AddCheck chExpr
                  in fmap addChk $ fromSing (sing :: Sing chks)
        addDefs = let addDef dfExpr = AlterTable tabN $ AlterColumn "col" $ AddDefault dfExpr
                  in fmap addDef $ fromSing (sing :: Sing ('DefSyms defs))
        tabColHList = singCols (Proxy @db) (Proxy :: Proxy (OriginalTableFields tab)) (Proxy @(ColumnNames tab))
        createTab = [CreateTable tabN $ recordToList tabColHList]
        tabN = T.pack $ fromSing (sing :: Sing (TableName tab))
    in addPks : concat [ createTab
                       , addUqs
                       , addFks
                       , addChks
                       , addDefs
                       ]

type family TyCxts (db :: *) (tys :: [*]) :: Constraint where
  TyCxts db (ty ': ts) = ( ShowPGType (GetPGTypeRep ty)
                         , SingAttrs db (GetTypeFields ty)
                         , TyCxts db ts
                         )
  TyCxts db '[]        = ()
mkMigrationTypes :: forall db tys.
                    ( Database db
                    , TyCxts db tys
                    ) => Proxy (db :: *) -> Sing (tys :: [*]) -> [Migration]
mkMigrationTypes _ SNil             = []
mkMigrationTypes pxyDB (SCons ty tys) = mkMigrationType pxyDB ty ++ mkMigrationTypes pxyDB tys

mkMigrationType :: forall db ty.
                  ( Database db
                  , ShowPGType (GetPGTypeRep ty)
                  , SingAttrs db (GetTypeFields ty)
                  ) => Proxy (db :: *) -> Sing (ty :: *) -> [Migration]
mkMigrationType _ _
  = let tyName = showPGType (Proxy @(GetPGTypeRep ty))
        tyAttrHList = singAttrs (Proxy @db) (Proxy :: Proxy (GetTypeFields ty))
        migTypes = case toTypeAttr tyAttrHList of
          EnumAttr cnames  -> [CreateEnum tyName cnames]
          ProdAttr cols    -> [CreateType tyName cols]
          SumAttr conAttrs ->
            let tyTag = (if T.last tyName == '"' then T.init tyName else tyName) `T.append` "_tags\""
                createSumTags cons = CreateEnum tyTag $ fmap fst cons
                sumTagTys = createSumTags conAttrs
                conTyName cn = "\"" `T.append` cn `T.append` "_con\""
                createConTy (cn, flds) = CreateType (conTyName cn) flds
                sumTy = CreateType tyName ( Column "tag" tyTag
                                          : fmap (\(cn,_) -> Column cn $ conTyName cn) conAttrs
                                          )
            in (sumTagTys : fmap createConTy conAttrs) ++ [sumTy]
    in concat [ migTypes
              ]

diffMigration :: [Migration] -> [Migration] -> [Migration] -> [Migration]
diffMigration _current _previous _reified = []

type TabName  = Text
type TypeName = Text

type ConstraintName = Text
type PrimaryKeys = [Text]

data Migration
  = CreateTable !TabName [Column]
  | CreateType !TypeName [Column]
  | CreateEnum !TypeName [Text]
  | DropTable !TabName
  | DropType !TypeName
  | AlterTable !TabName AlterTable
  | AlterType !TypeName AlterType
  deriving (Show)

data AlterTable
  = AddColumn Column
  | DropColumn !ColName
  | RenameColumn !ColName !ColName
  | AlterColumn !ColName AlterColumn
  | RenameTable !TabName
  | AddConstraint !ConstraintName AddConstraint
  | DropConstraint !ConstraintName
  deriving (Show)

data AddConstraint
  = AddPrimaryKey [ColName]
  | AddUnique [ColName]
  | AddCheck CheckExpr
  | AddForeignKey [ColName] !TabName [ColName]
  deriving (Show)

data AlterColumn
  = SetNotNull
  | DropNotNull
  | ChangeType !ColType
  | AddDefault DefExpr
  | DropDefault
  deriving (Show)

data AlterType
  = RenameType !TypeName
  | AddAttribute Column
  | DropAttribute !ColName
  | AlterAttribute !ColName AlterAttribute
  | AddAfterEnumVal !Text !Text
  deriving (Show)

data AlterAttribute
  = ChangeAttrType !ColType
  deriving (Show)

migrationSql :: Migration -> Text
migrationSql (CreateTable tab cols) = T.concat
  [ "CREATE TABLE "
  , doubleQuote tab
  , " ("
  , T.intercalate ", " $ fmap columnSql cols
  , ");"
  ]
migrationSql (CreateType ty cols) = T.concat
  [ "CREATE TYPE "
  , ty
  , " AS ("
  , T.intercalate ", " $ fmap columnSql cols
  , ");"
  ]
migrationSql (CreateEnum ty cols) = T.concat
  [ "CREATE TYPE "
  , ty
  , " AS ENUM ("
  , T.intercalate ", " (fmap singleQuote cols)
  , ");"
  ]
migrationSql (DropTable tab) = T.concat
  [ "DROP TABLE "
  , doubleQuote tab
  ,";"
  ]
migrationSql (DropType ty) = T.concat
  [ "DROP TYPE "
  , ty
  , ";"
  ]
migrationSql (AlterTable tab alter) = T.concat
  [ "ALTER TABLE "
  , doubleQuote tab
  , alterTableSql alter
  , ";"
  ]

columnSql :: Column -> Text
columnSql (Column name ty) = T.concat [doubleQuote name, " ", ty]

alterTableSql :: AlterTable -> Text
alterTableSql (AddColumn coln) = T.concat
  [ " ADD COLUMN "
  , columnSql coln
  ]

alterTableSql (DropColumn coln) = T.concat
  [ " DROP COLUMN "
  , coln
  ]
alterTableSql (RenameColumn oldn newn) = T.concat
  [ " RENAME COLUMN "
  , oldn
  , " TO "
  , newn
  ]
alterTableSql (AlterColumn coln alter) = T.concat
  [ " ALTER COLUMN "
  , coln
  , alterColumnSql alter
  ]
alterTableSql (RenameTable newn) = T.concat
  [ " RENAME TO "
  , newn
  ]
alterTableSql (AddConstraint cname con) = T.concat
  [ " ADD CONSTRAINT "
  , cname
  , addConstraintSql con
  ]
alterTableSql (DropConstraint cname) = T.concat
  [ " DROP CONSTRAINT "
  , cname
  ]

addConstraintSql :: AddConstraint -> Text
addConstraintSql (AddPrimaryKey cols) = T.concat
  [ " PRIMARY KEY "
  , "("
  , sepByComma cols
  , ")"
  ]
addConstraintSql (AddUnique cols) = T.concat
  [ " UNIQUE "
  , "("
  , sepByComma cols
  , ")"
  ]
addConstraintSql (AddCheck chkExpr) = T.concat
  [ " CHECK "
  , "("
  , chkExpr
  , ")"
  ]
addConstraintSql (AddForeignKey fcols rtab rcols) = T.concat
  [ " FOREIGN KEY "
  , "("
  , sepByComma fcols
  , ") REFERENCES "
  , doubleQuote rtab
  , " ("
  , sepByComma rcols
  , ")"
  ]

alterColumnSql :: AlterColumn -> Text
alterColumnSql SetNotNull = " SET NOT NULL"
alterColumnSql DropNotNull = " DROP NOT NULL"
alterColumnSql (ChangeType ctype) = T.concat
  [ " TYPE "
  , ctype
  ]
alterColumnSql (AddDefault defV) = T.concat
  [ " SET DEFAULT "
  , defV
  ]
alterColumnSql DropDefault = " DROP DEFAULT"

alterType :: AlterType -> Text
alterType (RenameType newTy) = T.concat
  [ " RENAME TO "
  , doubleQuote newTy
  ]
alterType (AddAttribute colN) = T.concat
  [ " ADD ATTRIBUTE "
  , columnSql colN
  ]
alterType (DropAttribute cname) = T.concat
  [ " DROP ATTRIBUTE "
  , cname
  ]
alterType (AddAfterEnumVal newVal oldVal) = T.concat
  [ " ADD VALUE "
  , newVal
  , " AFTER "
  , oldVal
  ]
alterType (AlterAttribute cname alter) = T.concat
  [ " ALTER ATTRIBUTE "
  , cname
  , alterAttr alter
  ]

alterAttr :: AlterAttribute -> Text
alterAttr (ChangeAttrType ty) = T.concat
  [ " SET DATA TYPE "
  , ty
  ]

sepByComma :: [Text] -> Text
sepByComma names = T.intercalate ", " names

singleQuote :: Text -> Text
singleQuote = quoteBy '\'' (Just '\'')

class (Applicative f) => GTypeToRec f rep xs | rep -> xs where
  gTypeToRec :: rep a -> HList f xs

instance GTypeToRec f d ts => GTypeToRec f (D1 ('MetaData tyn pkgn modn isNew) d) ts where
  gTypeToRec (M1 a) = gTypeToRec a

instance GTypeToRec f c ts => GTypeToRec f (C1 ('MetaCons cn fix 'True) c) ts where
  gTypeToRec (M1 a) = gTypeToRec a

instance ( GTypeToRec m f fs
         , GTypeToRec m g gs
         , ts ~ (fs :++ gs)
         ) => GTypeToRec m (f :*: g) ts where
  gTypeToRec (f :*: g) = gTypeToRec f `rappend` gTypeToRec g

instance (GTypeToRec m s '[t], Applicative m) => GTypeToRec m (S1 ('MetaSel ('Just fn) pack strict inf) s) '[fn ::: t] where
  gTypeToRec (M1 a) = case gTypeToRec a :: HList m '[t] of
    x :& Nil -> (Field <$> x) :& Nil

instance (Applicative m) => GTypeToRec m (K1 k f) '[f] where
  gTypeToRec (K1 a) = pure a :& Nil

rappend
  :: HList f as
  -> HList f bs
  -> HList f (as :++ bs)
rappend Nil ys = ys
rappend (x :& xs) ys = x :& (xs `rappend` ys)

typeToRec :: (Generic t, GTypeToRec f (Rep t) ts) => t -> HList f ts
typeToRec t = gTypeToRec (from t)

recToType :: (Generic t, GRecToType f (Rep t) ts, Extract f) => HList f ts -> t
recToType r = to $ gRecToType r

class GRecToType f rep ts where
  gRecToType :: Extract f => HList f ts -> rep a

instance (GRecToType f d xs) => GRecToType f (D1 ('MetaData tyn pkgn modn isNew) d) xs where
  gRecToType hrec = M1 $ gRecToType hrec

instance (GRecToType f c xs) => GRecToType f (C1 ('MetaCons cn fix 'True) c) xs where
  gRecToType hrec = M1 $ gRecToType hrec

instance (GRecToType f a xs, GRecToType f b xs) => GRecToType f (a :*: b) xs where
  gRecToType hrec = gRecToType hrec :*: gRecToType hrec

instance (RElem g fn ((fn' ::: t) ': xs) (fn == fn') f) => GRecToType g (S1 ('MetaSel ('Just fn) pack strict inf) (K1 k f)) ((fn' ::: t) ': xs) where
  gRecToType hrec = M1 $ K1 $ extract $ rGet (Proxy :: Proxy (fn == fn')) (Proxy :: Proxy fn) hrec

--instance GRecToType (K1 k f) xs where

class RElem f (fn :: Symbol) xs (mat :: Bool) r | fn xs -> r where
  rGet :: Proxy mat -> Proxy fn -> HList f xs -> f r

instance ( isMat ~ (fn == fn2)
         , RElem f fn ((fn2 ::: t2) ': xs) isMat t
         ) => RElem f fn ((fn1 ::: t1) ': (fn2 ::: t2) ': xs) 'False t where
  rGet _ fn (_ :& xs) = rGet (Proxy :: Proxy isMat) fn xs

instance TypeError ('Text "Unable to find field " ':<>: 'ShowType fn) => RElem f fn ((fn1 ::: t1) ': '[]) 'False () where
  rGet _ _ _ = error "Unreachable code"

instance Functor f => RElem f fn ((fn' ::: t) ': xs) 'True t where
  rGet _ _ (v :& _) = valOf <$> v

instance Extract Identity where
  extract (Identity i) = i

class Extract w where
  extract :: w a -> a

{- Invariants
   **********
User Def Type
============
A type can be used a both table and db-type
 Table
 -----
 Table can't be sum types
 Table should have named fields
 Type
 ---
 Type can be EnumLike Sum
 Un-named fields is not supported in initial version
  Can be sum type
 Type can't be U1 & V1

-}

getColumns :: IO ()
getColumns = do
  let _sql = T.concat ["SELECT "
                     ,"column_name "
                     ,",is_nullable "
                     ,",udt_name "
                     ,",column_default "
                     ,",numeric_precision "
                     ,",numeric_scale "
                     ,"FROM information_schema.columns "
                     ,"WHERE table_catalog=current_database() "
                     ,"AND table_schema=current_schema() "
                     ,"AND table_name=? "
                     ,"AND column_name <> ?"]
  let _sqlc = T.concat ["SELECT "
                      ,"c.constraint_name, "
                      ,"c.column_name "
                      ,"FROM information_schema.key_column_usage c, "
                      ,"information_schema.table_constraints k "
                      ,"WHERE c.table_catalog=current_database() "
                      ,"AND c.table_catalog=k.table_catalog "
                      ,"AND c.table_schema=current_schema() "
                      ,"AND c.table_schema=k.table_schema "
                      ,"AND c.table_name=? "
                      ,"AND c.table_name=k.table_name "
                      ,"AND c.column_name <> ? "
                      ,"AND c.constraint_name=k.constraint_name "
                      ,"AND NOT k.constraint_type IN ('PRIMARY KEY', 'FOREIGN KEY') "
                      ,"ORDER BY c.constraint_name, c.column_name"]
  return ()

type ConnectionString = ByteString
