{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DuplicateRecordFields      #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

module DBRecord.Postgres.Internal.Query
       ( module DBRecord.Postgres.Internal.Query
       , module DBRecord.Postgres.Internal.RegClass
       ) where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Control.Monad.Trans.Control as U
import qualified DBRecord.Internal.Sql.SqlGen as PG
import           DBRecord.Internal.Table (MQuery, execMQuery)
import qualified DBRecord.Internal.Table as DBRI
import           DBRecord.Postgres.Internal.RegClass
import qualified DBRecord.Postgres.Internal.Sql.Pretty as PG
import           Database.PostgreSQL.Simple.FromField.Composite
-- import           DBRecord.Old.Query
import           DBRecord.Types
import           DBRecord.Driver
 -- TODO: Internal Modules
import           DBRecord.Internal.Types
import           DBRecord.Internal.DBTypes
-- import           DBRecord.Internal.Expr
import           Data.Functor.Identity
import qualified Data.Pool as P
import           Data.String
import           Database.PostgreSQL.Simple as PGS
import           Database.PostgreSQL.Simple.Types (PGArray (..), Query (..), Null)
import           Database.PostgreSQL.Simple.FromField hiding (Text)
import           Database.PostgreSQL.Simple.FromRow as PGS
-- import qualified Database.PostgreSQL.Simple.Internal as PGSInt
import qualified UnliftIO as U
import           Data.Kind
import           GHC.Generics
import           GHC.TypeLits as GHC
import qualified Data.Aeson as A
import           Data.Proxy
import           Data.Int
import qualified Data.List as L
import qualified Data.ByteString.Char8 as Char8
import           Data.Typeable
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- import qualified Data.HashMap.Strict as HM
import           Text.Read
import qualified Data.Vector as V
import qualified Data.Attoparsec.ByteString.Char8 as Atto
-- import Control.Monad.Trans.State.Strict
import Record

newtype PostgresDBT (db :: Type) m a = PostgresDBT { runPostgresDB :: ReaderT PGS m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadReader PGS, U.MonadUnliftIO, MonadThrow, MonadCatch)

deriving newtype instance (U.MonadBaseControl IO m, MonadBase IO m) => U.MonadBaseControl IO (PostgresDBT db m)
deriving newtype instance (MonadBase IO m) => MonadBase IO (PostgresDBT db m)

type PostgresDB db = PostgresDBT db IO

instance DBDecoder PGS where
  type FromDBRowParser PGS = RowParser
  type FromDBRow PGS       = FromRowGen
  dbDecoder _ _ = fromRowGen

type instance ToDBRow PGS a = ToRow a

newtype AnnEntity (dbobj :: DBObjK) (isAuto :: Bool) (meta :: Type) a = AnnEntity {getEntity :: a}
  deriving Functor

class FromRowGen a where
  fromRowGen :: RowParser a

instance FromRow (AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) () a) => FromRowGen a where
  fromRowGen = getEntity <$> fromRow @(AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) () a)

instance (FromField a) => FromRow (AnnEntity ('NativeTypeObj dbk) auto meta a) where
  fromRow = AnnEntity <$> field
  {-# INLINE fromRow #-}

instance (FromField a) => FromRow (AnnEntity ('NullableObjOf a ('NativeTypeObj dbk)) auto meta (Maybe a)) where
  fromRow = AnnEntity <$> field
  {-# INLINE fromRow #-}

instance (Generic a, GFromRowOpt (Rep a)) => FromRow (AnnEntity ('NullableObjOf a 'TableObj) 'True meta (Maybe a)) where
  fromRow = (AnnEntity . fmap to) <$> gfromRowOpt @(Rep a)
  {-# INLINE fromRow #-}

instance (FromField a, Typeable a) => FromRow (AnnEntity ('NullableObjOf [a] ('ArrayObjOf arrElt ('NativeTypeObj eldbk))) 'True meta (Maybe [a])) where
  fromRow = AnnEntity <$> (fieldWith $ optionalField (\v cn -> fromPGArray <$> (fromField v cn)))
  {-# INLINE fromRow #-}

instance (Generic a, GFromRowOpt (Rep a)) => FromRow (AnnEntity ('ArrayObjOf a 'TableObj) 'True meta (V.Vector a)) where
  fromRow = (AnnEntity . maybe V.empty V.singleton . getEntity) <$> fromRow @(AnnEntity ('NullableObjOf a 'TableObj) 'True meta (Maybe a))
  {-# INLINE fromRow #-}

instance (FromField a, Typeable a) => FromRow (AnnEntity ('ArrayObjOf a ('NativeTypeObj dbk)) auto meta [a]) where
  fromRow = AnnEntity <$> (fromPGArray <$> fieldWith fromField)
  {-# INLINE fromRow #-}

instance (Generic a, GFromRow (Rep a)) => FromRow (AnnEntity 'TableObj 'True meta a) where
  fromRow = (AnnEntity . to) <$> gfromRow @(Rep a)
  {-# INLINE fromRow #-}

instance (FromRow a) => FromRow (AnnEntity 'TableObj 'False meta a) where
  fromRow = AnnEntity <$> fromRow @a
  {-# INLINE fromRow #-}

data SupportType (sup :: DBSupportK)

-- TODO: Complete the following instance impl
instance (Generic t, GFromRow (Rep t)) => FromRow (AnnEntity ('UDTypeObj ('UDRec 'FlatRec)) 'True (SupportType 'Synthesized) t) where
  fromRow = (AnnEntity . to) <$> gfromRow @(Rep t)

instance (Generic t, GFromSumOfRow (Rep t), MatchEnumTag enk, DBRepr 'Postgres t) => FromRow (AnnEntity ('UDTypeObj ('TaggedSum enk 'FlatRec)) 'True (SupportType 'Synthesized) t) where
  fromRow = do
    ctag <- field
    let mat cn = matchEnumTag (Proxy @enk) ctag (conAliases @'Postgres @t) cn
    ((fmap to) <$> gFromSumOfRow @(Rep t) mat) >>= \case
      Nothing -> error "Panic: [DBR-123]: Atleast one of the constructor should match"
      Just r -> pure $ AnnEntity r


instance ( Generic t, GFromSumOfRow (Rep t), MatchEnumTag enk, DBRepr 'Postgres t
         ) => FromRow (AnnEntity ('UDTypeObj ('TaggedSumMono enk ct 'FlatRec)) 'True (SupportType 'Synthesized) t) where
  fromRow = do
    ctag <- field
    let mat cn = matchEnumTag (Proxy @enk) ctag (conAliases @'Postgres @t) cn
    ((fmap to) <$> gFromSumOfRow @(Rep t) mat) >>= \case
      Nothing -> error "Panic: [DBR-123]: Atleast one of the constructor should match"
      Just r -> pure $ AnnEntity r

instance (Generic t, GFromSumOfRow (Rep t)) => FromRow (AnnEntity ('UDTypeObj ('SumOfCol 'FlatRec)) 'True (SupportType 'Synthesized) t) where
  fromRow = ((fmap to) <$> gFromSumOfRow @(Rep t) (const True)) >>= \case
    Nothing -> error "Panic: [DBR-123]: Atleast one of the constructor should match"
    Just r -> pure $ AnnEntity r

instance (UDFromField t udRep) => FromRow (AnnEntity ('UDTypeObj udRep) 'True (SupportType 'Native) t) where
  fromRow = AnnEntity <$> fieldWith (udFromField @t (Proxy @udRep))

instance (FromRow (AnnEntity ('UDTypeObj udRep) 'True (SupportType (GetDBSupportOf udRep)) t)) => FromRow (AnnEntity ('UDTypeObj udRep) 'True () t) where
  fromRow = fmap (AnnEntity . getEntity) $ fromRow @(AnnEntity ('UDTypeObj udRep) 'True (SupportType (GetDBSupportOf udRep)) t)

instance (UDFromField t udRep) => FromRow (AnnEntity ('NullableObjOf t ('UDTypeObj udRep)) 'True meta (Maybe t)) where
  fromRow = AnnEntity <$> fieldWith (optionalField $ udFromField @t (Proxy @udRep))

instance (UDFromField t udRep, Typeable t) => FromRow (AnnEntity ('ArrayObjOf t ('UDTypeObj udRep)) 'True meta [t]) where
  fromRow = AnnEntity <$> fieldWith (\v cn -> fromPGArray <$> pgArrayFieldParser (udFromField @t (Proxy @udRep)) v cn)

instance (FromField t) => FromRow (AnnEntity ('NullableObjOf  t ('UDTypeObj udRep)) 'False meta (Maybe t)) where
  fromRow = AnnEntity <$> fieldWith (optionalField (fromField @t))

instance (FromField t, Typeable t) => FromRow (AnnEntity ('ArrayObjOf t ('UDTypeObj udRep)) 'False meta [t]) where
  fromRow = AnnEntity <$> fieldWith (\v cn -> fromPGArray <$> pgArrayFieldParser (fromField @t) v cn)

instance (FromField t) => FromRow (AnnEntity ('UDTypeObj udRep) 'False meta t) where
  fromRow = AnnEntity <$> fieldWith (fromField @t)

class UDFromField (t :: Type) (udtMap :: UDTypeK) where
  udFromField :: Proxy udtMap -> FieldParser t

instance (Typeable t, DBRepr 'Postgres t, Matcher 'Postgres t ~ 'EnumMatcher t, ParseEnum enk) => UDFromField t ('UDEnum enk) where
  udFromField _ fld =
    let
      expTyName = _getTypeName (typeName @'Postgres @t)
      EnumMatchRep {ctors = cs} = sumRepr (Proxy @'( 'Postgres, t))
    in \case
      Nothing -> returnError UnexpectedNull fld ""
      Just val' -> do
        actTyName <- typename fld
        if actTyName == T.encodeUtf8 expTyName
          then do
          case parseEnum (Proxy @enk) val' (conAliases @'Postgres @t) cs of
            Left ex -> returnError Incompatible fld (show ex)
            Right ct -> pure ct
          else returnError Incompatible fld (L.concat ["Expected: "
                                                      , T.unpack expTyName
                                                      , ", Actual: "
                                                      , Char8.unpack actTyName
                                                      ])

class ParseEnum (enk :: UDEnumK) where
  parseEnum :: Proxy enk -> ByteString -> ConAliases 'Postgres t -> [(T.Text, t)] -> Either String t

instance ParseEnum 'EnumType where
  parseEnum _ bs caliases ctors = case T.decodeUtf8' bs of
    Left ex -> Left $ show ex
    Right ev -> case L.find (\(cn, _ct) -> ev == lookupConText cn caliases) ctors of
      Nothing -> Left $ "[DBR-123] Enum value not matched! Value: " ++ (T.unpack ev) ++ " Expecting one of: " ++ (show $ fmap (\(cn, _) -> lookupConText cn caliases) ctors)
      Just (_, ct) -> Right ct


instance ParseEnum 'EnumText where
  parseEnum _ = parseEnum (Proxy @'EnumType)

instance ParseEnum 'EnumNum where
  parseEnum _ bs caliases ctors = case parseInt8 bs of
    Left ex -> error ex
    -- TODO: Fix h.c
    Right ev -> case L.find (\(cn, _ct) -> ev == lookupConNum cn 0 caliases) ctors of
      Nothing -> Left $ "[DBR-123] Enum value not matched! Value: " ++ (show ev) ++ " Expecting one of: " ++ (show $ fmap (\(cn, _) -> lookupConText cn caliases) ctors)
      Just (_, ct) -> Right ct

class MatchEnumTag (enk :: UDEnumK) where
  matchEnumTag :: Proxy enk -> ByteString -> ConAliases 'Postgres t -> T.Text -> Bool

instance MatchEnumTag 'EnumType where
  matchEnumTag _ bs caliases cn = case T.decodeUtf8' bs of
    Left ex -> error $ show ex
    Right ev -> ev == lookupConText cn caliases

instance MatchEnumTag 'EnumText where
  matchEnumTag _ = matchEnumTag (Proxy @'EnumType)

instance MatchEnumTag 'EnumNum where
  matchEnumTag _ bs caliases cn = case parseInt8 bs of
    Left ex -> error ex
    -- TODO: Fix h.c
    Right ev -> ev == lookupConNum cn 0 caliases

parseInt8 :: ByteString -> Either String Int64
parseInt8 bs = Atto.parseOnly (Atto.signed Atto.decimal) bs
--

instance (Typeable t, DBRepr 'Postgres t, FromHK t, GConstructHK t (GFromComposite '(t, 'Nothing)) (TypeFields t)) => UDFromField t ('UDRec 'CompositeRec) where
  udFromField _ = compositeToFieldWith (gFromComp @t)

instance (TypeError ('GHC.Text "TODO: UDRec for JsonRec")) => UDFromField t ('UDRec 'JsonRec) where
  udFromField = error "TODO"

instance (A.FromJSON t, Typeable t) => UDFromField t ('SerializedBlob ('JsonContent 'Nothing)) where
  udFromField _ = fromJSONField

instance ( Typeable t
         , MatchEnumTag enk
         , DBRepr 'Postgres t
         , AllConsCxt t (Ctors t)
         , Matcher 'Postgres t ~ 'SumMatcher 'Postgres pfx t m
         , GMkCtorList t (Ctors t)
         ) => UDFromField t ('TaggedSum enk 'CompositeRec) where
  udFromField _ = compositeToFieldWith $ taggedSumCompositeParser (Proxy :: Proxy ('TaggedSum enk 'CompositeRec))

taggedSumCompositeParser ::
  forall t enk pfx m.
  ( Typeable t
  , MatchEnumTag enk
  , DBRepr 'Postgres t
  , AllConsCxt t (Ctors t)
  , Matcher 'Postgres t ~ 'SumMatcher 'Postgres pfx t m
  , GMkCtorList t (Ctors t)
  ) => Proxy ('TaggedSum enk 'CompositeRec) -> CompositeParser t
taggedSumCompositeParser _ = do
  ctag <- T.encodeUtf8 <$> compositeField @T.Text
  let
    mat cn = matchEnumTag (Proxy @enk) ctag (conAliases @'Postgres @t) cn
    SumMatchRep { sumCtors = ctors } = sumRepr (Proxy @'( 'Postgres, t))

    getNullaryVal :: forall cn cs.KnownSymbol cn => CtorList t ('(cn, 'Nothing) ': cs) -> CompositeParser (Maybe t)
    getNullaryVal (NullaryCtorCons v _) =
      if mat (T.pack $ symbolVal (Proxy @cn))
      then pure (Just v)
      else pure Nothing
    getUnaryVal :: forall cn carg cs.(KnownSymbol cn, DBRepr 'Postgres carg, Typeable carg, GFromComposite '(t, 'Just '(ToDBType 'Postgres carg, AutoCodec 'Postgres carg)) cn carg) => CtorList t ('(cn, 'Just carg) ': cs) -> CompositeParser (Maybe t)
    getUnaryVal (UnaryCtorCons f _) =
      if mat (T.pack $ symbolVal (Proxy @cn))
      then Just <$> (fmap f $ gfromComposite (Proxy @'(t, 'Just '(ToDBType 'Postgres carg, AutoCodec 'Postgres carg))) (Proxy @'(cn, carg)))
      else compositeField @Null >> pure Nothing
    matchCon :: forall cs.AllConsCxt t (cs) => CtorList t cs -> [CompositeParser (Maybe t)]
    matchCon CtorNil = []
    matchCon ncs@(NullaryCtorCons _ cs) = getNullaryVal ncs : matchCon cs
    matchCon ucs@(UnaryCtorCons _ cs) = getUnaryVal ucs : matchCon cs
  
  fmap (maybe (error $ "[DBR-123] Panic: Unexpected sum tag in db:" ++ (Char8.unpack ctag)) id) $ fmap asum $ sequenceA $ matchCon ctors

instance (TypeError ('GHC.Text "TODO: UDRec for JsonRec")) => UDFromField t ('TaggedSum enk 'JsonRec) where
  udFromField = error "TODO"

instance ( Typeable t
         , MatchEnumTag enk
         , DBRepr 'Postgres t
         , AllConsCxt t (Ctors t)
         , Matcher 'Postgres t ~ 'SumMatcher 'Postgres pfx t m
         , GMkCtorList t (Ctors t)
         ) => UDFromField t ('TaggedSumMono enk ct 'CompositeRec) where
  udFromField _ = compositeToFieldWith $ taggedSumMonoCompositeParser (Proxy :: Proxy ('TaggedSumMono enk ct 'CompositeRec))

taggedSumMonoCompositeParser ::
  forall t enk ct pfx m.
  ( Typeable t
  , MatchEnumTag enk
  , DBRepr 'Postgres t
  , AllConsCxt t (Ctors t)
  , Matcher 'Postgres t ~ 'SumMatcher 'Postgres pfx t m
  , GMkCtorList t (Ctors t)
  ) => Proxy ('TaggedSumMono enk ct 'CompositeRec) -> CompositeParser t
taggedSumMonoCompositeParser _ = do
  ctag <- T.encodeUtf8 <$> compositeField @T.Text
  let
    mat cn = matchEnumTag (Proxy @enk) ctag (conAliases @'Postgres @t) cn
    SumMatchRep { sumCtors = ctors } = sumRepr (Proxy @'( 'Postgres, t))

    getNullaryVal :: forall cn cs.KnownSymbol cn => CtorList t ('(cn, 'Nothing) ': cs) -> Maybe (CompositeParser t)
    getNullaryVal (NullaryCtorCons v _) =
      if mat (T.pack $ symbolVal (Proxy @cn))
      then Just (compositeField @Null >> pure v)
      else Nothing
    getUnaryVal :: forall cn carg cs.(KnownSymbol cn, DBRepr 'Postgres carg, Typeable carg, GFromComposite '(t, 'Just '(ToDBType 'Postgres carg, AutoCodec 'Postgres carg)) cn carg) => CtorList t ('(cn, 'Just carg) ': cs) -> Maybe (CompositeParser t)
    getUnaryVal (UnaryCtorCons f _) =
      if mat (T.pack $ symbolVal (Proxy @cn))
      then Just (fmap f $ gfromComposite (Proxy @'(t, 'Just '(ToDBType 'Postgres carg, AutoCodec 'Postgres carg))) (Proxy @'(cn, carg)))
      else Nothing
    matchCon :: forall cs.AllConsCxt t (cs) => CtorList t cs -> [Maybe (CompositeParser t)]
    matchCon CtorNil = []
    matchCon ncs@(NullaryCtorCons _ cs) = getNullaryVal ncs : matchCon cs
    matchCon ucs@(UnaryCtorCons _ cs) = getUnaryVal ucs : matchCon cs

  maybe (error $ "[DBR-123] Panic: Unexpected sum tag in db:" ++ (Char8.unpack ctag)) id $ asum $ matchCon ctors  

instance (TypeError ('GHC.Text "TODO: UDRec for JsonRec")) => UDFromField t ('TaggedSumMono enk ct 'JsonRec) where
  udFromField = error "TODO"

instance ( Typeable t
         , DBRepr 'Postgres t
         , AllSOCConsCxt t (Ctors t)
         , Matcher 'Postgres t ~ 'SumMatcher 'Postgres pfx t m
         , GMkCtorList t (Ctors t)
         ) => UDFromField t ('SumOfCol 'CompositeRec) where
  udFromField _ = compositeToFieldWith sumOfColCompositeParser

sumOfColCompositeParser ::
    forall t pfx m.
    ( Typeable t
    , DBRepr 'Postgres t
    , AllSOCConsCxt t (Ctors t)
    , Matcher 'Postgres t ~ 'SumMatcher 'Postgres pfx t m
    , GMkCtorList t (Ctors t)
    ) => CompositeParser t
sumOfColCompositeParser = do
  let
    SumMatchRep { sumCtors = ctors } = sumRepr (Proxy @'( 'Postgres, t))

    getUnaryVal :: forall cn carg cs.(KnownSymbol cn, DBRepr 'Postgres carg, Typeable carg, GFromComposite '(t, 'Just '(ToDBType 'Postgres (Maybe carg), AutoCodec 'Postgres (Maybe carg))) cn (Maybe carg)) => CtorList t ('(cn, 'Just carg) ': cs) -> CompositeParser (Maybe t)
    getUnaryVal (UnaryCtorCons f _) =
      (fmap . fmap) f $ gfromComposite (Proxy @'(t, 'Just '(ToDBType 'Postgres (Maybe carg), AutoCodec 'Postgres (Maybe carg)))) (Proxy @'(cn, Maybe carg))
    matchCon :: forall cs.AllSOCConsCxt t (cs) => CtorList t cs -> [CompositeParser (Maybe t)]
    matchCon CtorNil = []
    matchCon (NullaryCtorCons _ _) = error "[DBR-123] Panic: using Sum-Of-Col repr has nullary constructor"
    matchCon ucs@(UnaryCtorCons _ cs) = getUnaryVal ucs : matchCon cs

  fmap (maybe (error $ "[DBR-123] Panic: Atleast one of the constructor should match") id) $ fmap asum $ sequenceA $ matchCon ctors  

type family AllConsCxt (t :: Type) (cons :: [(Symbol, Maybe Type)]) :: Constraint where
  AllConsCxt t '[] = ()
  AllConsCxt t ('(cn, 'Nothing) ': cons) = (KnownSymbol cn, AllConsCxt t cons)
  AllConsCxt t ('(cn, 'Just carg) ': cons) = (KnownSymbol cn, DBRepr 'Postgres carg, Typeable carg, GFromComposite '(t, 'Just '(ToDBType 'Postgres carg, AutoCodec 'Postgres carg)) cn carg, AllConsCxt t cons)

type family AllSOCConsCxt (t :: Type) (cons :: [(Symbol, Maybe Type)]) :: Constraint where
  AllSOCConsCxt t '[] = ()
  AllSOCConsCxt t ('(cn, 'Nothing) ': cons) = TypeError ('Text "[DBR-123] Type: " ':<>: 'ShowType t ':<>: 'Text "using Sum-Of-Col repr has nullary constructor: " ':<>: 'ShowType cn)
  AllSOCConsCxt t ('(cn, 'Just carg) ': cons) = (KnownSymbol cn, DBRepr 'Postgres carg, Typeable carg, GFromComposite '(t, 'Just '(ToDBType 'Postgres (Maybe carg), AutoCodec 'Postgres (Maybe carg))) cn (Maybe carg), AllSOCConsCxt t cons)

-- Type class for default implementation of FromRow using generics
-- TODO: Uses Fields of DBRepr and HasField
class GFromRow f where
    gfromRow :: RowParser (f p)

instance GFromRow f => GFromRow (M1 c i f) where
    gfromRow = M1 <$> gfromRow

instance (GFromRow f, GFromRow g) => GFromRow (f :*: g) where
    gfromRow = liftA2 (:*:) gfromRow gfromRow

instance (FromRow (AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) () a)) => GFromRow (K1 R a) where
    gfromRow = (K1 . getEntity) <$> fromRow @(AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) () a)

instance GFromRow U1 where
    gfromRow = pure U1

class GFromRowOpt f where
  gfromRowOpt :: RowParser (Maybe (f p))

instance GFromRowOpt f => GFromRowOpt (M1 c i f) where
  gfromRowOpt = (fmap M1) <$> gfromRowOpt

instance (GFromRowOpt f, GFromRowOpt g) => GFromRowOpt (f :*: g) where
  gfromRowOpt = liftA2 (\l r -> liftA2 (:*:) l r) gfromRowOpt gfromRowOpt

instance (FromRow (AnnEntity ('NullableObjOf a (ToDBType 'Postgres a)) (AutoCodec 'Postgres a) () (Maybe a))) => GFromRowOpt (K1 k a) where
  gfromRowOpt = (fmap K1 . getEntity) <$> fromRow @(AnnEntity ('NullableObjOf a (ToDBType 'Postgres a)) (AutoCodec 'Postgres a) () (Maybe a))

instance GFromRowOpt U1 where
  gfromRowOpt = pure $ Just U1

class GFromSumOfRow f where
  gFromSumOfRow :: (T.Text -> Bool) -> RowParser (Maybe (f p))

instance GFromSumOfRow f => GFromSumOfRow (D1 d f) where
  gFromSumOfRow mat = (fmap M1) <$> gFromSumOfRow mat

instance (GFromSumOfRow f, GFromSumOfRow g) => GFromSumOfRow (f :+: g) where
  gFromSumOfRow mat = liftA2 (\l r -> asum [L1 <$> l, R1 <$> r]) (gFromSumOfRow @f mat) (gFromSumOfRow mat)

-- data Nullified a(t :: Type) = Nullified

-- fromNullified :: Nullified t -> Maybe t
-- fromNullified _ = Nothing

instance ( KnownSymbol cn
         , FromRow (AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) () a)
         , FromRow (AnnEntity ('NullableObjOf a (ToDBType 'Postgres a)) (AutoCodec 'Postgres a) () (Maybe a))
         ) => GFromSumOfRow (C1 ('MetaCons cn p isr) (S1 s (K1 k a))) where
  gFromSumOfRow mat =
    let cn = T.pack $ symbolVal (Proxy @cn)
    in if mat cn
       then ((Just . M1 . M1 . K1 . getEntity)) <$> fromRow @(AnnEntity (ToDBType 'Postgres a) (AutoCodec 'Postgres a) () a)
       else (fmap (M1 . M1 . K1) . getEntity) <$> fromRow @(AnnEntity ('NullableObjOf a (ToDBType 'Postgres a)) (AutoCodec 'Postgres a) () (Maybe a))
    -- TODO: Skipped value should only be NULL otherwise, follwing error should be thrown
    -- TODO: "Panic: [DBR-123]: Constructor " ++ (T.unpack cn) ++ " when not matched, it's argument should be null"


instance (KnownSymbol cn) => GFromSumOfRow (C1 ('MetaCons cn p isr) U1) where
  gFromSumOfRow mat =
    let cn = T.pack $ symbolVal (Proxy @cn)
    in if mat cn
       then pure $ Just $ M1 U1
       else pure Nothing

gFromComp :: forall t.
  ( DBRepr 'Postgres t
  , FromHK t
  , GConstructHK t (GFromComposite '(t, 'Nothing)) (TypeFields t)
  ) => CompositeParser t
gFromComp = fromHK $ constructHK @(GFromComposite '(t, 'Nothing)) @CompositeParser @t (gfromComposite (Proxy @'(t, 'Nothing)))

class GFromComposite (t :: (Type, Maybe (DBObjK, Bool))) (fn :: Symbol) (fty :: Type) where
  gfromComposite :: Proxy t -> Proxy '(fn, fty) -> CompositeParser fty

instance (GFromComposite '(t, 'Just '(ToDBType 'Postgres fty, AutoCodec 'Postgres fty)) fn fty) => GFromComposite '(t, 'Nothing) fn fty where
  gfromComposite _ = gfromComposite (Proxy @'(t, 'Just '(ToDBType 'Postgres fty, AutoCodec 'Postgres fty)))

instance (FromCompositeField fty) => GFromComposite '(t, 'Just '( 'NativeTypeObj nat, 'True)) fn fty where
  gfromComposite _ _ = compositeField @fty

instance (DBRepr 'Postgres fty, Typeable fty, Generic fty, FromHK fty, GConstructHK fty (GFromComposite '(fty, 'Nothing)) (TypeFields fty)) => GFromComposite '(t, 'Just '( 'UDTypeObj ('UDRec 'CompositeRec), 'True)) fn fty where
  gfromComposite _ _ = compositeFieldWith $ compositeToCompositeFieldWith $ gFromComp @fty

instance
  ( DBRepr 'Postgres fty
  , Typeable fty
  , MatchEnumTag enk
  , DBRepr 'Postgres fty
  , AllConsCxt fty (Ctors fty)
  , Matcher 'Postgres fty ~ 'SumMatcher 'Postgres pfx fty m
  , GMkCtorList fty (Ctors fty)
  ) => GFromComposite '(t, 'Just '( 'UDTypeObj ('TaggedSum enk 'CompositeRec), 'True)) fn fty where
  gfromComposite _ _ = compositeFieldWith $ compositeToCompositeFieldWith $ taggedSumCompositeParser (Proxy :: Proxy ('TaggedSum enk 'CompositeRec))

instance
  ( DBRepr 'Postgres fty
  , Typeable fty
  , MatchEnumTag enk
  , DBRepr 'Postgres fty
  , AllConsCxt fty (Ctors fty)
  , Matcher 'Postgres fty ~ 'SumMatcher 'Postgres pfx fty m
  , GMkCtorList fty (Ctors fty)
  ) => GFromComposite '(t, 'Just '( 'UDTypeObj ('TaggedSumMono enk ct 'CompositeRec), 'True)) fn fty where
  gfromComposite _ _ = compositeFieldWith $ compositeToCompositeFieldWith $ taggedSumMonoCompositeParser (Proxy :: Proxy ('TaggedSumMono enk ct 'CompositeRec))

instance (DBRepr 'Postgres fty
         , Typeable fty
         , DBRepr 'Postgres fty
         , AllSOCConsCxt fty (Ctors fty)
         , Matcher 'Postgres fty ~ 'SumMatcher 'Postgres pfx fty m
         , GMkCtorList fty (Ctors fty)         
         ) => GFromComposite '(t, 'Just '( 'UDTypeObj ('SumOfCol 'CompositeRec), 'True)) fn fty where
  gfromComposite _ _ = compositeFieldWith $ compositeToCompositeFieldWith $ sumOfColCompositeParser


instance (Typeable fty, A.FromJSON fty) => GFromComposite '(t, 'Just '( 'UDTypeObj ('SerializedBlob ('JsonContent 'Nothing)), 'True)) fn fty where
  gfromComposite _ _ = compositeFieldWith $ \f -> (nonNullCompositeField @fty $ \bs -> case A.eitherDecodeStrict bs of
                                              Left e -> returnCompositeError ConversionFailed f e
                                              Right r -> pure r) f

instance (Typeable fty, Read fty) => GFromComposite '(t, 'Just '( 'UDTypeObj ('SerializedBlob ('TextContent 'Nothing)), 'True)) fn fty where
  gfromComposite _ _ = compositeFieldWith $ \f bs -> do
    txt <- fromCompositeField @T.Text f bs
    case readMaybe $ T.unpack txt of
      Nothing -> returnCompositeError ConversionFailed f ("Unable to parse: " ++ T.unpack txt)
      Just v -> pure v

instance (TypeError ('Text "TODO: GFromComposite - UDRec JSONRec")) => GFromComposite '(t, 'Just '( 'UDTypeObj ('UDRec 'JsonRec), 'True)) fn fty where
  gfromComposite _ _ = error "[DBR-123]: Unreachable code"

instance (TypeError ('Text "[DBR-123] Panic: Nested flat composite not allowed")) => GFromComposite '(t, 'Just '( 'UDTypeObj ('UDRec 'FlatRec), 'True)) fn fty where
  gfromComposite _ _ = error "[DBR-123]: Unreachable code"

instance (TypeError ('Text "[DBR-123] Panic: Nested flat composite not allowed")) => GFromComposite '(t, 'Just '( 'NullableObjOf ety ('UDTypeObj ('UDRec 'FlatRec)), 'True)) fn fty where
  gfromComposite _ _ = error "[DBR-123]: Unreachable code"

instance (TypeError ('Text "[DBR-123] Panic: Nested flat composite not allowed")) => GFromComposite '(t, 'Just '( 'ArrayObjOf ety ('UDTypeObj ('UDRec 'FlatRec)), 'True)) fn fty where
  gfromComposite _ _ = error "[DBR-123]: Unreachable code"

instance (FromCompositeField ety) => GFromComposite '(t, 'Just '( 'NullableObjOf ety ('NativeTypeObj enat), 'True)) fn (Maybe ety) where
  gfromComposite _ _ = compositeFieldWith $ optionalCompositeFieldParser $ fromCompositeField @ety

instance (FromCompositeField ety, Typeable ety) => GFromComposite '(t, 'Just '( 'ArrayObjOf ety ('NativeTypeObj enat), 'True)) fn [ety] where
  gfromComposite _ _ = fmap V.toList $ compositeFieldWith $ arrayCompositeFieldParser $ fromCompositeField @ety

instance (FromCompositeField ety) => GFromComposite '(t, 'Just '( 'NullableObjOf ety ('UDTypeObj ('UDEnum enk)), 'True)) fn (Maybe ety) where
  gfromComposite _ _ = compositeFieldWith $ optionalCompositeFieldParser $ fromCompositeField @ety

instance (DBRepr 'Postgres ety, Typeable ety, Generic ety, FromHK ety, GConstructHK ety (GFromComposite '(ety, 'Nothing)) (TypeFields ety)) => GFromComposite '(t, 'Just '( 'NullableObjOf ety ('UDTypeObj ('UDRec 'CompositeRec)), 'True)) fn (Maybe ety) where
  gfromComposite _ _ = compositeFieldWith $ optionalCompositeFieldParser $ compositeToCompositeFieldWith $ gFromComp @ety

data PGS where
  PGS :: PGS.Connection -> PGS

instance Session PGS where
  data SessionConfig PGS where
    PGSConfig :: P.Pool PGS.Connection -> SessionConfig PGS
  runSession_ (PGSConfig pool) dbact f = do
    withResource pool (\conn -> f (PGS conn) (runReaderT dbact (PGS conn)))

instance HasTransaction PGS where
  withTransaction (PGS conn) dbact =
    U.withRunInIO (\f -> PGS.withTransaction conn (f dbact))

encodeQuery :: T.Text -> Query
encodeQuery =
  Query . T.encodeUtf8

instance HasUpdateRet PGS where
  dbUpdateRetWith parser (PGS conn) updateQ = do
    let updateSQL = PG.renderUpdate $ PG.updateSql $ updateQ
    queryWith_ parser conn (encodeQuery updateSQL)

instance HasUpdate PGS where
  dbUpdate (PGS conn) updateQ = do
    let updateSQL = PG.renderUpdate $ PG.updateSql $ updateQ
    execute_ conn (encodeQuery updateSQL)

instance HasQuery PGS where
  dbQueryWith parser (PGS conn) primQ = do
    let sqlQ = PG.renderQuery $ PG.sql primQ
    queryWith_ parser conn (encodeQuery sqlQ)

instance HasInsert PGS where
  dbInsert (PGS conn) insQ = do
    let insSQL = PG.renderInsert $ PG.insertSql insQ
    execute_ conn (encodeQuery insSQL)

instance HasInsertRet PGS where
  dbInsertRetWith parser (PGS conn) insQ = do
    let insSQL = PG.renderInsert $ PG.insertSql insQ
    queryWith_ parser conn (encodeQuery insSQL)

instance HasDelete PGS where
  dbDelete (PGS conn) deleteQ = do
    let delSQL = PG.renderDelete $ PG.deleteSql $ deleteQ
    execute_ conn (encodeQuery delSQL)

instance HasDeleteRet PGS where
  dbDeleteRetWith parser (PGS conn) deleteQ = do
    let delSQL = PG.renderDelete $ PG.deleteSql $ deleteQ
    queryWith_ parser conn (encodeQuery delSQL)

instance HasRawQuery PGS where
  dbRawQueryWith parser (PGS conn) q = queryWith_ parser conn (fromString $ T.unpack q)
  dbRawQuery_ (PGS conn) q = execute_ conn (fromString $ T.unpack q)

instance ShowQuery PGS where
  showQuery _ = PG.renderQuery . PG.sql
  showInsertQuery _ = PG.renderInsert . PG.insertSql
  showUpdateQuery _ = PG.renderUpdate . PG.updateSql
  showDeleteQuery _ = PG.renderDelete . PG.deleteSql

instance HasDDLQuery PGS where
  dbDDLQuery (PGS _conn) _ddlQs = undefined

runPGExpr :: Expr sc a -> T.Text
runPGExpr = PG.renderExpr . PG.toSqlExpr . getExpr

pgDefaultPool :: ConnectInfo -> IO (P.Pool Connection)
pgDefaultPool connectInfo =
#if MIN_VERSION_resource_pool(0,4,0)
  P.newPool (P.defaultPoolConfig (PGS.connect connectInfo) PGS.close 5 10)
#elif MIN_VERSION_resource_pool(0,3,0)
  P.newPool cfg
  where
    cfg = P.PoolConfig { P.createResource = PGS.connect connectInfo
                       , P.freeResource = PGS.close
                       , P.poolCacheTTL = 5
                       , P.poolMaxResources = 10
                       }
#else
  P.createPool (PGS.connect connectInfo) PGS.close 10 5 10
#endif

#if MIN_VERSION_postgresql_simple(0,6,3)
#else
instance (FromField a) => FromField (Identity a) where
  fromField f m = Identity <$> fromField f m
#endif

instance (FromField v) => FromField (Key (t :: k) v) where
  fromField f m = Key <$> fromField f m

instance (FromField a) => FromRow (Identity a) where
  fromRow = Identity <$> field

-- | Implementation based on MonadUnliftIO
withResource :: (U.MonadUnliftIO m) => P.Pool a -> (a -> m r) -> m r
withResource p k = U.withRunInIO $ \f -> P.withResource p (\a -> f $ k a)

-- TODO: Remove this
showMQuery :: MQuery sc r -> T.Text
showMQuery mQ = execMQuery
  (PG.renderInsert . PG.insertSql)
  (PG.renderUpdate . PG.updateSql)
  (PG.renderDelete . PG.deleteSql)
  "" mQ

-- TODO: Remove this
showQuery :: DBRI.Query' qt sc r -> T.Text
showQuery q = PG.renderQuery $ PG.sql $ DBRI.execQuery q


-- TODO: Orphan
instance (FromComposite a, Typeable a) => FromField (Composite a) where
  fromField b = fmap Composite_ <$> compositeToField b

instance (FromComposite a, Typeable a) => FromRow (Composite a) where
  fromRow = field
  
