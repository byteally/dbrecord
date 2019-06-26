{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module DBRecord.MySQL.Internal.FromRow where

import DBRecord.MySQL.Internal.FromField ( ResultError (..)
                                         , FieldParser (..)
                                         , FromField (..)
                                         )
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import Database.MySQL.Base
import Control.Monad.State
import Control.Monad.Except
import Data.Functor.Identity

class FromRow a where
  fromRow :: RowParser a

data ParserState = ParserState { columns       :: ![ColumnDef]
                               , values        :: ![MySQLValue]
                               } deriving (Show, Eq)

newtype RowParser a =
  RowParser { runRowParser :: StateT ParserState (ExceptT ResultError Identity) a }
  deriving (Functor, Applicative, Monad)

rowParser :: StateT ParserState (ExceptT ResultError Identity) a -> RowParser a
rowParser = RowParser

evalParser :: RowParser a -> [ColumnDef] -> [MySQLValue] -> Either ResultError a
evalParser p d v =
  let res = runExceptT (runStateT (runRowParser p) (ParserState d v))
  in  fmap fst (runIdentity res)

field :: forall a. (FromField a) => RowParser a
field = RowParser $ do
  mps <- gets currentColumn
  case mps of
    Just (def, val) -> do
      modify' consumeColumn
      lift $ exceptT (runFieldParser (fromField :: FieldParser a) def val)
    Nothing         -> runRowParser invalidParserState

  where currentColumn ps = case (columns ps, values ps) of
          (c : _, v: _) -> Just (c, v)
          _             -> Nothing

        consumeColumn ps = case (columns ps, values ps) of
          (_ : cs, _ : vs) -> ParserState { columns = cs, values = vs }
          _                -> error "Panic: impossible case in consumeColumn"


exceptT :: (Applicative m) => Either e a -> ExceptT e m a
exceptT = ExceptT . pure

{-
rowParser :: ([ColumnDef] -> [MySQLValue] -> Either ResultError a) -> RowParser a
rowParser = RowParser

instance ( FromField a
         , FromField b
         ) => FromRow (a, b) where
  fromRow = rowParser go
    where go [fa, fb] [va, vb] = (,) <$> a <*> b
            where a = runFieldParser fromField fa va
                  b = runFieldParser fromField fb vb
          go fs vs             = Left $ convertError fs vs 2

instance ( FromField a
         , FromField b
         , FromField c
         ) => FromRow (a, b, c) where
  fromRow = rowParser go
    where go [fa, fb, fc] [va, vb, vc] = (,,) <$> a <*> b <*> c
            where a = runFieldParser fromField fa va
                  b = runFieldParser fromField fb vb
                  c = runFieldParser fromField fc vc        
          go fs vs                     = Left $ convertError fs vs 3

instance ( FromField a
         , FromField b
         , FromField c
         , FromField d
         ) => FromRow (a, b, c, d) where
  fromRow = rowParser go
    where go [fa, fb, fc, fd] [va, vb, vc, vd] =
            (,,,) <$> a <*> b <*> c <*> d
            where a = runFieldParser fromField fa va
                  b = runFieldParser fromField fb vb
                  c = runFieldParser fromField fc vc
                  d = runFieldParser fromField fd vd        
          go fs vs                     = Left $ convertError fs vs 4          
    
  
convertError
  :: [ColumnDef]
     -- ^ Descriptors of fields to be converted.
  -> [MySQLValue]
     -- ^ Contents of the row to be converted.
  -> Int
     -- ^ Number of columns expected for conversion.  For
     -- instance, if converting to a 3-tuple, the number to
     -- provide here would be 3.
  -> ResultError 
convertError fs vs n =
  ConversionFailed
    (T.pack (show (map (B8.unpack . columnName) fs)))
    ("Tried to create a Tuple of " <> T.pack (show n) <> " elements")
    ("Mismatch between number of columns to convert and number in target type. Source: " <>
     (T.pack . show . length) vs <> " columns, Target: " <> T.pack (show n) <> " elements")

-}

invalidParserState :: RowParser a
invalidParserState = RowParser $ do
  ps <- get
  lift $ exceptT (Left (convertError (columns ps) (values ps)))

convertError
  :: [ColumnDef]
     -- ^ Remaining descriptors of fields to be converted.
  -> [MySQLValue]
     -- ^ Remaining contents of the row to be converted.
  -> ResultError 
convertError fs vs =
  ConversionFailed
    -- (T.pack (show (map (B8.unpack . columnName) fs)))
    -- ("Tried to create a Tuple of " <> T.pack (show n) <> " elements")
    ("Mismatch between number of columns to convert and number in target type. Source: " <>
     (T.pack . show . length) vs <> " columns, Target: " <> (T.pack (show (map (B8.unpack . columnName) fs))))
