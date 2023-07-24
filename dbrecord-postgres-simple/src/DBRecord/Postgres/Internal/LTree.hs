{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}

{-# OPTIONS_GHC -Wno-orphans             #-}

module DBRecord.Postgres.Internal.LTree
       ( LTree (..)
       , E.ltree
       ) where

import qualified DBRecord.Internal.Expr as E ( ltree )
import           DBRecord.Types ( LTree (..), unescape )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Database.PostgreSQL.Simple.FromField

ltree :: TypeInfo
ltree =  Basic {
    typoid      = ltreeOid,
    typcategory = 'U',
    typdelim    = ',',
    typname     = "ltree"
  }

ltreeOid :: Oid
ltreeOid = Oid 32874
{-# INLINE ltreeOid #-}

instance FromField LTree where
   fromField f mdata = do
      typeName <- typename f
      if typeName /= typname ltree
        then returnError Incompatible f ""
        else case mdata of
               Nothing  -> returnError UnexpectedNull f ""
               Just dat -> either conversionError go (T.decodeUtf8' dat)

     where
       splitByDot = T.split (== '.')
       go = pure . LTree . unescape . splitByDot  

{-
cdc_edi_message
edi_sequence
edi_spec
partner -- ltree_peice
platform_app_config
platform_ui_config
issue_scenario
mft_connection
edi_group
edi_message
-}
