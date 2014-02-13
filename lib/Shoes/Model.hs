{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Shoes.Model
-- Copyright   :  (C) 2014 Yorick Laupa
-- License     :  MIT-like (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Shoes.Model where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Aeson hiding (decode)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decode)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Shoes
    desc Text
    color Text
    size Text

Photo
    shoes ShoesId
    filePath Text
|]

data NewShoes
    = NewShoes
      { newShoes :: !Shoes
      , newPhoto :: !ByteString
      }

instance FromJSON Shoes where
    parseJSON (Object m) =
        Shoes              <$>
        m .: "description" <*>
        m .: "color"       <*>
        m .: "size"
    parseJSON _ = mzero

instance FromJSON NewShoes where
    parseJSON p@(Object m) =
        NewShoes    <$>
        parseJSON p <*>
        (m .: "photo" >>= go)
      where
        go txt =
            case decode $ encodeUtf8 txt of
                Left e      -> fail e
                Right bytes -> return bytes
    parseJSON _ = mzero
