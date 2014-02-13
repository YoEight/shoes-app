{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Shoes
-- Copyright   :  (C) 2014 Yorick Laupa
-- License     :  MIT-like (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Shoes where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Aeson hiding (decode)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decode)
import Data.Text.Encoding (encodeUtf8)

import Shoes.Model

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
