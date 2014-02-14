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
module Shoes
       ( getShoesR
       , getShoesListR
       , postNewShoesR
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Int (Int64)

import Data.Aeson hiding (decode)
import Data.Aeson.Types (Parser)
import Data.Attoparsec.ByteString hiding (Parser)
import Data.Conduit
import Data.Conduit.Binary
import Data.ByteString (ByteString, empty)
import Data.ByteString.Base64 (decode)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Yesod

import Foundation
import Shoes.Model

data NewShoes = NewShoes !Shoes !ByteString -- < photo bytes

instance FromJSON NewShoes where
    parseJSON p@(Object m) =
        NewShoes         <$>
        parseShoesJSON p <*>
        (m .: "photo" >>= go)
      where
        go txt =
            case decode $ encodeUtf8 txt of
                Left e      -> fail e
                Right bytes -> return bytes
    parseJSON _ = mzero

parseShoesJSON :: Value -> Parser Shoes
parseShoesJSON (Object m) =
    Shoes              <$>
    m .: "description" <*>
    m .: "color"       <*>
    m .: "size"
parseShoesJSON _ = mzero

postNewShoesR :: Handler ()
postNewShoesR = do
    value <- rawRequestBody $$ sinkJson
    case fromJSON value of
        Error e   -> invalidArgs [pack e]
        Success n -> saveNewShoes n

getShoesR :: ShoesId -> Handler Value
getShoesR = undefined

getShoesListR :: Handler Value
getShoesListR = undefined

sinkJson :: Sink ByteString Handler Value
sinkJson = do
    bOpt <- await
    res  <- parseWith action json' (fromMaybe empty bOpt)
    either reportError return (eitherResult res)
  where
    action = fmap (fromMaybe empty) await

    reportError e =
        invalidArgs [pack e]

saveNewShoes :: NewShoes -> Handler ()
saveNewShoes (NewShoes shoes photoBin) =
    runDB $ do
        nsId <- insert shoes
        let key      = keyToInt nsId
            filepath = "repo/" ++ show key ++ ".jpeg"
            photo    = Photo nsId (pack filepath)
            source   = yield photoBin
        liftIO $ runResourceT (source $$ sinkFile filepath)
        insert_ photo

keyToInt :: ShoesId -> Int64
keyToInt = go . unKey
  where
    go (PersistInt64 i) = i
    go _                = error "impossible situation in Shoes.hs"
