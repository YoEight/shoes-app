{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
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

import Blaze.ByteString.Builder (Builder)
import Data.Aeson hiding (decode)
import Data.Aeson.Types (Parser)
import Data.Attoparsec.ByteString hiding (Parser)
import Data.Conduit
import Data.Conduit.Binary
import Data.ByteString (ByteString, empty)
import Data.ByteString.Base64 (decode)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Yesod

import Foundation
import Shoes.Model

data NewShoes = NewShoes !Shoes !ByteString -- < photo bytes

newtype ShoesAndPhoto = ShoesAndPhoto (Shoes, Text)

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

getShoesR :: ShoesId -> Handler Html
getShoesR sId = do
    dir  <- getPhotoDir
    rOpt <- runDB $ do
            sOpt <- get sId
            pOpt <- selectFirst [PhotoShoes ==. sId] []
            return (select <$> sOpt <*> pOpt)
    maybe notFound (return . shoesAndPhotoHtml dir) rOpt
  where
    select s p =
        ShoesAndPhoto (s, photoFilePath $ entityVal p)

shoesAndPhotoHtml :: FilePath -> ShoesAndPhoto -> Html
shoesAndPhotoHtml dir (ShoesAndPhoto (s, path)) =
    [shamlet|<div>
                 <img src=#{fpath}>
                 <div>#{shoesDesc s}
                 <div>#{shoesColor s}
                 <div>#{shoesSize s}|]
  where
    fpath = dir ++ unpack path

getShoesListR :: Handler TypedContent
getShoesListR = respondSource "text/html" (source $= toShoesIdHtml)
  where
    source = transPipe runDB (selectSource [] [])

toShoesIdHtml :: Conduit (Entity Shoes) Handler (Flush Builder)
toShoesIdHtml = loop . go =<< lift getUrlRenderParams
  where
    loop k = do
        iOpt <- await
        case iOpt of
            Nothing -> yield Flush
            Just i  -> yield (Chunk $ k i) >> loop k

    go render = html render . entityKey

    html render i =
        renderHtmlBuilder ([hamlet|<a href=@{ShoesR i}>#{keyToInt i}|] render)

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
saveNewShoes (NewShoes shoes photoBin) = do
    dir <- getPhotoDir
    runDB $ do
        nsId <- insert shoes
        let key      = keyToInt nsId
            filename = show key ++ ".jpeg"
            photo    = Photo nsId (pack filename)
            source   = yield photoBin
        liftIO $ runResourceT (source $$ sinkFile (dir ++ filename))
        insert_ photo

getPhotoDir :: Handler FilePath
getPhotoDir = fmap appPhotoDir getYesod

keyToInt :: ShoesId -> Int64
keyToInt = go . unKey
  where
    go (PersistInt64 i) = i
    go _                = error "impossible situation in Shoes.hs"
