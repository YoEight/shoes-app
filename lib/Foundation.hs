{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Foundation
-- Copyright   :  (C) 2014 Yorick Laupa
-- License     :  MIT-like (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Foundation where

import Database.Persist.Sql
import Yesod

import Shoes.Model

data ShoesApp = ShoesApp

mkYesodData "ShoesApp" $(parseRoutesFile "lib/routes")

instance Yesod ShoesApp
