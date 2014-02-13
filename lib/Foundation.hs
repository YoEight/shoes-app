{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
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
import Database.Persist.Sqlite (SqliteConf)
import Yesod
import Yesod.Core.Types (Logger)

import Shoes.Model

data ShoesApp
    = ShoesApp
      { appPool   :: PersistConfigPool SqliteConf
      , appConfig :: SqliteConf
      , appLogger :: Logger
      }

mkYesodData "ShoesApp" $(parseRoutesFile "lib/routes")

instance Yesod ShoesApp

instance YesodPersist ShoesApp where
    type YesodPersistBackend ShoesApp = SqlPersistT

    runDB = defaultRunDB appConfig appPool
