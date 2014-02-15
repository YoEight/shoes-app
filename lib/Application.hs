{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Application
-- Copyright   :  (C) 2014 Yorick Laupa
-- License     :  MIT-like (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Application
       ( makeApplication
       , makeFoundation
       ) where


import           Control.Monad.Logger (runLoggingT)
import           Data.Configurator (Worth( Required ), load, require)
import           Data.Default (def)
import qualified Database.Persist
import           Database.Persist.Sqlite (SqliteConf, runMigration)
import           Network.Wai.Logger (clockDateCacher)
import qualified Network.Wai.Middleware.RequestLogger as RL
import           System.Log.FastLogger (newStdoutLoggerSet)
import           Yesod
import           Yesod.Core.Types (Logger (..))
import           Yesod.Default.Config

import Foundation
import Shoes
import Shoes.Model

mkYesodDispatch "ShoesApp" resourcesShoesApp

makeFoundation :: AppConfig DefaultEnv a -> IO ShoesApp
makeFoundation conf = do
    dbconf     <- withYamlEnvironment "config/sqlite.yaml" (appEnv conf)
                  Database.Persist.loadConfig >>= Database.Persist.applyEnv
    pool       <- Database.Persist.createPoolConfig
                  (dbconf :: SqliteConf)
    logSet     <- newStdoutLoggerSet 0
    (getter,_) <- clockDateCacher
    cfg        <- load [Required "config/app.cfg"]
    photoDir   <- require cfg "photo_dir"
    let logger     = Logger logSet getter
        foundation = ShoesApp pool dbconf logger photoDir

    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) pool)
        (messageLoggerSource foundation logger)

    return foundation

makeApplication :: AppConfig DefaultEnv a -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf
    logWare    <- RL.mkRequestLogger def
        { RL.outputFormat = RL.Detailed True
        , RL.destination  = RL.Logger $ loggerSet $ appLogger foundation
        }
    app        <- toWaiAppPlain foundation
    return $ logWare app
