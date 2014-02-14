-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2014 Yorick Laupa
-- License     :  MIT-like (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Main where

import Yesod.Default.Main (defaultMain)
import Yesod.Default.Config (DefaultEnv, fromArgs)

import Application (makeApplication)

main :: IO ()
main = defaultMain (fromArgs defaultEnv) makeApplication

defaultEnv :: Monad m => DefaultEnv -> ignore -> m ()
defaultEnv _ _ = return ()
