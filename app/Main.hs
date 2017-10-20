{-# LANGUAGE TypeApplications #-}
module Main where

import           System.Environment
import           Web.Spock
import           Web.Spock.Config

import           Lib

main :: IO ()
main = do
    port <- lookupEnv "PORT"
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase EmptyState
    runSpock (maybe @Int 3000 read port) $ spock spockCfg app

