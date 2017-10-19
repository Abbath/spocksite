module Main where

import           Data.Maybe
import           System.Environment
import           Web.Spock
import           Web.Spock.Config

import           Lib

main :: IO ()
main = do
    port <- lookupEnv "PORT"
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase EmptyState
    runSpock (maybe (3000 :: Int) read port) (spock spockCfg app)

