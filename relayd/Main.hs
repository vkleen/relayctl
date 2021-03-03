module Main (main) where

import LocalConfig (getCfg)

import Config
import Control
import MQTT

app :: App ()
app = withController \ctrl -> do
  logInfo "Starting relayd on MQTT"
  runMqtt ctrl

main :: IO ()
main = runReaderT app =<< getCfg
