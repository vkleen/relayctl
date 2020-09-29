{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main (main) where

import LocalConfig (getCfg)

import Config
import Control as C
import Routes

import System.Posix.ByteString
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal
import Network.Socket
import Control.Exception.Safe (bracket)
import Data.ByteString.Char8 (unpack)

runOnSocket :: Application -> Socket -> App ()
runOnSocket app s = do
  cfg <- ask
  liftIO $ runSettingsConnection defaultSettings (runReaderT getConn cfg) app
  where
    getConn :: App (Connection, SockAddr)
    getConn = do
      (s, sa) <- liftIO $ accept s
      liftIO $ setSocketCloseOnExec s
      Config{checkCredentials=checkCredentials} <- ask
      liftIO (getPeerCredential s) >>= checkCredentials >>= bool (liftIO $ close s) (pure ())
      conn <- liftIO $ socketConnection defaultSettings s
      pure (conn, sa)

withSocket :: RawFilePath -> (Socket -> App a) -> App a
withSocket p k = bracket
  do sock <- liftIO $ socket AF_UNIX Stream 0
     liftIO $ bind sock (SockAddrUnix (unpack p))
     liftIO $ listen sock maxListenQueue
     pure sock
  (liftIO . close)
  k

app :: App ()
app = withController \ctrl -> do
  logInfo "Starting relayd at @relayd"
  app <- serve ctrl
  withSocket "\0relayd" (runOnSocket app)

main :: IO ()
main = runReaderT app =<< getCfg
