{-# LANGUAGE NumDecimals, DeriveTraversable #-}
module Control ( withMockController
               , withController
               , InterfaceState(..)
               , Controller, Controller'(..)
               ) where

import qualified API as A
import Config
import TLE8108

import SPIDev

import qualified Data.DList as DL
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Concurrent.STM.Delay
import qualified Data.Vector.Sized as VB
import Control.Exception.Safe (bracket)
import Data.Aeson.Text

import GHC.Generics (Generic1)

data InterfaceState = InterfaceState
  { interface :: TVar A.Interface
  , commanded :: TVar A.InterfaceCommand
  , thread :: Async ()
  }
  deriving Generic

newtype Controller' a = Controller [a]
  deriving stock (Generic, Generic1, Functor, Foldable, Traversable)

type Controller = Controller' InterfaceState

stopController :: Controller -> App ()
stopController (Controller ics) = for_ ics \InterfaceState{..} -> do
  A.Interface{..} <- readTVarIO interface
  logInfo $ "Stopping interface controller for " <> coerce name
  liftIO $ uninterruptibleCancel thread

type OnCommand = TVar A.Interface -> InterfaceConfig -> A.InterfaceCommand -> App ()

startController :: App Controller
startController = Controller <$> (traverse (startInterface onCommand) . interfaces =<< ask)
  where
    onCommand interface InterfaceConfig{ devicePath = path } cmd = do
      cfg <- ask

      atomically . modifyTVar interface =<<
        do liftIO $ withSPIDev path \dev -> flip runReaderT cfg do
             sendCommand dev cmd

startMockController :: App Controller
startMockController = Controller <$> (traverse (startInterface onCommand) . interfaces =<< ask)
  where
    onCommand interface _ cmd =
      atomically do
        old <- readTVar interface
        let new = A.applyCommand cmd old
        writeTVar interface new

startInterface :: OnCommand -> InterfaceConfig -> App InterfaceState
startInterface onCommand ifCfg@InterfaceConfig{name = name, devicePath = path} = do
  interface <- newTVarIO $ A.Interface
    { name = name
    , ifStatus = A.Operational
    , ports = VB.replicate (A.Unknown, A.Off)
    }
  commanded <- newTVarIO . A.InterfaceCommand $ VB.replicate A.Off
  cfg <- ask
  thread <- liftIO . async . flip runReaderT cfg $ do
    logInfo $ "Starting mock interface controller for " <> coerce name <> " at " <> decodeUtf8 path
    let
      go :: A.InterfaceCommand -> App ()
      go oldCmd = do
        delay <- liftIO $ newDelay 1e6
        newCmd <- atomically do
          c <- readTVar commanded
          guard (c /= oldCmd) <|> waitDelay delay
          pure c
        when (newCmd /= oldCmd) $ do
          logInfo $ "Interface state change command:\n"
                  <> toStrict (encodeToLazyText (commandDifference oldCmd newCmd))
        onCommand interface ifCfg newCmd
        go newCmd
    go =<< readTVarIO commanded
  pure InterfaceState{..}

commandDifference :: A.InterfaceCommand -> A.InterfaceCommand -> [(A.PortIndex, A.PortState)]
commandDifference (A.InterfaceCommand old) (A.InterfaceCommand new) = DL.toList $ VB.ifoldl' go mempty new
  where go acc i n | (old `VB.index` i) == n = acc
                   | otherwise = acc `DL.snoc` (A.PortIndex i, n)

withController :: (Controller -> App a) -> App a
withController = bracket startController stopController

withMockController :: (Controller -> App a) -> App a
withMockController = bracket startMockController stopController
