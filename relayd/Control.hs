{-# LANGUAGE NumDecimals #-}
module Control where

import qualified API as A
import Config
import TLE8108

import SPIDev

import Data.Finite
import Control.Concurrent.Async
import Control.Concurrent.STM.Delay
import qualified Data.Vector.Sized as VB
import Control.Exception.Safe (bracket)
import Text.Printf
import qualified Data.Text as T

type Interface = VB.Vector 8 Bool

data InterfaceState = InterfaceState
  { interface :: TVar A.Interface
  , callback :: TVar (TMVar A.Interface)
  , thread :: Async ()
  }

type GetInterface = App A.Interface
type SetInterface = Interface -> App A.Interface
type UpdateInterface = (A.Interface -> Interface) -> App A.Interface

data Controller = Controller [InterfaceState]

apply :: Interface -> A.Interface -> A.Interface
apply ps ai@A.Interface { A.ports = ps' } =
  ai { A.ports = flip VB.imap ps' \n p -> p { A.state = coerce $ ps `VB.index` n } }

getInterface :: InterfaceState -> GetInterface
getInterface InterfaceState{ interface=i } = atomically $ readTVar i

setInterface :: InterfaceState -> SetInterface
setInterface InterfaceState{interface=i, callback=callback} i' = do
  cb <- liftIO newEmptyTMVarIO
  atomically do
    writeTVar callback cb
    modifyTVar' i (apply i')
  atomically $ takeTMVar cb

updateInterface :: InterfaceState -> UpdateInterface
updateInterface InterfaceState{interface=i, callback=callback} f = do
  cb <- liftIO newEmptyTMVarIO
  atomically do
    writeTVar callback cb
    modifyTVar' i (\ai -> apply (f ai) ai)
  atomically $ takeTMVar cb

startController :: App Controller
startController = Controller <$> (traverse startInterface =<< interfaces <$> ask)
  where
    startInterface :: InterfaceConfig -> App InterfaceState
    startInterface InterfaceConfig{name = name, portConfig = PortConfig ps, devicePath = path} = do
      interface <- newTVarIO $ A.Interface
        { name = name
        , ifStatus = A.Operational
        , ports = VB.generate \p -> (snd <$> find ((== (p+1)) . fst) ps) & makePort p
        }
      cb' <- newEmptyTMVarIO
      callback <- newTVarIO cb'
      cfg <- ask
      thread <- liftIO . async . flip runReaderT cfg $ do
        logInfo $ "Starting interface controller for " <> coerce name <> " at " <> decodeUtf8 path
        let
          go :: A.Interface -> App ()
          go old = do
            delay <- liftIO $ newDelay 1e6
            new <- atomically do
              c <- readTVar interface
              guard (c /= old) <|> waitDelay delay
              pure c
            new' <- liftIO $ withSPIDev path \dev -> flip runReaderT cfg do
              applyState dev new
            diagnose new new'
            atomically do
              cb <- readTVar callback
              putTMVar cb new' <|> pure ()
              writeTVar interface new'
            go new'
        go =<< atomically (readTVar interface)

      pure InterfaceState{..}

    makePort :: Finite 8 -> Maybe A.PortName -> A.Port
    makePort p Nothing = makePort p (Just . A.PortName . printFinite . shift $ p)
    makePort _ (Just n) = A.Port { name = n, diagnostic = A.Open, state = False }

    printFinite :: KnownNat n => Finite n -> T.Text
    printFinite n = T.pack $ printf "%d" (fromIntegral @_ @Int64 n)

    diagnose :: A.Interface -> A.Interface -> App ()
    diagnose A.Interface{A.ifStatus=A.Operational} A.Interface{A.ifStatus=A.Disconnected, A.name=n} =
      logInfo $ "Interface " <> coerce n <> " fails the sniff test. Assuming it is disconnected and turning off all ports."
    diagnose _ _ = pure ()

stopController :: Controller -> App ()
stopController (Controller ics) = flip traverse_ ics \InterfaceState{..} -> do
  A.Interface{..} <- readTVarIO interface
  logInfo $ "Stopping interface controller for " <> coerce name
  liftIO $ uninterruptibleCancel thread

withController :: (Controller -> App a) -> App a
withController = bracket startController stopController
