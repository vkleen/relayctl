{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Routes (routes, Routes.serve) where

import qualified API as A
import Config hiding (logInfo)
import Control as C

import qualified Data.Map.Strict as M

import qualified Data.Vector.Sized as VB
import Data.Finite

import Control.Lens.Operators ((.~))

import Servant
import Servant.Server.Generic

data Env = Env { _cfg :: Config
               , _ctrl :: Controller
               , _ifMap :: M.Map A.InterfaceName InterfaceState
               }


type AppH = ReaderT Env Handler

liftApp :: App a -> AppH a
liftApp x = do
  cfg <- _cfg <$> ask
  liftIO $ runReaderT x cfg

ctrl :: AppH Controller
ctrl = _ctrl <$> ask

findInterfaceState :: A.InterfaceName -> AppH (Maybe InterfaceState)
findInterfaceState n = (_ifMap <$> ask) >>= pure . (M.!? n)

withInterfaceState :: A.InterfaceName -> (InterfaceState -> AppH a) -> AppH a
withInterfaceState n k = findInterfaceState n >>=
  maybe (throwError $ err404 { errBody = "Unknown interface" })
        k

withInterface :: A.InterfaceName -> (A.Interface -> AppH a) -> AppH a
withInterface n k =  withInterfaceState n \st -> liftApp (getInterface st) >>= k

ifindPort :: A.Interface -> A.PortName -> AppH (Maybe (Finite 8, A.Port))
ifindPort A.Interface{..} p = pure . find ((\A.Port{..} -> name == p) . snd) $ VB.imap (,) ports

withIPort :: A.InterfaceName -> A.PortName -> (Finite 8 -> A.Port -> AppH a) -> AppH a
withIPort i p k = withInterface i \i' -> ifindPort i' p >>=
  maybe (throwError $ err404 { errBody = "Unknown port" })
        (uncurry k)

withPort :: A.InterfaceName -> A.PortName -> (A.Port -> AppH a) -> AppH a
withPort i p k = withIPort i p \_ x -> k x

setPortState :: A.InterfaceName -> A.PortName -> Bool -> AppH A.Port
setPortState i p s = do
  index <- withIPort i p \i _ -> pure i
  withInterfaceState i \st -> do
    A.Interface{ports=new} <- liftApp $ updateInterface st \A.Interface{ports=ports} ->
      (A.state <$> ports) & VB.ix index .~ s
    pure $ new `VB.index` index

setPorts :: A.InterfaceName -> [(A.PortName, Bool)] -> AppH A.Interface
setPorts i ps = do
  withInterfaceState i \st -> do
    liftApp $ updateInterface st \A.Interface{ports=old} ->
      flip fmap old \A.Port{A.name=n, A.state=os} ->
        find ((==n) . fst) ps & maybe os snd

routes :: A.Routes (AsServerT AppH)
routes = A.Routes
  { A._get'interfaces = do
      Controller ifs <- ctrl
      liftApp do
        A.Interfaces <$> traverse getInterface ifs
  , A._get'interface = \n -> withInterface n pure
  , A._get'port = \i p -> withPort i p pure
  , A._get'port'diagnostic = \i p -> withPort i p \A.Port{..} ->
      pure diagnostic
  , A._get'port'state = \i p -> withPort i p \A.Port{..} ->
      pure state
  , A._set'port'state = setPortState
  , A._set'ports = setPorts
  }

serve :: Controller -> App Application
serve c = do
  _cfg <- ask
  _ifMap <- buildIfMap c
  let _ctrl = c
      env = Env {..}
  pure $ genericServeT (flip runReaderT env) routes

  where
    buildIfMap :: Controller -> App (M.Map A.InterfaceName InterfaceState)
    buildIfMap (Controller ifs) = M.fromList <$>
      flip traverse ifs \s -> do
        A.Interface{name=name} <- getInterface s
        pure (name, s)
