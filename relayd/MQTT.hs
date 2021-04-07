{-# LANGUAGE NumDecimals #-}
module MQTT (runMqtt) where

import Data.Aeson
import Data.Aeson.Encoding
import Control.Exception.Safe (bracket)
import Data.Generics.Labels ()
import Control.Concurrent.Async
import qualified Control.Lens as L
import Control.Lens.Operators hiding ((??))
import qualified API as A
import Config hiding (logInfo)
import qualified Config as C
import Control
import qualified Network.MQTT.Client as MQ
import qualified Network.MQTT.Types as MQ
import qualified Data.Text as T
import System.Timeout (timeout)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.List (stripPrefix)
import Data.Traversable (for)

type IfMap = M.Map A.InterfaceName InterfaceState

data Env = Env { appConfig :: Config
               , mqttClient :: MQ.MQTTClient
               }
  deriving (Generic)
type MQTT = ReaderT Env IO

logInfo :: Text -> MQTT ()
logInfo = L.magnify #appConfig . C.logInfo

runMqtt :: Controller -> App ()
runMqtt ctrl = do
  appCfg <- ask
  ifMap <- buildIfMap ctrl
  liftIO $ mapConcurrently_ id
    [ flip runReaderT appCfg $ commandHandler ifMap
    , forConcurrently_ (ctrl ^.. L.traverse) (flip runReaderT appCfg . interfacePublishThread)
    ]

  where
    buildIfMap :: Controller -> App IfMap
    buildIfMap (Controller ifs) = M.fromList <$>
      for ifs \s -> do
        A.Interface{name=name} <- readTVarIO (s ^. #interface)
        pure (name, s)

connectMqtt :: Maybe (MQ.Topic -> BL.ByteString -> [MQ.Property] -> MQTT ())
            -> Maybe MQ.LastWill
            -> App Env
connectMqtt mcb mlw = do
  cfg <- mqttConfig

  Just mqttClient <- liftIO . timeout 180e6 $ MQ.runClientTLS cfg
  appConfig <- ask
  pure Env{..}
  where
    mqttConfig :: App MQ.MQTTConfig
    mqttConfig = do
      appConfig <- ask
      pure (appConfig ^. #mqttBroker)
        { MQ._lwt = mlw
        , MQ._msgCB = case mcb of
            Just cb -> MQ.SimpleCallback (\mqttClient t m ps ->
                                            flip runReaderT Env{..} $
                                              cb t m ps)
            Nothing -> MQ.NoCallback
        }

withMQTT :: Maybe (MQ.Topic -> BL.ByteString -> [MQ.Property] -> MQTT ())
         -> Maybe MQ.LastWill
         -> MQTT a
         -> App a
withMQTT mcb mlw x =
  bracket (connectMqtt mcb mlw)
          (\env -> liftIO $ MQ.disconnect (env ^. #mqttClient) MQ.DiscoDisconnectWithWill [])
          (liftIO . runReaderT x)

interfacePublishThread :: InterfaceState -> App ()
interfacePublishThread InterfaceState{interface = iVar, commanded = cVar} = do
  (i, c) <- atomically do
    (,) <$> readTVar iVar <*> readTVar cVar
  prefix <- L.view $ #mqttPrefix . L.coerced
  withMQTT Nothing (Just $ lw prefix i) do
    go i c

  where
    lw :: Text -> A.Interface -> MQ.LastWill
    lw prefix i = MQ.LastWill { MQ._willRetain = True
                              , MQ._willQoS = MQ.QoS0
                              , MQ._willTopic =
                                  encodeUtf8 prefix <> "/interfaces/"
                                                    <> i ^. #name . #_InterfaceName . L.to encodeUtf8
                                                    <> "/controller_alive"
                              , MQ._willMsg = "false"
                              , MQ._willProps = []
                              }
    go i c = do
      publishStatus i c
      (i', c') <- atomically do
        i' <- readTVar iVar
        c' <- readTVar cVar
        guard $ i /= i' || c /= c'
        pure (i', c')
      go i' c'

    publishStatus :: A.Interface -> A.InterfaceCommand -> MQTT ()
    publishStatus i c = do
      cl <- L.view #mqttClient
      prefix <- L.view $ #appConfig . #mqttPrefix . L.coerced
      liftIO $ MQ.publishq cl
                           (prefix <> "/interfaces/" <> i ^. #name . #_InterfaceName <> "/commanded")
                           (encodingToLazyByteString $ toEncoding c)
                           True
                           MQ.QoS0
                           [ ]
      liftIO $ MQ.publishq cl
                           (prefix <> "/interfaces/" <> i ^. #name . #_InterfaceName)
                           (encodingToLazyByteString $ toEncoding i)
                           True
                           MQ.QoS0
                           [ ]
      liftIO $ MQ.publishq cl
                           (prefix <> "/interfaces/" <> i ^. #name . #_InterfaceName <> "/controller_alive")
                           "true"
                           True
                           MQ.QoS0
                           [ ]

setupSubscriptions :: MQTT ()
setupSubscriptions = do
  client <- L.view #mqttClient
  prefix <- L.view $ #appConfig . #mqttPrefix . L.coerced
  void . liftIO $ MQ.subscribe client [ (prefix <> "/interfaces/+/change", MQ.subOptions ) ] []

data CommandTopic = InterfaceChange A.InterfaceName
  deriving (Generic, Show, Eq)

parseTopic :: MQTTPrefix -> MQ.Topic -> Maybe CommandTopic
parseTopic (MQTTPrefix prefix) n = case split n & stripPrefix (split prefix)  of
  Just ["interfaces", i, "change"] -> Just $ InterfaceChange (coerce i)
  _ -> Nothing

  where split = T.splitOn "/"

processCommand  :: IfMap -> CommandTopic -> BL.ByteString -> MQTT ()
processCommand ifMap (InterfaceChange i) m
  | Just (cmd :: A.InterfaceCommand) <- decode m
  = case ifMap M.!? i of
      Just ifState -> atomically do writeTVar (ifState ^. #commanded) cmd
      Nothing -> pure ()

processCommand _ _ _ = logInfo "processCommand: could not parse command"

subscribeCb :: IfMap -> MQ.Topic -> BL.ByteString -> [MQ.Property] -> MQTT ()
subscribeCb ifMap t m _ = do
  prefix <- L.view $ #appConfig . #mqttPrefix
  case parseTopic prefix t of
    Just cmd -> unless (BL.null m)
                  do processCommand ifMap cmd m
    Nothing -> logInfo $ "subscribeCb: unknown topic " <> t

waitOnClient :: MQTT ()
waitOnClient = L.view #mqttClient >>= liftIO . MQ.waitForClient

commandHandler :: IfMap -> App ()
commandHandler ifMap = do
  withMQTT (Just $ subscribeCb ifMap) Nothing do
    setupSubscriptions
    waitOnClient
