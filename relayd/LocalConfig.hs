{-# OPTIONS_GHC -Wno-orphans #-}
module LocalConfig (getCfg) where

import Config

import qualified Data.ByteString.Base64 as B64
import           Data.Default.Class (def)
import qualified Data.X509.Validation as TLS
import           Data.Generics.Wrapped (_Wrapped)
import qualified Network.MQTT.Client as MQ
import qualified Network.Connection as TLS
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS
import qualified Control.Lens as L
import Control.Lens.Operators hiding ((??))

deriving instance Generic TLS.Fingerprint

data DhallConfig = DhallConfig
  { interfaces :: [InterfaceConfig]
  , brokerHostname :: Text
  , brokerPort :: Int
  , brokerFingerprint :: Text
  , brokerUid :: Text
  , brokerPassword :: Text
  , mqttPrefix :: MQTTPrefix
  }
  deriving (Generic, Show, NFData)

getCfg :: IO Config
getCfg = makeConfig
  [ InterfaceConfig { name = "rack"
                    , devicePath = "/dev/spidev0.0"
                    }
  , InterfaceConfig { name = "closet"
                    , devicePath = "/dev/spidev0.1"
                    }
  ]
  (MQTTPrefix "relays")
  mqttBroker

  where
    pinnedFingerprint :: TLS.Fingerprint
    pinnedFingerprint = B64.decode "+RPIvEGOxsVt0a7wAty5UKo6+i3uE8JoM+7ystfUHcY=" ^?! L._Right . _Wrapped

    hostname = "boron.auenheim.kleen.org"
    port = 8883
    serverID = (hostname, show port)

    mqttBroker :: MQ.MQTTConfig
    mqttBroker =
      MQ.mqttConfig
        { MQ._tlsSettings = TLS.TLSSettings $ (uncurry TLS.defaultParamsClient serverID)
            { TLS.clientSupported = def
                { TLS.supportedVersions = [ TLS.TLS13 ]
                , TLS.supportedCiphers = [ TLS.cipher_TLS13_CHACHA20POLY1305_SHA256 ]
                }
            , TLS.clientShared = def
                { TLS.sharedValidationCache =
                    TLS.exceptionValidationCache [( serverID, pinnedFingerprint )]
                }
            }
        , MQ._protocol = MQ.Protocol50
        , MQ._hostname = hostname
        , MQ._port = port
        , MQ._connID = ""
        , MQ._username = Just "relayd"
        , MQ._password = Just "relayd"
        }

  -- [ DeviceConfig { name = "chlorine"
  --                , ports = [ ("closet", 1)
  --                          , ("closet", 5)
  --                          ]
  --                }
  -- , DeviceConfig { name = "rigol-osc"
  --                , ports = [ ("rack", 1) ]
  --                }
  -- , DeviceConfig { name = "110V"
  --                , ports = [ ("rack", 7) ]
  --                }
  -- , DeviceConfig { name = "anritsu"
  --                , ports = [ ("rack", 7)
  --                          , ("rack", 8)
  --                          ]
  --                }
  -- ]
