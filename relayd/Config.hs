module Config ( Config(..)
              , InterfaceConfig(..)
              , NonZeroFinite(..)
              , DeviceConfig(..)
              , App
              , makeConfig
              , logInfo
              , logDebug
              , bug'
              ) where

import API
import System.Posix.ByteString
import System.Log.FastLogger
import Data.Finite
import Prelude hiding (error, natVal)
import GHC.Err (error)
import GHC.TypeLits

import qualified Network.MQTT.Client as M

data Config = Config
  { interfaces :: [InterfaceConfig]
  , mqttBroker :: M.MQTTConfig
  , doLogInfo :: Text -> App ()
  , doLogDebug :: Text -> App ()
  }
  deriving (Generic)

data InterfaceConfig = InterfaceConfig
  { name :: InterfaceName
  , devicePath :: RawFilePath
  }
  deriving (Generic, Show, NFData)

newtype NonZeroFinite n = NonZeroFinite (Finite n)
  deriving Show via (Finite n)
  deriving (Generic, NFData)

instance KnownNat n => Num (NonZeroFinite n) where
  fromInteger x
    | x >= natVal @n Proxy || x <= 0 =
        error $ "fromInteger: Integer " ++ show x ++ " is not representable in NonZeroFinite " ++ show (natVal @n Proxy)
    | otherwise = coerce @(Finite n) $ fromInteger x

-- dec :: NonZeroFinite (n+1) -> Finite n
-- dec = fromJust . unshiftProxy (Proxy @1) . coerce

data DeviceConfig = DeviceConfig
  { name :: DeviceName
  , ports :: [(InterfaceName, NonZeroFinite 9)]
  }
  deriving (Generic, Show, NFData)

type App = ReaderT Config IO

makeConfig :: [InterfaceConfig] -> M.MQTTConfig -> IO Config
makeConfig ics mqttBroker = do
  stdoutLogger <- newStdoutLoggerSet defaultBufSize
  pure . deepseq ics $
    Config { interfaces = ics
           , mqttBroker = mqttBroker
           , doLogInfo = liftIO . pushLogStrLn stdoutLogger . toLogStr
           , doLogDebug = const (pure ())
           }

logInfo :: Text -> App ()
logInfo m = do
  Config{doLogInfo=doLogInfo} <- ask
  doLogInfo m

logDebug :: Text -> App ()
logDebug m = do
  Config{doLogDebug=doLogDebug} <- ask
  doLogDebug m

data Impossibru = Impossibru
  deriving Show
instance Exception Impossibru

bug' :: HasCallStack => a
bug' = bug Impossibru
