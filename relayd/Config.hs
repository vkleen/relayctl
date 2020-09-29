module Config where

import API
import System.Posix.ByteString
import System.Log.FastLogger
import Data.Finite
import Foreign.C.Types

data Config = Config
  { interfaces :: [InterfaceConfig]
  , checkCredentials :: (Maybe CUInt, Maybe CUInt, Maybe CUInt) -> App Bool
  , doLogInfo :: Text -> App ()
  }

data InterfaceConfig = InterfaceConfig
  { name :: InterfaceName,
    devicePath :: RawFilePath,
    portConfig :: PortConfig
  }

newtype PortConfig = PortConfig [(Finite 8, PortName)]

type App = ReaderT Config IO

makeConfig :: (Maybe CUInt, Maybe CUInt) -> [InterfaceConfig] -> IO Config
makeConfig allowedUGId ics = do
  stdoutLogger <- newStdoutLoggerSet defaultBufSize
  pure Config { interfaces = ics
              , checkCredentials = defaultCheck allowedUGId
              , doLogInfo = liftIO . pushLogStrLn stdoutLogger . toLogStr
              }

  where defaultCheck (Nothing, Nothing) _ = pure True
        defaultCheck (Just uid, Nothing) (_, Just uid', _) = pure $ uid == uid'
        defaultCheck (Nothing, Just gid) (_, _, Just gid') = pure $ gid == gid'
        defaultCheck (Just uid, Just gid) (_, Just uid', Just gid') =
          pure $ uid == uid' || gid == gid'
        defaultCheck _ _ = pure False

logInfo :: Text -> App ()
logInfo m = do
  Config{doLogInfo=doLogInfo} <- ask
  doLogInfo m

data Impossibru = Impossibru
  deriving Show
instance Exception Impossibru

bug' :: HasCallStack => a
bug' = bug Impossibru
