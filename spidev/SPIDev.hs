module SPIDev
  ( module SPIDev.Buffer,
    module SPIDev.Send,
    module SPIDev.Types,
    openSPIDev,
    openSPIDev',
    closeSPIDev,
    withSPIDev,
    withSPIDev',
  )
where

import Control.Exception (bracket)
import SPIDev.Buffer
import SPIDev.Send
import SPIDev.Types
import System.Posix.ByteString

openSPIDev :: forall s. RawFilePath -> IO (SPIDev s)
openSPIDev p = SPIDev <$> openFd p ReadWrite Nothing defaultFileFlags

openSPIDev' :: forall s. SPIDevParams -> RawFilePath -> IO (SPIDev s)
openSPIDev' par p = do
  fd <- openFd p ReadWrite Nothing defaultFileFlags
  setParameters par fd
  pure $ SPIDev fd

closeSPIDev :: forall s. (SPIDev s) -> IO ()
closeSPIDev (SPIDev _fd) = closeFd _fd

withSPIDev :: RawFilePath -> (forall s. SPIDev s -> IO r) -> IO r
withSPIDev p = bracket (openSPIDev p) closeSPIDev

withSPIDev' :: SPIDevParams -> RawFilePath -> (forall s. SPIDev s -> IO r) -> IO r
withSPIDev' par p = bracket (openSPIDev' par p) closeSPIDev
