module SPIDev.Types where

import System.Posix.ByteString

data TransferParameters = TransferParameters
  { speedHz :: Word32,
    delayUsecs :: Word16,
    csChange :: Bool,
    txWidth :: Word8,
    rxWidth :: Word8,
    wordDelayUsecs :: Word8
  }
  deriving (Show)

defaultTransferParameters :: TransferParameters
defaultTransferParameters =
  TransferParameters
    { speedHz = 0,
      delayUsecs = 0,
      csChange = False,
      txWidth = 0,
      rxWidth = 0,
      wordDelayUsecs = 0
    }

newtype SPIDev s = SPIDev {_fd :: Fd}

data SPIDevParams = SPIDevParams
  { spiClockPhase :: Bool
  , spiClockPolarity :: Bool
  }
  deriving (Show)

defaultSPIDevParams :: SPIDevParams
defaultSPIDevParams =
  SPIDevParams
    { spiClockPhase = False
    , spiClockPolarity = False
    }
