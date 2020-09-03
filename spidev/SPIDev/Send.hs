{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module SPIDev.Send (send_, send, setParameters) where

import SPIDev.Types
import SPIDev.Buffer
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Vector.Storable as Unsized
import qualified Data.Vector.Storable.Mutable as UnsizedM
import qualified Data.Vector.Storable.Sized as V
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import System.Posix.Types (Fd(..))
import Foreign.C.Error

import Unsafe (fromJust)

C.context (C.baseCtx)
C.include "<stdint.h>"
C.include "<sys/ioctl.h>"
C.include "<linux/spi/spidev.h>"

send_ :: forall (b :: Nat) (l :: Nat) s.
         (KnownNat b, KnownNat l, Storable (SPIWord b))
      => SPIBuffer b l -> TransferParameters -> SPIDev s -> IO ()
send_ t TransferParameters{..} d =
  Unsized.unsafeWith tvec \tx' ->
    let tx = castPtr tx'
    in throwErrnoIfMinus1_ "ioctl" [CU.block| int {
      struct spi_ioc_transfer trans = {
        .tx_buf = (uint64_t)$(void* tx),
        .rx_buf = 0,
        .len = $(uint32_t len),
        .speed_hz = $(uint32_t speedHz),
        .delay_usecs = $(uint16_t delayUsecs),
        .bits_per_word = $(uint8_t bpw),
        .cs_change = $(uint8_t csChangeWord),
        .tx_nbits = $(uint8_t txWidth),
        .rx_nbits = $(uint8_t rxWidth),
        .word_delay_usecs = $(uint8_t wordDelayUsecs),
        .pad = 0
      };
      return ioctl($(int intFd), SPI_IOC_MESSAGE(1), &trans);
    } |]
  where
    tvec :: Unsized.Vector (SPIWord b)
    tvec = V.fromSized $ coerce t

    len = fromIntegral $ natVal @l Proxy
    bpw = fromIntegral $ natVal @b Proxy
    csChangeWord = if csChange then 1 else 0
    intFd = coerce d

send :: forall (b :: Nat) (l :: Nat) s.
        (KnownNat b, KnownNat l, Storable (SPIWord b))
     => SPIBuffer b l -> TransferParameters -> SPIDev s -> IO (SPIBuffer b l)
send t TransferParameters{..} d = do
  txrx <- Unsized.thaw tvec
  UnsizedM.unsafeWith txrx \tx' ->
    let tx = castPtr tx'
    in throwErrnoIfMinus1_ "ioctl" [CU.block| int {
      struct spi_ioc_transfer trans = {
        .tx_buf = (uint64_t)$(void* tx),
        .rx_buf = (uint64_t)$(void* tx),
        .len = $(uint32_t len),
        .speed_hz = $(uint32_t speedHz),
        .delay_usecs = $(uint16_t delayUsecs),
        .bits_per_word = $(uint8_t bpw),
        .cs_change = $(uint8_t csChangeWord),
        .tx_nbits = $(uint8_t txWidth),
        .rx_nbits = $(uint8_t rxWidth),
        .word_delay_usecs = $(uint8_t wordDelayUsecs),
        .pad = 0
      };
      return ioctl($(int intFd), SPI_IOC_MESSAGE(1), &trans);
    } |]
  coerce . fromJust . V.toSized @l <$> Unsized.unsafeFreeze txrx
  where
    tvec :: Unsized.Vector (SPIWord b)
    tvec = V.fromSized $ coerce t

    len = fromIntegral $ natVal @l Proxy
    bpw = fromIntegral $ natVal @b Proxy
    csChangeWord = if csChange then 1 else 0
    intFd = coerce d

setParameters :: SPIDevParams -> Fd -> IO ()
setParameters SPIDevParams{..} fd =
  throwErrnoIfMinus1_ "ioctl" [CU.block| int{
    uint32_t mode =
        ($(uint8_t cpha) ? SPI_CPHA : 0)
      | ($(uint8_t cpol) ? SPI_CPOL : 0)
      ;
    return ioctl($(int intFd), SPI_IOC_WR_MODE32, &mode);
  } |]
  where
    cpha = if spiClockPhase then 1 else 0
    cpol = if spiClockPolarity then 1 else 0
    intFd = coerce fd
