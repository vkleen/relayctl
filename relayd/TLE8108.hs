{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NumDecimals #-}

module TLE8108 where

import qualified API as A
import Config
import Data.Bits
import qualified Data.Text as T
import qualified Data.Vector.Storable.Sized as VS
import qualified Data.Vector.Sized as VB
import qualified Data.Vector.Generic.Sized as VG
import SPIDev
import Text.Printf
import Unsafe (fromJust)

challenge :: [Word8]
challenge = [0x0b, 0xad, 0xf0, 0x0d]

assembleBuffer :: A.Interface -> SPIBuffer 8 6
assembleBuffer A.Interface {ports = ps} =
  Prelude.fromList $
    challenge
      ++ [ fromIntegral $ (msg .&. 0xff00) `shiftR` 8,
           fromIntegral $ (msg .&. 0x00ff)
         ]
  where
    bits :: VB.Vector 8 Word16
    bits = flip fmap ps \A.Port {state = s} ->
      bool 0b11 0b10 s
    msg :: Word16
    msg = VB.ifoldl' (\a n x -> a .|. (x `shiftL` (2 * fromIntegral n))) (0 :: Word16) bits

sniffCheck :: SPIBuffer 8 6 -> A.Interface -> A.Interface
sniffCheck r i = i { A.ifStatus = go }
  where go = if VS.drop @2 (coerce r) == fromJust (VS.fromList @_ @4 challenge)
             then A.Operational
             else A.Disconnected

diagnose :: SPIBuffer 8 6 -> A.Interface -> A.Interface
diagnose r int = VS.generate @8 (\i -> statusWord `extract2` (2 * fromIntegral i))
               & VG.convert
               & VB.map (\case
                           0b00 -> A.Grounded
                           0b01 -> A.Open
                           0b10 -> A.Overload
                           0b11 -> A.Okay
                           _ -> bug'
                        )
               & VB.imap (\n d -> ((A.ports int) `VB.index` n) { A.diagnostic = d })
               & \p' -> int { A.ports = p' }
  where
    statusWord :: Word16
    statusWord =     (fromIntegral $ coerce r `VS.unsafeIndex` 0) `shiftL` 8
                 .|. (fromIntegral $ coerce r `VS.unsafeIndex` 1)

    extract2 x k = (x .&. (0b11 `shiftL` k)) `shiftR` k

turnOffFailures :: A.Interface -> A.Interface
turnOffFailures i@A.Interface{ifStatus=A.Disconnected, ports=ps} =
  i { A.ports = VB.map reset ps }
  where reset :: A.Port -> A.Port
        reset p = p { A.diagnostic = A.Okay, A.state = False }

turnOffFailures i@A.Interface{ports=ps} =
  i {A.ports = VB.map turnOff ps}
  where
    turnOff :: A.Port -> A.Port
    turnOff p@A.Port{ diagnostic=A.Okay } = p
    turnOff p@A.Port{ diagnostic=_ } = p { A.state = False }

prettyBuffer :: SPIBuffer 8 n -> Text
prettyBuffer b = VS.foldl' (\a x -> a <> T.pack (printf "%02x" x)) (T.pack "") (coerce b)

applyState :: SPIDev s -> A.Interface -> App A.Interface
applyState dev new = do
  let msg = assembleBuffer new
  _ <- doSend msg
  r <- doSend msg
  pure $
    new & sniffCheck r & diagnose r & turnOffFailures

  where doSend msg = liftIO $ send msg
                                   defaultTransferParameters { speedHz = 1e6
                                                             , txWidth = 1
                                                             , rxWidth = 1
                                                             }
                                   dev

