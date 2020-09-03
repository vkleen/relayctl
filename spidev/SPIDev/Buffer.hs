module SPIDev.Buffer (SPIBuffer(..), SomeSPIBuffer(..), SPIWord) where

import qualified Data.Vector.Storable.Sized as V
import Foreign.Storable
import GHC.Exts
import GHC.TypeLits
import Unsafe (fromJust)

type family Word' (s :: Nat) :: Type where
  Word' 8 = Word8
  Word' 16 = Word16
  Word' 32 = Word32
  Word' 64 = Word64

type family WordSizeFor' (b8 :: Nat) (o :: Ordering) (l :: Nat) :: Nat where
  WordSizeFor' 0 _ _ = 8
  WordSizeFor' _ 'EQ l = 2 ^ l
  WordSizeFor' _ _ l = 2 ^ (l + 1)

type WordSizeFor (b :: Nat) = WordSizeFor' (b `Div` 8) (b `CmpNat` (2 ^ (Log2 b))) (Log2 b)

type SPIWord (b :: Nat) = Word' (WordSizeFor b)

newtype SPIBuffer (bitsPerWord :: Nat) (len :: Nat) =
  SPIBuffer (V.Vector len (SPIWord bitsPerWord))

data SomeSPIBuffer (bitsPerWord :: Nat) where
  SomeSPIBuffer :: forall (bitsPerWord :: Nat) (len :: Nat). SPIBuffer bitsPerWord len -> SomeSPIBuffer bitsPerWord

deriving instance forall (bitsPerWord :: Nat) (l :: Nat).
     ( Show (SPIWord bitsPerWord)
     , Storable (SPIWord bitsPerWord)
     )
  => Show (SPIBuffer bitsPerWord l)

instance forall (b :: Nat) (l :: Nat).
         (KnownNat b, KnownNat l, Storable (SPIWord b))
      => IsList (SPIBuffer b l) where
  type instance Item (SPIBuffer b l) = SPIWord b
  fromList = fromJust . coerce . V.fromList @(SPIWord b) @l
