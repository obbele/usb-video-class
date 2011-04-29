{-# LANGUAGE UnicodeSyntax #-} -- providing: ∷ ⇒ ∀ → ← ⤙ ⤚ ⤛ ⤜
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module ExtraUtils where

import Control.Monad.Unicode       ( (≫=) )
import Prelude.Unicode             ( (≡), (∘) )

import qualified Data.Text as T

import System.USB
import Utils         ( decodeBCD, bits )

import Data.Data     ( Data )
import Data.Typeable ( Typeable )
import Data.Word     ( Word8, Word16 )
import Data.Bits     ( Bits, testBit, setBit )
import Data.List     ( find )

type BitMaskTable a = [(Int, a)]
newtype BitMask a   = BitMask { unBitMask ∷ [a] }
    deriving (Eq, Show, Data, Typeable)

unmarshalBitmask ∷ Bits α ⇒ BitMaskTable a → α → BitMask a
unmarshalBitmask table value = BitMask $ foldr test [] table
  where test (i, ctrl) acc | value `testBit` i = (ctrl:acc)
                           | otherwise         = acc

marshalBitmask ∷ (Eq a, Bits α) ⇒ BitMaskTable a → BitMask a → α
marshalBitmask table (BitMask xs) = foldr test 0 xs
  where test ctrl acc = case find (\(_,a) → a ≡ ctrl) table of
               Just (i,_) → acc `setBit` i
               _          → acc

getUSBString ∷ DeviceHandle → Maybe StrIx → IO (Maybe String)
getUSBString devh mStrIx =
    case mStrIx of
         Nothing → return Nothing
         Just i  → getStrDesc devh i langID 50 ≫= return ∘ Just ∘ T.unpack
  where
    langID = (0x09, 0x01) -- US english

getDeviceName ∷ Device → IO (Maybe String)
getDeviceName dev = withDeviceHandle dev $ flip getUSBString strIx
  where
    strIx = deviceProductStrIx (deviceDesc dev)

{----------------------------------------------------------------------
-- Copied-and-pasted from System.USB.Internal
----------------------------------------------------------------------}

unmarshalReleaseNumber ∷ Word16 → ReleaseNumber
unmarshalReleaseNumber abcd = (a, b, c, d)
    where
      [a, b, c, d] = map fromIntegral $ decodeBCD 4 abcd

unmarshalStrIx ∷ Word8 → Maybe StrIx
unmarshalStrIx 0     = Nothing
unmarshalStrIx strIx = Just strIx

unmarshalEndpointAddress ∷ Word8 → EndpointAddress
unmarshalEndpointAddress a =
    EndpointAddress { endpointNumber    = fromIntegral $ bits 0 3 a
                    , transferDirection = if testBit a 7 then In else Out
                    }
