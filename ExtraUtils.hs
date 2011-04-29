{-# LANGUAGE UnicodeSyntax #-} -- providing: ∷ ⇒ ∀ → ← ⤙ ⤚ ⤛ ⤜

module ExtraUtils where

import Control.Monad.Unicode       ( (≫=) )
import Prelude.Unicode             ( (≡), (∘) )

import qualified Data.Text as T

import System.USB
import Utils ( decodeBCD, bits )

import Data.Word
import Data.Bits
import Data.List ( find )

unmarshalBitmask ∷ Bits α ⇒ [(Int, a)] → α → [a]
unmarshalBitmask table value = foldr test [] table
  where test (i, ctrl) acc | value `testBit` i = (ctrl:acc)
                           | otherwise         = acc

marshalBitmask ∷ (Eq a, Bits α) ⇒ [(Int, a)] → [a] → α
marshalBitmask table xs = foldr test 0 xs
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
