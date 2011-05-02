{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module ExtraUtils where

import qualified Data.Text as T

import System.USB            ( Device, deviceDesc
                             , DeviceHandle, withDeviceHandle
                             , StrIx, getStrDesc, deviceProductStrIx )

import Data.Data             ( Data )
import Data.Typeable         ( Typeable )
import Data.Bits             ( Bits, testBit, setBit )
import Data.List             ( find )

import Control.Monad.Unicode ( (≫=) )
import Prelude.Unicode       ( (≡), (∘) )

-- | A type to help creating 'Serialize' instance of binary objects
-- containing flags.
newtype BitMask a   = BitMask { unBitMask ∷ [a] }
    deriving (Eq, Show, Data, Typeable)

-- | An association list mapping bit position (from 0 to n-1) to their
-- equivalent Haskell representation.
type BitMaskTable a = [(Int, a)]

unmarshalBitmask ∷ Bits α ⇒ BitMaskTable a → α → BitMask a
unmarshalBitmask table value = BitMask $ foldr test [] table
  where test (i, ctrl) acc | value `testBit` i = (ctrl:acc)
                           | otherwise         = acc

marshalBitmask ∷ (Eq a, Bits α) ⇒ BitMaskTable a → BitMask a → α
marshalBitmask table (BitMask xs) = foldr test 0 xs
  where test ctrl acc = case find (\(_,a) → a ≡ ctrl) table of
               Just (i,_) → acc `setBit` i
               _          → acc

-- | Retrieve a string descriptor (in US_english) from a device.
-- May throw 'USBException's.
getUSBString ∷ DeviceHandle → Maybe StrIx → IO (Maybe String)
getUSBString devh mStrIx =
    case mStrIx of
         Nothing → return Nothing
         Just i  → getStrDesc devh i langID 50 ≫= return ∘ Just ∘ T.unpack
  where
    langID = (0x09, 0x01) -- US english

-- | Retrieve the device product string descriptor in US_english.
-- May throw 'USBException's.
getDeviceName ∷ Device → IO (Maybe String)
getDeviceName dev = withDeviceHandle dev $ flip getUSBString strIx
  where
    strIx = deviceProductStrIx (deviceDesc dev)
