{-# LANGUAGE UnicodeSyntax #-}

-- | Provide miscellaneous utilities.

module ExtraUtils where

import qualified Data.Text as T

import System.USB            ( Device, deviceDesc
                             , DeviceHandle, withDeviceHandle
                             , StrIx, getStrDesc, deviceProductStrIx )

import Control.Monad.Unicode ( (≫=) )
import Prelude.Unicode       ( (∘) )

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
