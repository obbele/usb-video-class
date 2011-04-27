{-# LANGUAGE UnicodeSyntax #-}

module Main where

-- Just because we can (?=with vim)
-- The UnicodeSyntax PRAGMA provides -- ∷ ⇒ ∀ → ← ⤙ ⤚ ⤛ ⤜
--import Control.Applicative.Unicode -- ⊛ ∅
--import Control.Arrow.Unicode       -- ⋙  ⋘  ⁂  ⧻  ⫴
--import Control.Category.Unicode    -- ≫> ⋘  ∘
import Control.Monad.Unicode       -- ≫= ≫ =≪
import Data.Bool.Unicode           -- ∧ ∨ ¬
import Data.Eq.Unicode             -- ≡ ≠
--import Data.Foldable.Unicode       -- ∈ ∋ ∉ ∌
import Data.Function.Unicode       -- ∘
import Data.List.Unicode           -- ⧺ ∪ ∖ ∆ ∩
--import Data.Monoid.Unicode         -- ∅ ⊕
--import Data.Ord.Unicode            -- ≤ ≥ ≮ ≯
--import Prelude.Unicode             -- π ÷ ⊥ ⋅ ∈
--import Data.IntSet.Unicode         -- ∅ ⊆ ⊇ ⊈ ⊉ ⊂ ⊃ ⊄ ⊅
--import Data.Sequence.Unicode       -- ⊲ ⊳ ⋈

import qualified Control.Exception as E
import qualified Data.ByteString   as B

import System.USB
import System.USB.IO.Asynchronous
import System.USB.UVC.Internals

import Control.Monad            ( replicateM )
import Data.List                ( find )
import Text.Printf              ( printf )
import System.IO                ( stdout, hSetBuffering, BufferMode(..) )

{----------------------------------------------------------------------
-- USB operations.
----------------------------------------------------------------------}


main ∷ IO ()
main = catchCommonUSBException $
    testISO ≫= inspectStreamHeaders

catchCommonUSBException ∷ IO α → IO α
catchCommonUSBException io =
    E.catch io handle
  where
    handle e = case e of
        BusyException   → error "Device already used by another driver"

        AccessException → error "Cannot open the device, have you the read/write permissions on /dev/bus/usb/*/* files ?"

        _ → E.throw e

{----------------------------------------------------------------------
-- Searching for a USB Video Class device.
----------------------------------------------------------------------}

findVideoDevice ∷ IO Device
findVideoDevice = newCtx ≫= getDevices ≫= \devices →
    case find hasVideoInterface devices of
         Nothing → error "Video device not found !"
         Just d  → do putStrLn $ "Using VideoDevice := " ⧺ show d
                      return d
  where
    --predicate := isMyVideoDevice isMy2ndVideoDevice hasVideoInterface

isMyVideoDevice ∷ Device → Bool
isMyVideoDevice dev = deviceVendorId  devDesc ≡ 0x0471
                    ∧ deviceProductId devDesc ≡ 0x0332
  where
    devDesc = deviceDesc dev

isMy2ndVideoDevice ∷ Device → Bool
isMy2ndVideoDevice dev = deviceVendorId  devDesc ≡ 0x13d3
                       ∧ deviceProductId devDesc ≡ 0x5130
  where
    devDesc = deviceDesc dev

{----------------------------------------------------------------------
-- USB Video Class (Cheap) Implementation.
----------------------------------------------------------------------}

tests ∷ IO ()
tests = do
    testVIA
    testVC
    testVS
    testProbe
    return ()

-- Testing if we can parse the extra bits in the video configuration
-- descriptor.
testVIA ∷ IO VideoDevice
testVIA = findVideoDevice ≫= getVideoDevice

-- Testing if we can parse the extra bits in the video control interface
-- descriptor.
testVC ∷ IO [ComponentDesc]
testVC = do
    video ← getVideoDevice =≪ findVideoDevice
    let components = vcdComponentDescs ∘ videoCtrlDesc $ video
    mapM_ print components
    putStrLn "----\n----\n----"
    return components

-- Testing if we can parse the extra bits in the first video streaming
-- interface descriptor.
testVS ∷ IO [VSDescriptor]
testVS = do
    video ← getVideoDevice =≪ findVideoDevice
    let xs = vsdStreamDescs ∘ head ∘ videoStrDescs $ video
    mapM_ print xs
    putStrLn "----\n----\n----"
    return xs

-- Testing if we can negotiate some control sets with the device.
testProbe ∷ IO ProbeCommitControl
testProbe = findVideoDevice ≫= getVideoDevice ≫= \video →
    withVideoDeviceHandle video $ \devh →
        negotiatePCControl video devh (simplestProbeCommitControl video)

testISO ∷ IO [[B.ByteString]]
testISO = findVideoDevice ≫= getVideoDevice ≫= \video →
  withVideoDeviceHandle video $ \devh → do
    -- Probe the device for a control set.
    ctrl ← negotiatePCControl video devh
                                 (simplestProbeCommitControl video)

    -- Now, we need to extract from the control set useful information,
    -- including:
    -- ⋅ the isopackets payload size which will allow us to choose a
    --   correct alternate setting.
    let transferSize = pcMaxPayloadTransferSize ctrl
        interface    = head ∘ videoStreams $ video
        interfaceN   = interfaceNumber ∘ head $ interface

    case findIsochronousAltSettings interface transferSize of
         Nothing → error "Cannot find the right alt-settings."
         Just x  → do
             -- Insert here code to retrieve isochronous data.
             let altInterface = interface !! fromIntegral x
                 Just ep = find isIsoEndpoint (interfaceEndpoints altInterface)
                 addr = endpointAddress ep

                 -- Compute the number of isopacket based on the frame
                 -- size.
                 frameSize = pcMaxVideoFrameSize ctrl
                 ratio ∷ Double
                 ratio = fromIntegral frameSize / fromIntegral transferSize
                 power ∷ Integer
                 power = ceiling $ logBase 2 ratio
                 numberOfIsoPackets ∷ Int
                 numberOfIsoPackets = 2^power
                 sizes = replicate numberOfIsoPackets transferSize
                 interval = pcFrameInterval ctrl -- in units of 100ns.

                 -- Additionnal information.
                 frameIndex = pcFrameIndex ctrl
                 frameDesc  = head ∘ filter (\f → fuFrameIndex f ≡ frameIndex)
                            ∘ filter isFrameUncompressed ∘ vsdStreamDescs
                            ∘ head ∘ videoStrDescs
                            $ video
                 height = fuHeight frameDesc
                 width  = fuWidth  frameDesc

             printf "Number of iso packets:  %7d\n" numberOfIsoPackets
             printf "Iso-packets size:       %7d\n" transferSize
             printf "FrameInterval: (*100ns) %7d\n" interval
             printf "Dimensions:             %dx%d\n" height width
             --threadDelay (1500 ⋅ 1000)

             setInterfaceAltSetting devh interfaceN x
             result ← withUnbufferStdout $ replicateM 500 $ do
                 waitFrameInterval interval
                 xs ← readIsochronous devh addr sizes timeout
                 putStr "."
                 return xs
             putStr "\n"

             return result

  where timeout = 1000 -- in milliseconds

-- | Display the last 5 frame headers.
inspectStreamHeaders ∷ [[B.ByteString]] → IO ()
inspectStreamHeaders xs = mapM_ (mapM_ (print ∘ extractStreamHeader)) xs'
  where
    size = length xs
    xs'  = drop (size - 5) xs

withUnbufferStdout ∷ IO α → IO α
withUnbufferStdout =
  E.bracket_ (hSetBuffering stdout NoBuffering)
             (hSetBuffering stdout LineBuffering)
