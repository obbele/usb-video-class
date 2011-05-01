{-# LANGUAGE UnicodeSyntax #-} -- providing: ∷ ⇒ ∀ → ← ⤙ ⤚ ⤛ ⤜

module Main where

import qualified Control.Exception as E
import qualified Data.ByteString   as B

import Codec.BMP                ( writeBMP )

import System.USB
import System.USB.UVC.Internals
import BMP                      ( yuy2ToBMP )

import Data.List                ( find )
import Text.Printf              ( printf )
import System.Environment       ( getArgs )

import Control.Monad.Unicode    ( (≫=), (=≪) )
import Data.List.Unicode        ( (⧺) )
import Prelude.Unicode          ( (∧), (≡), (∘) )

{----------------------------------------------------------------------
-- USB operations.
----------------------------------------------------------------------}

main ∷ IO ()
main = do
    args ← getArgs
    let action = case args of
         ("video":_)  → writeRawDataToDisk
         ("images":_) → writeBMPImages
         _            → inspectData

    catchCommonUSBException action

writeRawDataToDisk ∷ IO ()
writeRawDataToDisk = do
    VideoPipe _ w h xs ← testISO
    let frames = extractFrames w h $ xs
        filename = printf "/tmp/uvc_%dx%d.yuy2" w h
    printf "writing raw video flux to [%s]\n" filename
    B.writeFile filename (B.concat frames)

inspectData ∷ IO ()
inspectData = do
    VideoPipe _ _ _ xs ← testISO
    mapM_ inspectStreamHeader xs

  where
    inspectStreamHeader packet =
        printf "[%d] %s\n" (B.length packet) (show $ extractStreamHeader packet)

writeBMPImages ∷ IO ()
writeBMPImages = do
    VideoPipe _ w h xs ← testISO
    let bitmaps = map (yuy2ToBMP w h) ∘ extractFrames w h $ xs
    foo (0 ∷ Int) bitmaps
    return ()

  where
    foo _ []     = return ()
    foo i (x:xs) = do
        let filename = printf "/tmp/uvc_%03d.bmp" i
        printf "writing file [%s]\n" filename
        writeBMP filename x
        foo (i+1) xs

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

handleEMErrors ∷ USBException → IO ()
handleEMErrors e = print $ "EVENT MANAGER ERROR: " ⧺ show e

initCtx ∷ IO Ctx
initCtx = do
    ctx ← newCtx' handleEMErrors
    setDebug ctx PrintWarnings
    return ctx

findVideoDevice ∷ IO Device
findVideoDevice = initCtx ≫= getDevices ≫= \devices →
    case find isMyVideoDevice devices of
         Nothing → error "Video device not found !"
         Just d  → do putStrLn $ "Using VideoDevice := " ⧺ show d
                      return d

  --where predicate := isMyVideoDevice isMy2ndVideoDevice hasVideoInterface

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

testISO ∷ IO VideoPipe
testISO = findVideoDevice ≫= getVideoDevice ≫= \video →
  withVideoDeviceHandle video $ \devh → do
    ctrl ← negotiatePCControl video devh (defaultProbeCommitControl video)
    readVideoData video devh ctrl nframes timeout
  where
    nframes = 100
    timeout = noTimeout
