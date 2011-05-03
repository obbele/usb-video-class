{-# LANGUAGE UnicodeSyntax #-} -- providing: ∷ ⇒ ∀ → ← ⤙ ⤚ ⤛ ⤜

module Main where

import qualified Control.Exception as E
import qualified Data.ByteString   as B

import Codec.BMP                ( writeBMP )

import System.USB
import System.USB.UVC.Internals
import BMP                      ( rgbaToBMP )
import Codec.UVC.RGBA           ( nv12ToRGBA, yuy2ToRGBA )

import Control.Monad
import Data.List                ( find )
import Text.Printf              ( printf )
import System.Environment       ( getArgs )

import Control.Monad.Unicode    ( (≫=), (=≪) )
import Data.List.Unicode        ( (⧺) )
import Prelude.Unicode          ( (∧), (≡), (∘) )

{----------------------------------------------------------------------
-- USB operations.
----------------------------------------------------------------------}

-- | Parse command line arguments.
--
-- * if @video@ is given, run 'writeRawDataToDisk';
--
-- * if @images@ is given, run 'writeBMPImages';
--
-- * otherwise, run 'inspectData';
--
main ∷ IO ()
main = do
    args ← getArgs
    let action = case args of
         ("video":_)  → writeRawDataToDisk
         ("images":_) → writeBMPImages
         _            → inspectData

    catchCommonUSBException action

-- | Write raw YUY2\/NV12 content to a file @\/tmp\/uvc_WxH.yuy2@ where
-- @W@ is the width and @H@ the height.
writeRawDataToDisk ∷ IO ()
writeRawDataToDisk = do
    VideoPipe _ _ w h frames ← testISO
    let filename = printf "/tmp/uvc_%dx%d.yuy2" w h
    printf "writing raw video flux to [%s]\n" filename
    B.writeFile filename (B.concat frames)

-- | Print the 'StreamHeader' of every retrieved iso-packet.
inspectData ∷ IO ()
inspectData = do
    VideoPipe _ _ _ _ xs ← testISO
    mapM_ inspectStreamHeader xs

  where
    inspectStreamHeader packet =
        printf "[%d] %s\n" (B.length packet) (show $ extractStreamHeader packet)

-- | Convert an YUY2 raw stream to a set of RGBA 'BMP' files.
-- This function is not optimised and consume /a lot/ of CPU ressources.
writeBMPImages ∷ IO ()
writeBMPImages = do
    VideoPipe fmt _ w h frames ← testISO
    let bitmaps = case fmt of
            NV12 → map (rgbaToBMP w h ∘ nv12ToRGBA w h) frames
            YUY2 → map (rgbaToBMP w h ∘ yuy2ToRGBA) frames
            _    → error "Unknown format"
    foo (0 ∷ Int) bitmaps
    return ()

  where
    foo _ []     = return ()
    foo i (x:xs) = do
        let filename = printf "/tmp/uvc_%03d.bmp" i
        printf "writing file [%s]\n" filename
        writeBMP filename x
        foo (i+1) xs

-- | Warn the user if the USB device is not accessible due to missing
-- file access permissions.
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

-- | Handling EventManager errors.
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

-- | Retrieve Philips (or NXP) SPC 1000NC PC Camera.
isMyVideoDevice ∷ Device → Bool
isMyVideoDevice dev = deviceVendorId  devDesc ≡ 0x0471
                    ∧ deviceProductId devDesc ≡ 0x0332
  where
    devDesc = deviceDesc dev

-- | Retrieve IMC Networks camera.
isMy2ndVideoDevice ∷ Device → Bool
isMy2ndVideoDevice dev = deviceVendorId  devDesc ≡ 0x13d3
                       ∧ deviceProductId devDesc ≡ 0x5130
  where
    devDesc = deviceDesc dev

{----------------------------------------------------------------------
-- USB Video Class (Cheap) Implementation.
----------------------------------------------------------------------}

-- | Run 'testVIA', 'testVC', 'testVS' and 'testProbe'∘
tests ∷ IO ()
tests = do
    testVIA
    testVC
    testVS
    testProbe
    return ()

-- | Testing if we can parse the extra bits in the video configuration
-- descriptor.
testVIA ∷ IO VideoDevice
testVIA = findVideoDevice ≫= getVideoDevice

-- | Testing if we can parse the extra bits in the video control
-- interface descriptor.
testVC ∷ IO [ComponentDesc]
testVC = do
    video ← getVideoDevice =≪ findVideoDevice
    let components = vcdComponentDescs ∘ videoCtrlDesc $ video
    mapM_ print components
    putStrLn "----\n----\n----"
    return components

-- | Testing if we can parse the extra bits in the first video streaming
-- interface descriptor.
testVS ∷ IO [VSInterface]
testVS = do
    video ← getVideoDevice =≪ findVideoDevice
    let ifaces = vsdInterfaces ∘ head ∘ videoStrDescs $ video
    forM_ ifaces $ \iface → do
        print iface
        forM_ (vsiFormats iface) $ \format → do
            print format
            forM_ (fFrames format) print

    putStrLn "----\n----\n----"
    return ifaces

-- | Testing if we can negotiate some control sets with the device.
testProbe ∷ IO ProbeCommitControl
testProbe = findVideoDevice ≫= getVideoDevice ≫= \video →
    withVideoDeviceHandle video $ \devh →
        negotiatePCControl video devh (simplestProbeCommitControl video)

-- | Testing the 'readVideoData' function to get raw video frames.
testISO ∷ IO VideoPipe
testISO = findVideoDevice ≫= getVideoDevice ≫= \video →
  withVideoDeviceHandle video $ \devh → do
    ctrl ← negotiatePCControl video devh (defaultProbeCommitControl video)
    readVideoData video devh ctrl nframes timeout
  where
    nframes = 100
    timeout = noTimeout
