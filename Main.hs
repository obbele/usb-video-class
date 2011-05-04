{-# LANGUAGE UnicodeSyntax #-} -- providing: ∷ ⇒ ∀ → ← ⤙ ⤚ ⤛ ⤜

module Main where

import qualified Control.Exception as E
import qualified Data.ByteString   as B

import System.USB
import System.USB.UVC.Internals

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
-- * if @saveraw@ is given, run 'writeRawDataToDisk';
--
-- * if @inspect@ is given, run 'inspectData';
--
-- * otherwise, run 'testVideoStream';
--
main ∷ IO ()
main = do
    let usage = "Usage: test [options]\n\
                \\t-h|--help) display this help\n\
                \\tsaveraw)   get default video stream and save to disk\n\
                \\tinspect)   get default video stream and display headers\n\
                \\t[default]  prompt for configuration and save to disk\n"

    args ← getArgs
    let action = case args of
         ("-h":_)      → putStr usage
         ("--help":_)  → putStr usage
         ("saveraw":_) → writeRawDataToDisk
         ("inspect":_) → inspectData
         _             → testVideoStream

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

inspectStreamHeader ∷ Frame → IO String
inspectStreamHeader packet =
    printf "[%d] %s\n" (B.length packet) (show $ extractStreamHeader packet)

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
    case find hasVideoInterface devices of
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
    VideoPipe a b w h frames ← readVideoData video devh ctrl nframes timeout
    return $ VideoPipe a b w h (reorderFrames w h frames)
  where
    nframes = 100
    timeout = noTimeout

-- | Interactively ask for the frame index and the number of frames.
testVideoStream ∷ IO ()
testVideoStream = findVideoDevice ≫= getVideoDevice ≫= \video →
  withVideoDeviceHandle video $ \devh → do
    print video

    putStrLn "Available frames:"
    mapM_ printFrame (fFrames ∘ getFormatUncompressed $ video)
    putStrLn "Enter the frame index number:"
    idx ← read `fmap` getLine

    putStrLn "Enter the number of desired frames:"
    nframes ← read `fmap` getLine

    ctrl ← negotiatePCControl video devh (customProbeCommitControl video idx)
    VideoPipe _ _ w h frames ← readVideoData video devh ctrl nframes timeout

    let filename = printf "/tmp/uvc_%dx%d.yuy2" w h
    printf "writing raw video flux to [%s]\n" filename
    B.writeFile filename (B.concat ∘ reorderFrames w h $ frames)

  where
    timeout = noTimeout

    printFrame ∷ VSFrame → IO String
    printFrame f = printf "UncompressedFrame [%d] @ %dx%d\n" idx w h
      where idx = fFrameIndex f
            w = fWidth f
            h = fHeight f
