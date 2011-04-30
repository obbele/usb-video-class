{-# LANGUAGE UnicodeSyntax #-} -- providing: ∷ ⇒ ∀ → ← ⤙ ⤚ ⤛ ⤜

module Main where

import Control.Monad.Unicode       ( (≫=), (=≪) )
import Data.List.Unicode           ( (⧺) )
import Prelude.Unicode             ( (∧), (≡), (≥), (∘), (∈) )

import qualified Control.Exception as E
import qualified Data.ByteString   as B

import System.USB
import System.USB.UVC.Internals
import ExtraUtils
import Codec.BMP

import Control.Monad            ( replicateM, forM_ )
import Control.Concurrent       ( forkIO )
import Control.Concurrent.Chan
import Data.Bits
import Data.Function            ( on )
import Data.List                ( find, sortBy, foldl' )
import Data.Word                ( Word8, Word32 )
import Text.Printf              ( printf )
import System.IO                ( stdout, hSetBuffering, BufferMode(..) )
import System.Environment       ( getArgs )

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
    (w, h, xs) ← testISO
    let frames = extractFrames w h $ xs
        filename = printf "/tmp/uvc_%d_%d.yuy2" w h
    printf "writing raw video flux to [%s]\n" filename
    B.writeFile filename (B.concat frames)

inspectData ∷ IO ()
inspectData = do
    (_, _, xs) ← testISO
    mapM_ inspectStreamHeader xs

writeBMPImages ∷ IO ()
writeBMPImages = do
    (w, h, xs) ← testISO
    let bitmaps = map (yuy2ToBMP w h) ∘ extractFrames w h $ xs
    foo (0 ∷ Int) bitmaps
    return ()

  where
    foo i (x:xs) = do
        let filename = printf "/tmp/uvc_%03d.bmp" i
        printf "writing file [%s]\n" filename
        writeBMP filename x
        foo (i+1) xs

    foo _ [] | otherwise = return ()

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

testISO ∷ IO (Int, Int, [B.ByteString])
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
                 numberOfIsoPackets ∷ Int
                 numberOfIsoPackets = ceiling ratio + 1
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
                 fps = intervalToFPS interval

             printf "dwMaxPayloadTransferSize:  %7d\n" transferSize
             printf "dwMaxVideoFrameSize:       %7d\n" frameSize
             printf "Number of iso packets:     %7d\n" numberOfIsoPackets
             printf "Iso-packets size:          %7d\n" transferSize
             printf "FrameInterval: (*100ns)    %7d\n" interval
             printf "Frames per seconds:          %3.2f\n" fps
             printf "Dimensions:                %dx%d\n" width height
             printf "Sizes: %s\n" (show sizes)
             --threadDelay (1500 ⋅ 1000)

             chan ← newChan

             let worker _  0 = return ()
                 worker idx i = do
                     xs ← readIsochronous devh addr sizes timeout
                     waitFrameInterval interval
                     writeChan chan (idx, xs)
                     worker idx (i - 1) -- loop i times

                 ids = ['a'..'j']
                 ntimes = 100

             -- launch (length ids) threads iterating (mtimes) times
             _ ← forM_ ids $ \idx → forkIO $ worker (idx:[]) ntimes

             result ← withInterfaceAltSetting devh interfaceN x
                    ∘ withUnbufferStdout
                    ∘ replicateM (ntimes * length ids)
                    $ do
                (idx, xs) ← readChan chan
                putStr idx
                return xs

             putStr "\n"

             -- We cheat here since our camera provide SCR time.
             let xs = sortBy (compare `on` getSCRTime) (concat result)

             return (width, height, xs)

  where timeout = 1000 -- in milliseconds

getSCRTime ∷ B.ByteString → Word32
getSCRTime bs =
    let StreamHeader _ _ _ (Just (t, _)) = extractStreamHeader bs
    in t

getFrameParity ∷ B.ByteString → StreamHeaderFlag
getFrameParity bs =
    let StreamHeader _ (BitMask xs) _ _ = extractStreamHeader bs
        parity = if EvenFrame ∈ xs then EvenFrame else OddFrame
    in parity

isEndOfFrame ∷ B.ByteString → Bool
isEndOfFrame bs =
    let StreamHeader _ (BitMask xs) _ _ = extractStreamHeader bs
    in EndOfFrame ∈ xs


toggleParity ∷ StreamHeaderFlag → StreamHeaderFlag
toggleParity EvenFrame = OddFrame
toggleParity _         = EvenFrame

-- | Given a list of ordered payload, returns a list of raw data yuy2
-- frames. That is we skip empty payloads, remove frame headers and
-- concatenate together the different payloads of a single image frame.
-- Last but not the least, we assert that every frame as a correct size.
extractFrames ∷ Int → Int → [B.ByteString] → [B.ByteString]
extractFrames w h bs =
    map normalizeSize ∘ groupFrames ∘ removeEmptyPayload $ bs

  where
    -- remove empty payloads.
    removeEmptyPayload = filter ((> 12) ∘ B.length)

    groupFrames xs =
        let parity0 = getFrameParity ∘ head $ bs
            (_, result, _) = foldl' groupFrame ([], [], parity0) xs
        in reverse result

    -- scan every frame of same parity until we found the EOF flag.
    groupFrame (frame,acc,parity) x

        -- add this payload to our current frame.
        | parity ≡ getFrameParity x
        ∧ (not $ isEndOfFrame x)     = let payload = B.drop 12 x
                                       in ((payload:frame),acc,parity)

        -- add this payload to our current frame.
        -- and flush our current frame to the acc result.
        | parity ≡ getFrameParity x
        ∧ isEndOfFrame x             = let payload = B.drop 12 x
                                           frame' = B.concat
                                                  ∘ reverse
                                                  $ (payload:frame)
                                           parity' = toggleParity parity
                                       in ([], frame':acc, parity')

        -- Ignore anything else.
        | otherwise                  = (frame, acc, parity)

    -- FIXME: Size should be with * height * (2 =?= bits-per-pixel)
    normalizeSize x =
        let actualSize = B.length x
            frameSize  = w * h * 2
        in case compare actualSize frameSize of
             -- pad the image if our stream was truncated.
             LT → B.concat [x, B.replicate (frameSize - actualSize) 0]

             -- the first frame is often too big.
             -- truncating to a correct size.
             GT → B.drop (actualSize - frameSize) x

             EQ → x

intervalToFPS ∷ Int → Float
intervalToFPS x = 10000000 / fromIntegral x

inspectStreamHeader ∷ B.ByteString → IO ()
inspectStreamHeader = f
  where
    f packet = printf "[%d] %s\n" (B.length packet) (show $ extractStreamHeader packet)

withUnbufferStdout ∷ IO α → IO α
withUnbufferStdout =
    E.bracket_ (hSetBuffering stdout NoBuffering)
               (hSetBuffering stdout LineBuffering)

withInterfaceAltSetting ∷ DeviceHandle → InterfaceNumber → InterfaceAltSetting → IO α → IO α
withInterfaceAltSetting devh iface alt =
    E.bracket_ (setInterfaceAltSetting devh iface alt)
               (setInterfaceAltSetting devh iface 0)

yuy2ToBMP ∷ Int → Int → B.ByteString → BMP
yuy2ToBMP w h bs = packRGBA32ToBMP w h bs'
  where
    bs' = reorderBMPByteString w ∘ yuy2ToRGBA $ bs

-- | The lines are presented in reverse order to the packRGBA32ToBMP
-- function.
reorderBMPByteString ∷ Int → B.ByteString → B.ByteString
reorderBMPByteString w bs =
    let rows = cut [] bs
    in B.concat rows

  where
    cut acc xs | B.null xs = acc
               | otherwise = cut (B.take l xs:acc) (B.drop l xs)

    l = w * 4 -- width * 4 bits per pixel (rgb+a)

-- | Converting from YUY2 4:2:0 pixels to RGBA ones.
yuy2ToRGBA ∷ B.ByteString → B.ByteString
yuy2ToRGBA bs = f [] bs
  where
    f acc xs | B.null xs = B.pack $ reverse acc
             | otherwise = let [y0, u0, y1, v0] = B.unpack (B.take 4 xs)
                               (r0,g0,b0) = yuv2rgb (y0,u0,v0)
                               (r1,g1,b1) = yuv2rgb (y1,u0,v0)
                           in f (255:b1:g1:r1:255:b0:g0:r0:acc) (B.drop 4 xs)

-- | see http://msdn.microsoft.com/en-us/library/ms893078.aspx
yuv2rgb ∷ (Word8, Word8, Word8) → (Word8, Word8, Word8)
yuv2rgb (y,u,v) = (r, g, b)
  where
    c, d, e ∷ Int
    c = fromIntegral y - 16
    d = fromIntegral u - 128
    e = fromIntegral v - 128
    r = clamp $ ((298 * c + 409 * e + 128) `shiftR` 8)
    g = clamp $ ((298 * c - 100 * d - 208 * e + 128) `shiftR` 8)
    b = clamp $ ((298 * c + 516 * d + 128) `shiftR` 8)
    clamp x | x ≥ 255   = 255
            | x < 0     = 0
            | otherwise = fromIntegral x
