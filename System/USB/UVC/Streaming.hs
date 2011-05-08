{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}

module System.USB.UVC.Streaming
    (
    -- * Uncompressed stream header
      extractStreamHeader
    , StreamHeader(..)
    , StreamHeaderFlag(..)
    , Parity(..)
    , SourceClockReference
    , PresentationTimeStamp

    -- * Video data streaming
    , VideoPipe(..)
    , readVideoData
    , reorderFrames
    , intervalToFPS

    ) where

-- Qualified imports.
import qualified Data.ByteString    as B
import qualified Control.Exception  as E
import qualified Data.Serialize.Get as Get
import qualified Data.Serialize.Put as Put

-- Private libraries.
#include <uvc.h>
import System.USB.UVC.Requests
import System.USB.UVC.Descriptors
import ExtraUtils                 ( unmarshalBitmask, marshalBitmask
                                  , BitMask(..), BitMaskTable )

-- Third parties.
import System.USB
import Data.Serialize             ( Serialize(..) )

-- Base system.
import Control.Arrow              ( (&&&) )
import Control.Concurrent         ( threadDelay, forkIO )
import Control.Concurrent.Chan    ( newChan, readChan, writeChan )
import Control.Concurrent.MVar    ( MVar, newEmptyMVar, newMVar, readMVar
                                  , takeMVar, putMVar, modifyMVar, modifyMVar_ )
import Data.Bits                  ( Bits, testBit )
import Data.Data                  ( Data )
import Data.Function              ( on )
import Data.List                  ( find, foldl', sortBy )
import Data.Typeable              ( Typeable )
import Data.Word                  ( Word8, Word16, Word32 )
import Text.Printf                ( printf )
import System.IO                  ( stdout, hSetBuffering, BufferMode(..) )

import Control.Monad.Unicode      ( (≫) )
import Data.List.Unicode          ( (⧺) )
import Prelude.Unicode            ( (⊥), (∧), (≡), (≤), (≥)
                                  , (⋅), (∘), (∈) )

{----------------------------------------------------------------------
-- Video Data retrieving.
----------------------------------------------------------------------}

-- | A data type holding the information needed to decode an
-- uncompressed video stream.
data VideoPipe = VideoPipe
    { vpFormat ∷ CompressionFormat
    , vpColors ∷ Maybe ColorMatching
    , vpWidth  ∷ Width
    , vpHeight ∷ Height
    , vpFrames ∷ [Frame] -- FIXME: use a Chan instead of a (lazy?) list.
    -- , vpStarved ∷ Bool
    } deriving (Eq, Data, Typeable)

instance Show VideoPipe where
    show x = printf "VideoPipe { vpFormat = %s, \\
                    \vpColors = %s, \\
                    \vpWidth = %d, vpHeight = %d, \\
                    \vpFrames = [%d frames] }"
                    (show $ vpFormat x)
                    (show $ vpColors x)
                    (vpWidth x)
                    (vpHeight x)
                    (length $ vpFrames x)

-- | Read video frames.
-- Throw 'InvalidParamException' if the dwMaxPayloadTransferSize requested
-- by the ProbeCommitControl could not be found.
readVideoData ∷ VideoDevice → DeviceHandle → ProbeCommitControl → Int
              → Timeout → IO VideoPipe
readVideoData video devh ctrl nframes timeout = do
    let transferSize = pcMaxPayloadTransferSize ctrl
        interface    = head ∘ videoStreams $ video
        endpoint     = vsiEndpointAddress
                     ∘ head ∘ vsdInterfaces
                     ∘ head ∘ videoStrDescs
                     $ video

        altsetting = findIsochronousAltSettings interface endpoint transferSize

        -- Extractin _a lot_ of information from our video device
        -- and our control parameters.
        altInterface = interface !! fromIntegral altsetting
        interfaceN   = interfaceNumber altInterface
        frameSize    = pcMaxVideoFrameSize ctrl
        interval     = pcFrameInterval ctrl

        -- Compute the number of isopackets and their size
        -- based on the frame size.
        -- N.B.: the number of packets is caped to 64. I don't
        -- know why but the system could not process high-res
        -- frame requiring npackets > 2000.
        ratio ∷ Double
        ratio = fromIntegral frameSize / fromIntegral transferSize
        npackets ∷ Int
        npackets = if ratio > 64 then 64 else 1 + ceiling ratio
        sizes = replicate npackets transferSize

        -- ajusting the interval according to the actual number
        -- of packets.
        isopayload = npackets * transferSize
        ratio' ∷ Double
        ratio' = fromIntegral isopayload
               * fromIntegral interval
               / fromIntegral frameSize
        interval' = round ratio' + 1

        -- Additionnal information.
        fmtDsc = getFormatUncompressed video
        format = fFormat fmtDsc
        colors = fColors fmtDsc
        (w,h)  = (fWidth &&& fHeight)
               ∘ head
               ∘ filter (\f → fFrameIndex f ≡ pcFrameIndex ctrl)
               ∘ getFramesUncompressed
               $ video

    printIsoInformations transferSize frameSize npackets
                         interval format w h

    frames ← withInterfaceAltSetting devh interfaceN altsetting $
        retrieveNFrames devh endpoint sizes interval' timeout nframes

    -- we cheat here since our headers have a SCR field.
    --let xs = sortBy (compare `on` scrTime) frames

    return $ VideoPipe
           { vpFormat = format
           , vpColors = colors
           , vpWidth  = w
           , vpHeight = h
           , vpFrames = frames
           }

type Lock = MVar ()

-- | Execute a computation in a separate thread and provide a 'Lock' to
-- wait for its termination.
boundWorker ∷ IO () → IO Lock
boundWorker io = do
    lock ← newEmptyMVar
    forkIO $ io `E.finally` putMVar lock ()
    return lock

-- | Wait until a 'boundWorder' has finished.
waitBoundWorker ∷ Lock → IO ()
waitBoundWorker = takeMVar

-- | Retrieve n frames from an isochronous endpoint.
retrieveNFrames ∷ DeviceHandle
                → EndpointAddress   -- ^ an isochronous endpoint
                → [Int]             -- ^ size of the packets
                → FrameInterval
                → Timeout
                → Int               -- ^ number of frames
                → IO [Frame]
retrieveNFrames devh addr sizes interval timeout nframes = do
    let nworkers = 10
        deltaT = interval `div` (fromIntegral nworkers)

    count ← newMVar 0
    chan  ← newChan

    let readStream = handleShutdown count $ do
          xs ← readIsochronous devh addr sizes timeout
          writeChan chan xs

          done ← (≥ nframes) `fmap` readMVar count
          if done
             then return () -- end
             else waitFrameInterval interval ≫ readStream -- loop

    E.bracket
        -- launch (length ids) boundWorkers.
        --(replicateM nworkers (boundWorker readStream))
        (mapM (\i → waitFrameInterval (deltaT * i)
                    ≫ boundWorker readStream) [1..nworkers])


        -- When interrupted/finished, assert the end
        -- and wait for every background threads.
        (\workers → do modifyMVar_ count (\_ → return nframes)
                       mapM_ waitBoundWorker workers)

        -- Do it: retrieved iso-packet frames.
        (\_ → withUnbufferStdout $ consumeVideoStream [] chan count)


  where
    consumeVideoStream acc chan count = do
        xs ← readChan chan
        putStr "."

        -- FIXME: do not recompute stream header each times.
        let newFrames = length ∘ filter isEndOfFrame $ xs

        n' ← modifyMVar count $ \n → return (n + newFrames, n + newFrames)
        if n' ≥ nframes
           then return ∘ concat ∘ reverse $ (xs:acc)
           else consumeVideoStream (xs:acc) chan count

    -- When a background worker encounter an exception, just stop
    -- processing video data by setting (count = nframes).
    handleShutdown ∷ MVar Int → IO () → IO ()
    handleShutdown count io = E.catch io (shutdown count)

    shutdown ∷ MVar Int → E.SomeException → IO ()
    shutdown count _ = modifyMVar_ count (\_ → return nframes)

-- | Print a handful bunch of information concerning a video stream.
printIsoInformations ∷ Int → Int → Int
                     → FrameInterval → CompressionFormat → Int → Int
                     → IO ()
printIsoInformations xferSize frameSize npackets ival fmt w h = do
    printf "----------------------------------\n"
    printf "dwMaxVideoFrameSize:       %7d\n"     frameSize
    printf "Number of iso packets:     %7d\n"     npackets
    printf "Iso-packets size:          %7d\n"     xferSize
    printf "FrameInterval: (*100ns)    %7d\n"     ival
    printf "Frames per second:          %6.2f\n"  fps
    printf "FormatUncompressed:           %s\n"   (show fmt)
    printf "Dimensions:              %9s\n"       dimensions
    printf "----------------------------------\n"

  where
    fps = intervalToFPS ival

    dimensions ∷ String
    dimensions = printf "%dx%d" w h

-- | Search a (stream) interface and select the correct alt-setting for
-- which the isochronous endpoint has a payload equal or greater than
-- xferSize.
--
-- Throw 'InvalidParamException' if the dwMaxPayloadTransferSize requested
-- by the ProbeCommitControl could not be found.
findIsochronousAltSettings ∷ Interface → EndpointAddress → Int
                           → InterfaceAltSetting
findIsochronousAltSettings iface epaddr xferSize =
    case find (\(size,_) → xferSize ≤ size) sizesAltsettings of
      Nothing   → E.throw InvalidParamException
      Just (_,x)→ x
  where
    -- An association list of (size, alt-setting) orderded by increasing
    -- size.
    sizesAltsettings ∷ [(Int, InterfaceAltSetting)]
    sizesAltsettings = sortBy (compare `on` fst)
                     ∘ map (getSize &&& interfaceAltSetting)
                     ∘ getAlt
                     $ iface

    -- Compute the endpoint transfer size for every alt-settings.
    getSize ∷ InterfaceDesc → Int
    getSize = epSize ∘ head ∘ filter isOurEndpoint ∘ interfaceEndpoints

    -- Select an alternate setting having the desired endpoint.
    getAlt ∷ Interface → [InterfaceDesc]
    getAlt = filter (any isOurEndpoint ∘ interfaceEndpoints)

    -- Predicate on our endpoint address.
    isOurEndpoint = \ep → endpointAddress ep ≡ epaddr

    -- MaxPacketSize of an isochronous endpoint ≡ packet_size ⋅ x
    -- where x is the number of packets, i.e opportunity + 1.
    epSize ep = let MaxPacketSize x y = endpointMaxPacketSize ep
                in x ⋅ (1 + fromEnum y)

-- | Convert from units of 100 ns to 'threadDelay' microseconds.
waitFrameInterval ∷ FrameInterval → IO ()
waitFrameInterval t = threadDelay (fromIntegral t `div` 10)

-- Interval is in units of 100ns
-- ⇒ There is 1e7 units of 100ns in one second …
intervalToFPS ∷ FrameInterval → Float
intervalToFPS x = 10000000 / fromIntegral x

withInterfaceAltSetting ∷ DeviceHandle
                        → InterfaceNumber → InterfaceAltSetting
                        → IO α → IO α
withInterfaceAltSetting devh iface alt io =
    E.bracket_ (setInterfaceAltSetting devh iface alt)
               (setInterfaceAltSetting devh iface 0)
               (threadDelay 500 ≫ io)

withUnbufferStdout ∷ IO α → IO α
withUnbufferStdout =
    E.bracket_ (hSetBuffering stdout NoBuffering)
               (hSetBuffering stdout LineBuffering ≫ putStr "\n")

{----------------------------------------------------------------------
-- Decoding uncompressed video frames.
----------------------------------------------------------------------}

-- | Given a list of ordered payload, returns a list of raw data yuy2
-- frames. That is we skip empty payloads, remove frame headers and
-- concatenate together the different payloads of a single image frame.
-- Last but not the least, we assert that every frame as a correct size.
reorderFrames ∷ Int → Int → [B.ByteString] → [Frame]
reorderFrames w h bs =
    map normalizeSize ∘ groupFrames ∘ removeEmptyPayload $ bs

  where
    -- remove empty payloads (whose size is inferior to the stream
    -- header minimum).
    removeEmptyPayload = filter ((> 2) ∘ B.length)

    groupFrames xs =
        let (result, _, _) = foldl' groupFrame ([], [], []) xs
        in reverse result

    -- Maintain two frame buffer, one odds and one even.
    -- Flush the appropriate buffer when receiving an EndOfFrame.
    groupFrame (acc, evens, odds) x

        -- Flush the buffer.
        | endOfFrame
        ∧ (parity ≡ Odd)   = let frame = makeFrame (payload:odds)
                             in (frame:acc, evens, [])

        | endOfFrame
        ∧ (parity ≡ Even)  = let frame = makeFrame (payload:evens)
                             in (frame:acc, [], odds)

        -- add this payload to the appropriate frame.
        | (parity ≡ Odd)   = (acc, evens, payload:odds)

        | (parity ≡ Even)  = (acc, payload:evens, odds)

        -- Ignore anything else.
        | otherwise        = (acc, evens, odds)

      where
        payload       = removeStreamHeader x
        StreamHeader (BitMask flags) _ _ = extractStreamHeader x
        endOfFrame    = EndOfFrame ∈ flags
        parity        = if FID Odd ∈ flags then Odd else Even
        makeFrame     = B.concat ∘ reverse

        -- XXX: we don't use PTS and SCR slots.
        -- it works without them, so why bother ?
        --scrTime f = let StreamHeader _ _ (Just (t,_)) = extractStreamHeader f in t
        --ptsTime f = let StreamHeader _ (Just t) _ = extractStreamHeader f in t

    -- FIXME: Size should be with * height * (2 =?= bits-per-pixel)
    normalizeSize x =
        let actualSize = B.length x
            frameSize  = w * h * 2
        in case compare actualSize frameSize of
             -- pad the image if our stream was truncated.
             LT → B.append x (B.replicate (frameSize - actualSize) 0)

             -- the first frame is often too big.
             -- truncating to a correct size.
             GT → B.drop (actualSize - frameSize) x

             EQ → x

removeStreamHeader ∷ B.ByteString → B.ByteString
removeStreamHeader bs = B.drop l bs
  where
    l = fromIntegral $ B.head bs -- bLength is the first byte.

isEndOfFrame ∷ B.ByteString → Bool
isEndOfFrame bs =
    let StreamHeader (BitMask xs) _ _ = extractStreamHeader bs
    in EndOfFrame ∈ xs

{----------------------------------------------------------------------
-- (Uncompressed) Payload Frame Header.
----------------------------------------------------------------------}

-- | The first 32 bits is th source time clock in native device clock
-- units. The following 16 bits is a 1KHz SOF token counter.
-- See UVC specification 1.0a, table 2-6.
type SourceClockReference = (Word32, Word16)

-- | The source clock time in native device clock units when the raw
-- frame capture begins. This field may be repeated for multiple payload
-- transfers comprising a single video frame, with the restriction that
-- the value shall remain the same throughout that video frame.
-- /UVC v1.1 only: the PTS is in the same units as specified in the/
-- /pcCloClockFrequency field of the Video Probe Control response/.
type PresentationTimeStamp = Word32

-- | Each isopacket payload comes with a header.
-- See UVC specifications, section 2.4.3.3 /Video and Still Image Payload Headers/
-- for more information.
data StreamHeader = StreamHeader !(BitMask StreamHeaderFlag)
                                 !(Maybe PresentationTimeStamp)
                                 !(Maybe SourceClockReference)
    deriving (Eq, Show, Data, Typeable)

-- | For frame-based formats, this flag toggles between Even and Odd
-- every time a new video frame begins.
data Parity = Odd | Even
    deriving (Eq, Show, Data, Typeable)

-- | See UVC specifications, Table 2-5 /Format of the Payload Header/
-- for more information.
data StreamHeaderFlag
   = FID Parity   -- ^ Frame ID. Toggles between 'Odd' and 'Even' on each frame.
   | EndOfFrame   -- ^ set on the last payload of a video/image frame
   | PTS          -- ^ a 'PresentationTimeStamp' is provided.
   | SCR          -- ^ a 'SourceClockReference' is provided.
   | StillImage   -- ^ the following data is a still image frame.
   | ErrorBit     -- ^ error during the transmission, use a Stream Error Code control to get more information.
   deriving (Eq, Show, Data, Typeable)

instance Serialize (BitMask StreamHeaderFlag) where
    put = Put.putWord8 ∘ marshalBitmask stream_header_bitmask
    get = unmarshalBFH `fmap` Get.getWord8

stream_header_bitmask ∷ BitMaskTable StreamHeaderFlag
stream_header_bitmask =
    [ (1, EndOfFrame)
    , (2, PTS)
    , (3, SCR)
    -- bit 4 is reserved.
    , (5, StillImage)
    , (6, ErrorBit)
    -- , (7, EndOfHeader) mark the end of the header, reserved for
    -- future extensions and useless in Haskell.
    ]

unmarshalBFH ∷ Word8 → BitMask StreamHeaderFlag
unmarshalBFH w8 =
    let BitMask xs = unmarshalBitmask stream_header_bitmask w8
    in if w8 `testBit` 0
          then BitMask (FID  Odd:xs)
          else BitMask (FID Even:xs)

parseStreamHeader ∷ Get.Get StreamHeader
parseStreamHeader = do
    Get.skip 1 -- skipping bLength
    bfh@(BitMask xs) ← unmarshalBFH `fmap` Get.getWord8
    mPTS ← if PTS ∈ xs
                then Just `fmap` Get.getWord32le
                else return Nothing
    mSCR ← if SCR ∈ xs
                then do x0 ← Get.getWord32le
                        x1 ← Get.getWord16le
                        return $ Just (x0, x1)
                else return Nothing

    return $ StreamHeader bfh mPTS mSCR

instance Serialize StreamHeader where
    put = (⊥)
    get = parseStreamHeader

-- See UVC specifications 1.0a, Table 2.2.
-- FIXME: do not export ?
extractStreamHeader ∷ B.ByteString → StreamHeader
extractStreamHeader bs = runGetExact bs

runGetExact ∷ Serialize a ⇒ B.ByteString → a
runGetExact bs =
    case Get.runGet get bs of
         Right a → a
         Left s  → error $ "runGetExact: " ⧺ s
