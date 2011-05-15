{-# LANGUAGE UnicodeSyntax #-}

-- | An implementation of video streams using John Lato's and Bas van
-- Djik's (usb-)iteratee libraries.

module System.USB.UVC.Iteratee where

import qualified Data.Iteratee as I
import qualified System.USB.IO.Iteratee as I

-- Qualified imports.
import qualified Data.ByteString    as B
import qualified Control.Exception  as E

-- Private libraries.
import System.USB.UVC.Requests
import System.USB.UVC.Descriptors
import System.USB.UVC.Streaming hiding ( readNFrames )

-- Third parties.
import System.USB

-- Base system.
import Control.Arrow              ( (&&&) )
import Control.Concurrent         ( threadDelay, forkIO )
import Control.Concurrent.Chan    ( newChan, readChan, writeChan
                                  , Chan, isEmptyChan )
import Control.Concurrent.MVar    ( MVar, newEmptyMVar, newMVar, readMVar
                                  , takeMVar, putMVar, modifyMVar, modifyMVar_ )
import Control.Monad.IO.Class     ( liftIO )
import Data.Function              ( on )
import Data.List                  ( find, sortBy )
import Text.Printf                ( printf )
import System.IO                  ( stdout, hSetBuffering, BufferMode(..) )

import Control.Monad.Unicode      ( (≫) )
import Prelude.Unicode            ( (⊥), (∧), (≡), (≤), (≥), (∘), (∈) )

{----------------------------------------------------------------------
-- Iteratee laboratory
----------------------------------------------------------------------}

iterWorker ∷ Chan [B.ByteString]
           → FrameInterval
           → Int
           → (I.Iteratee [B.ByteString] IO ())
iterWorker chan interval n = I.liftI $ iter n
  where
    iter 0 s           = I.idone () s
    iter _ s@(I.EOF _) = I.idone () s
    iter i (I.Chunk c) = do liftIO (writeChan chan c)
                            liftIO (waitFrameInterval interval)
                            I.icont (iter (i-1)) Nothing

enumWorker ∷ Chan [B.ByteString]
           → DeviceHandle
           → EndpointAddress
           → [Size]
           → FrameInterval
           → Timeout
           → IO (I.Iteratee [B.ByteString] IO ())
enumWorker chan devh addr sizes interval timeout = do
    let enum = I.enumReadIsochronous devh addr sizes timeout
    enum $ iterWorker chan interval 100 -- XXX: arbitrary

{----------------------------------------------------------------------
-- Copy-Pasted from System.USB.UVC.Streaming
----------------------------------------------------------------------}
{----------------------------------------------------------------------
-- Video Data retrieving.
----------------------------------------------------------------------}

-- | Read video frames.
-- Throw 'InvalidParamException' if the dwMaxPayloadTransferSize requested
-- by the ProbeCommitControl could not be found.
readNFrames ∷ VideoDevice → DeviceHandle → ProbeCommitControl → Int
            → Timeout → IO Pipe
readNFrames video devh ctrl nframes timeout = do
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

    return $ Pipe
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

    let readStream' = handleShutdown count $ do
          xs ← readIsochronous devh addr sizes timeout
          writeChan chan xs

          done ← (≥ nframes) `fmap` readMVar count
          if done
             then return () -- end
             else waitFrameInterval interval ≫ readStream' -- loop

    let readStream = handleShutdown count $ do
          iter ← enumWorker chan devh addr sizes interval timeout
          I.run iter
          return ()

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
        --(\_ → withUnbufferedStdout $ consumeVideoStream [] chan count)
        (\_ → withUnbufferedStdout $ consumeSimple [] chan)


  where
    consumeSimple acc chan = do
        isEmpty ← isEmptyChan chan
        if isEmpty
           then return . concat . reverse $ acc
           else do x ← readChan chan
                   putStr "."
                   waitFrameInterval (interval * 1) -- XXX: arbitrary
                   consumeSimple (x:acc) chan

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
    getSize = maxIsoPacketSize
            ∘ head ∘ filter isOurEndpoint
            ∘ interfaceEndpoints

    -- Select an alternate setting having the desired endpoint.
    getAlt ∷ Interface → [InterfaceDesc]
    getAlt = filter (any isOurEndpoint ∘ interfaceEndpoints)

    -- Predicate on our endpoint address.
    isOurEndpoint = \ep → endpointAddress ep ≡ epaddr

-- | Convert from units of 100 ns to 'threadDelay' microseconds.
waitFrameInterval ∷ FrameInterval → IO ()
waitFrameInterval t = threadDelay (fromIntegral t `div` 10)

withInterfaceAltSetting ∷ DeviceHandle
                        → InterfaceNumber → InterfaceAltSetting
                        → IO α → IO α
withInterfaceAltSetting devh iface alt io =
    E.bracket_ (setInterfaceAltSetting devh iface alt)
               (setInterfaceAltSetting devh iface 0)
               (threadDelay 500 ≫ io)

withUnbufferedStdout ∷ IO α → IO α
withUnbufferedStdout =
    E.bracket_ (hSetBuffering stdout NoBuffering)
               (hSetBuffering stdout LineBuffering ≫ putStr "\n")

isEndOfFrame ∷ B.ByteString → Bool
isEndOfFrame bs =
    let StreamHeader (Bitmask xs) _ _ = extractStreamHeader bs
    in EndOfFrame ∈ xs

