{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}

module System.USB.UVC.Requests
    (
    -- * Video control requests
      VideoRequestType(..)
    , VideoRequest(..)

    -- * Probe and Commit Control
    , ProbeCommitControl(..)
    , ProbeHint(..)
    , negotiatePCControl
    , tryPCControl
    , simplestProbeCommitControl
    , defaultProbeCommitControl
    , customProbeCommitControl
    , probeCommitSet
    , probeCommitGet
    , ProbeAction(..)

    ) where


-- Qualified imports.
import qualified Control.Exception  as E
import qualified Data.ByteString    as B
import qualified Data.Serialize.Get as Get
import qualified Data.Serialize.Put as Put

-- Private libraries.
#include <uvc.h>
import System.USB.UVC.Descriptors
import Utils                 ( genFromEnum )
import System.USB.UVC.Unsafe ( unmarshalBitmask, marshalBitmask )

-- Third parties.
import System.USB

-- Base System.
import Control.Monad         ( replicateM_, when )
import Data.Bits             ( Bits, (.|.), shiftL )
import Data.Data             ( Data )
import Data.Function         ( on )
import Data.List             ( minimumBy )
import Data.Typeable         ( Typeable )
import Data.Word             ( Word16 )

import Prelude.Unicode       ( (≡), (≠), (∘) )


{----------------------------------------------------------------------
-- Configuring the video device.
----------------------------------------------------------------------}

-- What the frack is going on ?
-- Well, the System.USB module defines two sort of control requests:
-- the read ones and the write ones.
--
-- controlAction devh RequestType Recipient Request Value Index α
-- readControl   devh RequestType Recipient Request Value Index Size       Timeout → IO (ByteString, Status)
-- writeControl  devh RequestType Recipient Request Value Index ByteString Timeout → IO (Size,       Status)
--
-- But the UVC standard use a GET/SET notation which is redundent with
-- the distinction between readControl and writeControl.
--
-- Can We Simplify The Situation ?

data VideoRequestType
   = Set
   | Get
   deriving (Eq, Show, Enum, Data, Typeable)

data VideoRequest
   = VideoDummyRequest -- enum start at 0
   | Current           -- fromEnum ≡ 1
   | Minimum
   | Maximum
   | Resolution
   | DataLength
   | Information
   | Default
   deriving (Eq, Show, Enum, Data, Typeable)

data ProbeAction
   = Probe
   | Commit
   deriving (Eq, Show, Data, Typeable)

-- | See UVC specifications 1.0a, Table 4-45.
probeCommitGet ∷ VideoDevice -> DeviceHandle -> ProbeAction -> VideoRequest
               -> IO ProbeCommitControl
probeCommitGet video devh action attr = do
    let cs = if action ≡ Probe then VS_PROBE_CONTROL else VS_COMMIT_CONTROL
        release = vcdUVC ∘ videoCtrlDesc $ video

        bRequest  = 0x80 .|. genFromEnum attr
        wValue    = cs `shiftL` 8
        wIndex    = fromIntegral ∘ head ∘ videoStrIfaces $ video
        wLength   = if (vcdUVC $ videoCtrlDesc video) ≡ (0, 1, 0, 0)
                       then 26
                       else 34

        -- FIXME: hardcoded timeout
        timeout   = 500

    bs ← readControlExact devh Class ToInterface
                               bRequest wValue wIndex wLength timeout

    case extractPCControl release bs of
      Nothing → E.throw $ IOException "Could not read the probe control set."
      Just x  → return x

-- | See UVC specifications 1.0a, Table 4-45.
probeCommitSet ∷ VideoDevice -> DeviceHandle -> ProbeAction -> VideoRequest
               -> ProbeCommitControl → IO ()
probeCommitSet video devh action attr ctrl =
    let cs = if action ≡ Probe then VS_PROBE_CONTROL else VS_COMMIT_CONTROL
        release = vcdUVC ∘ videoCtrlDesc $ video

        bRequest   = genFromEnum attr
        wValue     = cs `shiftL` 8
        wIndex     = fromIntegral ∘ head ∘ videoStrIfaces $ video
        bytestring = intractPCControl release ctrl

        -- FIXME: hardcoded timeout
        timeout    = 500

    in writeControlExact devh Class ToInterface
                         bRequest wValue wIndex bytestring timeout

-- | Select an uncompressed frame by its index number.
customProbeCommitControl ∷ VideoDevice → FrameIndex → ProbeCommitControl
customProbeCommitControl video idx =
    let format = getFormatUncompressed video

        frame = head ∘ filter (\f → fFrameIndex f ≡ idx)
              ∘ getFramesUncompressed
              $ video

    in ProbeCommitControl
        { pcHint                   = Bitmask [HintFrameInterval]
        , pcFormatIndex            = fFormatIndex format
        , pcFrameIndex             = fFrameIndex frame
        , pcFrameInterval          = fDefaultFrameInterval frame
        -- The following 4 parameters are only valid if the
        -- corresponding '(ihControls !! formatIndex) inputHeader' is
        -- enable.
        , pcKeyFrameRate           = 0
        , pcPFrameRate             = 0
        , pcCompQuality            = 0
        , pcCompWindowSize         = 0
        -- Parameter read only for the host. Keep your hands off !
        , pcDelay                  = 0
        -- Parameter read only for the host. Keep your hands off !
        , pcMaxVideoFrameSize      = 0
        -- FIXME: should we use information from the isochronous
        -- endpoint ?
        , pcMaxPayloadTransferSize = 0
        }

-- | Select the recommended uncompressed frame.
defaultProbeCommitControl ∷ VideoDevice → ProbeCommitControl
defaultProbeCommitControl video =
    let format = getFormatUncompressed video

        -- Select the frame with the minimum data payload, because I'm
        -- lazy.
        frameIndex = fDefaultFrameIndex format

    in customProbeCommitControl video frameIndex

-- | Select the frame with the minimum data payload.
simplestProbeCommitControl ∷ VideoDevice → ProbeCommitControl
simplestProbeCommitControl video =
    let frame = minimumBy (compare `on` fMinBitRate)
              ∘ getFramesUncompressed
              $ video

    in customProbeCommitControl video (fFrameIndex frame)

-- | Set the given 'ProbeCommitControl'. If the device answers with an
-- identical control set, commits it and returns @Right control@, else
-- just returns @Left newControl@.
tryPCControl ∷ VideoDevice → DeviceHandle → ProbeCommitControl
             → IO (Either ProbeCommitControl ProbeCommitControl)
tryPCControl video devh ctrl = do
    probeCommitSet video devh Probe Current ctrl
    ctrl' ← probeCommitGet video devh Probe Current
    if (ctrl' ≠ ctrl)
       -- Return the modified control.
       then return $ Left ctrl'
       -- Commit the provided control and return it.
       else do probeCommitSet video devh Commit Current ctrl'
               return $ Right ctrl'

-- Ok, so now if your are lucky enough, the probe and commit protocol
-- negociation should look like the following:
--
-- λ> video
-- VideoDevice "USB Camera" on {Bus 002 Device 002: ID 13d3:5130}
--  +-- controlIface = 0
--  +-- streamIfaces = [1]
--
-- λ> probeCommitSet video devh Probe Current (simplestProbeCommitControl video)
-- DEBUG: writeControlAsync 0x21 0x01 0x0100 0x0001 0x001a
-- (26,Completed)
--
-- λ> probeCommitGet video devh Probe Current
-- DEBUG: readControlAsync 0xa1 0x81 0x0100 0x0001 0x001a
-- Just (ProbeCommitControl {pcHint = [HintFrameInterval], pcFormatIndex = 1, pcFrameIndex = 5, pcFrameInterval = 333333, pcKeyFrameRate = 0, pcPFrameRate = 0, pcCompQuality = 0, pcCompWindowSize = 0, pcDelay = 8671, pcMaxVideoFrameSize = 38400, pcMaxPayloadTransferSize = 1600})
--
-- λ> Just control <- probeCommitGet video devh Probe Current
-- DEBUG: readControlAsync 0xa1 0x81 0x0100 0x0001 0x001a
--
-- λ> control
-- ProbeCommitControl {pcHint = [HintFrameInterval], pcFormatIndex = 1, pcFrameIndex = 5, pcFrameInterval = 333333, pcKeyFrameRate = 0, pcPFrameRate = 0, pcCompQuality = 0, pcCompWindowSize = 0, pcDelay = 8671, pcMaxVideoFrameSize = 38400, pcMaxPayloadTransferSize = 1600}
--
-- λ> probeCommitSet video devh Probe Current control
-- DEBUG: writeControlAsync 0x21 0x01 0x0100 0x0001 0x001a
-- (26,Completed)

-- | This function will try to simply negotiate the format, frame, video
-- characteristics and USB bandwith used in future asynchronous
-- communication.
--
-- See UVC specifications 1.0a, Section 4.3.1.1
-- /Video Prove and Commit Control/ for more information.
negotiatePCControl ∷ VideoDevice → DeviceHandle → ProbeCommitControl
                   → IO ProbeCommitControl
negotiatePCControl video devh ctrl0 = do
    -- Try with our default control.
    result ← tryPCControl video devh ctrl0

    -- The device should have replied with a modified control set.
    case result of
         Right ctrl0' → return ctrl0' -- should not work, but who knows.
         Left ctrl1 → do
             -- Using the new device provided control set.
             result' ← tryPCControl video devh ctrl1
             case result' of
                  Right _ → return ctrl1
                  Left  _ → error "Could not use default probe control."


-- FIXME: See page 75.
-- queryAvailableControls = undefined
-- videoRequestGetInfo =

{----------------------------------------------------------------------
-- Probe and Commit Control.
----------------------------------------------------------------------}

-- See UVC specifications 1.0a, Section 4.3.1.1 and Table 4-46.

data ProbeCommitControl = ProbeCommitControl
    { pcHint                   ∷ !(Bitmask ProbeHint)
    , pcFormatIndex            ∷ !FormatIndex
    , pcFrameIndex             ∷ !FrameIndex
    , pcFrameInterval          ∷ !FrameInterval
    , pcKeyFrameRate           ∷ !Word16
    , pcPFrameRate             ∷ !Word16
    , pcCompQuality            ∷ !Word16
    , pcCompWindowSize         ∷ !Word16
    , pcDelay                  ∷ !Word16
    , pcMaxVideoFrameSize      ∷ !Int
    , pcMaxPayloadTransferSize ∷ !Int
    } deriving (Eq, Show, Data, Typeable)

data ProbeHint
   = HintFrameInterval
   | HintKeyFrameRate
   | HintPFrameRate
   | HintCompQuality
   | HintCompWindowSize
   deriving (Eq, Show, Data, Typeable)

probe_hint_bitmask ∷ BitmaskTable ProbeHint
probe_hint_bitmask =
   [ (0,  HintFrameInterval)
   , (1,  HintKeyFrameRate)
   , (2,  HintPFrameRate)
   , (3,  HintCompQuality)
   , (4,  HintCompWindowSize)
   ]

unmarshalProbeHint ∷ Bits α ⇒ α → Bitmask ProbeHint
unmarshalProbeHint = unmarshalBitmask probe_hint_bitmask

marshalProbeHint ∷ Bitmask ProbeHint → Word16
marshalProbeHint = marshalBitmask probe_hint_bitmask

extractPCControl ∷ ReleaseNumber → B.ByteString → Maybe ProbeCommitControl
extractPCControl release bs =
    case Get.runGet (parseVideoProbeCommitControl release) bs of
         Left _  → Nothing
         Right x → Just x

-- What ? "intract" is not an english word ? but who cares !
intractPCControl ∷ ReleaseNumber → ProbeCommitControl → B.ByteString
intractPCControl release ctrl =
    Put.runPut $ unparseVideoProbeCommitControl release ctrl

unparseVideoProbeCommitControl ∷ ReleaseNumber → ProbeCommitControl → Put.Put
unparseVideoProbeCommitControl release value = do
    let bmHint                   = marshalProbeHint $ pcHint value
        bFormatIndex             = pcFormatIndex value
        bFrameIndex              = pcFrameIndex value
        dwFrameInterval          = pcFrameInterval value
        wKeyFrameRate            = pcKeyFrameRate value
        wPFrameRate              = pcPFrameRate value
        wCompQuality             = pcCompQuality value
        wCompWindowSize          = pcCompWindowSize value
        wDelay                   = pcDelay value
        dwMaxVideoFrameSize      = fromIntegral $ pcMaxVideoFrameSize value
        dwMaxPayloadTransferSize = fromIntegral $ pcMaxPayloadTransferSize value

    Put.putWord16le bmHint
    Put.putWord8    bFormatIndex
    Put.putWord8    bFrameIndex
    Put.putWord32le dwFrameInterval
    Put.putWord16le wKeyFrameRate
    Put.putWord16le wPFrameRate
    Put.putWord16le wCompQuality
    Put.putWord16le wCompWindowSize
    Put.putWord16le wDelay
    Put.putWord32le dwMaxVideoFrameSize
    Put.putWord32le dwMaxPayloadTransferSize

    when (release ≠ (0, 1, 0, 0)) $
        -- Pad the extra bits use in UVC version 1.1.
        replicateM_ (34 - 26) (Put.putWord8 0)

-- There are huge changes between version 1.0a and 1.1 of the UVC specs,
-- so we need the UVC release number reported by the device to cope with
-- it.
-- FIXME: handle UVC specs 1.1.
parseVideoProbeCommitControl ∷ ReleaseNumber → Get.Get ProbeCommitControl
parseVideoProbeCommitControl _ = do
    bmHint ← unmarshalProbeHint `fmap` Get.getWord16le
    bFormatIndex ← Get.getWord8
    bFrameIndex ← Get.getWord8
    dwFrameInterval ← Get.getWord32le
    wKeyFrameRate ← Get.getWord16le
    wPFrameRate ← Get.getWord16le
    wCompQuality ← Get.getWord16le
    wCompWindowSize ← Get.getWord16le
    wDelay ← Get.getWord16le
    dwMaxVideoFrameSize ← fromIntegral `fmap` Get.getWord32le
    dwMaxPayloadTransferSize ← fromIntegral `fmap` Get.getWord32le

    return $ ProbeCommitControl
           { pcHint                   = bmHint
           , pcFormatIndex            = bFormatIndex
           , pcFrameIndex             = bFrameIndex
           , pcFrameInterval          = dwFrameInterval
           , pcKeyFrameRate           = wKeyFrameRate
           , pcPFrameRate             = wPFrameRate
           , pcCompQuality            = wCompQuality
           , pcCompWindowSize         = wCompWindowSize
           , pcDelay                  = wDelay
           , pcMaxVideoFrameSize      = dwMaxVideoFrameSize
           , pcMaxPayloadTransferSize = dwMaxPayloadTransferSize
           }


