{-# LANGUAGE BangPatterns #-} -- where are they coming from ?
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}

-- Ho man ! How much would I like to have a library in Haskell similar
-- to Common-Lisp's gigamonkeys binary-data.

module System.USB.UVC.Internals where

-- Qualified imports.
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import qualified Control.Exception      as E
import qualified Data.Binary.Strict.Get as Get
import qualified Data.Binary.Put        as Put

-- Private libraries.
import Utils               ( bits, genToEnum, genFromEnum )
import ExtraUtils          ( unmarshalBitmask, marshalBitmask
                           , unmarshalStrIx, unmarshalReleaseNumber
                           , unmarshalEndpointAddress
                           , getUSBString )

-- Private System.USB stuff.
import System.USB hiding ( readControlExact, writeControlExact )
-- FIXME: why should we import Asynchronous ourself ?
import System.USB.IO.Asynchronous

-- Private base system∘
import Control.Monad       ( replicateM_, replicateM, when )
import Control.Concurrent  ( threadDelay )
import Data.Bits           ( Bits, testBit, (.|.), shiftL )
import Data.Function       ( on )
import Data.Maybe          ( catMaybes )
import Data.Word           ( Word8, Word16, Word32 )
import Data.Data           ( Data )
import Data.Typeable       ( Typeable )
import Data.List           ( find, minimumBy )
import Text.Printf         ( printf )

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
--import Data.List.Unicode           -- ⧺ ∪ ∖ ∆ ∩
--import Data.Monoid.Unicode         -- ∅ ⊕
--import Data.Ord.Unicode            -- ≤ ≥ ≮ ≯
import Prelude.Unicode             -- π ÷ ⊥ ⋅ ∈
--import Data.IntSet.Unicode         -- ∅ ⊆ ⊇ ⊈ ⊉ ⊂ ⊃ ⊄ ⊅
--import Data.Sequence.Unicode       -- ⊲ ⊳ ⋈

{----------------------------------------------------------------------
-- Boring stuff.
-- Note that before theses CPP lines, the Haskell code should be able to
-- run withtout any "vulgar" reference to the UVC specifications.
----------------------------------------------------------------------}

#define INTERFACE_ASSOCIATION                     0x0B

-- USB Video Class 1.1
-- A1: Video Interface Class Code
#define CC_VIDEO                                  0x0E
-- A2: Video Subclass Code
#define SC_UNDEFINED                              0x00
#define SC_VIDEOCONTROL                           0x01
#define SC_VIDEOSTREAMING                         0x02
#define SC_VIDEO_INTERFACE_COLLECTION             0x03
-- A3: Video Interface Procotol
#define PC_PROTOCOL_UNDEFINED                     0x00
-- A4: Video Class-Specific Descriptor types
#define CS_UNDEFINED                              0x20
#define CS_DEVICE                                 0x21
#define CS_CONFIGURATION                          0x22
#define CS_STRING                                 0x23
#define CS_INTERFACE                              0x24
#define CS_ENDPOINT                               0x25
-- A5: Video Class-Specific VC Interface Descriptor Subtypes
#define VC_DESCRIPTOR_UNDEFINED                   0x00
#define VC_HEADER                                 0x01
#define VC_INPUT_TERMINAL                         0x02
#define VC_OUTPUT_TERMINAL                        0x03
#define VC_SELECTOR_UNIT                          0x04
#define VC_PROCESSING_UNIT                        0x05
#define VC_EXTENSION_UNIT                         0x06
-- A6: Video Class-Specific VS Interface Descriptor Subtypes
#define VS_UNDEFINED                              0x00
#define VS_INPUT_HEADER                           0x01
#define VS_OUTPUT_HEADER                          0x02
#define VS_STILL_IMAGE_FRAME                      0x03
#define VS_FORMAT_UNCOMPRESSED                    0x04
#define VS_FRAME_UNCOMPRESSED                     0x05
#define VS_FORMAT_MJPEG                           0x06
#define VS_FRAME_MJPEG                            0x07
#define VS_FORMAT_MPEG2TS                         0x0A
#define VS_FORMAT_DV                              0x0C
#define VS_COLORFORMAT                            0x0D
#define VS_FORMAT_FRAME_BASED                     0x10
#define VS_FRAME_FRAME_BASED                      0x11
#define VS_FORMAT_STREAM_BASED                    0x12
-- A7: Video Class-Specific Endpoint Descriptor Subtypes
#define EP_UNDEFINED                              0x00
#define EP_GENERAL                                0x01
#define EP_ENDPOINT                               0x02
#define EP_INTERRUPT                              0x03
-- A8: Video Class-Specific Request Codes
#define RC_UNDEFINED                              0x00
#define SET_CUR                                   0x01
#define GET_CUR                                   0x81
#define GET_MIN                                   0x82
#define GET_MAX                                   0x83
#define GET_RES                                   0x84
#define GET_LEN                                   0x85
#define GET_INFO                                  0x86
#define GET_DEF                                   0x87
-- A9.1: VideoControl Interface Control Selectors
#define VC_CONTROL_UNDEFINED                      0x00
#define VC_VIDEO_POWER_MODE_CONTROL               0x01
#define VC_REQUEST_ERROR_CODE_CONTROL             0x02
-- A9.2: Terminal Control Selectors
#define TE_CONTROL_UNDEFINED                      0x00
-- A9.3: Selector Unit Control Selectors
#define SU_CONTROL_UNDEFINED                      0x00
#define SU_INPUT_SELECT_CONTROL                   0x01
-- A9.4: Camera Terminal Control Selectors
#define CT_CONTROL_UNDEFINED                      0x00
#define CT_SCANNING_MODE_CONTROL                  0x01
#define CT_AE_MODE_CONTROL                        0x02
#define CT_AE_PRIORITY_CONTROL                    0x03
#define CT_EXPOSURE_TIME_ABSOLUTE_CONTROL         0x04
#define CT_EXPOSURE_TIME_RELATIVE_CONTROL         0x05
#define CT_FOCUS_ABSOLUTE_CONTROL                 0x06
#define CT_FOCUS_RELATIVE_CONTROL                 0x07
#define CT_FOCUS_AUTO_CONTROL                     0x08
#define CT_IRIS_ABSOLUTE_CONTROL                  0x09
#define CT_IRIS_RELATIVE_CONTROL                  0x0A
#define CT_ZOOM_ABSOLUTE_CONTROL                  0x0B
#define CT_ZOOM_RELATIVE_CONTROL                  0x0C
#define CT_PANTILT_ABSOLUTE_CONTROL               0x0D
#define CT_PANTILT_RELATIVE_CONTROL               0x0E
#define CT_ROLL_ABSOLUTE_CONTROL                  0x0F
#define CT_ROLL_RELATIVE_CONTROL                  0x10
#define CT_PRIVACY_CONTROL                        0x11
-- A9.5: Processing Unit Control Selectors
#define PU_CONTROL_UNDEFINED                      0x00
#define PU_BACKLIGHT_COMPENSATION_CONTROL         0x01
#define PU_BRIGHTNESS_CONTROL                     0x02
#define PU_CONTRAST_CONTROL                       0x03
#define PU_GAIN_CONTROL                           0x04
#define PU_POWER_LINE_FREQUENCY_CONTROL           0x05
#define PU_HUE_CONTROL                            0x06
#define PU_SATURATION_CONTROL                     0x07
#define PU_SHARPNESS_CONTROL                      0x08
#define PU_GAMMA_CONTROL                          0x09
#define PU_WHITE_BALANCE_TEMPERATURE_CONTROL      0x0A
#define PU_WHITE_BALANCE_TEMPERATURE_AUTO_CONTROL 0x0B
#define PU_WHITE_BALANCE_COMPONENT_CONTROL        0x0C
#define PU_WHITE_BALANCE_COMPONENT_AUTO_CONTROL   0x0D
#define PU_DIGITAL_MULTIPLIER_CONTROL             0x0E
#define PU_DIGITAL_MULTIPLIER_LIMIT_CONTROL       0x0F
#define PU_HUE_AUTO_CONTROL                       0x10
#define PU_ANALOG_VIDEO_STANDARD_CONTROL          0x11
#define PU_ANALOG_LOCK_STATUS_CONTROL             0x12
-- A9.6: Extension Unit Control Selectors
#define XU_CONTROL_UNDEFINED                      0x00
-- A9.7: VideoStreaming Interface Control Selectors
#define VS_CONTROL_UNDEFINED                      0x00
#define VS_PROBE_CONTROL                          0x01
#define VS_COMMIT_CONTROL                         0x02
#define VS_STILL_PROBE_CONTROL                    0x03
#define VS_STILL_COMMIT_CONTROL                   0x04
#define VS_STILL_IMAGE_TRIGGER_CONTROL            0x05
#define VS_STREAM_ERROR_CODE_CONTROL              0x06
#define VS_GENERATE_KEY_FRAME_CONTROL             0x07
#define VS_UPDATE_FRAME_SEGMENT_CONTROL           0x08
#define VS_SYNCH_DELAY_CONTROL                    0x09
-- B1: USB Terminal Types
#define TT_VENDOR_SPECIFIC                        0x0100
#define TT_STREAMING                              0x0101
-- B2: Input Terminal Types
#define ITT_VENDOR_SPECIFIC                       0x0200
#define ITT_CAMERA                                0x0201
#define ITT_MEDIA_TRANSPORT_INPUT                 0x0202
-- B3: Output Terminal Types
#define OTT_VENDOR_SPECIFIC                       0x0300
#define OTT_DISPLAY                               0x0301
#define OTT_MEDIA_TRANSPORT_OUTPUT                0x0302
-- B4: External Terminal Types
#define EXTERNAL_VENDOR_SPECIFIC                  0x0400
#define COMPOSITE_CONNECTOR                       0x0401
#define SVIDEO_CONNECTOR                          0x0402
#define COMPONENT_CONNECTOR                       0x0403

-- USB Video Class 1.0a / Payload_uncompressed
-- Table2-1: USB Video Payload uncompressed
-- YUY2 32595559-0000-0010-8000-00AA00389B71
-- NV12 3231564E-0000-0010-8000-00AA00389B71
guid_YUY2, guid_NV12 ∷ B.ByteString
guid_YUY2 = B.pack [ 0x32, 0x59, 0x55, 0x59
                   , 0x00, 0x00, 0x00, 0x10
                   , 0x80, 0x00, 0x00, 0xAA
                   , 0x00, 0x38, 0x9B, 0x71]

guid_NV12 = B.pack [ 0x32, 0x31, 0x56, 0x4E
                   , 0x00, 0x00, 0x00, 0x10
                   , 0x80, 0x00, 0x00, 0xAA
                   , 0x00, 0x38, 0x9B, 0x71]

-- Oki so things get strange because USB data seem to be provided with
-- the wrong bit-endianness (well only subpart of the guid are reversed,
-- which is even more confusing). For example, my webcam device is
-- returning (1) instead of (2) :
-- (1) YUY2 59555932-0000-1000-8000-00AA00389B71
-- (2) YUY2 32595559-0000-0010-8000-00AA00389B71
--
-- Note: this behavior is "normal", the first 8 bytes are in big-endian
-- notations, cf RFC 4122.
guid_YUY2', guid_NV12' ∷ B.ByteString
guid_YUY2' = B.pack [ 0x59, 0x55, 0x59, 0x32
                    , 0x00, 0x00, 0x10, 0x00
                    , 0x80, 0x00, 0x00, 0xAA
                    , 0x00, 0x38, 0x9B, 0x71]

guid_NV12' = B.pack [ 0x4E, 0x56, 0x31, 0x32
                    , 0x00, 0x00, 0x10, 0x00
                    , 0x80, 0x00, 0x00, 0xAA
                    , 0x00, 0x38, 0x9B, 0x71]

{----------------------------------------------------------------------
-- Abstracting the video function quite a long way away.
----------------------------------------------------------------------}

{----------------------------------------------------------------------
-- Start to get some isochronous fun.
----------------------------------------------------------------------}

-- Search a (stream) interface and select the correct alt-setting for
-- which the isochronous endpoint has a payload equal to xferSize.
findIsochronousAltSettings ∷ Interface → Int → Maybe InterfaceAltSetting
findIsochronousAltSettings iface xferSize =
    find p iface ≫= return ∘ interfaceAltSetting -- … in the Maybe monad.
  where
    -- ⋅ search the alt-iface endpoints for an isochronous one;
    -- ⋅ check its size is the right one;
    -- ⋅ otherwise, return False.
    -- Hint: the 'maybe' code must be read in reverse O_o
    p alt = maybe False
                  (\ep → xferSize ≡ epSize ep)
                  (find isIsoEndpoint (interfaceEndpoints alt))

    -- MaxPacketSize of an isochronous endpoint ≡ packet_size ⋅ x
    -- where x is the number of packets, i.e opportunity + 1.
    epSize ep = let MaxPacketSize x y = endpointMaxPacketSize ep
                in x ⋅ (1 + fromEnum y)

-- | Convert from units of 100 ns to 'threadDelay' microseconds.
waitFrameInterval ∷ Int → IO ()
waitFrameInterval t = threadDelay (t `div` 10)

isIsoEndpoint ∷ EndpointDesc → Bool
isIsoEndpoint ep = case endpointAttribs ep of
                     Isochronous _ _ → True
                     _               → False

{----------------------------------------------------------------------
-- Decoding uncompressed video frames.
----------------------------------------------------------------------}

-- | The first 32 bits is th source time clock in native device clock
-- units. The following 16 bits is a 1KHz SOF token counter.
-- See UVC specification 1.0a, table 2-6.
type SourceClockReference = (Word32, Word16)

data StreamHeader = StreamHeader !Int
                                 ![StreamHeaderFlag]
                                 !(Maybe Word32)
                                 !(Maybe SourceClockReference)
    deriving (Eq, Show, Data, Typeable)

data StreamHeaderFlag
   = OddFrame
   | EvenFrame
   | EndOfFrame
   | PTS
   | SCR
   | StillImage
   | ErrorBit
   | EndOfHeader
   deriving (Eq, Show, Data, Typeable)

stream_header_bitmask ∷ [(Int, StreamHeaderFlag)]
stream_header_bitmask =
    [ (0, OddFrame) -- yes, this is completely arbitrary.
    , (1, EndOfFrame)
    , (2, PTS)
    , (3, SCR)
    -- bit 4 is reserved.
    , (5, StillImage)
    , (6, ErrorBit)
    , (7, EndOfHeader)
    ]

unmarshalBFH ∷ Word8 → [StreamHeaderFlag]
unmarshalBFH w8 =
    let flags = unmarshalBitmask stream_header_bitmask w8
    -- If OddFrame is not present, add EvenFrame to the set.
    in if OddFrame ∈ flags
          then flags
          else EvenFrame : flags

-- See UVC specifications 1.0a, Table 2.2.
parseStreamHeader ∷ Get.Get StreamHeader
parseStreamHeader = do
    bLength ← fromIntegral `fmap` Get.getWord8
    bfh ← unmarshalBFH `fmap` Get.getWord8
    mPTS ← if PTS ∈ bfh
                then Just `fmap` Get.getWord32le
                else return Nothing
    mSCR ← if SCR ∈ bfh
                then do x0 ← Get.getWord32le
                        x1 ← Get.getWord16le
                        return $ Just (x0, x1)
                else return Nothing

    return $ StreamHeader bLength bfh mPTS mSCR

extractStreamHeader ∷ B.ByteString → StreamHeader
extractStreamHeader bs =
    let (Right h, _) = Get.runGet parseStreamHeader (B.take 12 bs)
    in h


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

isFormatUncompressed ∷ VSDescriptor → Bool
isFormatUncompressed (FormatUncompressed _ _ _ _ _ _ _ _ _) = True
isFormatUncompressed _                                      = False

isFrameUncompressed ∷ VSDescriptor → Bool
isFrameUncompressed (FrameUncompressed _ _ _ _ _ _ _ _ _) = True
isFrameUncompressed _                                     = False

simplestProbeCommitControl ∷ VideoDevice → ProbeCommitControl
simplestProbeCommitControl video =
    let descriptors = vsdStreamDescs ∘ head ∘ videoStrDescs $ video

        format = case find isFormatUncompressed descriptors of
                      Nothing → error "No FormatUncompressed descriptor !"
                      Just x  → x

        -- Select the frame with the minimum data payload, because I'm
        -- lazy.
        frames = filter isFrameUncompressed descriptors
        frame = minimumBy (compare `on` fuMinBitRate) frames

    in ProbeCommitControl
        { pcHint                   = [HintFrameInterval]
        , pcFormatIndex            = fuFormatIndex format
        , pcFrameIndex             = fuFrameIndex frame
        , pcFrameInterval          = fuDefaultFrameInterval frame
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
-- For now, we will just try to get the smallest data stream.
-- FIXME: handle more complex cases.
--
-- See UVC specifications 1.0a, Section 4.3.1.1 Video Prove and Commit
-- Control for more information.
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
    { pcHint                   ∷ ![ProbeHint]
    , pcFormatIndex            ∷ !Word8
    , pcFrameIndex             ∷ !Word8
    , pcFrameInterval          ∷ !Int
    , pcKeyFrameRate           ∷ !Int
    , pcPFrameRate             ∷ !Int
    , pcCompQuality            ∷ !Int
    , pcCompWindowSize         ∷ !Int
    , pcDelay                  ∷ !Int
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

probe_hint_bitmask ∷ [(Int, ProbeHint)]
probe_hint_bitmask =
   [ (0,  HintFrameInterval)
   , (1,  HintKeyFrameRate)
   , (2,  HintPFrameRate)
   , (3,  HintCompQuality)
   , (4,  HintCompWindowSize)
   ]

unmarshalProbeHint ∷ Bits α ⇒ α → [ProbeHint]
unmarshalProbeHint = unmarshalBitmask probe_hint_bitmask

marshalProbeHint ∷ [ProbeHint] → Word16
marshalProbeHint = marshalBitmask probe_hint_bitmask

lazyToStrict ∷ BL.ByteString → B.ByteString
lazyToStrict = B.concat ∘ BL.toChunks

extractPCControl ∷ ReleaseNumber → B.ByteString → Maybe ProbeCommitControl
extractPCControl release bs =
    case Get.runGet (parseVideoProbeCommitControl release) bs of
         (Left _, _)  → Nothing
         (Right x, _) → Just x

-- What ? "intract" is not an english word ? but who cares !
intractPCControl ∷ ReleaseNumber → ProbeCommitControl → B.ByteString
intractPCControl release ctrl =
    lazyToStrict $
        Put.runPut $
            unparseVideoProbeCommitControl release ctrl

unparseVideoProbeCommitControl ∷ ReleaseNumber → ProbeCommitControl → Put.Put
unparseVideoProbeCommitControl release value = do
    let bmHint                   = marshalProbeHint $ pcHint value
        bFormatIndex             = pcFormatIndex value
        bFrameIndex              = pcFrameIndex value
        dwFrameInterval          = fromIntegral $ pcFrameInterval value
        wKeyFrameRate            = fromIntegral $ pcKeyFrameRate value
        wPFrameRate              = fromIntegral $ pcPFrameRate value
        wCompQuality             = fromIntegral $ pcCompQuality value
        wCompWindowSize          = fromIntegral $ pcCompWindowSize value
        wDelay                   = fromIntegral $ pcDelay value
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
    dwFrameInterval ← fromIntegral `fmap` Get.getWord32le
    wKeyFrameRate ← fromIntegral `fmap` Get.getWord16le
    wPFrameRate ← fromIntegral `fmap` Get.getWord16le
    wCompQuality ← fromIntegral `fmap` Get.getWord16le
    wCompWindowSize ← fromIntegral `fmap` Get.getWord16le
    wDelay ← fromIntegral `fmap` Get.getWord16le
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



{----------------------------------------------------------------------
-- VideoDevice Handling.
----------------------------------------------------------------------}

unsafeOpenVideoDevice ∷ VideoDevice → IO DeviceHandle
unsafeOpenVideoDevice v = do
    devh ← openDevice device

    printf "Acquiring interface number %s\n" (show ctrl)
    ignoreNotFound $ detachKernelDriver devh ctrl
    claimInterface devh ctrl

    printf "Acquiring interface number %s\n" (show stream0)
    ignoreNotFound $ detachKernelDriver devh stream0
    claimInterface devh stream0

    return devh

  where
    device  = videoDevice v
    ctrl    = videoCtrlIface v
    stream0 = head ∘ videoStrIfaces $ v

unsafeCloseVideoDevice ∷ VideoDevice → DeviceHandle → IO ()
unsafeCloseVideoDevice v devh = do
    releaseInterface devh ctrl0
    ignoreNotFound $ attachKernelDriver devh ctrl0

    releaseInterface devh stream0
    ignoreNotFound $ attachKernelDriver devh stream0

    closeDevice devh

  where
    ctrl0   = videoCtrlIface v
    stream0 = head ∘ videoStrIfaces $ v

withVideoDeviceHandle ∷ VideoDevice → (DeviceHandle → IO α) → IO α
withVideoDeviceHandle v io =
    withDeviceHandle device $ \devh →
    withDetachedKernelDriver devh ctrl0 $
    withClaimedInterface devh ctrl0 $
    withDetachedKernelDriver devh stream0 $
    withClaimedInterface devh stream0 $
    io devh
  where
    device  = videoDevice v
    ctrl0   = videoCtrlIface v
    stream0 = head ∘ videoStrIfaces $ v

-- We ignore NotFoundException throwed by attach/detachKernalDriver when
-- no driver can be found.
ignoreNotFound ∷ IO () → IO ()
ignoreNotFound io = E.catch io handle
  where handle e = case e of
            NotFoundException → return ()
            _                 → E.throw e

{----------------------------------------------------------------------
-- Searching for a video device.
-- TODO: handle configurations.
-- Does a USB device really provide multiple configurations ?
----------------------------------------------------------------------}

-- Check if a device implements an UVC interface:
-- ⋅ scan every device available
-- ⋅ for every device, scan their configurations
-- ⋅ for every configuration, scan their interfaces
-- ⋅ for every interface alt-settings, check that interfaceClass ≡ Video
getVideoInterfaces ∷ Device → [Interface]
getVideoInterfaces dev =
    -- invoking []'s supernatural monad power.
    let ifaces = [deviceDesc dev]
              ≫= deviceConfigs
              ≫= configInterfaces
        isVIf  = any (\alt → interfaceClass alt ≡ videoClass)
    in filter isVIf ifaces
  where
    videoClass = CC_VIDEO

hasVideoInterface ∷ Device → Bool
hasVideoInterface = not ∘ null ∘ getVideoInterfaces

{----------------------------------------------------------------------
-- The VideoDevice Object and its interface association.
----------------------------------------------------------------------}

data VideoDevice = VideoDevice
    { videoDevice    ∷ Device
    , videoCtrlIface ∷ InterfaceNumber
    , videoStrIfaces ∷ [InterfaceNumber]
    , videoControl   ∷ Interface
    , videoStreams   ∷ [Interface]
    , videoCtrlDesc  ∷ VideoControlDesc
    , videoStrDescs  ∷ [VideoStreamingDesc]
    , videoName      ∷ Maybe String
    }
    deriving (Eq, Typeable)

isProcessingUnit ∷ ComponentDesc → Bool
isProcessingUnit (ProcessingUnitDesc _ _ _ _ _ _) = True
isProcessingUnit _                                = False

isCameraTerminal ∷ ComponentDesc → Bool
isCameraTerminal (CameraTerminalDesc _ _ _ _ _ _ _ _) = True
isCameraTerminal _                                    = False

isColorMatching ∷ VSDescriptor → Bool
isColorMatching (ColorMatchingDesc _ _ _) = True
isColorMatching _                         = False

instance Show VideoDevice where
    show video = printf "VideoDevice \"%s\" on {%s} \n\\
                        \%s\\
                        \%s"
                        name (show device)
                        vcontrol
                        streams
      where
        name     = maybe "NoName" id (videoName video)
        device   = videoDevice video

        -- Pretty printing the control interface.
        vcontrol ∷ String
        vcontrol = pcontrol ∘ videoCtrlDesc $ video
        pcontrol = \c → printf " +-- controlIface(iface=%d)\n\\
                               \%s\\
                               \%s"
                               (vcdInterfaceNumber c)
                               (punits c)
                               (pcameras c)

        -- Pretty printing the processing units.
        punits   ∷ VideoControlDesc → String
        punits   = concatMap ppunit
                 ∘ filter isProcessingUnit
                 ∘ vcdComponentDescs
        ppunit   = \p → printf " ----- ProcessingUnit[%d]\n\\
                               \%s"
                               (puId p)
                               (concatMap puctrl ∘ puControls $ p)
        puctrl   = \c → " --------- " ⧺ (show c) ⧺ "\n"

        -- Pretty printing the camera terminal.
        pcameras ∷ VideoControlDesc → String
        pcameras = concatMap pcamera
                 ∘ filter isCameraTerminal
                 ∘ vcdComponentDescs
        pcamera  = \c → printf " ----- CameraTerminal[%d]\n\\
                               \%s"
                               (ctId c)
                               (concatMap pcctrl ∘ ctControls $ c)
        pcctrl   = \c → " --------- " ⧺ (show c) ⧺ "\n"

        -- Pretty printing streaming interfaces.
        streams  ∷ String
        streams  = concatMap pstream ∘ videoStrDescs $ video
        pstream  = \s → printf " +-- streamIfaces[%d]\n\\
                               \%s"
                               (vsdInterfaceNumber s)
                               (formats s ⧺ frames s ⧺ colors s)

        -- Pretty printing uncompressed formats.
        formats  ∷ VideoStreamingDesc → String
        formats  = concatMap pformat
                 ∘ filter isFormatUncompressed
                 ∘ vsdStreamDescs
        pformat  = \f → printf " ----- FormatUncompressed[%d]\n\\
                               \ --------- format: %s\n\\
                               \ --------- bits-per-pixel: %d\n\\
                               \ --------- flags: %s\n"
                               (fuFormatIndex f)
                               (show $ fuFormat f)
                               (fuBitsPerPixel f)
                               (show $ fuInterlaceFlags f)

        -- Pretty printing uncompressed frames.
        frames   ∷ VideoStreamingDesc → String
        frames   = concatMap pframe
                 ∘ filter isFrameUncompressed
                 ∘ vsdStreamDescs
        pframe   = \f → printf " ----- FrameUncompressed[%d] @ %dx%d\n"
                               (fuFrameIndex f)
                               (fuWidth f)
                               (fuHeight f)

        -- Pretty printing color matching descriptors.
        colors   ∷ VideoStreamingDesc → String
        colors   = concatMap pcolor
                 ∘ filter isColorMatching
                 ∘ vsdStreamDescs
        pcolor   = \c → printf " ----- ColorMatching\n\\
                               \ --------- primaries: %s\n\\
                               \ --------- xfer characteristics: %s\n\\
                               \ --------- matrix coeffs.: %s\n"
                               (show $ cmColorPrimaries c)
                               (show $ cmTransferCharacteristics c)
                               (show $ cmMatrixCoefficients c)


-- | Search the Device configurations for an existing video interface
-- asssociation. If one is found, return its control and streaming
-- interfaces in a new VideoDevice.
getVideoDevice ∷ Device → IO VideoDevice
getVideoDevice dev = do
    -- First, search for a configuration containing a video interface
    -- association as described in USB Video Class 1.1, table 3-1.
    -- or USB Video Class 1.0a specs, section 3.7.
    let configs = deviceConfigs ∘ deviceDesc $ dev
    case find hasVideoInterfaceAssociation configs of

        Nothing     → error "This is not an UVC video device !"

        Just config → do
            -- TODO:
            -- should I move this thing in a 'withVideoDevice' function ?
            --
            -- Assure the right configuration is set.
            let videoConfigValue = Just ∘ configValue $ config
            currentConfig ← withDeviceHandle dev getConfig
            when (currentConfig ≠ videoConfigValue) $
                withDeviceHandle dev (flip setConfig videoConfigValue)

            -- Retrieve information.
            let extra             = configExtra config
                bFirstInterface   = B.index extra 2
                bInterfaceCount   = B.index extra 3
                iFunction         = unmarshalStrIx $ B.index extra 7

            -- Bonus: get the video function name, if any.
            mname ← withDeviceHandle dev $ flip getUSBString iFunction

            -- Return our (pretty) object.
            let ifaces   = configInterfaces config

                -- bFirstInterface is the control interface.
                vcontrol = ifaces !! fromIntegral bFirstInterface
                controlD = extractVideoControlDesc vcontrol

                -- bInterfaceCount counts the number of (contiguous)
                -- control AND streaming interfaces. So for streaming
                -- interfaces, the range is …
                range    = [bFirstInterface + 1 .. bInterfaceCount - 1]
                isStream = any (\alt → interfaceNumber alt ∈ range)
                streams  = filter isStream ifaces
                streamDs = map extractVideoStreamDesc streams

            -- Put some warning concerning the different UVC versions.
            when (vcdUVC controlD ≠ (0, 1, 0, 0)) $
                printf "\n\nWarning: this device uses a newer version of \\
                       \the USB Video Class specification, namely \\
                       \the %s one. It may not be supported.\n\n"
                       (show (vcdUVC controlD))

            return $ VideoDevice
                   { videoDevice    = dev
                   , videoCtrlIface = bFirstInterface
                   , videoStrIfaces = range
                   , videoControl   = vcontrol
                   , videoStreams   = streams
                   , videoCtrlDesc  = controlD
                   , videoStrDescs  = streamDs
                   , videoName      = mname
                   }

  where
    hasVideoInterfaceAssociation ∷ ConfigDesc → Bool
    hasVideoInterfaceAssociation config =
        let extra             = configExtra config
            bLength           = B.index extra 0
            bDescriptorType   = B.index extra 1
            bFunctionClass    = B.index extra 4
            bFunctionSubClass = B.index extra 5
            bFunctionProtocol = B.index extra 6

        in bLength           ≡ 8
         ∧ bDescriptorType   ≡ INTERFACE_ASSOCIATION
         ∧ bFunctionClass    ≡ CC_VIDEO
         ∧ bFunctionSubClass ≡ SC_VIDEO_INTERFACE_COLLECTION
         ∧ bFunctionProtocol ≡ PC_PROTOCOL_UNDEFINED

{----------------------------------------------------------------------
-- Prelude to binary(-strict) parsing.
----------------------------------------------------------------------}

-- | A wrapper around Get parser to check if every (bLength) bytes have
-- been read, and flush the buffer when needed.
safeBoundParser ∷ Int → Get.Get a → Get.Get a
safeBoundParser size parser = do
    start  ← Get.bytesRead
    result ← parser
    end    ← Get.bytesRead

    let remainingBytes = size - end + start
    if remainingBytes ≠ 0
       then Get.skip remainingBytes
       else return ()

    return result

{----------------------------------------------------------------------
-- VideoControl Interface Descriptors.
----------------------------------------------------------------------}

-- See USB Video Class 1.0a specifications, section 3.7.

-- XXX: data structure not used.
-- VideoControl:
-- ⋅ control endpoint @ 0x00
-- ⋅ status endpoint optional
-- ⋅ alternate settings ≡ [ 0x00 ]
data VideoControl = VideoControl
   { vcControlEndpoint ∷ EndpointDesc
   , vcStatusEndpoint  ∷ (Maybe EndpointDesc)
   , vcDescriptor      ∷ VideoControlDesc
   } deriving (Eq, Show, Data, Typeable)

data VideoControlDesc = VideoControlDesc
    { vcdUVC             ∷ ReleaseNumber
    , vcdClockFrequency  ∷ Int -- in Hertz (deprecated in the specs).
    , vcdInCollection    ∷ Int
    , vcdInterfaces      ∷ [Int]
    , vcdComponentDescs  ∷ [ComponentDesc]
    , vcdInterfaceNumber ∷ InterfaceNumber
   } deriving (Eq, Show, Data, Typeable)

-- | Compononent := Terminal ∨ Unit
type ComponentID = Word8

data ComponentDesc
   = UnknownDesc !Int !Word8
   | SelectorUnitDesc
       { suId                      ∷ !ComponentID
       , suNrInPins                ∷ !Int
       , suSourceID                ∷ ![ComponentID]
       , suSelector                ∷ !(Maybe StrIx)
       }
   | ProcessingUnitDesc
       { puId                      ∷ !ComponentID
       , puSourceID                ∷ !ComponentID
       , puMaxMultiplier           ∷ !Int
       , puControls                ∷ ![ProcessingControl]
       , puVideoStandards          ∷ ![VideoStandard]
       , puProcessing              ∷ !(Maybe StrIx)
       }
   | ExtensionUnitDesc
       { xuId                      ∷ !ComponentID
       , xuNrInPins                ∷ !Int
       , xuSourceID                ∷ ![ComponentID]
       , xuExtension               ∷ !(Maybe StrIx)
       }
   | CameraTerminalDesc
       { ctId                      ∷ !ComponentID
       , ctType                    ∷ !TerminalType
       , ctAssociatedTerminal      ∷ !ComponentID
       , ctTerminal                ∷ !(Maybe StrIx)
       , ctObjectiveFocalLengthMin ∷ !Int
       , ctObjectiveFocalLengthMax ∷ !Int
       , ctOcularFocalLength       ∷ !Int
       , ctControls                ∷ ![CameraControl]
       }
   | InputTerminalDesc
       { itId                      ∷ !ComponentID
       , itType                    ∷ !TerminalType
       , itAssociatedTerminal      ∷ !ComponentID
       , itTerminal                ∷ !(Maybe StrIx)
       , itExtra                   ∷ !B.ByteString
       }
   | OutputTerminalDesc
       { otId                      ∷ !ComponentID
       , otType                    ∷ !TerminalType
       , otAssociatedTerminal      ∷ !ComponentID
       , otSourceID                ∷ !ComponentID
       , otTerminal                ∷ !(Maybe StrIx)
       , otExtra                   ∷ !B.ByteString
       }
   deriving (Eq, Show, Data, Typeable)

data TerminalType
   = TerminalVendorSpecific
   | TerminalStreaming
   | TerminalVendorSpecificInput
   | TerminalCamera
   | TerminalMediaTransportInput
   | TerminalVendorSpecificOutput
   | TerminalDisplay
   | TerminalMediaTransportOutput
   | TerminalVendorSpecificExternal
   | TerminalCompositeConnector
   | TerminalSVideoConnector
   | TerminalComponentConnector
   deriving (Eq, Show, Data, Typeable)

unmarshalTerminalType ∷ Word16 → TerminalType
unmarshalTerminalType w16 =
    case w16 of
         TT_VENDOR_SPECIFIC         → TerminalVendorSpecific
         TT_STREAMING               → TerminalStreaming
         ITT_VENDOR_SPECIFIC        → TerminalVendorSpecificInput
         ITT_CAMERA                 → TerminalCamera
         ITT_MEDIA_TRANSPORT_INPUT  → TerminalMediaTransportInput
         OTT_VENDOR_SPECIFIC        → TerminalVendorSpecificOutput
         OTT_DISPLAY                → TerminalDisplay
         OTT_MEDIA_TRANSPORT_OUTPUT → TerminalMediaTransportOutput
         EXTERNAL_VENDOR_SPECIFIC   → TerminalVendorSpecificExternal
         COMPOSITE_CONNECTOR        → TerminalCompositeConnector
         SVIDEO_CONNECTOR           → TerminalSVideoConnector
         COMPONENT_CONNECTOR        → TerminalComponentConnector
         _ → error "Unknown terminal type"

data CameraControl
   = CameraScanningMode
   | CameraAutoExposureMode
   | CameraAutoExposurePriority
   | CameraExposureTimeAbsolute
   | CameraExposureTimeRelative
   | CameraFocusAbsolute
   | CameraFocusRelative
   | CameraIrisAbsolute
   | CameraIrisRelative
   | CameraZoomAbsolute
   | CameraZoomRelative
   | CameraPanTiltAbsolute
   | CameraPanTiltRelative
   | CameraRollAbsolute
   | CameraRollRelative
   | CameraFocusAuto
   deriving (Eq, Show, Data, Typeable)

camera_control_bitmask ∷ [(Int, CameraControl)]
camera_control_bitmask =
   [ (0,  CameraScanningMode)
   , (1,  CameraAutoExposureMode)
   , (2,  CameraAutoExposurePriority)
   , (3,  CameraExposureTimeAbsolute)
   , (4,  CameraExposureTimeRelative)
   , (5,  CameraFocusAbsolute)
   , (6,  CameraFocusRelative)
   , (7,  CameraIrisAbsolute)
   , (8,  CameraIrisRelative)
   , (9,  CameraZoomAbsolute)
   , (10, CameraZoomRelative)
   , (11, CameraPanTiltAbsolute)
   , (12, CameraPanTiltRelative)
   , (13, CameraRollAbsolute)
   , (14, CameraRollRelative)
   , (17, CameraFocusAuto)
   ]

unmarshalCameraControl ∷ Bits α ⇒ α → [CameraControl]
unmarshalCameraControl = unmarshalBitmask camera_control_bitmask

data ProcessingControl
   = ControlBrightness
   | ControlContrast
   | ControlHue
   | ControlSaturation
   | ControlSharpness
   | ControlGamma
   | ControlWhiteBalanceTemperature
   | ControlWhiteBalanceComponent
   | ControlBacklightCompensation
   | ControlGain
   | ControlPowerLineFrequency
   | ControlHueAuto
   | ControlWhiteBalanceTemperatureAuto
   | ControlWhiteBalanceComponentAuto
   | ControlDigitalMultiplier
   | ControlDigitalMultiplierLimit
   | ControlAnalogVideoStandard
   | ControlAnalogVideoLockStatus
   deriving (Eq, Show, Data, Typeable)

processing_control_bitmask ∷ [(Int, ProcessingControl)]
processing_control_bitmask =
    [ (0,  ControlBrightness)
    , (1,  ControlContrast)
    , (2,  ControlHue)
    , (3,  ControlSaturation)
    , (4,  ControlSharpness)
    , (5,  ControlGamma)
    , (6,  ControlWhiteBalanceTemperature)
    , (7,  ControlWhiteBalanceComponent)
    , (8,  ControlBacklightCompensation)
    , (9,  ControlGain)
    , (10, ControlPowerLineFrequency)
    , (11, ControlHueAuto)
    , (12, ControlWhiteBalanceTemperatureAuto)
    , (13, ControlWhiteBalanceComponentAuto)
    , (14, ControlDigitalMultiplier)
    , (15, ControlDigitalMultiplierLimit)
    , (16, ControlAnalogVideoStandard)
    , (17, ControlAnalogVideoLockStatus)
    ]

unmarshalProcessingControls ∷ Bits α ⇒ α → [ProcessingControl]
unmarshalProcessingControls = unmarshalBitmask processing_control_bitmask

data VideoStandard
   = VideoNone
   | VideoNTSC_525_60
   | VideoPAL_625_50
   | VideoSECAM_625_50
   | VideoNTSC_625_50
   | VideoPAL_525_60
   deriving (Eq, Show, Data, Typeable)

video_standards_bitmask ∷ [(Int, VideoStandard)]
video_standards_bitmask =
    [ (0, VideoNone)
    , (1, VideoNTSC_525_60)
    , (2, VideoPAL_625_50)
    , (3, VideoSECAM_625_50)
    , (4, VideoNTSC_625_50)
    , (5, VideoPAL_525_60)
    ]

unmarshalVideoStandards ∷ Bits α ⇒ α → [VideoStandard]
unmarshalVideoStandards = unmarshalBitmask video_standards_bitmask

extractVideoControlDesc ∷ Interface → VideoControlDesc
extractVideoControlDesc iface =
    let Just alternate = find (not ∘ B.null ∘ interfaceExtra) iface
        extra = interfaceExtra alternate
        number = interfaceNumber alternate
        (Right controlDesc, _) = Get.runGet (parseVCHeader number) extra
    in controlDesc

parseVCHeader ∷ InterfaceNumber → Get.Get VideoControlDesc
parseVCHeader ifaceNumber = do
    _                  ← Get.getWord8 -- skip bLength
    bDescriptorType    ← Get.getWord8
    bDescriptorSubType ← Get.getWord8
    bcdUVC             ← unmarshalReleaseNumber `fmap` Get.getWord16le
    _                  ← Get.getWord16le -- skip wTotalLength
    dwClockFrequency   ← fromIntegral `fmap` Get.getWord32le
    bInCollection      ← fromIntegral `fmap` Get.getWord8
    baInterfaceNrs     ← replicateM bInCollection
                                    (fromIntegral `fmap` Get.getWord8)

    components ← parseVCComponents []

    if bDescriptorType    ≠ CS_INTERFACE
     ∨ bDescriptorSubType ≠ VC_HEADER
     then error "This is not a VideoControl interface descriptor"
     else return $ VideoControlDesc
                { vcdUVC             = bcdUVC
                , vcdClockFrequency  = dwClockFrequency
                , vcdInCollection    = bInCollection
                , vcdInterfaces      = baInterfaceNrs
                , vcdComponentDescs  = components
                , vcdInterfaceNumber = ifaceNumber
                }

parseVCComponents ∷ [ComponentDesc] → Get.Get [ComponentDesc]
parseVCComponents acc = do
    atEnd ← Get.isEmpty
    if atEnd
      then return $ reverse acc
      else do bLength ← fromIntegral `fmap` Get.getWord8
              bDescriptorType ← Get.getWord8
              bDescriptorSubType ← Get.getWord8

              when (bDescriptorType ≠ CS_INTERFACE) $
                   error "This is not a video unit/terminal descriptor."

               -- remaining size = length - bLength - bDescriptor{,Sub}Type
              let size = bLength - 1 - 1 - 1

              x ← case bDescriptorSubType of
                    VC_INPUT_TERMINAL  → parseInputTerminalDesc  size
                    VC_OUTPUT_TERMINAL → parseOutputTerminalDesc size
                    VC_SELECTOR_UNIT   → parseSelectorUnitDesc   size
                    VC_PROCESSING_UNIT → parseProcessingUnitDesc size
                    VC_EXTENSION_UNIT  → parseExtensionUnitDesc  size
                    _ → parseDummyDesc size bDescriptorSubType

              parseVCComponents (x:acc) -- loop

-- | Default fallback parser.
parseDummyDesc ∷ Int → Word8 → Get.Get ComponentDesc
parseDummyDesc bLength bDescriptorSubType =
  safeBoundParser (bLength - 1 - 1 - 1) $ do
    return $ UnknownDesc bLength bDescriptorSubType

parseInputTerminalDesc  ∷ Int → Get.Get ComponentDesc
parseInputTerminalDesc size =
  safeBoundParser size $ do
    bTerminalID    ← Get.getWord8
    wTerminalType  ← unmarshalTerminalType `fmap` Get.getWord16le
    bAssocTerminal ← Get.getWord8
    iTerminal      ← unmarshalStrIx `fmap` Get.getWord8

    case wTerminalType of
        TerminalCamera → do
            wObjectiveFocalLengthMin ← fromIntegral `fmap` Get.getWord16le
            wObjectiveFocalLengthMax ← fromIntegral `fmap` Get.getWord16le
            wOcularFocalLength ← fromIntegral `fmap` Get.getWord16le
            bControlSize ← fromIntegral `fmap` Get.getWord8
            -- TODO: handle bmControls where size > 2
            bmControls ← unmarshalCameraControl `fmap` Get.getWord16le
            Get.skip (bControlSize - 2)

            return $ CameraTerminalDesc
                   { ctId                      = bTerminalID
                   , ctType                    = wTerminalType
                   , ctAssociatedTerminal      = bAssocTerminal
                   , ctTerminal                = iTerminal
                   , ctObjectiveFocalLengthMin = wObjectiveFocalLengthMin
                   , ctObjectiveFocalLengthMax = wObjectiveFocalLengthMax
                   , ctOcularFocalLength       = wOcularFocalLength
                   , ctControls                = bmControls
                   }

        -- if Not ITT_CAMERA, then …
        _ → do
            extra ← Get.getByteString (size - 5)
            return $ InputTerminalDesc
                   { itId                 = bTerminalID
                   , itType               = wTerminalType
                   , itAssociatedTerminal = bAssocTerminal
                   , itTerminal           = iTerminal
                   , itExtra              = extra
                   }

parseOutputTerminalDesc ∷ Int → Get.Get ComponentDesc
parseOutputTerminalDesc size =
  safeBoundParser size $ do
    bTerminalID    ← Get.getWord8
    wTerminalType  ← unmarshalTerminalType `fmap` Get.getWord16le
    bAssocTerminal ← Get.getWord8
    bSourceID      ← Get.getWord8
    iTerminal      ← unmarshalStrIx `fmap` Get.getWord8
    extra          ← Get.getByteString (size - 6)

    return $ OutputTerminalDesc
           { otId                 = bTerminalID
           , otType               = wTerminalType
           , otAssociatedTerminal = bAssocTerminal
           , otSourceID           = bSourceID
           , otTerminal           = iTerminal
           , otExtra              = extra
           }

parseSelectorUnitDesc ∷ Int → Get.Get ComponentDesc
parseSelectorUnitDesc size =
  safeBoundParser size $ do
    bUnitID     ← Get.getWord8
    bNrInPins   ← fromIntegral `fmap` Get.getWord8
    baSourceIDs ← replicateM bNrInPins Get.getWord8
    iSelector   ← unmarshalStrIx `fmap` Get.getWord8

    return $ SelectorUnitDesc
           { suId       = bUnitID
           , suNrInPins = bNrInPins
           , suSourceID = baSourceIDs
           , suSelector = iSelector
           }

parseProcessingUnitDesc ∷ Int → Get.Get ComponentDesc
parseProcessingUnitDesc size =
  safeBoundParser size $ do
    bUnitID        ← Get.getWord8
    baSourceID     ← Get.getWord8
    wMaxMultiplier ← fromIntegral `fmap` Get.getWord16le
    bControlSize   ← fromIntegral `fmap` Get.getWord8

    -- TODO: handle bmControls size bigger than 16bits.
    bmControls ← unmarshalProcessingControls `fmap` Get.getWord16le
    Get.skip (bControlSize - 2)

    iProcessing ← unmarshalStrIx `fmap` Get.getWord8
    bmVideoStandards ← if (6 + bControlSize) < size
                            then unmarshalVideoStandards `fmap` Get.getWord8
                              -- the UVC standard 1.0a did not define
                              -- this field. Ignoring it.
                            else return []

    return $ ProcessingUnitDesc
           { puId             = bUnitID
           , puSourceID       = baSourceID
           , puMaxMultiplier  = wMaxMultiplier
           , puControls       = bmControls
           , puVideoStandards = bmVideoStandards
           , puProcessing     = iProcessing
           }

parseExtensionUnitDesc ∷ Int → Get.Get ComponentDesc
parseExtensionUnitDesc size =
  safeBoundParser size $ do
    bUnitID      ← Get.getWord8
    Get.skip (16 + 1) -- skipping guidExtensionCode & bNumControls.
    bNrInPins    ← fromIntegral `fmap` Get.getWord8
    baSourceIDs  ← replicateM bNrInPins Get.getWord8
    bControlSize ← fromIntegral `fmap` Get.getWord8
    Get.skip bControlSize -- skipping extension unit bmControls.
    iExtension   ← unmarshalStrIx `fmap` Get.getWord8

    return $ ExtensionUnitDesc
           { xuId        = bUnitID
           , xuNrInPins  = bNrInPins
           , xuSourceID  = baSourceIDs
           , xuExtension = iExtension
           }

{----------------------------------------------------------------------
-- Video Endpoint Descriptor.
----------------------------------------------------------------------}

-- not implemented !
-- because I don't care™.

{----------------------------------------------------------------------
-- VideoStreaming Interface Descriptors.
----------------------------------------------------------------------}

-- See USB Video Class 1.0a specifications, section 3.9.

data VideoStreamingDesc = VideoStreamingDesc
    { vsdStreamDescs     ∷ [VSDescriptor]
    , vsdInterfaceNumber ∷ InterfaceNumber
    } deriving (Eq, Show, Data, Typeable)

data VSDescriptor
   = UnknownFrameOrFormat !Int !Word8
   | InputHeaderDesc
           { ihNumFormats              ∷ !Int
           , ihTotalLength             ∷ !Int
           , ihEndpointAddress         ∷ !EndpointAddress
           , ihInfo                    ∷ ![VSCapability]
           , ihTerminalLink            ∷ !Word8
           , ihStillCaptureMethod      ∷ !Int
           , ihTriggerSupport          ∷ !Bool
           , ihTriggerUsage            ∷ !TriggerUsage
           , ihControls                ∷ ![[VSControl]]
           }
   | OutputHeaderDesc
           { ohNumFormats              ∷ !Int
           , ohTotalLength             ∷ !Int
           , ohEndpointAddress         ∷ !EndpointAddress
           , ohTerminalLink            ∷ !Word8
           }
   | FormatUncompressed
           { fuFormatIndex             ∷ !Word8
           , fuNumFrameDescriptors     ∷ !Int
           , fuFormat                  ∷ !CompressionFormat
           , fuBitsPerPixel            ∷ !Int
           , fuDefaultFrameIndex       ∷ !Word8
           , fuAspectRatioX            ∷ !Int
           , fuAspectRatioY            ∷ !Int
           , fuInterlaceFlags          ∷ ![InterlaceFlag]
           , fuCopyProtect             ∷ !Bool
           }
   | FrameUncompressed
           { fuFrameIndex              ∷ !Word8
           , fuStillImageSupported     ∷ !Bool
           , fuWidth                   ∷ !Int
           , fuHeight                  ∷ !Int
           , fuMinBitRate              ∷ !Int
           , fuMaxBitRate              ∷ !Int
           , fuMaxVideoFrameBufferSize ∷ !Int
           , fuDefaultFrameInterval    ∷ !Int
           , fuFrameIntervalType       ∷ !FrameIntervalType
           }
   | ColorMatchingDesc
           { cmColorPrimaries          ∷ !ColorPrimaries
           , cmTransferCharacteristics ∷ !TransferCharacteristics
           , cmMatrixCoefficients      ∷ !MatrixCoefficients
           }
   deriving (Eq, Show, Data, Typeable)

data VSCapability
   = DynamicFormatChangeSupported
   deriving (Eq, Show, Data, Typeable)

vs_capability_bitmask ∷ [(Int, VSCapability)]
vs_capability_bitmask =
   [ (0,  DynamicFormatChangeSupported)
   ]

unmarshalVSCapability ∷ Bits α ⇒ α → [VSCapability]
unmarshalVSCapability = unmarshalBitmask vs_capability_bitmask

unmarshalTriggerSupport ∷ Word8 → Bool
unmarshalTriggerSupport = (0x01 ≡)

data TriggerUsage
   = InitiateStillImageCapture
   | GeneralPurposeButton
   deriving (Eq, Show, Data, Typeable)

unmarshalTriggerUsage ∷ Word8 → TriggerUsage
unmarshalTriggerUsage 0x01 = GeneralPurposeButton
unmarshalTriggerUsage _    = InitiateStillImageCapture

data VSControl
   = Keyframerate
   | PFramerate
   | CompressionQuality
   | CompressionWindowsize
   | GenerateKeyFrame
   | UpdateFrameSegment
   deriving (Eq, Show, Data, Typeable)

vs_control_bitmask ∷ [(Int, VSControl)]
vs_control_bitmask =
    [ (0, Keyframerate)
    , (1, PFramerate)
    , (2, CompressionQuality)
    , (3, CompressionWindowsize)
    , (4, GenerateKeyFrame)
    , (5, UpdateFrameSegment)
    ]

unmarshalVSControl ∷ Bits α ⇒ α → [VSControl]
unmarshalVSControl = unmarshalBitmask vs_control_bitmask

data CompressionFormat
   = UnknownCompressionFormat B.ByteString
   | YUY2
   | NV12
   deriving (Eq, Show, Data, Typeable)

unmarshalGuidFormat ∷ B.ByteString → CompressionFormat
unmarshalGuidFormat bs | bs ≡ guid_YUY2  = YUY2
                       | bs ≡ guid_YUY2' = YUY2
                       | bs ≡ guid_NV12  = NV12
                       | bs ≡ guid_NV12' = NV12
                       | otherwise       = UnknownCompressionFormat bs

data InterlaceFlag
   = Interlaced
   | FieldsPerFrame Int
   | Field1First
   | Field1Only
   | Field2Only
   | RegularField1And2
   | RandomField1And2
   | DisplayBobOnly
   | DisplayWeaveOnly
   | DisplayBobOrWeave
   deriving (Eq, Show, Data, Typeable)

-- On the road [to bitmask parsing] again, yehaaa ... wait no ;(
unmarshalInterlaceFlags ∷ Word8 → [InterlaceFlag]
unmarshalInterlaceFlags value = catMaybes [ testInterlaced
                                          , testFieldsPerFrame
                                          , testField1First
                                          , testFieldPattern
                                          , testDisplayMode ]

  where testInterlaced     = if value `testBit` 0
                                then Just Interlaced
                                else Nothing

        testFieldsPerFrame = if value `testBit` 1
                                then Just (FieldsPerFrame 1)
                                else Just (FieldsPerFrame 2)

        testField1First    = if value `testBit` 2
                                then Just Field1First
                                else Nothing

        testFieldPattern   = case bits 4 5 value of
                                  0x00 → Just Field1Only
                                  0x01 → Just Field2Only
                                  0x02 → Just RegularField1And2
                                  _    → Just RandomField1And2

        testDisplayMode    = case bits 6 7 value of
                                  0x00 → Just DisplayBobOnly
                                  0x01 → Just DisplayWeaveOnly
                                  _    → Just DisplayBobOrWeave

unmarshalUncompressedCapabilities ∷ Word8 → Bool
unmarshalUncompressedCapabilities = (`testBit` 0)

data FrameIntervalType
   = FrameIntervalContinous
        { fitMin  ∷ !Int
        , fitMax  ∷ !Int
        , fitStep ∷ !Int
        }
   | FrameIntervalDiscrete ![Int]
   deriving (Eq, Show, Data, Typeable)

data ColorPrimaries
   = Color_Unknown
   | Color_BT709_sRGB
   | Color_BT470_2M
   | Color_BT470_2BG
   | Color_SMPTE_170M
   | Color_SMPTE_240M
   deriving (Eq, Show, Enum, Data, Typeable)

unmarshalColorPrimaries ∷ Word8 → ColorPrimaries
unmarshalColorPrimaries = genToEnum

data TransferCharacteristics
   = TransferCharacteristicUnknown
   | TransferCharacteristic_BT709
   | TransferCharacteristic_BT470_2M
   | TransferCharacteristic_BT470_2BG
   | TransferCharacteristic_SMPTE_170M
   | TransferCharacteristic_SMPTE_240M
   | TransferCharacteristic_Linear
   | TransferCharacteristic_sRGB
   deriving (Eq, Show, Enum, Data, Typeable)

unmarshalTCharacteristics ∷ Word8 → TransferCharacteristics
unmarshalTCharacteristics = genToEnum

data MatrixCoefficients
   = Matrix_Unknown
   | Matrix_BT709
   | Matrix_FCC
   | Matrix_BT470_BG
   | Matrix_SMPTE_170M_BT601
   | Matrix_SMPTE_240M
   deriving (Eq, Show, Enum, Data, Typeable)

unmarshalMatrixCoefficients ∷ Word8 → MatrixCoefficients
unmarshalMatrixCoefficients = genToEnum

extractVideoStreamDesc ∷ Interface → VideoStreamingDesc
extractVideoStreamDesc iface =
    let Just alternate = find (not ∘ B.null ∘ interfaceExtra) iface
        extra = interfaceExtra alternate
        number = interfaceNumber alternate
        (Right xs, _) = Get.runGet (parseVideoStreamDescs []) extra
    in VideoStreamingDesc xs number

-- | Read the extra information of the video streaming interface
-- descriptor and extract every sub-video streaming descriptors.
parseVideoStreamDescs ∷ [VSDescriptor] → Get.Get [VSDescriptor]
parseVideoStreamDescs acc = do
    atEnd ← Get.isEmpty
    if atEnd
       then return $ reverse acc
       else do bLength ← fromIntegral `fmap` Get.getWord8
               bDescriptorType ← Get.getWord8
               bDescriptorSubType ← Get.getWord8

               when (bDescriptorType ≠ CS_INTERFACE) $
                 error "This is not a Video Streaming interface."

               -- remaining size = length - bLength - bDescriptor{,Sub}Type
               let size = bLength - 1 - 1 - 1

               x ← case bDescriptorSubType of
                     VS_OUTPUT_HEADER       → parseVSOutputHeader       size
                     VS_INPUT_HEADER        → parseVSInputHeader        size
                     VS_FORMAT_UNCOMPRESSED → parseVSFormatUncompressed size
                     VS_FRAME_UNCOMPRESSED  → parseVSFrameUncompressed  size
                     VS_COLORFORMAT         → parseVSColorMatching      size
                     _ → parseUnknownFrameOrFormat bLength bDescriptorSubType

               parseVideoStreamDescs (x:acc) -- loop recursion

-- | Default fallback parser.
parseUnknownFrameOrFormat ∷ Int → Word8 → Get.Get VSDescriptor
parseUnknownFrameOrFormat bLength bDescriptorSubType =
  safeBoundParser (bLength - 1 - 1 - 1) $ do
    return $ UnknownFrameOrFormat bLength bDescriptorSubType

-- | Read Uncompressed Video Format Descriptors as specified
-- in USB Video Class 1.0a / Payload_uncompressed, table 3-1.
parseVSFormatUncompressed ∷ Int → Get.Get VSDescriptor
parseVSFormatUncompressed size =
  safeBoundParser size $ do
    bFormatIndex ← Get.getWord8
    bNumFrameDescriptors ← fromIntegral `fmap` Get.getWord8
    guidFormat ← unmarshalGuidFormat `fmap` Get.getByteString 16
    bBitsPerPixel ← fromIntegral `fmap` Get.getWord8
    bDefaultFrameIndex ← Get.getWord8
    bAspectRatioX ← fromIntegral `fmap` Get.getWord8
    bAspectRatioY ← fromIntegral `fmap` Get.getWord8
    bmInterlaceFlags ← unmarshalInterlaceFlags `fmap` Get.getWord8
    bCopyProtect ← (≡ 0x01) `fmap` Get.getWord8

    return $ FormatUncompressed
           { fuFormatIndex         = bFormatIndex
           , fuNumFrameDescriptors = bNumFrameDescriptors
           , fuFormat              = guidFormat
           , fuBitsPerPixel        = bBitsPerPixel
           , fuDefaultFrameIndex   = bDefaultFrameIndex
           , fuAspectRatioX        = bAspectRatioX
           , fuAspectRatioY        = bAspectRatioY
           , fuInterlaceFlags      = bmInterlaceFlags
           , fuCopyProtect         = bCopyProtect
           }

-- | Read Uncompressed Video Frame Descriptors as specified
-- in USB Video Class 1.0a / Payload_uncompressed, table 3-2.
parseVSFrameUncompressed ∷ Int → Get.Get VSDescriptor
parseVSFrameUncompressed size =
  safeBoundParser size $ do
    bFrameIndex ← Get.getWord8
    bmCapabilities ← unmarshalUncompressedCapabilities `fmap` Get.getWord8
    wWidth ← fromIntegral `fmap` Get.getWord16le
    wHeight ← fromIntegral `fmap` Get.getWord16le
    dwMinBitRate ← fromIntegral `fmap` Get.getWord32le
    dwMaxBitRate ← fromIntegral `fmap` Get.getWord32le
    dwMaxVideoFrameBufferSize ← fromIntegral `fmap` Get.getWord32le
    dwDefaultFrameInterval ← fromIntegral `fmap` Get.getWord32le
    bFrameIntervalType ← fromIntegral `fmap` Get.getWord8

    frameType ← if bFrameIntervalType ≡ 0
       then do min'  ← fromIntegral `fmap` Get.getWord32le
               max'  ← fromIntegral `fmap` Get.getWord32le
               step' ← fromIntegral `fmap` Get.getWord32le
               return $ FrameIntervalContinous min' max' step'

       else do xs ← replicateM bFrameIntervalType
                               (fromIntegral `fmap` Get.getWord32le)
               return $ FrameIntervalDiscrete xs

    return $ FrameUncompressed
           { fuFrameIndex              = bFrameIndex
           , fuStillImageSupported     = bmCapabilities
           , fuWidth                   = wWidth
           , fuHeight                  = wHeight
           , fuMinBitRate              = dwMinBitRate
           , fuMaxBitRate              = dwMaxBitRate
           , fuMaxVideoFrameBufferSize = dwMaxVideoFrameBufferSize
           , fuDefaultFrameInterval    = dwDefaultFrameInterval
           , fuFrameIntervalType       = frameType
           }

-- | Read VS Interface Input Header Descriptors as specified
-- in USB Video Class 1.0a, table 3-13.
parseVSInputHeader ∷ Int → Get.Get VSDescriptor
parseVSInputHeader size =
  safeBoundParser size $ do
    bNumFormats ← fromIntegral `fmap` Get.getWord8
    wTotalLength ← fromIntegral `fmap` Get.getWord16le
    bEndpointAddress ← unmarshalEndpointAddress `fmap` Get.getWord8
    bmInfo ← unmarshalVSCapability `fmap` Get.getWord8
    bTerminalLink ← Get.getWord8
    bStillCaptureMethod ← fromIntegral `fmap` Get.getWord8
    bTriggerSupport ← unmarshalTriggerSupport `fmap` Get.getWord8
    bTriggerUsage ← unmarshalTriggerUsage `fmap` Get.getWord8
    bControlSize ← fromIntegral `fmap` Get.getWord8
    bmaControls ← replicateM bNumFormats $ do
                        bitmask ← unmarshalVSControl `fmap` Get.getWord8
                        Get.skip (bControlSize - 1) -- skipping extra bm bits
                        return bitmask

    return $ InputHeaderDesc
           { ihNumFormats         = bNumFormats
           , ihTotalLength        = wTotalLength
           , ihEndpointAddress    = bEndpointAddress
           , ihInfo               = bmInfo
           , ihTerminalLink       = bTerminalLink
           , ihStillCaptureMethod = bStillCaptureMethod
           , ihTriggerSupport     = bTriggerSupport
           , ihTriggerUsage       = bTriggerUsage
           , ihControls           = bmaControls
           }


-- | Read VS Interface Output Header Descriptors as specified
-- in USB Video Class 1.0a, table 3-14.
parseVSOutputHeader ∷ Int → Get.Get VSDescriptor
parseVSOutputHeader size =
  safeBoundParser size $ do

    bNumFormats ← fromIntegral `fmap` Get.getWord8
    wTotalLength ← fromIntegral `fmap` Get.getWord16le
    bEndpointAddress ← unmarshalEndpointAddress `fmap` Get.getWord8
    bTerminalLink ← Get.getWord8

    return $ OutputHeaderDesc
           { ohNumFormats         = bNumFormats
           , ohTotalLength        = wTotalLength
           , ohEndpointAddress    = bEndpointAddress
           , ohTerminalLink       = bTerminalLink
           }

-- | Read VS Interface Color Matching Descriptors as specified
-- in USB Video Class 1.0a, table 3-18.
parseVSColorMatching ∷ Int → Get.Get VSDescriptor
parseVSColorMatching size =
  safeBoundParser size $ do
    bColorPrimaries ← unmarshalColorPrimaries `fmap` Get.getWord8
    bTransferCharacteristics ← unmarshalTCharacteristics `fmap` Get.getWord8
    bMatrixCoefficients ← unmarshalMatrixCoefficients `fmap` Get.getWord8


    return $ ColorMatchingDesc
           { cmColorPrimaries          = bColorPrimaries
           , cmTransferCharacteristics = bTransferCharacteristics
           , cmMatrixCoefficients      = bMatrixCoefficients
           }
