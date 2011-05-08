{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}

-- Ho man ! How much would I like to have a library in Haskell similar
-- to Common-Lisp's gigamonkeys binary-data.

module System.USB.UVC.Descriptors
    ( VideoDevice(..)
    , videoDescription
    , hasVideoInterface

    -- * Common types
    , Width
    , Height
    , FormatIndex
    , FrameIndex
    , FrameInterval
    , GUID(..)

    -- * VideoStreaming Interface Descriptors
    --, extractVideoStreamDesc
    , StreamingDescriptor(..)
    , StreamingInterface(..)
    , FormatDescriptor(..)
    , FrameDescriptor(..)
    , ColorMatching(..)
    , StillImageFrame(..)
    , StreamingCapability(..)
    , TriggerUsage(..)
    , StreamingControl(..)
    , CompressionFormat(..)
    , InterlaceFlag(..)
    , FrameIntervalType(..)
    , ColorPrimaries(..)
    , TransferCharacteristics(..)
    , MatrixCoefficients(..)
    , StillCaptureMethod
    , getFramesUncompressed
    , getFormatUncompressed
    , isFormatUncompressed

    -- * VideoControl Interface Descriptor
    --, extractControlDescriptor
    , ControlDescriptor(..)
    , ComponentDescriptor(..)
    , ComponentID
    , ProcessingControl(..)
    , VideoStandard(..)
    , TerminalType(..)
    , CameraControl(..)

    ) where

-- Qualified imports.
import qualified Data.ByteString    as B
import qualified Data.Serialize.Get as Get

-- Private libraries.
#include <uvc.h>
import Utils                 ( bits, genToEnum )
import ExtraUtils            ( unmarshalBitmask, BitMask(..), BitMaskTable
                             , getUSBString )

-- Third parties.
import System.USB
import System.USB.Internal   ( unmarshalStrIx, unmarshalReleaseNumber
                             , unmarshalEndpointAddress )

-- Base System.
import Control.Applicative   ( (<|>) )
import Control.Monad         ( replicateM, when )
import Data.Bits             ( Bits, testBit )
import Data.Data             ( Data )
import Data.List             ( find )
import Data.Maybe            ( catMaybes )
import Data.Typeable         ( Typeable )
import Data.Word             ( Word8, Word16, Word32 )
import Text.Printf           ( printf )
import System.IO.Unsafe      ( unsafePerformIO )

import Control.Monad.Unicode ( (≫=), (≫) )
import Data.List.Unicode     ( (⧺) )
import Prelude.Unicode       ( (∧), (∨), (≡), (≠), (≤), (∘), (∈) )

{----------------------------------------------------------------------
-- Abstracting the video descriptors quite a long way away.
----------------------------------------------------------------------}

type Width  = Int
type Height = Int

-- | Global Unique Identifier.
-- Used to identify video formats and vendor specific extension units.
--
-- See RFC 4122  <http://www.ietf.org/rfc/rfc4122.txt>
newtype GUID = GUID B.ByteString
    deriving (Eq, Data, Typeable)

instance Show GUID where
    show (GUID bs) =
        printf "{%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x}"
               (B.index bs 3)
               (B.index bs 2)
               (B.index bs 1)
               (B.index bs 0)
               (B.index bs 5)
               (B.index bs 4)
               (B.index bs 7)
               (B.index bs 6)
               (B.index bs 8)
               (B.index bs 9)
               (B.index bs 10)
               (B.index bs 11)
               (B.index bs 12)
               (B.index bs 13)
               (B.index bs 14)
               (B.index bs 15)

-- Oki so things get strange because USB data seem to be provided with
-- the wrong bit-endianness (well only subpart of the guid are reversed,
-- which is even more confusing). For example, my webcam device is
-- returning (1) instead of (2) :
-- (1) YUY2 59555932-0000-1000-8000-00AA00389B71
-- (2) YUY2 32595559-0000-0010-8000-00AA00389B71
--
-- Note: this behavior is "normal", the first 8 bytes are in big-endian
-- notations, cf RFC 4122.

-- | USB Video Class 1.0a / Payload_uncompressed
-- Table2-1: USB Video Payload uncompressed
-- YUY2 @{32595559-0000-0010-8000-00AA00389B71}@
--
guid_YUY2 ∷ GUID
guid_YUY2 = GUID $ B.pack [ 0x59, 0x55, 0x59, 0x32
                          , 0x00, 0x00, 0x10, 0x00
                          , 0x80, 0x00, 0x00, 0xAA
                          , 0x00, 0x38, 0x9B, 0x71]

-- | USB Video Class 1.0a / Payload_uncompressed
-- Table2-1: USB Video Payload uncompressed
-- NV12 @{3231564E-0000-0010-8000-00AA00389B71}@
--
guid_NV12 ∷ GUID
guid_NV12 = GUID $ B.pack [ 0x4E, 0x56, 0x31, 0x32
                          , 0x00, 0x00, 0x10, 0x00
                          , 0x80, 0x00, 0x00, 0xAA
                          , 0x00, 0x38, 0x9B, 0x71]

{----------------------------------------------------------------------
-- The VideoDevice Object and its interface association.
----------------------------------------------------------------------}

-- | An abstract data type holding useful information about an UVC
-- device, including reference to the original Device, the
-- configuration number used, and for every control or streaming
-- interface its number, standard descriptor and UVC descriptor.
data VideoDevice = VideoDevice
    { videoDevice    ∷ Device
    , videoConfig    ∷ ConfigValue
    , videoCtrlIface ∷ InterfaceNumber
    , videoControl   ∷ Interface
    , videoCtrlDesc  ∷ ControlDescriptor
    , videoStreams   ∷ [Interface]
    , videoStrIfaces ∷ [InterfaceNumber]
    , videoStrDescs  ∷ [StreamingDescriptor]
    , videoName      ∷ Maybe String
    }
    deriving (Eq, Typeable)

isProcessingUnit ∷ ComponentDescriptor → Bool
isProcessingUnit (ProcessingUnit _ _ _ _ _ _) = True
isProcessingUnit _                                = False

isCameraTerminal ∷ ComponentDescriptor → Bool
isCameraTerminal (CameraTerminal _ _ _ _ _ _ _ _) = True
isCameraTerminal _                                    = False

isExtensionUnit ∷ ComponentDescriptor → Bool
isExtensionUnit (ExtensionUnit _ _ _ _ _) = True
isExtensionUnit _                             = False

-- | Using a pretty-printer to display the video device.
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
                               \%s\\
                               \%s"
                               (vcdInterfaceNumber c)
                               (pcameras c)
                               (punits c)
                               (pxunits c)

        -- Pretty printing the processing units.
        punits   ∷ ControlDescriptor → String
        punits   = concatMap ppunit
                 ∘ filter isProcessingUnit
                 ∘ vcdComponentDescriptors
        ppunit   = \p → printf " ----- ProcessingUnit[%d]\n\\
                               \%s"
                               (puId p)
                               (concatMap puctrl ∘ unBitMask ∘ puControls $ p)
        puctrl   = \c → " --------- " ⧺ (show c) ⧺ "\n"

        -- Pretty printing the camera terminal.
        pcameras ∷ ControlDescriptor → String
        pcameras = concatMap pcamera
                 ∘ filter isCameraTerminal
                 ∘ vcdComponentDescriptors
        pcamera  = \c → printf " ----- CameraTerminal[%d]\n\\
                               \%s"
                               (ctId c)
                               (concatMap pcctrl ∘ unBitMask ∘ ctControls $ c)
        pcctrl   = \c → " --------- " ⧺ (show c) ⧺ "\n"

        -- Pretty printing extension units.
        pxunits  ∷ ControlDescriptor → String
        pxunits  = concatMap pxunit
                 ∘ filter isExtensionUnit
                 ∘ vcdComponentDescriptors
        pxunit   = \u → printf " ----- ExtensionUnit[%d] %s\n"
                               (xuId u)
                               (show $ xuGuid u)

        -- Pretty printing streaming interfaces.
        streams  ∷ String
        streams  = concatMap pstream ∘ videoStrDescs $ video
        pstream  = \s → printf " +-- streamIfaces[%d]\n\\
                               \%s"
                               (vsdInterfaceNumber s)
                               (formats s)

        -- Pretty printing uncompressed formats.
        formats  ∷ StreamingDescriptor → String
        formats  = concatMap pformat
                 ∘ filter isFormatUncompressed ∘ vsiFormats
                 ∘ head ∘ vsdInterfaces
        pformat  = \f → printf " ----- FormatUncompressed[%d]\n\\
                               \ --------- format: %s\n\\
                               \ --------- bits-per-pixel: %d\n\\
                               \ --------- flags: %s\n\\
                               \%s\\
                               \%s\\
                               \%s"
                               (fFormatIndex f)
                               (show $ fFormat f)
                               (fBitsPerPixel f)
                               (show ∘ unBitMask $ fInterlaceFlags f)
                               (fFrames f ≫= pframe)
                               (sframe $ fStillImageFrame f)
                               (colors $ fColors f)

        -- Pretty printing uncompressed frames.
        pframe   ∷ FrameDescriptor → String
        pframe   = \f → printf " -------- FrameUncompressed[%d] @ %dx%d\n"
                               (fFrameIndex f)
                               (fWidth f)
                               (fHeight f)

        -- Pretty printing still image frame descriptor.
        sframe   ∷ Maybe StillImageFrame → String
        sframe Nothing  = " -------- NoStillImageFrame\n"
        sframe (Just f) = printf " -------- StillImageFrame\n\\
                                 \%s\\
                                 \%s"
                                (siDimensions f ≫= dimension)
                                (siCompressions f ≫= compression)

        dimension ∷ (Width, Height) → String
        dimension (w,h) = printf " ------------ resolution: %dx%d\n" w h

        compression ∷ Int → String
        compression c = printf " ------------ compression: %dx\n" c

        -- Pretty printing color matching descriptors.
        colors   ∷ Maybe ColorMatching → String
        colors Nothing  = " -------- NoColorMatching\n"
        colors (Just c) = pcolor c
        pcolor   = \c → printf " -------- ColorMatching\n\\
                               \ ------------ primaries: %s\n\\
                               \ ------------ xfer characteristics: %s\n\\
                               \ ------------ matrix coeffs.: %s\n"
                               (show $ cmColorPrimaries c)
                               (show $ cmTransferCharacteristics c)
                               (show $ cmMatrixCoefficients c)


-- | Search the Device configurations for an existing video interface
-- asssociation. If one is found, return its control and streaming
-- interfaces in a new VideoDevice.
videoDescription ∷ Device → VideoDevice
videoDescription dev = unsafePerformIO $ do
    -- First, search for a configuration containing a video interface
    -- association as described in USB Video Class 1.1, table 3-1.
    -- or USB Video Class 1.0a specs, section 3.7.
    let configs = deviceConfigs ∘ deviceDesc $ dev
    case find hasVideoInterfaceAssociation configs of

        Nothing     → error "This is not an UVC video device !"

        Just config → do
            -- Retrieve information.
            let videoConfigValue  = configValue config
                extra             = configExtra config
                bFirstInterface   = B.index extra 2
                bInterfaceCount   = B.index extra 3
                iFunction         = unmarshalStrIx $ B.index extra 7

            -- Bonus: get the video function name, if any.
            mname ← withDeviceHandle dev $ flip getUSBString iFunction

            -- Return our (pretty) object.
            let ifaces   = configInterfaces config

                -- bFirstInterface is the control interface.
                vcontrol = ifaces !! fromIntegral bFirstInterface
                controlD = extractControlDescriptor vcontrol

                -- bInterfaceCount counts the number of (contiguous)
                -- control AND streaming interfaces. So for streaming
                -- interfaces, the range is …
                range    = [bFirstInterface + 1 .. bInterfaceCount - 1]
                isStream = \iface → let alt0 = head iface
                                    in interfaceNumber alt0 ∈ range
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
                   , videoConfig    = videoConfigValue
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

-- | Get the one (and unique?) uncompressed format video streaming
-- descriptor.
getFormatUncompressed ∷ VideoDevice → FormatDescriptor
getFormatUncompressed video =
    let formats = vsiFormats
                ∘ head ∘ vsdInterfaces
                ∘ head ∘ videoStrDescs
                $ video
    in case find isFormatUncompressed formats of
         Nothing → error "No FormatUncompressed descriptor !"
         Just x  → x

-- | Get every uncompressed frames video streaming descriptors.
getFramesUncompressed ∷ VideoDevice → [FrameDescriptor]
getFramesUncompressed = fFrames ∘ getFormatUncompressed

{----------------------------------------------------------------------
-- Searching for a video device.
-- TODO: handle configurations.
-- Does a USB device really provide multiple configurations ?
----------------------------------------------------------------------}

-- Check if a device implements an UVC interface:
-- ⋅ scan every device available
-- ⋅ for every device, scan their configurations
-- ⋅ for every configuration, scan their interfaces
-- ⋅ for every interface, check that interfaceClass ≡ Video
getVideoInterfaces ∷ Device → [Interface]
getVideoInterfaces dev =
    -- invoking []'s supernatural monad power.
    let ifaces = [deviceDesc dev]
              ≫= deviceConfigs
              ≫= configInterfaces
        isVIf  = \iface → let alt0 = head iface
                          in interfaceClass alt0 ≡ videoClass
    in filter isVIf ifaces
  where
    videoClass = CC_VIDEO

-- | Check if an USB device has an USB video function interface.
hasVideoInterface ∷ Device → Bool
hasVideoInterface = not ∘ null ∘ getVideoInterfaces

{----------------------------------------------------------------------
-- Prelude to binary(-strict) parsing.
----------------------------------------------------------------------}

-- | A wrapper around Get parser to check if every (bLength) bytes have
-- been read, and flush the buffer when needed.
boundParser ∷ Int → Get.Get a → Get.Get a
boundParser size parser = do
    start  ← Get.remaining
    result ← parser
    end    ← Get.remaining

    let remainingBytes = size - (start - end)
    if remainingBytes ≠ 0
       then Get.skip remainingBytes
       else return ()

    return result

{----------------------------------------------------------------------
-- VideoControl Interface Descriptors.
----------------------------------------------------------------------}

-- | To control the functional behavior of a particular video function,
-- the Host can manipulate the Units and Terminals inside the video
-- function. To make these objects accessible, the video function must
-- expose a single VideoControl interface.
--
-- See sections 2.4.2 and 3.7 of the UVC specifications.
data ControlDescriptor = ControlDescriptor
    { vcdUVC             ∷ ReleaseNumber
    , vcdClockFrequency  ∷ Int -- ^ in Hertz (deprecated in the specs).
    , vcdStreamIfaces    ∷ [InterfaceNumber]
    , vcdComponentDescriptors  ∷ [ComponentDescriptor]
    , vcdInterfaceNumber ∷ InterfaceNumber
   } deriving (Eq, Show, Data, Typeable)

-- | The identifier number of either a unit or a component.
type ComponentID = Word8

data ComponentDescriptor
   = UnknownDesc !Int !Word8
   -- | The Selector Unit (SU) selects from n input data streams and
   -- routes them unaltered to the single output stream. It represents a
   -- source selector, capable of selecting among a number of sources.
   -- It has an Input Pin for each source stream and a single Output
   -- Pin.
   | SelectorUnit
       { suId                      ∷ !ComponentID
       , suNrInPins                ∷ !Int
       , suSourceID                ∷ ![ComponentID]
       , suSelector                ∷ !(Maybe StrIx)
       }
   -- | The Processing Unit (PU) controls image attributes of the video
   -- being streamed through it. It has a single input and output pin.
   | ProcessingUnit
       { puId                      ∷ !ComponentID
       , puSourceID                ∷ !ComponentID
       , puMaxMultiplier           ∷ !Int
       , puControls                ∷ !(BitMask ProcessingControl)
       , puVideoStandards          ∷ !(BitMask VideoStandard)
       , puProcessing              ∷ !(Maybe StrIx)
       }
   -- | The Extension Unit (XU) is the method provided by this
   -- specification to add vendor-specific building blocks to the
   -- specification. The Extension Unit can have one or more Input Pins
   -- and has a single Output Pin.
   | ExtensionUnit
       { xuId                      ∷ !ComponentID
       , xuGuid                    ∷ !GUID
       , xuNrInPins                ∷ !Int
       , xuSourceID                ∷ ![ComponentID]
       , xuExtension               ∷ !(Maybe StrIx)
       }
   -- | The Camera Terminal (CT) controls mechanical (or equivalent
   -- digital) features of the device component that transmits the video
   -- stream. As such, it is only applicable to video capture devices
   -- with controllable lens or sensor characteristics. A Camera
   -- Terminal is always represented as an Input Terminal with a single
   -- output pin.
   | CameraTerminal
       { ctId                      ∷ !ComponentID
       , ctType                    ∷ !TerminalType
       , ctAssociatedTerminal      ∷ !ComponentID
       , ctTerminal                ∷ !(Maybe StrIx)
       , ctObjectiveFocalLengthMin ∷ !Int
       , ctObjectiveFocalLengthMax ∷ !Int
       , ctOcularFocalLength       ∷ !Int
       , ctControls                ∷ !(BitMask CameraControl)
       }
   -- | The Input Terminal (IT) is used as an interface between the
   -- video function\’s \"outside world\" and other Units inside the
   -- video function. It serves as a receptacle for data flowing into
   -- the video function. Its function is to represent a source of
   -- incoming data after this data has been extracted from the data
   -- source. The data may include audio and metadata associated with a
   -- video stream. These physical streams are grouped into a cluster
   -- of logical streams, leaving the Input Terminal through a single
   -- Output Pin.
   | InputTerminal
       { itId                      ∷ !ComponentID
       , itType                    ∷ !TerminalType
       , itAssociatedTerminal      ∷ !ComponentID
       , itTerminal                ∷ !(Maybe StrIx)
       , itExtra                   ∷ !B.ByteString
       }
   -- | The Output Terminal (OT) is used as an interface between Units
   -- inside the video function and the \"outside world\". It serves as
   -- an outlet for video information, flowing out of the video
   -- function. Its function is to represent a sink of outgoing data.
   -- The video data stream enters the Output Terminal through a single
   -- Input Pin.
   | OutputTerminal
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

camera_control_bitmask ∷ BitMaskTable CameraControl
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

unmarshalCameraControl ∷ Bits α ⇒ α → BitMask CameraControl
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

processing_control_bitmask ∷ BitMaskTable ProcessingControl
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

unmarshalProcessingControls ∷ Bits α ⇒ α → BitMask ProcessingControl
unmarshalProcessingControls = unmarshalBitmask processing_control_bitmask

data VideoStandard
   = VideoNone
   | VideoNTSC_525_60
   | VideoPAL_625_50
   | VideoSECAM_625_50
   | VideoNTSC_625_50
   | VideoPAL_525_60
   deriving (Eq, Show, Data, Typeable)

video_standards_bitmask ∷ BitMaskTable VideoStandard
video_standards_bitmask =
    [ (0, VideoNone)
    , (1, VideoNTSC_525_60)
    , (2, VideoPAL_625_50)
    , (3, VideoSECAM_625_50)
    , (4, VideoNTSC_625_50)
    , (5, VideoPAL_525_60)
    ]

unmarshalVideoStandards ∷ Bits α ⇒ α → BitMask VideoStandard
unmarshalVideoStandards = unmarshalBitmask video_standards_bitmask

-- | Read the extra information of the video control interface
-- descriptor and extract every sub-video control descriptors.
--
-- See Section 3.7 /VideoControl Interface Descriptors/ of the UVC
-- specs.
extractControlDescriptor ∷ Interface → ControlDescriptor
extractControlDescriptor iface =
    let Just alternate = find (not ∘ B.null ∘ interfaceExtra) iface
        extra = interfaceExtra alternate
        number = interfaceNumber alternate
        Right controlDesc = Get.runGet (parseVCHeader number) extra
    in controlDesc


parseVCHeader ∷ InterfaceNumber → Get.Get ControlDescriptor
parseVCHeader ifaceNumber = do
    Get.skip 1 -- skip bLength
    bDescriptorType    ← Get.getWord8
    bDescriptorSubType ← Get.getWord8
    bcdUVC             ← unmarshalReleaseNumber `fmap` Get.getWord16le
    Get.skip 2 -- skip wTotalLength
    dwClockFrequency   ← fromIntegral `fmap` Get.getWord32le
    bInCollection      ← fromIntegral `fmap` Get.getWord8
    baInterfaceNrs     ← replicateM bInCollection Get.getWord8

    components ← parseVCComponents []

    if bDescriptorType    ≠ CS_INTERFACE
     ∨ bDescriptorSubType ≠ VC_HEADER
     then error "This is not a VideoControl interface descriptor"
     else return $ ControlDescriptor
                { vcdUVC             = bcdUVC
                , vcdClockFrequency  = dwClockFrequency
                , vcdStreamIfaces    = baInterfaceNrs
                , vcdComponentDescriptors  = components
                , vcdInterfaceNumber = ifaceNumber
                }

parseVCComponents ∷ [ComponentDescriptor] → Get.Get [ComponentDescriptor]
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
                    VC_INPUT_TERMINAL  → parseInputTerminal  size
                    VC_OUTPUT_TERMINAL → parseOutputTerminal size
                    VC_SELECTOR_UNIT   → parseSelectorUnit   size
                    VC_PROCESSING_UNIT → parseProcessingUnit size
                    VC_EXTENSION_UNIT  → parseExtensionUnit  size
                    _ → parseDummyDesc size bDescriptorSubType

              parseVCComponents (x:acc) -- loop

-- | Default fallback parser.
parseDummyDesc ∷ Int → Word8 → Get.Get ComponentDescriptor
parseDummyDesc bLength bDescriptorSubType =
  boundParser (bLength - 1 - 1 - 1) $ do
    return $ UnknownDesc bLength bDescriptorSubType

parseInputTerminal  ∷ Int → Get.Get ComponentDescriptor
parseInputTerminal size =
  boundParser size $ do
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

            return $ CameraTerminal
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
            return $ InputTerminal
                   { itId                 = bTerminalID
                   , itType               = wTerminalType
                   , itAssociatedTerminal = bAssocTerminal
                   , itTerminal           = iTerminal
                   , itExtra              = extra
                   }

parseOutputTerminal ∷ Int → Get.Get ComponentDescriptor
parseOutputTerminal size =
  boundParser size $ do
    bTerminalID    ← Get.getWord8
    wTerminalType  ← unmarshalTerminalType `fmap` Get.getWord16le
    bAssocTerminal ← Get.getWord8
    bSourceID      ← Get.getWord8
    iTerminal      ← unmarshalStrIx `fmap` Get.getWord8
    extra          ← Get.getByteString (size - 6)

    return $ OutputTerminal
           { otId                 = bTerminalID
           , otType               = wTerminalType
           , otAssociatedTerminal = bAssocTerminal
           , otSourceID           = bSourceID
           , otTerminal           = iTerminal
           , otExtra              = extra
           }

parseSelectorUnit ∷ Int → Get.Get ComponentDescriptor
parseSelectorUnit size =
  boundParser size $ do
    bUnitID     ← Get.getWord8
    bNrInPins   ← fromIntegral `fmap` Get.getWord8
    baSourceIDs ← replicateM bNrInPins Get.getWord8
    iSelector   ← unmarshalStrIx `fmap` Get.getWord8

    return $ SelectorUnit
           { suId       = bUnitID
           , suNrInPins = bNrInPins
           , suSourceID = baSourceIDs
           , suSelector = iSelector
           }

parseProcessingUnit ∷ Int → Get.Get ComponentDescriptor
parseProcessingUnit size =
  boundParser size $ do
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
                            else return (BitMask [])

    return $ ProcessingUnit
           { puId             = bUnitID
           , puSourceID       = baSourceID
           , puMaxMultiplier  = wMaxMultiplier
           , puControls       = bmControls
           , puVideoStandards = bmVideoStandards
           , puProcessing     = iProcessing
           }

parseExtensionUnit ∷ Int → Get.Get ComponentDescriptor
parseExtensionUnit size =
  boundParser size $ do
    bUnitID      ← Get.getWord8
    guidExtensionCode ← GUID `fmap` Get.getBytes 16
    Get.skip 1 -- skipping bNumControls.
    bNrInPins    ← fromIntegral `fmap` Get.getWord8
    baSourceIDs  ← replicateM bNrInPins Get.getWord8
    bControlSize ← fromIntegral `fmap` Get.getWord8
    Get.skip bControlSize -- skipping extension unit bmControls.
    iExtension   ← unmarshalStrIx `fmap` Get.getWord8

    return $ ExtensionUnit
           { xuId        = bUnitID
           , xuGuid      = guidExtensionCode
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

-- | VideoStreaming interfaces are used to interchange digital data
-- streams between the Host and the video function. They are optional. A
-- video function can have zero or more VideoStreaming interfaces
-- associated with it, each possibly carrying data of a different nature
-- and format. Each VideoStreaming interface can have one isochronous or
-- bulk data endpoint for video, and an optional dedicated bulk endpoint
-- for still images related to the video (only for method 3 of still
-- image transfer. See section 2.4.2.4 "Still Image Capture"). This
-- construction guarantees a one-to- one relationship between the
-- VideoStreaming interface and the single data stream related to the
-- endpoint.
--
-- See sections 2.4.3 and 3.9 of the UVC specifications.
data StreamingDescriptor = StreamingDescriptor
    { vsdInterfaceNumber ∷ InterfaceNumber
    , vsdInterfaces      ∷ [StreamingInterface]
    } deriving (Eq, Show, Data, Typeable)

data StreamingInterface
   -- | The Input Header descriptor is used for VS interfaces that
   -- contain an IN endpoint for streaming video data.
   = InputInterface
           { vsiEndpointAddress        ∷ !EndpointAddress
           , vsiTerminalLink           ∷ !ComponentID
           , iiInfo                    ∷ !(BitMask StreamingCapability)
           , iiStillCaptureMethod      ∷ !StillCaptureMethod
           , iiTriggerSupport          ∷ !Bool
           , iiTriggerUsage            ∷ !TriggerUsage
           , vsiFormats                ∷ [FormatDescriptor]
           }
   -- | The Output Header descriptor is used for VS interfaces that
   -- contain an OUT endpoint for streaming video data.
   | OutputInterface
           { vsiEndpointAddress        ∷ !EndpointAddress
           , vsiTerminalLink           ∷ !ComponentID
           , vsiFormats                ∷ [FormatDescriptor]
           }
    deriving (Eq, Show, Data, Typeable)

-- | A (Payload) Format descriptor defines the characteristics of a
-- video stream with its specific format.
data FormatDescriptor
   = UnknownFormatDescriptor
           { fControls                 ∷ !(BitMask StreamingControl)
           , fFormatIndex              ∷ !FormatIndex
           , fName                     ∷ !String
           , fFrames                   ∷ [FrameDescriptor]
           , fStillImageFrame          ∷ Maybe StillImageFrame
           , fColors                   ∷ Maybe ColorMatching
           }
   | FormatUncompressed
           { fControls                 ∷ !(BitMask StreamingControl)
           , fFormatIndex              ∷ !FormatIndex
           , fFormat                   ∷ !CompressionFormat
           , fBitsPerPixel             ∷ !Int
           , fDefaultFrameIndex        ∷ !FrameIndex
           , fAspectRatioX             ∷ !Int
           , fAspectRatioY             ∷ !Int
           , fInterlaceFlags           ∷ !(BitMask InterlaceFlag)
           , fCopyProtect              ∷ !Bool
           , fFrames                   ∷ [FrameDescriptor]
           , fStillImageFrame          ∷ Maybe StillImageFrame
           , fColors                   ∷ Maybe ColorMatching
           }
    deriving (Eq, Show, Data, Typeable)

-- | A Video Frame descriptor (or Frame descriptor for short) is used to
-- describe the decoded video and still image frame dimensions and other
-- frame-specific characteristics supported by Frame- based formats.
data FrameDescriptor
   = UnknownFrameDescriptor String
   | FrameUncompressed
           { fFrameIndex               ∷ !FrameIndex
           , fStillImageSupported      ∷ !Bool
           , fWidth                    ∷ !Int
           , fHeight                   ∷ !Int
           , fMinBitRate               ∷ !Int
           , fMaxBitRate               ∷ !Int
           , fMaxVideoFrameBufferSize  ∷ !Int
           , fDefaultFrameInterval     ∷ !FrameInterval
           , fFrameIntervalType        ∷ !FrameIntervalType
           }
   deriving (Eq, Show, Data, Typeable)

isFormatUncompressed ∷ FormatDescriptor → Bool
isFormatUncompressed (FormatUncompressed _ _ _ _ _ _ _ _ _ _ _ _) = True
isFormatUncompressed _                                            = False

-- | The Still Image Frame descriptor is only applicable for a VS
-- interface that supports method 2 or 3 of still image capture in
-- conjunction with frame-based Payload formats (e.g., MJPEG,
-- uncompressed, etc.). A single still Image Frame descriptor is present
-- by Format descriptor group. If the Input Header descriptor’s
-- bStillCaptureMethod field is set to method 2 or 3, this Still Image
-- Frame descriptor shall be defined (see section 3.9.2.1, \"Input Header
-- Descriptor\").
data StillImageFrame
   = StillImageFrame
           { siEndpointAddress         ∷ !EndpointAddress
           , siDimensions              ∷ ![(Width, Height)]
           , siCompressions            ∷ ![Int]
           }
   deriving (Eq, Show, Data, Typeable)

-- | The Color Matching descriptor is an optional descriptor used to
-- describe the color profile of the video data in an unambiguous way.
-- Only one instance is allowed for a given format and if present, the
-- Color Matching descriptor shall be placed following the Video and
-- Still Image Frame descriptors for that format.
--
-- The viewing conditions and monitor setup are implicitly based on sRGB
-- and the device should compensate for them (D50 ambient white, dim
-- viewing or 64 lux ambient illuminance, 2.2 γ reference CRT, etc).
data ColorMatching
   = ColorMatching
           { cmColorPrimaries           ∷ !ColorPrimaries
           , cmTransferCharacteristics  ∷ !TransferCharacteristics
           , cmMatrixCoefficients       ∷ !MatrixCoefficients
           }
   deriving (Eq, Show, Data, Typeable)

-- | Method of still image capture supported as describe in USB Video
-- Class specification v1.0a, section 2.4.2.4 /Still Image Capture/.
type StillCaptureMethod = Word8

-- | The frame interval is the average display time of a single decoded
-- video frame in 100ns units.
type FrameInterval = Word32
type FormatIndex = Word8
type FrameIndex = Word8

data StreamingCapability
   = DynamicFormatChangeSupported
   deriving (Eq, Show, Data, Typeable)

vs_capability_bitmask ∷ BitMaskTable StreamingCapability
vs_capability_bitmask =
   [ (0,  DynamicFormatChangeSupported)
   ]

unmarshalStreamingCapability ∷ Bits α ⇒ α → BitMask StreamingCapability
unmarshalStreamingCapability = unmarshalBitmask vs_capability_bitmask

unmarshalTriggerSupport ∷ Word8 → Bool
unmarshalTriggerSupport = (0x01 ≡)

-- | Specifies how the host software shall respond to a hardware trigger
-- interrupt event from this interface.
data TriggerUsage
   -- | Initiate still image capture.
   = InitiateStillImageCapture
   -- | Host driver will notify client application of button press and
   -- button release events.
   | GeneralPurposeButton
   deriving (Eq, Show, Data, Typeable)

unmarshalTriggerUsage ∷ Word8 → TriggerUsage
unmarshalTriggerUsage 0x01 = GeneralPurposeButton
unmarshalTriggerUsage _    = InitiateStillImageCapture

data StreamingControl
   = Keyframerate
   | PFramerate
   | CompressionQuality
   | CompressionWindowsize
   | GenerateKeyFrame
   | UpdateFrameSegment
   deriving (Eq, Show, Data, Typeable)

vs_control_bitmask ∷ BitMaskTable StreamingControl
vs_control_bitmask =
    [ (0, Keyframerate)
    , (1, PFramerate)
    , (2, CompressionQuality)
    , (3, CompressionWindowsize)
    , (4, GenerateKeyFrame)
    , (5, UpdateFrameSegment)
    ]

unmarshalStreamingControl ∷ Bits α ⇒ α → BitMask StreamingControl
unmarshalStreamingControl = unmarshalBitmask vs_control_bitmask

data CompressionFormat
   = UnknownCompressionFormat GUID
   | YUY2
   | NV12
   deriving (Eq, Show, Data, Typeable)

unmarshalGuidFormat ∷ B.ByteString → CompressionFormat
unmarshalGuidFormat bs | GUID bs ≡ guid_YUY2  = YUY2
                       | GUID bs ≡ guid_NV12  = NV12
                       | otherwise = UnknownCompressionFormat (GUID bs)

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
unmarshalInterlaceFlags ∷ Word8 → BitMask InterlaceFlag
unmarshalInterlaceFlags value = BitMask $ catMaybes [ testInterlaced
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

-- | The frame interval is the average display time of a single decoded
-- video frame in 100ns units.
data FrameIntervalType
   = FrameIntervalContinous
        { fitMin  ∷ !FrameInterval
        , fitMax  ∷ !FrameInterval
        , fitStep ∷ !FrameInterval
        }
   | FrameIntervalDiscrete ![FrameInterval]
   deriving (Eq, Show, Data, Typeable)

-- | This defines the color primaries and the reference white.
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

-- | This field defines the opto-electronic transfer characteristic of
-- the source picture also called the gamma function.
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

-- | Matrix used to compute luma and chroma values from the color
-- primaries.
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

-- | Read the extra information of the video streaming interface
-- descriptor and extract every sub-video streaming descriptors.
--
-- See Section 3.9 /VideoStreaming Interface Descriptors/ of the UVC
-- specs.
extractVideoStreamDesc ∷ Interface → StreamingDescriptor
extractVideoStreamDesc iface =
    let Just alternate = find (not ∘ B.null ∘ interfaceExtra) iface
        extra = interfaceExtra alternate
        number = interfaceNumber alternate
        Right xs = Get.runGet (parseVideoStreamInterfaces []) extra
    in StreamingDescriptor number xs

-- | Begin to read a Video Streaming Interface header and dispatch to
-- the right specialised parser.
parseVideoStreamInterfaces ∷ [StreamingInterface] → Get.Get [StreamingInterface]
parseVideoStreamInterfaces acc = do
    atEnd ← Get.isEmpty
    if atEnd
       then return $ reverse acc
       else do bLength ← fromIntegral `fmap` Get.getWord8
               bDescriptorType ← Get.getWord8
               bDescriptorSubType ← Get.getWord8
               bNumFormats ← fromIntegral `fmap` Get.getWord8
               wTotalLength ← fromIntegral `fmap` Get.getWord16le

               when (bDescriptorType ≠ CS_INTERFACE) $
                 error "This is not a Video Streaming interface."

               -- remaining size = length
               --                - bLength
               --                - bDescriptor{,Sub}Type
               --                - bNumFormats
               --                - wTotalLength
               let s = bLength - 1 - 1 - 1 - 1 - 2
                   ts = wTotalLength - 1 - 1 - 1 - 1 - 2

               x ← case bDescriptorSubType of
                     VS_OUTPUT_HEADER → parseVSOutputHeader s ts bNumFormats
                     VS_INPUT_HEADER  → parseVSInputHeader  s ts bNumFormats
                     _ → error "No valid Video Streaming interface header."

               parseVideoStreamInterfaces (x:acc) -- loop recursion

-- | Read VS Interface Input Header Descriptors as specified
-- in USB Video Class 1.0a, table 3-13.
parseVSInputHeader ∷ Int → Int → Int → Get.Get StreamingInterface
parseVSInputHeader size total_size nformats = boundParser total_size $ do

  -- Parse header specific content.
  bEndpointAddress ← unmarshalEndpointAddress `fmap` Get.getWord8
  bmInfo ← unmarshalStreamingCapability `fmap` Get.getWord8
  bTerminalLink ← Get.getWord8
  bStillCaptureMethod ← Get.getWord8
  bTriggerSupport ← unmarshalTriggerSupport `fmap` Get.getWord8
  bTriggerUsage ← unmarshalTriggerUsage `fmap` Get.getWord8
  bControlSize ← fromIntegral `fmap` Get.getWord8
  bmaControls ← replicateM nformats $ do
                      bitmask ← unmarshalStreamingControl `fmap` Get.getWord8
                      Get.skip (bControlSize - 1) -- skipping extra bits
                      return bitmask

  -- Skip any remaining bytes in the header descriptor.
  Get.skip (size - 7 - nformats * bControlSize)

  -- Parse the different formats.
  formats ← mapM parseFormatDescriptors bmaControls

  return $ InputInterface
         { vsiEndpointAddress   = bEndpointAddress
         , vsiTerminalLink      = bTerminalLink
         , iiInfo               = bmInfo
         , iiStillCaptureMethod = bStillCaptureMethod
         , iiTriggerSupport     = bTriggerSupport
         , iiTriggerUsage       = bTriggerUsage
         , vsiFormats           = formats
         }

-- | Read VS Interface Output Header Descriptors as specified
-- in USB Video Class 1.0a, table 3-14.
parseVSOutputHeader ∷ Int → Int → Int → Get.Get StreamingInterface
parseVSOutputHeader size total_size nformats = boundParser total_size $ do
  bEndpointAddress ← unmarshalEndpointAddress `fmap` Get.getWord8
  bTerminalLink ← Get.getWord8

  bmaControls ← if (size ≤ 2) -- only in UVC v1.1
     then do -- skip remaining descriptor bytes.
             Get.skip (size - 2)
             return $ replicate nformats (BitMask [])

     else do bControlSize ← fromIntegral `fmap` Get.getWord8
             xs ← replicateM nformats $ do
                bitmask ← unmarshalStreamingControl `fmap` Get.getWord8
                Get.skip (bControlSize - 1) -- skipping extra bits
                return bitmask
             -- skip remaining descriptor bytes.
             Get.skip (size - 2 - nformats * bControlSize)
             return xs

  -- Parse the different formats.
  formats ← mapM parseFormatDescriptors bmaControls

  return $ OutputInterface
         { vsiEndpointAddress = bEndpointAddress
         , vsiTerminalLink    = bTerminalLink
         , vsiFormats         = formats
         }

-- | Begin to read a Video Streaming Descriptor and dispatch to the
-- right specialised parser.
parseFormatDescriptors ∷ BitMask StreamingControl → Get.Get FormatDescriptor
parseFormatDescriptors ctrl = do
    bLength ← fromIntegral `fmap` Get.getWord8
    bDescriptorType ← Get.getWord8
    bDescriptorSubType ← Get.getWord8

    when (bDescriptorType ≠ CS_INTERFACE) $
      error "This is not a Video Streaming interface."

    -- remaining size = length - bLength - bDescriptor{,Sub}Type
    let size = bLength - 1 - 1 - 1

    let fallback name = parseVSUnknownFormat name ctrl size

    case bDescriptorSubType of
          VS_FORMAT_UNCOMPRESSED → parseFormatDescriptorUncompressed ctrl size
          VS_FORMAT_MJPEG        → fallback "FORMAT_MJPEG"
          VS_FORMAT_MPEG2TS      → fallback "FORMAT_MPEG2TS"
          VS_FORMAT_DV           → fallback "FORMAT_DV"
          VS_FORMAT_FRAME_BASED  → fallback "FORMAT_FRAME_BASED"
          VS_FORMAT_STREAM_BASED → fallback "FORMAT_STREAM_BASED"
          _ → fallback $ "FORMAT_(" ⧺ show bDescriptorSubType ⧺ ")"

parseVSUnknownFormat ∷ String → BitMask StreamingControl → Int → Get.Get FormatDescriptor
parseVSUnknownFormat name ctrl size = do
    bFormatIndex ← Get.getWord8
    bNumFrameDescriptors ← fromIntegral `fmap` Get.getWord8
    Get.skip (size - 2) -- skip the rest of the format header.
    frames ← replicateM bNumFrameDescriptors parseFrameDescriptor
    mstillimage ← parseVSStillImageFrame
    mcolors ← parseVSColorMatching
    return $ UnknownFormatDescriptor ctrl bFormatIndex name
                                     frames mstillimage mcolors

-- | Read Uncompressed Video Format Descriptors as specified
-- in USB Video Class 1.0a / Payload_uncompressed, table 3-1.
parseFormatDescriptorUncompressed ∷ BitMask StreamingControl → Int → Get.Get FormatDescriptor
parseFormatDescriptorUncompressed ctrl size = do
    bFormatIndex ← Get.getWord8
    bNumFrameDescriptors ← fromIntegral `fmap` Get.getWord8
    guidFormat ← unmarshalGuidFormat `fmap` Get.getByteString 16
    bBitsPerPixel ← fromIntegral `fmap` Get.getWord8
    bDefaultFrameIndex ← Get.getWord8
    bAspectRatioX ← fromIntegral `fmap` Get.getWord8
    bAspectRatioY ← fromIntegral `fmap` Get.getWord8
    bmInterlaceFlags ← unmarshalInterlaceFlags `fmap` Get.getWord8
    bCopyProtect ← (≡ 0x01) `fmap` Get.getWord8
    Get.skip (size - 24) -- 24 ≡ length consumed by the previous operations.

    frames ← replicateM bNumFrameDescriptors parseFrameDescriptor
    mstillframe ← parseVSStillImageFrame
    mcolors ← parseVSColorMatching

    return $ FormatUncompressed
           { fControls            = ctrl
           , fFormatIndex         = bFormatIndex
           , fFormat              = guidFormat
           , fBitsPerPixel        = bBitsPerPixel
           , fDefaultFrameIndex   = bDefaultFrameIndex
           , fAspectRatioX        = bAspectRatioX
           , fAspectRatioY        = bAspectRatioY
           , fInterlaceFlags      = bmInterlaceFlags
           , fCopyProtect         = bCopyProtect
           , fFrames              = reverse frames
           , fStillImageFrame     = mstillframe
           , fColors              = mcolors
           }

-- | Parse frames or color matching descriptors.
parseFrameDescriptor ∷ Get.Get FrameDescriptor
parseFrameDescriptor = do
    bLength ← fromIntegral `fmap` Get.getWord8
    bDescriptorType ← Get.getWord8
    bDescriptorSubType ← Get.getWord8

    when (bDescriptorType ≠ CS_INTERFACE) $
        error "This is not a Video Streaming interface."

    -- remaining size = length - bLength - bDescriptor{,Sub}Type
    let size = bLength - 1 - 1 - 1

    let fallback name = boundParser size $
            return (UnknownFrameDescriptor name)

    case bDescriptorSubType of
       VS_FRAME_UNCOMPRESSED  → parseFrameDescriptorUncompressed size
       VS_STILL_IMAGE_FRAME   → fallback "STILL_IMAGE_FRAME"
       VS_FRAME_MJPEG         → fallback "FRAME_MJPEG"
       VS_FRAME_FRAME_BASED   → fallback "FRAME_FRAME_BASED"
       _ → fallback $ "FRAME(" ⧺ show bDescriptorSubType ⧺ ")"

-- | Read Uncompressed Video Frame Descriptors as specified
-- in USB Video Class 1.0a / Payload_uncompressed, table 3-2.
parseFrameDescriptorUncompressed ∷ Int → Get.Get FrameDescriptor
parseFrameDescriptorUncompressed size = boundParser size $ do
    bFrameIndex ← Get.getWord8
    bmCapabilities ← unmarshalUncompressedCapabilities `fmap` Get.getWord8
    wWidth ← fromIntegral `fmap` Get.getWord16le
    wHeight ← fromIntegral `fmap` Get.getWord16le
    dwMinBitRate ← fromIntegral `fmap` Get.getWord32le
    dwMaxBitRate ← fromIntegral `fmap` Get.getWord32le
    dwMaxVideoFrameBufferSize ← fromIntegral `fmap` Get.getWord32le
    dwDefaultFrameInterval ← Get.getWord32le
    bFrameIntervalType ← fromIntegral `fmap` Get.getWord8

    frameType ← if bFrameIntervalType ≡ 0
       then do min'  ← Get.getWord32le
               max'  ← Get.getWord32le
               step' ← Get.getWord32le
               return $ FrameIntervalContinous min' max' step'

       else do xs ← replicateM bFrameIntervalType Get.getWord32le
               return $ FrameIntervalDiscrete xs

    return $ FrameUncompressed
           { fFrameIndex              = bFrameIndex
           , fStillImageSupported     = bmCapabilities
           , fWidth                   = wWidth
           , fHeight                  = wHeight
           , fMinBitRate              = dwMinBitRate
           , fMaxBitRate              = dwMaxBitRate
           , fMaxVideoFrameBufferSize = dwMaxVideoFrameBufferSize
           , fDefaultFrameInterval    = dwDefaultFrameInterval
           , fFrameIntervalType       = frameType
           }

-- Check the next bDescriptorSubType.
checkNextVSDescriptor ∷ Word8 → Get.Get Bool
checkNextVSDescriptor subtype =
     Get.lookAhead (Get.skip 2 ≫ Get.getWord8 ≫= return ∘ (≡ subtype))
 <|> return False

-- | Read VS Interface Color Matching Descriptors as specified
-- in USB Video Class 1.0a, table 3-18.
parseVSColorMatching ∷ Get.Get (Maybe ColorMatching)
parseVSColorMatching = do
    isColorMatching ← checkNextVSDescriptor VS_COLORFORMAT
    if not isColorMatching
       then return Nothing
       else do
           bLength ← fromIntegral `fmap` Get.getWord8
           boundParser (bLength - 1) $ do
               Get.skip 2 -- skipping bDescriptorType & bDescriptorSubType
               cp ← unmarshalColorPrimaries `fmap` Get.getWord8
               tc ← unmarshalTCharacteristics `fmap` Get.getWord8
               mc ← unmarshalMatrixCoefficients `fmap` Get.getWord8
               return ∘ Just $ ColorMatching
                             { cmColorPrimaries          = cp
                             , cmTransferCharacteristics = tc
                             , cmMatrixCoefficients      = mc
                             }

parseVSStillImageFrame ∷ Get.Get (Maybe StillImageFrame)
parseVSStillImageFrame = do
    isStillFrame ← checkNextVSDescriptor VS_STILL_IMAGE_FRAME
    if not isStillFrame
       then return Nothing
       else do
           bLength ← fromIntegral `fmap` Get.getWord8
           boundParser (bLength - 1) $ do
               Get.skip 2 -- skipping bDescriptorType & bDescriptorSubType
               ep ← unmarshalEndpointAddress `fmap` Get.getWord8

               npatterns ← fromIntegral `fmap` Get.getWord8
               dimensions ← replicateM npatterns $ do
                   width ← fromIntegral `fmap` Get.getWord16le
                   height ← fromIntegral `fmap` Get.getWord16le
                   return (width, height)

               ncompressions ← fromIntegral `fmap` Get.getWord8
               compressions ← replicateM ncompressions $ do
                   fromIntegral `fmap` Get.getWord8

               return ∘ Just $ StillImageFrame
                             { siEndpointAddress = ep
                             , siDimensions      = dimensions
                             , siCompressions    = compressions
                             }
