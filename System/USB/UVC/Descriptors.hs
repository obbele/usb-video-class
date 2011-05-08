{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}

-- Ho man ! How much would I like to have a library in Haskell similar
-- to Common-Lisp's gigamonkeys binary-data.

module System.USB.UVC.Descriptors
    ( VideoDevice(..)
    , getVideoDevice
    , hasVideoInterface

    -- * Common types
    , Width
    , Height
    , Frame
    , FormatIndex
    , FrameIndex
    , FrameInterval
    , GUID(..)

    -- * VideoStreaming Interface Descriptors
    --, extractVideoStreamDesc
    , VideoStreamingDesc(..)
    , VSInterface(..)
    , VSFormat(..)
    , VSFrame(..)
    , ColorMatching(..)
    , StillImageFrame(..)
    , VSCapability(..)
    , TriggerUsage(..)
    , VSControl(..)
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
    --, extractVideoControlDesc
    , VideoControlDesc(..)
    , ComponentDesc(..)
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

import Control.Monad.Unicode ( (≫=), (≫) )
import Data.List.Unicode     ( (⧺) )
import Prelude.Unicode       ( (∧), (∨), (≡), (≠), (≤), (∘), (∈) )

{----------------------------------------------------------------------
-- Abstracting the video descriptors quite a long way away.
----------------------------------------------------------------------}

type Width  = Int
type Height = Int
type Frame  = B.ByteString

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

isExtensionUnit ∷ ComponentDesc → Bool
isExtensionUnit (ExtensionUnitDesc _ _ _ _ _) = True
isExtensionUnit _                             = False

-- | Using a pretty-printer when display a video device.
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
        punits   ∷ VideoControlDesc → String
        punits   = concatMap ppunit
                 ∘ filter isProcessingUnit
                 ∘ vcdComponentDescs
        ppunit   = \p → printf " ----- ProcessingUnit[%d]\n\\
                               \%s"
                               (puId p)
                               (concatMap puctrl ∘ unBitMask ∘ puControls $ p)
        puctrl   = \c → " --------- " ⧺ (show c) ⧺ "\n"

        -- Pretty printing the camera terminal.
        pcameras ∷ VideoControlDesc → String
        pcameras = concatMap pcamera
                 ∘ filter isCameraTerminal
                 ∘ vcdComponentDescs
        pcamera  = \c → printf " ----- CameraTerminal[%d]\n\\
                               \%s"
                               (ctId c)
                               (concatMap pcctrl ∘ unBitMask ∘ ctControls $ c)
        pcctrl   = \c → " --------- " ⧺ (show c) ⧺ "\n"

        -- Pretty printing extension units.
        pxunits  ∷ VideoControlDesc → String
        pxunits  = concatMap pxunit
                 ∘ filter isExtensionUnit
                 ∘ vcdComponentDescs
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
        formats  ∷ VideoStreamingDesc → String
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
        pframe   ∷ VSFrame → String
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
getFormatUncompressed ∷ VideoDevice → VSFormat
getFormatUncompressed video =
    let formats = vsiFormats
                ∘ head ∘ vsdInterfaces
                ∘ head ∘ videoStrDescs
                $ video
    in case find isFormatUncompressed formats of
         Nothing → error "No FormatUncompressed descriptor !"
         Just x  → x

-- | Get every uncompressed frames video streaming descriptors.
getFramesUncompressed ∷ VideoDevice → [VSFrame]
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

-- See USB Video Class 1.0a specifications, section 3.7.

{-
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
-}

data VideoControlDesc = VideoControlDesc
    { vcdUVC             ∷ ReleaseNumber
    , vcdClockFrequency  ∷ Int -- ^ in Hertz (deprecated in the specs).
    , vcdStreamIfaces    ∷ [InterfaceNumber]
    , vcdComponentDescs  ∷ [ComponentDesc]
    , vcdInterfaceNumber ∷ InterfaceNumber
   } deriving (Eq, Show, Data, Typeable)

-- | Compononent <- Terminal | Unit
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
       , puControls                ∷ !(BitMask ProcessingControl)
       , puVideoStandards          ∷ !(BitMask VideoStandard)
       , puProcessing              ∷ !(Maybe StrIx)
       }
   | ExtensionUnitDesc
       { xuId                      ∷ !ComponentID
       , xuGuid                    ∷ !GUID
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
       , ctControls                ∷ !(BitMask CameraControl)
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
extractVideoControlDesc ∷ Interface → VideoControlDesc
extractVideoControlDesc iface =
    let Just alternate = find (not ∘ B.null ∘ interfaceExtra) iface
        extra = interfaceExtra alternate
        number = interfaceNumber alternate
        Right controlDesc = Get.runGet (parseVCHeader number) extra
    in controlDesc


parseVCHeader ∷ InterfaceNumber → Get.Get VideoControlDesc
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
     else return $ VideoControlDesc
                { vcdUVC             = bcdUVC
                , vcdClockFrequency  = dwClockFrequency
                , vcdStreamIfaces    = baInterfaceNrs
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
  boundParser (bLength - 1 - 1 - 1) $ do
    return $ UnknownDesc bLength bDescriptorSubType

parseInputTerminalDesc  ∷ Int → Get.Get ComponentDesc
parseInputTerminalDesc size =
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
  boundParser size $ do
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
  boundParser size $ do
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
  boundParser size $ do
    bUnitID      ← Get.getWord8
    guidExtensionCode ← GUID `fmap` Get.getBytes 16
    Get.skip 1 -- skipping bNumControls.
    bNrInPins    ← fromIntegral `fmap` Get.getWord8
    baSourceIDs  ← replicateM bNrInPins Get.getWord8
    bControlSize ← fromIntegral `fmap` Get.getWord8
    Get.skip bControlSize -- skipping extension unit bmControls.
    iExtension   ← unmarshalStrIx `fmap` Get.getWord8

    return $ ExtensionUnitDesc
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

-- See USB Video Class 1.0a specifications, section 3.9.

data VideoStreamingDesc = VideoStreamingDesc
    { vsdInterfaceNumber ∷ InterfaceNumber
    , vsdInterfaces      ∷ [VSInterface]
    } deriving (Eq, Show, Data, Typeable)

data VSInterface
   = InputInterface
           { vsiEndpointAddress        ∷ !EndpointAddress
           , vsiTerminalLink           ∷ !Word8
           , iiInfo                    ∷ !(BitMask VSCapability)
           , iiStillCaptureMethod      ∷ !StillCaptureMethod
           , iiTriggerSupport          ∷ !Bool
           , iiTriggerUsage            ∷ !TriggerUsage
           , vsiFormats                ∷ [VSFormat]
           }
   | OutputInterface
           { vsiEndpointAddress        ∷ !EndpointAddress
           , vsiTerminalLink           ∷ !Word8
           , vsiFormats                ∷ [VSFormat]
           }
    deriving (Eq, Show, Data, Typeable)

data VSFormat
   = UnknownFormatDescriptor
           { fControls                 ∷ !(BitMask VSControl)
           , fFormatIndex              ∷ !FormatIndex
           , fName                     ∷ !String
           , fFrames                   ∷ [VSFrame]
           , fStillImageFrame          ∷ Maybe StillImageFrame
           , fColors                   ∷ Maybe ColorMatching
           }
   | FormatUncompressed
           { fControls                 ∷ !(BitMask VSControl)
           , fFormatIndex              ∷ !FormatIndex
           , fFormat                   ∷ !CompressionFormat
           , fBitsPerPixel             ∷ !Int
           , fDefaultFrameIndex        ∷ !FrameIndex
           , fAspectRatioX             ∷ !Int
           , fAspectRatioY             ∷ !Int
           , fInterlaceFlags           ∷ !(BitMask InterlaceFlag)
           , fCopyProtect              ∷ !Bool
           , fFrames                   ∷ [VSFrame]
           , fStillImageFrame          ∷ Maybe StillImageFrame
           , fColors                   ∷ Maybe ColorMatching
           }
    deriving (Eq, Show, Data, Typeable)

data VSFrame
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

isFormatUncompressed ∷ VSFormat → Bool
isFormatUncompressed (FormatUncompressed _ _ _ _ _ _ _ _ _ _ _ _) = True
isFormatUncompressed _                                            = False

data StillImageFrame
   = StillImageFrame
           { siEndpointAddress         ∷ !EndpointAddress
           , siDimensions              ∷ ![(Width, Height)]
           , siCompressions            ∷ ![Int]
           }
   deriving (Eq, Show, Data, Typeable)

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

-- | In units of 100ns.
type FrameInterval = Word32
type FormatIndex = Word8
type FrameIndex = Word8

data VSCapability
   = DynamicFormatChangeSupported
   deriving (Eq, Show, Data, Typeable)

vs_capability_bitmask ∷ BitMaskTable VSCapability
vs_capability_bitmask =
   [ (0,  DynamicFormatChangeSupported)
   ]

unmarshalVSCapability ∷ Bits α ⇒ α → BitMask VSCapability
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

vs_control_bitmask ∷ BitMaskTable VSControl
vs_control_bitmask =
    [ (0, Keyframerate)
    , (1, PFramerate)
    , (2, CompressionQuality)
    , (3, CompressionWindowsize)
    , (4, GenerateKeyFrame)
    , (5, UpdateFrameSegment)
    ]

unmarshalVSControl ∷ Bits α ⇒ α → BitMask VSControl
unmarshalVSControl = unmarshalBitmask vs_control_bitmask

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

data FrameIntervalType
   = FrameIntervalContinous
        { fitMin  ∷ !FrameInterval
        , fitMax  ∷ !FrameInterval
        , fitStep ∷ !FrameInterval
        }
   | FrameIntervalDiscrete ![FrameInterval]
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

-- | Read the extra information of the video streaming interface
-- descriptor and extract every sub-video streaming descriptors.
--
-- See Section 3.9 /VideoStreaming Interface Descriptors/ of the UVC
-- specs.
extractVideoStreamDesc ∷ Interface → VideoStreamingDesc
extractVideoStreamDesc iface =
    let Just alternate = find (not ∘ B.null ∘ interfaceExtra) iface
        extra = interfaceExtra alternate
        number = interfaceNumber alternate
        Right xs = Get.runGet (parseVideoStreamInterfaces []) extra
    in VideoStreamingDesc number xs

-- | Begin to read a Video Streaming Interface header and dispatch to
-- the right specialised parser.
parseVideoStreamInterfaces ∷ [VSInterface] → Get.Get [VSInterface]
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
parseVSInputHeader ∷ Int → Int → Int → Get.Get VSInterface
parseVSInputHeader size total_size nformats = boundParser total_size $ do

  -- Parse header specific content.
  bEndpointAddress ← unmarshalEndpointAddress `fmap` Get.getWord8
  bmInfo ← unmarshalVSCapability `fmap` Get.getWord8
  bTerminalLink ← Get.getWord8
  bStillCaptureMethod ← Get.getWord8
  bTriggerSupport ← unmarshalTriggerSupport `fmap` Get.getWord8
  bTriggerUsage ← unmarshalTriggerUsage `fmap` Get.getWord8
  bControlSize ← fromIntegral `fmap` Get.getWord8
  bmaControls ← replicateM nformats $ do
                      bitmask ← unmarshalVSControl `fmap` Get.getWord8
                      Get.skip (bControlSize - 1) -- skipping extra bits
                      return bitmask

  -- Skip any remaining bytes in the header descriptor.
  Get.skip (size - 7 - nformats * bControlSize)

  -- Parse the different formats.
  formats ← mapM parseVSFormats bmaControls

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
parseVSOutputHeader ∷ Int → Int → Int → Get.Get VSInterface
parseVSOutputHeader size total_size nformats = boundParser total_size $ do
  bEndpointAddress ← unmarshalEndpointAddress `fmap` Get.getWord8
  bTerminalLink ← Get.getWord8

  bmaControls ← if (size ≤ 2) -- only in UVC v1.1
     then do -- skip remaining descriptor bytes.
             Get.skip (size - 2)
             return $ replicate nformats (BitMask [])

     else do bControlSize ← fromIntegral `fmap` Get.getWord8
             xs ← replicateM nformats $ do
                bitmask ← unmarshalVSControl `fmap` Get.getWord8
                Get.skip (bControlSize - 1) -- skipping extra bits
                return bitmask
             -- skip remaining descriptor bytes.
             Get.skip (size - 2 - nformats * bControlSize)
             return xs

  -- Parse the different formats.
  formats ← mapM parseVSFormats bmaControls

  return $ OutputInterface
         { vsiEndpointAddress = bEndpointAddress
         , vsiTerminalLink    = bTerminalLink
         , vsiFormats         = formats
         }

-- | Begin to read a Video Streaming Descriptor and dispatch to the
-- right specialised parser.
parseVSFormats ∷ BitMask VSControl → Get.Get VSFormat
parseVSFormats ctrl = do
    bLength ← fromIntegral `fmap` Get.getWord8
    bDescriptorType ← Get.getWord8
    bDescriptorSubType ← Get.getWord8

    when (bDescriptorType ≠ CS_INTERFACE) $
      error "This is not a Video Streaming interface."

    -- remaining size = length - bLength - bDescriptor{,Sub}Type
    let size = bLength - 1 - 1 - 1

    let fallback name = parseVSUnknownFormat name ctrl size

    case bDescriptorSubType of
          VS_FORMAT_UNCOMPRESSED → parseVSFormatUncompressed ctrl size
          VS_FORMAT_MJPEG        → fallback "FORMAT_MJPEG"
          VS_FORMAT_MPEG2TS      → fallback "FORMAT_MPEG2TS"
          VS_FORMAT_DV           → fallback "FORMAT_DV"
          VS_FORMAT_FRAME_BASED  → fallback "FORMAT_FRAME_BASED"
          VS_FORMAT_STREAM_BASED → fallback "FORMAT_STREAM_BASED"
          _ → fallback $ "FORMAT_(" ⧺ show bDescriptorSubType ⧺ ")"

parseVSUnknownFormat ∷ String → BitMask VSControl → Int → Get.Get VSFormat
parseVSUnknownFormat name ctrl size = do
    bFormatIndex ← Get.getWord8
    bNumFrameDescriptors ← fromIntegral `fmap` Get.getWord8
    Get.skip (size - 2) -- skip the rest of the format header.
    frames ← replicateM bNumFrameDescriptors parseVSFrame
    mstillimage ← parseVSStillImageFrame
    mcolors ← parseVSColorMatching
    return $ UnknownFormatDescriptor ctrl bFormatIndex name
                                     frames mstillimage mcolors

-- | Read Uncompressed Video Format Descriptors as specified
-- in USB Video Class 1.0a / Payload_uncompressed, table 3-1.
parseVSFormatUncompressed ∷ BitMask VSControl → Int → Get.Get VSFormat
parseVSFormatUncompressed ctrl size = do
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

    frames ← replicateM bNumFrameDescriptors parseVSFrame
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
parseVSFrame ∷ Get.Get VSFrame
parseVSFrame = do
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
       VS_FRAME_UNCOMPRESSED  → parseVSFrameUncompressed size
       VS_STILL_IMAGE_FRAME   → fallback "STILL_IMAGE_FRAME"
       VS_FRAME_MJPEG         → fallback "FRAME_MJPEG"
       VS_FRAME_FRAME_BASED   → fallback "FRAME_FRAME_BASED"
       _ → fallback $ "FRAME(" ⧺ show bDescriptorSubType ⧺ ")"

-- | Read Uncompressed Video Frame Descriptors as specified
-- in USB Video Class 1.0a / Payload_uncompressed, table 3-2.
parseVSFrameUncompressed ∷ Int → Get.Get VSFrame
parseVSFrameUncompressed size = boundParser size $ do
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
