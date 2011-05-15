{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Opening and closing a video device.

module System.USB.UVC.DeviceHandling
    ( withVideoDeviceHandle
    , openVideoDevice
    , closeVideoDevice

    ) where

-- Qualified imports.
import qualified Control.Exception as E

-- Private libraries.
import System.USB.UVC.Descriptors

-- Third parties.
import System.USB

-- Base System.
import Control.Monad              ( forM_, when )
import Text.Printf                ( printf )
import Prelude.Unicode            ( (∘), (≠) )

{----------------------------------------------------------------------
-- VideoDevice Handling.
----------------------------------------------------------------------}

setVideoConfig ∷ VideoDevice → IO ()
setVideoConfig video = withDeviceHandle dev $ \devh → do
    currentConfig ← getConfig devh
    when (currentConfig ≠ config) $ setConfig devh config
  where
    dev    = videoDevice video
    config = Just (videoConfig video)

-- | Open a VideoDevice by:
--
-- 0. getting the DeviceHandle;
--
-- 1. switching to the right USB configuration;
--
-- 2. detaching the original kernel driver, if needed;
--
-- 3. claiming required (control + streaming) interfaces.
openVideoDevice ∷ VideoDevice → IO DeviceHandle
openVideoDevice v = do
    devh ← openDevice device

    printf "Selecting the right configuration\n"
    setVideoConfig v

    printf "Acquiring interface number %s\n" (show ctrl)
    ignoreNotFound $ detachKernelDriver devh ctrl
    claimInterface devh ctrl

    forM_ streams $ \stream → do
        printf "Acquiring interface number %s\n" (show stream)
        ignoreNotFound $ detachKernelDriver devh stream
        claimInterface devh stream

    return devh

  where
    device  = videoDevice v
    ctrl    = videoCtrlIface v
    streams = videoStrIfaces v

-- | Close a VideoDevice by:
--
-- 3. releasing the interfaces (control + streaming);
--
-- 2. re-attaching the original kernel driver, if possible;
--
-- 1. closing the USB DeviceHandle.
closeVideoDevice ∷ VideoDevice → DeviceHandle → IO ()
closeVideoDevice v devh = do
    releaseInterface devh ctrl0
    ignoreNotFound $ attachKernelDriver devh ctrl0

    forM_ streams $ \stream → do
        releaseInterface devh stream
        ignoreNotFound $ attachKernelDriver devh stream

    closeDevice devh

  where
    ctrl0   = videoCtrlIface v
    streams = videoStrIfaces v

-- | A safe wrapper around 'openVideoDevice' and 'closeVideoDevice'.
-- The IO computation is run after opening the video device and the
-- device is guaranteed to be closed, even in case of an exception.
withVideoDeviceHandle ∷ VideoDevice → (DeviceHandle → IO α) → IO α
withVideoDeviceHandle v io = do
    setVideoConfig v
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

