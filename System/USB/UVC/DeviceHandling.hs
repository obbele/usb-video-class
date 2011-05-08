{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}

module System.USB.UVC.DeviceHandling (

    -- * VideoDevice handling
      withVideoDeviceHandle
    , unsafeOpenVideoDevice
    , unsafeCloseVideoDevice

    ) where

-- Qualified imports.
import qualified Control.Exception as E

-- Private libraries.
#include <uvc.h>
import System.USB.UVC.Descriptors

-- Third parties.
import System.USB

-- Base System.
import Text.Printf                ( printf )
import Prelude.Unicode            ( (∘) )

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

