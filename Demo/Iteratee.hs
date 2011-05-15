{-# LANGUAGE UnicodeSyntax #-}

module Main where

import qualified Data.ByteString         as B
import qualified System.USB.UVC.Iteratee as I

import Demo.Common
import System.USB.UVC

import System.USB

import Text.Printf              ( printf )
import Control.Monad.Unicode    ( (≫=) )

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
main = catchCommonUSBException writeRawDataToDisk'

-- | Write raw YUY2\/NV12 content to a file @\/tmp\/uvc_WxH.yuy2@ where
-- @W@ is the width and @H@ the height.
writeRawDataToDisk' ∷ IO ()
writeRawDataToDisk' = do
    Pipe _ _ w h frames ← testIteratee
    let filename = printf "/tmp/uvc_%dx%d.yuy2" w h
    printf "writing raw video flux to [%s]\n" filename
    B.writeFile filename (B.concat frames)

-- | Testing the 'readNFrames' function to get raw video frames.
testIteratee ∷ IO Pipe
testIteratee = findVideoDevice ≫= \video →
  withVideoDeviceHandle video $ \devh → do
    ctrl ← negotiatePCControl video devh (defaultProbeCommitControl video)
    Pipe a b w h frames ← I.readNFrames video devh ctrl nframes timeout
    return $ Pipe a b w h (reorderFrames w h frames)
  where
    nframes = 100
    timeout = noTimeout
