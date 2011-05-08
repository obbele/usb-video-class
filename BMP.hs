{-# LANGUAGE UnicodeSyntax #-}

-- | Handling RGBA images with the "Codec.BMP" module.
module Main where

import qualified Data.ByteString   as B

import System.USB
import System.USB.UVC
import Codec.UVC.RGBA           ( nv12ToRGBA, yuy2ToRGBA )

import Codec.BMP                ( BMP, packRGBA32ToBMP, writeBMP )

import Data.List                ( find )
import Text.Printf              ( printf )
import Control.Monad.Unicode    ( (≫=) )
import Data.List.Unicode        ( (⧺) )
import Prelude.Unicode          ( (∘) )

main ∷ IO ()
main = writeBMPImages

-- | Convert an YUY2 raw stream to a set of RGBA 'BMP' files.
-- This function is not optimised and consume /a lot/ of CPU ressources.
writeBMPImages ∷ IO ()
writeBMPImages = findVideoDevice ≫= getVideoDevice ≫= \video →
  withVideoDeviceHandle video $ \devh → do
    ctrl ← negotiatePCControl video devh (defaultProbeCommitControl video)
    VideoPipe fmt _ w h xs ← readVideoData video devh ctrl nframes timeout
    let frames = reorderFrames w h xs
        bitmaps = case fmt of
            NV12 → map (rgbaToBMP w h ∘ nv12ToRGBA w h) frames
            YUY2 → map (rgbaToBMP w h ∘ yuy2ToRGBA) frames
            _    → error "Unknown format"
    foo (0 ∷ Int) bitmaps
    return ()

  where
    nframes = 10
    timeout = noTimeout

    foo _ []     = return ()
    foo i (x:xs) = do
        let filename = printf "/tmp/uvc_%03d.bmp" i
        printf "writing file [%s]\n" filename
        writeBMP filename x
        foo (i+1) xs

findVideoDevice ∷ IO Device
findVideoDevice = newCtx ≫= getDevices ≫= \devices →
    case find hasVideoInterface devices of
         Nothing → error "Video device not found !"
         Just d  → do putStrLn $ "Using VideoDevice := " ⧺ show d
                      return d

-- | Convert a raw RGBA frame of dimension @Width@x@Height@ to an RGBA
-- 'BMP' image.
rgbaToBMP ∷ Width → Height → Frame → BMP
rgbaToBMP w h bs = packRGBA32ToBMP w h bs'
  where
    bs' = reorderBMPByteString w bs

-- | The lines are presented in reverse order to the packRGBA32ToBMP
-- function.
reorderBMPByteString ∷ Width → B.ByteString → B.ByteString
reorderBMPByteString w bs =
    let rows = cut [] bs
    in B.concat rows

  where
    cut acc xs | B.null xs = acc
               | otherwise = cut (B.take l xs:acc) (B.drop l xs)

    l = w * 4 -- width * 4 bits per pixel (rgb+a)

