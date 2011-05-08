{-# LANGUAGE UnicodeSyntax #-}
-- | An utility module to convert from YUY2 to RGBA.

module Codec.UVC.RGBA ( yuy2ToRGBA, nv12ToRGBA ) where

import qualified Control.Exception as E
import qualified Data.ByteString   as B

import System.USB.UVC.Descriptors ( Width, Height )

import System.USB                 ( USBException(IOException) )
import Data.Bits                  ( shiftR )
import Data.Word                  ( Word8 )

import Prelude.Unicode            ( (≥), (∘) )

{----------------------------------------------------------------------
-- Converting from YUY2 to RGBA pixels.
----------------------------------------------------------------------}

-- | Converting from packed YUY2 4:2:2 pixels to RGBA ones.
yuy2ToRGBA ∷ B.ByteString → B.ByteString
yuy2ToRGBA bs = f [] bs
  where
    f acc xs | B.null xs = B.pack $ reverse acc
             | otherwise = let [y0, u0, y1, v0] = B.unpack (B.take 4 xs)
                               (r0,g0,b0) = yuv2rgb (y0,u0,v0)
                               (r1,g1,b1) = yuv2rgb (y1,u0,v0)
                           in f (255:b1:g1:r1:255:b0:g0:r0:acc) (B.drop 4 xs)

{----------------------------------------------------------------------
-- Converting from NV12 to RGBA pixels.
----------------------------------------------------------------------}

-- | Converting from planar NV12 4:2:0 pixels to RGBA ones.
--
-- /EXPERIMENTAL/
nv12ToRGBA ∷ Width → Height → B.ByteString → B.ByteString
nv12ToRGBA w h bs | B.length bs < w * h =         -- not enougth Y values
    E.throw $ IOException "Corrupted NV12 stream"

nv12ToRGBA w h bs | 2 * B.length bs < w * h * 3 = -- not enough U/V values
    B.pack ∘ reverse ∘ B.foldl' f [] $ bs
  where
    f acc x = (255:x:x:x:acc) -- put only the Y value,
                              -- resulting in a gray-level image.

nv12ToRGBA w h bs | otherwise =                   -- good NV12 content
    B.pack $ concatMap f [0.. w * h - 1]
  where
    f i = let m = (i `div` w) `div` 2  -- line index modulo 2
              n = (i `rem` w) `div` 2  -- column index modulo 2
              iuv = m * w `div` 2 + n
              (r,g,b) = yuv2rgb (pixelY i, pixelU iuv, pixelV iuv)
          in [r,g,b,255]

    pixelY i   = B.index bs i
    pixelU i   = B.index bs (w*h+2*i)
    pixelV i   = B.index bs (w*h+2*i+1)

{----------------------------------------------------------------------
-- YUV to RGB pixel conversion.
----------------------------------------------------------------------}

-- | see http://msdn.microsoft.com/en-us/library/ms893078.aspx
yuv2rgb ∷ (Word8, Word8, Word8) → (Word8, Word8, Word8)
yuv2rgb (y,u,v) = (r, g, b)
  where
    c, d, e ∷ Int
    c = fromIntegral y - 16
    d = fromIntegral u - 128
    e = fromIntegral v - 128
    r = clamp $ ((298 * c + 409 * e + 128) `shiftR` 8)
    g = clamp $ ((298 * c - 100 * d - 208 * e + 128) `shiftR` 8)
    b = clamp $ ((298 * c + 516 * d + 128) `shiftR` 8)
    clamp x | x ≥ 255   = 255
            | x < 0     = 0
            | otherwise = fromIntegral x

