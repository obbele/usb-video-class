{-# LANGUAGE UnicodeSyntax #-}
-- | An utility module to convert from YUY2 to RGBA.

module Codec.UVC.RGBA ( yuy2ToRGBA ) where

import qualified Data.ByteString as B

import Data.Bits       ( shiftR )
import Data.Word       ( Word8 )

import Prelude.Unicode ( (≥) )

{----------------------------------------------------------------------
-- Converting from YUY2 to RGBA pixels.
----------------------------------------------------------------------}

-- | Converting from YUY2 4:2:0 pixels to RGBA ones.
yuy2ToRGBA ∷ B.ByteString → B.ByteString
yuy2ToRGBA bs = f [] bs
  where
    f acc xs | B.null xs = B.pack $ reverse acc
             | otherwise = let [y0, u0, y1, v0] = B.unpack (B.take 4 xs)
                               (r0,g0,b0) = yuv2rgb (y0,u0,v0)
                               (r1,g1,b1) = yuv2rgb (y1,u0,v0)
                           in f (255:b1:g1:r1:255:b0:g0:r0:acc) (B.drop 4 xs)

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

