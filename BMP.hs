{-# LANGUAGE UnicodeSyntax #-}

-- | Handling RGBA images with the "Codec.BMP" module.
module BMP ( rgbaToBMP ) where

import qualified Data.ByteString   as B

import Codec.BMP                ( BMP, packRGBA32ToBMP )
import System.USB.UVC.Internals ( Width, Height, Frame )

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

