{-# LANGUAGE UnicodeSyntax #-}

-- | Converting from YUY2 video stream to a list of "Codec.BMP" images.
module BMP ( yuy2ToBMP ) where

import qualified Data.ByteString   as B

import Codec.BMP                ( BMP, packRGBA32ToBMP )
import Codec.UVC.RGBA           ( yuy2ToRGBA )
import System.USB.UVC.Internals ( Width, Height, Frame )

import Prelude.Unicode          ( (∘) )

-- | Convert a raw YUY2 frame of dimension @Width@x@Height@ to an RGBA
-- 'BMP' image.
yuy2ToBMP ∷ Width → Height → Frame → BMP
yuy2ToBMP w h bs = packRGBA32ToBMP w h bs'
  where
    bs' = reorderBMPByteString w ∘ yuy2ToRGBA $ bs

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

