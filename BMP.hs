{-# LANGUAGE UnicodeSyntax #-}

-- | Converting from YUY2 video stream to a list of RGBA BMP images.
module BMP ( yuy2ToBMP ) where

import qualified Data.ByteString   as B

import Codec.UVC.RGBA  ( yuy2ToRGBA )
import Codec.BMP       ( BMP, packRGBA32ToBMP )

import Prelude.Unicode ( (∘) )

yuy2ToBMP ∷ Int → Int → B.ByteString → BMP
yuy2ToBMP w h bs = packRGBA32ToBMP w h bs'
  where
    bs' = reorderBMPByteString w ∘ yuy2ToRGBA $ bs

-- | The lines are presented in reverse order to the packRGBA32ToBMP
-- function.
reorderBMPByteString ∷ Int → B.ByteString → B.ByteString
reorderBMPByteString w bs =
    let rows = cut [] bs
    in B.concat rows

  where
    cut acc xs | B.null xs = acc
               | otherwise = cut (B.take l xs:acc) (B.drop l xs)

    l = w * 4 -- width * 4 bits per pixel (rgb+a)

