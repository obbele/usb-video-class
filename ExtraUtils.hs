{-# LANGUAGE UnicodeSyntax #-}

module ExtraUtils where

-- The UnicodeSyntax provides         -- ∷ ⇒ ∀ → ← ⤙ ⤚ ⤛ ⤜
-- import Control.Applicative.Unicode -- ⊛ ∅
-- import Control.Arrow.Unicode       -- ⋙  ⋘  ⁂  ⧻  ⫴
-- import Control.Category.Unicode    -- ≫> ⋘  ∘
import Control.Monad.Unicode       -- ≫= ≫ =≪
-- import Data.Bool.Unicode           -- ∧ ∨ ¬
-- import Data.Eq.Unicode             -- ≡ ≠
-- import Data.Foldable.Unicode       -- ∈ ∋ ∉ ∌
import Data.Function.Unicode       -- ∘
-- import Data.List.Unicode           -- ⧺ ∪ ∖ ∆ ∩
-- import Data.Monoid.Unicode         -- ∅ ⊕
-- import Data.Ord.Unicode            -- ≤ ≥ ≮ ≯
import Prelude.Unicode             -- π ÷ ⊥ ⋅
-- import Data.IntSet.Unicode         -- ∅ ⊆ ⊇ ⊈ ⊉ ⊂ ⊃ ⊄ ⊅
-- import Data.Sequence.Unicode       -- ⊲ ⊳ ⋈

import System.USB
import Utils ( decodeBCD, bits )

import qualified Data.ByteString as B
import qualified Data.Text       as T

import Data.Word
import Data.Bits
import Data.List ( find )

bGet ∷ B.ByteString → Int → Word8
bGet bs offset = B.index bs offset

wGet ∷ B.ByteString → Int → Word16
wGet bs offset = byteStringToInteger $ subByteString bs offset 2

dwGet ∷ B.ByteString → Int → Word32
dwGet bs offset = byteStringToInteger $ subByteString bs offset 4

byteStringToInteger ∷ Num α ⇒ B.ByteString → α
byteStringToInteger bs = fromInteger $ B.foldr' f 0 bs
  where
    f w8 acc = acc ⋅ 256 + toInteger w8

subByteString ∷ B.ByteString → Int → Int → B.ByteString
subByteString bs offset len = B.take len ∘ B.drop offset $ bs

unmarshalBitmask ∷ Bits α ⇒ [(Int, a)] → α → [a]
unmarshalBitmask table value = foldr test [] table
  where test (i, ctrl) acc | value `testBit` i = (ctrl:acc)
                           | otherwise         = acc

marshalBitmask ∷ (Eq a, Bits α) ⇒ [(Int, a)] → [a] → α
marshalBitmask table xs = foldr test 0 xs
  where test ctrl acc = case find (\(_,a) → a ≡ ctrl) table of
               Just (i,_) → acc `setBit` i
               _          → acc

getUSBString ∷ DeviceHandle → Maybe StrIx → IO (Maybe String)
getUSBString devh mStrIx =
    case mStrIx of
         Nothing → return Nothing
         Just i  → getStrDesc devh i langID 50 ≫= return ∘ Just ∘ T.unpack
  where
    langID = (0x09, 0x01) -- US english

getDeviceName ∷ Device → IO (Maybe String)
getDeviceName dev = withDeviceHandle dev $ flip getUSBString strIx
  where
    strIx = deviceProductStrIx (deviceDesc dev)

{----------------------------------------------------------------------
-- Copied-and-pasted from System.USB.Internal
----------------------------------------------------------------------}

unmarshalReleaseNumber ∷ Word16 → ReleaseNumber
unmarshalReleaseNumber abcd = (a, b, c, d)
    where
      [a, b, c, d] = map fromIntegral $ decodeBCD 4 abcd

unmarshalStrIx ∷ Word8 → Maybe StrIx
unmarshalStrIx 0     = Nothing
unmarshalStrIx strIx = Just strIx

unmarshalEndpointAddress ∷ Word8 → EndpointAddress
unmarshalEndpointAddress a =
    EndpointAddress { endpointNumber    = fromIntegral $ bits 0 3 a
                    , transferDirection = if testBit a 7 then In else Out
                    }
