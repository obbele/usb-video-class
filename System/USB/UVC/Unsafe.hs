{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}

-- Provide parsing facilities for the Descriptors, Requests and
-- Streaming modules.

module System.USB.UVC.Unsafe
    ( Bitmask(..)
    , BitmaskTable
    , unmarshalBitmask
    , marshalBitmask
    ) where

import Data.Bits       ( Bits, testBit, setBit )
import Data.Data       ( Data )
import Data.List       ( find )
import Data.Typeable   ( Typeable )

import Prelude.Unicode ( (≡) )

-- | A type to help creating 'Serialize' instance of binary objects
-- containing flags.
newtype Bitmask a   = Bitmask { unBitmask ∷ [a] }
    deriving (Eq, Data, Typeable)

instance Show a ⇒ Show (Bitmask a) where
    show (Bitmask a) = show a

-- | An association list mapping bit position (from 0 to n-1) to their
-- equivalent Haskell representation.
type BitmaskTable a = [(Int, a)]

unmarshalBitmask ∷ Bits α ⇒ BitmaskTable a → α → Bitmask a
unmarshalBitmask table value = Bitmask $ foldr test [] table
  where test (i, ctrl) acc | value `testBit` i = (ctrl:acc)
                           | otherwise         = acc

marshalBitmask ∷ (Eq a, Bits α) ⇒ BitmaskTable a → Bitmask a → α
marshalBitmask table (Bitmask xs) = foldr test 0 xs
  where test ctrl acc = case find (\(_,a) → a ≡ ctrl) table of
               Just (i,_) → acc `setBit` i
               _          → acc

