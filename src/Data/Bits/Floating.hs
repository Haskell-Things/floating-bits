{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Anselm Jonas Scholl
-- License     :  BSD3
-- Maintainer  :  Anselm Jonas Scholl <anselm.scholl@tu-harburg.de>
-- Stability   :  experimental
-- Portability :  GHC-specific
--
-- Conversions between floating point values and integral values preserving
-- the bit-patterns.
----------------------------------------------------------------------------
module Data.Bits.Floating (
     double2WordBitwise
    ,word2DoubleBitwise
    ,float2WordBitwise
    ,word2FloatBitwise
    ) where

import GHC.Exts
import GHC.Word

foreign import prim "double2WordBwzh"
    double2WordBitwise# :: Double# -> Word#
foreign import prim "word2DoubleBwzh"
    word2DoubleBitwise# :: Word# -> Double#
foreign import prim "float2WordBwzh"
    float2WordBitwise# :: Float# -> Word#
foreign import prim "word2FloatBwzh"
    word2FloatBitwise# :: Word# -> Float#

-- | Convert a 'Double' to a 'Word64' while preserving the bit-pattern.
{-# INLINABLE double2WordBitwise #-}
double2WordBitwise :: Double -> Word64
double2WordBitwise (D# d) = W64# (double2WordBitwise# d)

-- | Convert a 'Word64' to a 'Double' while preserving the bit-pattern.
{-# INLINABLE word2DoubleBitwise #-}
word2DoubleBitwise :: Word64 -> Double
word2DoubleBitwise (W64# w) = D# (word2DoubleBitwise# w)

-- | Convert a 'Float' to a 'Word32' while preserving the bit-pattern.
{-# INLINABLE float2WordBitwise #-}
float2WordBitwise :: Float -> Word32
float2WordBitwise (F# f) = W32# (float2WordBitwise# f)

-- | Convert a 'Word32' to a 'Float' while preserving the bit-pattern.
{-# INLINABLE word2FloatBitwise #-}
word2FloatBitwise :: Word32 -> Float
word2FloatBitwise (W32# w) = F# (word2FloatBitwise# w)
