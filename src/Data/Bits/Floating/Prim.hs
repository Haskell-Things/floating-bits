{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE GHCForeignImportPrim     #-}
{-# LANGUAGE UnliftedFFITypes         #-}
{-# LANGUAGE CPP                      #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Anselm Jonas Scholl
-- License     :  BSD3
-- Maintainer  :  Anselm Jonas Scholl <anselm.scholl@tu-harburg.de>
-- Stability   :  experimental
-- Portability :  GHC-specific
--
-- Primitive operations to coerce floating point numbers to integral numbers
-- preserving their bitwise representations as well as functions operating on
-- boxed values.
----------------------------------------------------------------------------

#include "MachDeps.h"

module Data.Bits.Floating.Prim where

import GHC.Exts
import GHC.Word

#if WORD_SIZE_IN_BITS == 64 && MIN_VERSION_base(4,17,0)
-- The name of Word# changed to Word64# in ghc 9.4.1
foreign import prim "double2WordBwzh"
    double2WordBitwise# :: Double# -> Word64#
foreign import prim "word2DoubleBwzh"
    word2DoubleBitwise# :: Word64# -> Double#
#elif WORD_SIZE_IN_BITS == 64
foreign import prim "double2WordBwzh"
    double2WordBitwise# :: Double# -> Word#
foreign import prim "word2DoubleBwzh"
    word2DoubleBitwise# :: Word# -> Double#
#elif WORD_SIZE_IN_BITS == 32 && MIN_VERSION_base(4,17,0)
-- The name of Word# changed to Word64# in ghc 9.4.1                                                                                                                              foreign import prim "double2WordBwzh"
    double2WordBitwise# :: Double# -> Word64#
foreign import prim "word2DoubleBwzh"
    word2DoubleBitwise# :: Word64# -> Double#
#elif WORD_SIZE_IN_BITS == 32
foreign import prim "double2WordBwzh"
    double2WordBitwise# :: Double# -> Word#
foreign import prim "word2DoubleBwzh"
    word2DoubleBitwise# :: Word# -> Double#
#else
#error "Unsupported word size"
#endif

foreign import prim "float2WordBwzh"
    float2WordBitwise# :: Float# -> Word32#
foreign import prim "word2FloatBwzh"
    word2FloatBitwise# :: Word32# -> Float#

-- | Convert a 'Double' to a 'Word64' while preserving the bit-pattern.
{-# INLINE double2WordBitwise #-}
double2WordBitwise :: Double -> Word64
double2WordBitwise (D# d) = W64# (double2WordBitwise# d)

-- | Convert a 'Word64' to a 'Double' while preserving the bit-pattern.
{-# INLINE word2DoubleBitwise #-}
word2DoubleBitwise :: Word64 -> Double
word2DoubleBitwise (W64# w) = D# (word2DoubleBitwise# w)

-- | Convert a 'Float' to a 'Word32' while preserving the bit-pattern.
{-# INLINE float2WordBitwise #-}
float2WordBitwise :: Float -> Word32
float2WordBitwise (F# f) = W32# (float2WordBitwise# f)

-- | Convert a 'Word32' to a 'Float' while preserving the bit-pattern.
{-# INLINE word2FloatBitwise #-}
word2FloatBitwise :: Word32 -> Float
word2FloatBitwise (W32# w) = F# (word2FloatBitwise# w)
