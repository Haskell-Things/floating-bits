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

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE GHCForeignImportPrim     #-}
{-# LANGUAGE UnliftedFFITypes         #-}
{-# LANGUAGE CPP                      #-}

#include "MachDeps.h"

module Data.Bits.Floating.Prim (
  double2WordBitwise,
  float2WordBitwise,
  word2DoubleBitwise,
  word2FloatBitwise) where

import GHC.Exts (Double#, Double(D#), Float#, Float(F#))
import GHC.Word (Word32(W32#), Word64(W64#))

#if WORD_SIZE_IN_BITS == 64 && MIN_VERSION_base(4,17,0)
-- The name of Word# changed to Word64# in ghc 9.4.1
import GHC.Exts (Word64#, Word32#)
#define WORD64 Word64
#define WORD32 Word32
#elif WORD_SIZE_IN_BITS == 64
import GHC.Exts (Word#)
#define WORD64 Word
#if MIN_VERSION_base(4,15,0)
import GHC.Exts (Word32#)
#define WORD32 Word32
#else
#define WORD32 Word
#endif
#else
#error non-X86_64 architectures not supported
#endif

foreign import prim "double2WordBwzh"
    double2WordBitwise# :: Double# -> WORD64#
foreign import prim "word2DoubleBwzh"
    word2DoubleBitwise# :: WORD64# -> Double#
foreign import prim "float2WordBwzh"
    float2WordBitwise# :: Float# -> WORD32#
foreign import prim "word2FloatBwzh"
    word2FloatBitwise# :: WORD32# -> Float#

#undef WORD64
#undef WORD32

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

