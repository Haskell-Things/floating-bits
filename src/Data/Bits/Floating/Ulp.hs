{-# LANGUAGE ScopedTypeVariables #-}
module Data.Bits.Floating.Ulp (
     doubleNextUlp
    ,doublePrevUlp
    ,doubleUlp

    ,floatNextUlp
    ,floatPrevUlp
    ,floatUlp
    ) where

import Data.Int
import Data.Bits
import Data.Bits.Floating.Prim

{-# ANN module "HLint: ignore Eta reduce" #-}

---------------------
-- * Double constants
---------------------

doubleMinValue :: Double
doubleMinValue = 4.94065645841246544176568792868E-324

doubleMaxExponent :: Int64
doubleMaxExponent = 1023

doubleMinExponent :: Int64
doubleMinExponent = -1022

doubleSignificandWidth :: Int64
doubleSignificandWidth = 53

doubleExpBias :: Int64
doubleExpBias = 1023

doubleExpBitMask :: Int64
doubleExpBitMask = 0x7FF0000000000000

--------------------
-- * Float constants
--------------------

floatMinValue :: Float
floatMinValue = 1.40129846432481707092372958329E-45

floatMaxExponent :: Int32
floatMaxExponent = 127

floatMinExponent :: Int32
floatMinExponent = -126

floatSignificandWidth :: Int32
floatSignificandWidth = 24

floatExpBias :: Int32
floatExpBias = 127

floatExpBitMask :: Int32
floatExpBitMask = 0x7F800000

---------------------------
-- * Generic implementation
---------------------------

{-# INLINE genericUp #-}
genericUp :: (RealFloat f, Num w) => (f -> w) -> (w -> f) -> f -> f
genericUp mkW mkF f
    | isNaN f || (isInfinite f && f > 0.0) = f
    | isNegativeZero f = genericUp mkW mkF 0.0
    | otherwise = mkF $ mkW f + (if f >= 0.0 then 1 else -1)

{-# INLINE genericDown #-}
genericDown :: (RealFloat f, Num w) => (f -> w) -> (w -> f) -> f -> f -> f
genericDown mkW mkF minValue f
    | isNaN f || (isInfinite f && f < 0.0) = f
    | f == 0.0  = -minValue
    | otherwise = mkF $ mkW f + (if f > 0.0 then -1 else 1)

{-# INLINE genericUlp #-}
genericUlp :: forall f i w . (RealFloat f, Bits w, Integral w, Bits i, Integral i, Show i) =>
    (f -> w) -> (w -> f) -> i -> i -> i -> i -> i -> f -> f -> f
genericUlp mkW mkF expBitMask significandWidth expBias maxExponent minExponent minValue f
    | expnt == (maxExponent + 1) = abs f -- NaN or INF
    | expnt == (minExponent - 1) = minValue
    | expnt2 >= minExponent      = powerOfTwo expnt2
--    | expnt > maxExponent        = error $ "Exponent too large: " ++ show expnt ++ " > " ++ show maxExponent
--    | expnt < minExponent        = error $ "Exponent too small: " ++ show expnt ++ " < " ++ show minExponent
    | otherwise                  = mkF $ 1 `shiftL` fromIntegral (expnt2 - (minExponent - (significandWidth - 1)))
    where
        expnt :: i
        expnt = ((fromIntegral (mkW f) .&. expBitMask) `shiftR` fromIntegral (significandWidth - 1)) - expBias
        expnt2 :: i
        expnt2 = expnt - (significandWidth - 1)
        powerOfTwo :: i -> f
        powerOfTwo n = mkF $ fromIntegral $ ((n + expBias) `shiftL` fromIntegral (significandWidth - 1)) .&. expBitMask

----------------------------
-- * Specific implementation
----------------------------

-- | Advance a 'Double' by one ULP.
{-# INLINABLE doubleNextUlp #-}
doubleNextUlp :: Double -> Double
doubleNextUlp d = genericUp double2WordBitwise word2DoubleBitwise d

-- | Subtract one ULP from a 'Double'.
{-# INLINABLE doublePrevUlp #-}
doublePrevUlp :: Double -> Double
doublePrevUlp d = genericDown double2WordBitwise word2DoubleBitwise doubleMinValue d

-- | Return the distance to the next floating point number.
{-# INLINABLE doubleUlp #-}
doubleUlp :: Double -> Double
doubleUlp d = genericUlp double2WordBitwise word2DoubleBitwise doubleExpBitMask doubleSignificandWidth doubleExpBias doubleMaxExponent doubleMinExponent doubleMinValue d

-- | Advance a 'Float' by one ULP.
{-# INLINABLE floatNextUlp #-}
floatNextUlp :: Float -> Float
floatNextUlp f = genericUp float2WordBitwise word2FloatBitwise f

-- | Subtract one ULP from a 'Float'.
{-# INLINABLE floatPrevUlp #-}
floatPrevUlp :: Float -> Float
floatPrevUlp f = genericDown float2WordBitwise word2FloatBitwise floatMinValue f

-- | Return the distance to the next floating point number.
{-# INLINABLE floatUlp #-}
floatUlp :: Float -> Float
floatUlp f = genericUlp float2WordBitwise word2FloatBitwise floatExpBitMask floatSignificandWidth floatExpBias floatMaxExponent floatMinExponent floatMinValue f
