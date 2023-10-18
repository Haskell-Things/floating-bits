{-# LANGUAGE ForeignFunctionInterface #-}
module TestUtils where

import Data.Word
import Data.Bits

import Numeric

-- we do not test NaNs here because their binary representation can be changed
-- without changing their value and some processors have been observed to do that.
-- they are handled in the exhaustive test only.

testD :: [(Double, Word64)]
testD = [( 0.0,   0x0000000000000000)
        ,(-0.0,   0x8000000000000000)
        ,( 1.0,   0x3FF0000000000000)
        ,(-1.0,   0xBFF0000000000000)
        ,( 2.0,   0x4000000000000000)
        ,(-2.0,   0xC000000000000000)
        ,( 1.5,   0x3FF8000000000000)
        ,(-1.5,   0xBFF8000000000000)
        ,(pi,     0x400921fb54442d18)
        ,(sqrt 2, 0x3ff6a09e667f3bcd)
        ,( 1E100, 0x54b249ad2594c37d)
        ,(-1E100, 0xD4b249ad2594c37d)
        ,(1/0,    0x7ff0000000000000)
        ,(-1/0,   0xfff0000000000000)
        ,( 1.7976931348623157e+308, 0x7fefffffffffffff)
        ,(-1.7976931348623157e+308, 0xffefffffffffffff)
        ,( 4.94065645841246544176568792868E-324, 0x0000000000000001)
        ,( 9.88131291682493088353137585736E-324, 0x0000000000000002)
        ,( 1.26480805335359115309201610974E-321, 0x0000000000000100)
        ,( 7.29112201955639749211007456815E-304, 0x0100000000000000)
        ,( 5.48612406879368990142018767619E303,  0x7F00000000000001)
        ,( 1.19453052916149551265420436166E103,  0x5555555555555555)
        ,(-4.94065645841246544176568792868E-324, 0x8000000000000001)
        ,(-9.88131291682493088353137585736E-324, 0x8000000000000002)
        ,(-1.26480805335359115309201610974E-321, 0x8000000000000100)
        ,(-7.29112201955639749211007456815E-304, 0x8100000000000000)
        ,(-5.48612406879368990142018767619E303,  0xFF00000000000001)
        ,(-1.19453052916149551265420436166E103,  0xD555555555555555)
        ]

testF :: [(Float, Word32)]
testF = [( 0.0,   0x00000000)
        ,(-0.0,   0x80000000)
        ,( 1.0,   0x3F800000)
        ,(-1.0,   0xBF800000)
        ,( 2.0,   0x40000000)
        ,(-2.0,   0xC0000000)
        ,( 1.5,   0x3FC00000)
        ,(-1.5,   0xBFC00000)
        ,(pi,     0x40490fdb)
        ,(sqrt 2, 0x3fb504f3)
        ,( 1E30,  0x7149f2ca)
        ,(-1E30,  0xf149f2ca)
        ,(1/0,    0x7f800000)
        ,(-1/0,   0xff800000)
        ,( 3.4028235e+38, 0x7f7fffff)
        ,(-3.4028235e+38, 0xff7fffff)
        ,( 1.40129846432481707092372958329E-45, 0x00000001)
        ,( 2.80259692864963414184745916658E-45, 0x00000002)
        ,( 3.58732406867153170156474773322E-43, 0x00000100)
        ,( 2.35098870164457501593747307444E-38, 0x01000000)
        ,( 1.70141203742878835383357727663E38,  0x7F000001)
        ,( 1.4660154687488E13,                  0x55555555)
        ,(-1.40129846432481707092372958329E-45, 0x80000001)
        ,(-2.80259692864963414184745916658E-45, 0x80000002)
        ,(-3.58732406867153170156474773322E-43, 0x80000100)
        ,(-2.35098870164457501593747307444E-38, 0x81000000)
        ,(-1.70141203742878835383357727663E38,  0xFF000001)
        ,(-1.4660154687488E13,                  0xD5555555)
        ]

foreign import ccall unsafe "float2word_ref" refFloatToWord :: Float -> Word32
foreign import ccall unsafe "word2float_ref" refWordToFloat :: Word32 -> Float
foreign import ccall unsafe "double2word_ref" refDoubleToWord :: Double -> Word64
foreign import ccall unsafe "word2double_ref" refWordToDouble :: Word64 -> Double

refFloatDown, refFloatUp :: Float -> Float
refFloatDown x = c_nextafterf x (-1/0)
refFloatUp   x = c_nextafterf x (1/0)

refDoubleDown, refDoubleUp :: Double -> Double
refDoubleDown x = c_nextafter x (-1/0)
refDoubleUp   x = c_nextafter x (1/0)

foreign import ccall unsafe "nextafter" c_nextafter :: Double -> Double -> Double
foreign import ccall unsafe "nextafterf" c_nextafterf :: Float -> Float -> Float

foreign import ccall unsafe "float_ulp_ref" refFloatUlp :: Float -> Float
foreign import ccall unsafe "double_ulp_ref" refDoubleUlp :: Double -> Double

-- | Show the hexadecimal representation of a number.
{-# SPECIALIZE showW :: Word32 -> String #-}
{-# SPECIALIZE showW :: Word64 -> String #-}
showW :: (Integral a, Show a) => a -> String
showW w = '0' : 'x' : showHex w ""
