module Main where

import Data.Word
import Data.Bits.Floating
import Numeric

-- it would be nice to test NaNs, but equality is hard for NaNs and their sign
-- bit seems to be undefined in GHC (or in general? what does it even mean?)...

testD :: [(Double, Word64)]
testD = [( 0.0,     0x0000000000000000)
        ,(-0.0,     0x8000000000000000)
        ,( 1.0,     0x3FF0000000000000)
        ,(-1.0,     0xBFF0000000000000)
        ,( 2.0,     0x4000000000000000)
        ,(-2.0,     0xC000000000000000)
        ,( 1.5,     0x3FF8000000000000)
        ,(-1.5,     0xBFF8000000000000)
        ,(pi,       0x400921fb54442d18)
        ,(sqrt 2,   0x3ff6a09e667f3bcd)
        ,(10**100,  0x54b249ad2594c37d)
        ,(-10**100, 0xD4b249ad2594c37d)
        ,(1/0,      0x7ff0000000000000)
        ,(-1/0,     0xfff0000000000000)
        ]

testF :: [(Float, Word32)]
testF = [( 0.0,     0x00000000)
        ,(-0.0,     0x80000000)
        ,( 1.0,     0x3F800000)
        ,(-1.0,     0xBF800000)
        ,( 2.0,     0x40000000)
        ,(-2.0,     0xC0000000)
        ,( 1.5,     0x3FC00000)
        ,(-1.5,     0xBFC00000)
        ,(pi,       0x40490fdb)
        ,(sqrt 2,   0x3fb504f3)
        ,(10**30,   0x7149f2ca)
        ,(-10**30,  0xf149f2ca)
        ,(1/0,      0x7f800000)
        ,(-1/0,     0xff800000)
        ]

main :: IO ()
main = do
    mapM_ helperD testD
    mapM_ helperF testF
    where
        helperD (d, w) | double2WordBitwise d /= w        = failTest (show d) (showW w) (showW (double2WordBitwise d))
                       | word2DoubleBitwise w `eqFloat` d = passTest (showW w) (show d)
                       | otherwise                        = failTest (showW w) (show d) (show (word2DoubleBitwise w))
        helperF (f, w) | float2WordBitwise f /= w        = failTest (show f) (showW w) (showW (float2WordBitwise f))
                       | word2FloatBitwise w `eqFloat` f = passTest (showW w) (show f)
                       | otherwise                       = failTest (showW w) (show f) (show (word2FloatBitwise w))

failTest :: String -> String -> String -> IO ()
failTest a b c = fail $ "Conversion from " ++ a ++ " to " ++ b ++ " failed. Got " ++ c

passTest :: String -> String -> IO ()
passTest a b = putStrLn $ "Conversion between " ++ a ++ " and " ++ b ++ " worked."

showW :: (Integral a, Show a) => a -> String
showW w = '0' : 'x' : showHex w ""

eqFloat :: RealFloat f => f -> f -> Bool
eqFloat a b = (a == b) && (isNegativeZero a == isNegativeZero b)