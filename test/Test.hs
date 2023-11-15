{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import TestUtils

import Data.Word
import Data.Bits
import Data.Bits.Floating

import Control.Concurrent
import Control.Exception
import Control.Monad

main :: IO ()
main = do
    -- test coercions
    mapM_ testCoercion testD
    mapM_ testCoercion testF
    -- test special values
    smallTest
    -- test every float
    exhaustiveTest

{-# INLINE debug #-}
debug :: Bool
debug = False

-- | Given a tuple of a floating point number and the corresponding word,
--   test if coercion between these values works.
{-# SPECIALIZE testCoercion :: (Float, Word32) -> IO () #-}
{-# SPECIALIZE testCoercion :: (Double, Word64) -> IO () #-}
testCoercion
  :: forall f w m
   . ( Show f
     , Show w
     , Integral w
     , RealFloat f
     , FloatingBits f w
     , MonadFail m
     )
  => (f, w)
  -> m ()
testCoercion (f, w) = do
    let w'  = (coerceToWord :: f -> w) f
        f'  = (coerceToFloat :: w -> f)  w
        w'' = (coerceToWord :: f -> w) f'
    unless (w' == w) $ failTest (show f) (showW w) (showW w')
    unless (f' `eqFloat` f) $ failTest (showW w) (show f) (show f')
    unless (w'' == w) $ failTest (show f') (showW w) (showW w'')

-- | Called when a conversion fails.
{-# SPECIALIZE failTest :: String -> String -> String -> IO () #-}
failTest :: MonadFail m => String -> String -> String -> m ()
failTest from wanted got = fail $ "Conversion from " ++ from ++ " to " ++ wanted ++ " failed. Got " ++ got

-- | Check if two floats are really equal: Not only equal as defined by the IEEE
--   754 standard, but also that if one is a negative zero, the other is one too.
--   If one argument is a NaN, the other should be also a NaN.
{-# SPECIALIZE eqFloat :: Float -> Float -> Bool #-}
{-# SPECIALIZE eqFloat :: Double -> Double -> Bool #-}
eqFloat :: RealFloat f => f -> f -> Bool
eqFloat a b | isNaN a   = isNaN b
            | otherwise = (a == b) && (isNegativeZero a == isNegativeZero b)

{-# SPECIALIZE eqFloatSpecial :: (Float -> Float) -> (Float -> Float) -> Float -> Float -> Bool #-}
{-# SPECIALIZE eqFloatSpecial :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Bool #-}
eqFloatSpecial :: RealFloat f => (f -> f) -> (f -> f) -> f -> f -> Bool
eqFloatSpecial f g a b | isInfinite b && not (isInfinite a) = eqFloatZero (f a) b
                       | isInfinite a && not (isInfinite b) = eqFloatZero a (g b)
                       | otherwise = eqFloatZero a b

{-# SPECIALIZE eqFloatZero :: Float -> Float -> Bool #-}
{-# SPECIALIZE eqFloatZero :: Double -> Double -> Bool #-}
eqFloatZero :: RealFloat f => f -> f -> Bool
eqFloatZero a b | isNaN a   = isNaN b
                | otherwise = a == b

{-# SPECIALIZE geqFloat :: Float -> Float -> Bool #-}
{-# SPECIALIZE geqFloat :: Double -> Double -> Bool #-}
geqFloat :: RealFloat f => f -> f -> Bool
geqFloat a b | isNaN a   = isNaN b
             | otherwise = a >= b

{-# SPECIALIZE leqFloat :: Float -> Float -> Bool #-}
{-# SPECIALIZE leqFloat :: Double -> Double -> Bool #-}
leqFloat :: RealFloat f => f -> f -> Bool
leqFloat a b | isNaN a   = isNaN b
             | otherwise = a <= b

-- | Check if two words describe the same floating point value.
--
--   We need this because the processor may decide to change the representation
--   of a NaN value we load into a floating point register.
eqWord :: Word32 -> Word32 -> Bool
eqWord w1 w2 = w1 == w2 || (isNaNWord w1 && isNaNWord w2)
    where
        isNaNWord w = (w .&. complement 0x7F800000 /= 0) && (w .&. 0x7F800000 == 0x7F800000)

-- | Test interesting float and double values.
smallTest :: IO ()
smallTest = do
    mapM_ (doTest . snd) testF
    mapM_ (testNextPrev refDoubleUp refDoubleDown . fst) testD

-- | Test ALL 2^32 floating point values for required properties.
exhaustiveTest :: IO ()
exhaustiveTest = do
    cpus <- fromIntegral <$> getNumCapabilities
    mvar <- newEmptyMVar
    forM_ [0..cpus - 1] $ \ cpu -> void $ forkIO $ do
        res <- try $ go cpus (minBound + cpu)
        putMVar mvar $ either (throwIO :: SomeException -> IO ()) return res
    replicateM_ (fromIntegral cpus) $ readResult mvar

-- | Read the contents of an MVar and execute them.
readResult :: MVar (IO ()) -> IO ()
readResult mvar = join $ takeMVar mvar

go :: Word32 -> Word32 -> IO ()
go cpus cur = do
    doTest cur
    reportProgress cur
    unless (maxBound - cur < cpus) $ go cpus (cur + cpus)

{-# SPECIALIZE doTest :: Word32 -> IO () #-}
doTest :: MonadFail m => Word32 -> m ()
doTest w = do
    -- test coercions
    let !refFloat   = refWordToFloat w :: Float
        !refWord    = refFloatToWord refFloat :: Word32
        !double     = realToFrac refFloat :: Double
        !refWord2   = refDoubleToWord double :: Word64
        !refDouble  = refWordToDouble refWord2 :: Double

        !testFloat  = coerceToFloat w :: Float
        !testWord   = coerceToWord testFloat :: Word32
        !testWord2  = coerceToWord double :: Word64
        !testDouble = coerceToFloat testWord2 :: Double

        !refFUlp    = refFloatUlp testFloat
        !refDUlp    = refDoubleUlp testDouble
        !fUlp       = ulp testFloat
        !dUlp       = ulp testDouble
    -- assert reference and real implementation yield the same floats
    testAssert (showW w) refFloat testFloat eqFloat "==" "word -> float"
    -- same for doubles
    testAssert (showW refWord2) refDouble testDouble eqFloat "==" "word -> double"
    -- test coercing back
    testAssert (showFloat testFloat) refWord testWord (==) "==" "float -> word"
    -- and the same for doubles
    testAssert (showFloat testDouble) refWord2 testWord2 (==) "==" "double -> word"
    -- check that the reference implementation yields the same value
    -- as in the beginning
    -- we do not have to check the real implementation, as we already
    -- checked refWord == testWord.
    testAssert (showFloat refFloat) refWord w eqWord "==" "word -> float -> word (ref)"
    -- test nextUp / nextDown
    testNextPrev refFloatUp refFloatDown testFloat
    testNextPrev refDoubleUp refDoubleDown testDouble
    -- test ulp
    testAssert (showFloat testFloat) refFUlp fUlp eqFloat "==" "ulp (float)"
    testAssert (showFloat testDouble) refDUlp dUlp eqFloat "==" "ulp (double)"

{-# SPECIALIZE INLINE testNextPrev :: (Float -> Float) -> (Float -> Float) -> Float -> IO () #-}
{-# SPECIALIZE INLINE testNextPrev :: (Double -> Double) -> (Double -> Double) -> Double -> IO () #-}
testNextPrev
  :: ( MonadFail m
     , RealFloat f
     , FloatingBits f w
     , Show f
     , ShowFloat f
     )
  => (f -> f)
  -> (f -> f)
  -> f
  -> m ()
testNextPrev refNextUp refNextDown testFloat = do
    let !refNextFloat = refNextUp testFloat
        !refPrevFloat = refNextDown testFloat
        !refOrigFloat1 = refNextDown refNextFloat
        !refOrigFloat2 = refNextUp refPrevFloat
        !nextFloat = nextUp testFloat
        !prevFloat = nextDown testFloat
        !origFloat1 = nextDown nextFloat
        !origFloat2 = nextUp prevFloat
    -- check next float operation
    testAssert (showFloat testFloat) refNextFloat nextFloat eqFloat "==" "next float"
    -- check prev float operation
    testAssert (showFloat testFloat) refPrevFloat prevFloat eqFloat "==" "prev float"
    -- check that next -> prev (or prev -> next) yields the same float again
    testAssert (showFloat testFloat) refOrigFloat1 origFloat1 eqFloat "==" "next float -> prev float"
    testAssert (showFloat testFloat) refOrigFloat2 origFloat2 eqFloat "==" "prev float -> next float"
    -- check of the two ways yield the same float
    testAssert (showFloat testFloat) origFloat1 origFloat2 (eqFloatSpecial refNextUp refNextDown) "==" "next -> prev == prev -> next"
    -- check that next is >= orig
    testAssert (showFloat testFloat) nextFloat testFloat geqFloat ">=" "next >= orig"
    -- check prev <= orig
    testAssert (showFloat testFloat) prevFloat testFloat leqFloat "<=" "prev <= orig"

{-# SPECIALIZE testAssert :: String -> Word32 -> Word32 -> (Word32 -> Word32 -> Bool) -> String -> String -> IO () #-}
{-# SPECIALIZE testAssert :: String -> Float -> Float -> (Float -> Float -> Bool) -> String -> String -> IO () #-}
{-# SPECIALIZE testAssert :: String -> Word64 -> Word64 -> (Word64 -> Word64 -> Bool) -> String -> String -> IO () #-}
{-# SPECIALIZE testAssert :: String -> Double -> Double -> (Double -> Double -> Bool) -> String -> String -> IO () #-}
testAssert :: (MonadFail m, Show a) => String -> a -> a -> (a -> a -> Bool) -> String -> String -> m ()
testAssert ts a b f s s2 = unless (f a b) $
    fail $ "Assert failed: " ++ show a ++ " " ++ s ++ " " ++ show b ++ ": " ++ s2 ++ " (" ++ ts ++ ")"

reportProgress :: Word32 -> IO ()
reportProgress w = when (w `rem` percent == 0) $
    putStrLn $ "Exhaustive test: " ++ shows (w `quot` percent) " %"
    where
        percent :: Word32
        percent = maxBound `quot` 100
