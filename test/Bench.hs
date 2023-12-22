-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Anselm Jonas Scholl, (C) 2023 Julia Longtin
-- License     :  BSD3
-- Maintainer  :  Julia Longtin <Julia.Longtin@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC-specific
--

module Main where

import Prelude (Double, Float, IO, ($), (++), concatMap, map, shows, unzip)

import TestUtils (refDoubleDown, refDoubleToWord, refDoubleUp, refDoubleUlp, refFloatDown, refFloatToWord, refFloatUp, refFloatUlp, refWordToDouble, refWordToFloat, showW, testD, testF)

import Data.Word (Word32, Word64)

import Data.Bits.Floating (coerceToFloat, coerceToWord, nextDown, nextUp, ulp)

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nf)

mkSingleBenchmarks :: ((Float, Word32) -> [Benchmark]) -> ((Double, Word64) -> [Benchmark]) -> [Benchmark]
mkSingleBenchmarks mkFTest mkDTest = concatMap mkFTest testF ++ concatMap mkDTest testD

singleCoercionBenchmarks :: [Benchmark]
singleCoercionBenchmarks = mkSingleBenchmarks mkFTest mkDTest
    where
        mkFTest :: (Float, Word32) -> [Benchmark]
        mkFTest (f, w) = [bench ("coerceToWord :: Float -> Word32 (" ++ shows f ")") $ nf coerceToWord f
                         ,bench ("reference coerceToWord :: Float -> Word32 (" ++ shows f ")") $ nf refFloatToWord f
                         ,bench ("coerceToFloat :: Word32 -> Float (" ++ showW w ++ ")") $ nf (coerceToFloat :: Word32 -> Float) w
                         ,bench ("reference coerceToFloat :: Word32 -> Float (" ++ showW w ++ ")") $ nf refWordToFloat w
                         ]
        mkDTest :: (Double, Word64) -> [Benchmark]
        mkDTest (f, w) = [bench ("coerceToWord :: Double -> Word64 (" ++ shows f ")") $ nf coerceToWord f
                         ,bench ("reference coerceToWord :: Double -> Word64 (" ++ shows f ")") $ nf refDoubleToWord f
                         ,bench ("coerceToFloat :: Word64 -> Double (" ++ showW w ++ ")") $ nf (coerceToFloat :: Word64 -> Double) w
                         ,bench ("reference coerceToFloat :: Word64 -> Double (" ++ showW w ++ ")") $ nf refWordToDouble w
                         ]

singleAdjacentBenchmarks :: [Benchmark]
singleAdjacentBenchmarks = mkSingleBenchmarks mkFTest mkDTest
    where
        mkFTest :: (Float, Word32) -> [Benchmark]
        mkFTest (f, _) = [bench ("nextUp Float (" ++ shows f ")") $ nf nextUp f
                         ,bench ("reference nextUp Float (" ++ shows f ")") $ nf refFloatUp f
                         ,bench ("nextDown Float (" ++ shows f ")") $ nf nextDown f
                         ,bench ("reference nextDown Float (" ++ shows f ")") $ nf refFloatDown f
                         ]
        mkDTest :: (Double, Word64) -> [Benchmark]
        mkDTest (f, _) = [bench ("nextUp Double (" ++ shows f ")") $ nf nextUp f
                         ,bench ("reference nextUp Double (" ++ shows f ")") $ nf refDoubleUp f
                         ,bench ("nextDown Double (" ++ shows f ")") $ nf nextDown f
                         ,bench ("reference nextDown Double (" ++ shows f ")") $ nf refDoubleDown f
                         ]

singleUlpBenchmarks :: [Benchmark]
singleUlpBenchmarks = mkSingleBenchmarks mkFTest mkDTest
    where
        mkFTest :: (Float, Word32) -> [Benchmark]
        mkFTest (f, _) = [bench ("ulp Float (" ++ shows f ")") $ nf ulp f
                         ,bench ("reference ulp Float (" ++ shows f ")") $ nf refFloatUlp f
                         ]
        mkDTest :: (Double, Word64) -> [Benchmark]
        mkDTest (f, _) = [bench ("ulp Double (" ++ shows f ")") $ nf ulp f
                         ,bench ("reference ulp Double (" ++ shows f ")") $ nf refDoubleUlp f
                         ]

batchCoercionBenchmarks :: [Benchmark]
batchCoercionBenchmarks =
    [bench "coerceToWord :: Float -> Word32"  $ nf (map coerceToWord) fs
    ,bench "coerceToWord :: Double -> Word64"  $ nf (map coerceToWord) ds
    ,bench "coerceToFloat :: Word32 -> Float"  $ nf (map coerceToFloat :: [Word32] -> [Float]) w32s
    ,bench "coerceToFloat :: Word64 -> Double"  $ nf (map coerceToFloat :: [Word64] -> [Double]) w64s

    ,bench "reference coerceToWord :: Float -> Word32"  $ nf (map refFloatToWord) fs
    ,bench "reference coerceToWord :: Double -> Word64"  $ nf (map refDoubleToWord) ds
    ,bench "reference coerceToFloat :: Word32 -> Float"  $ nf (map refWordToFloat) w32s
    ,bench "reference coerceToFloat :: Word64 -> Double"  $ nf (map refWordToDouble) w64s
    ]
    where
        (fs, w32s) = unzip testF
        (ds, w64s) = unzip testD

batchAdjacentBenchmarks :: [Benchmark]
batchAdjacentBenchmarks =
    [bench "nextUp Float"  $ nf (map nextUp) fs
    ,bench "nextUp Double"  $ nf (map nextUp) ds
    ,bench "nextDown Float"  $ nf (map nextDown) fs
    ,bench "nextDown Double"  $ nf (map nextDown) ds

    ,bench "reference nextUp Float"  $ nf (map refFloatUp) fs
    ,bench "reference nextUp Double"  $ nf (map refDoubleUp) ds
    ,bench "reference nextDown Float"  $ nf (map refFloatDown) fs
    ,bench "reference nextDown Double"  $ nf (map refDoubleDown) ds
    ]
    where
        (fs, _) = unzip testF
        (ds, _) = unzip testD

batchUlpBenchmarks :: [Benchmark]
batchUlpBenchmarks =
    [bench "ulp Float"  $ nf (map ulp) fs
    ,bench "ulp Double" $ nf (map ulp) ds
    ,bench "reference ulp Float"  $ nf (map refFloatUlp) fs
    ,bench "reference ulp Double" $ nf (map refDoubleUlp) ds
    ]
    where
        (fs, _) = unzip testF
        (ds, _) = unzip testD

main :: IO ()
main = defaultMain
    [bgroup "coercions - single" singleCoercionBenchmarks
    ,bgroup "coercions - batch" batchCoercionBenchmarks
    ,bgroup "adjacent - single" singleAdjacentBenchmarks
    ,bgroup "adjacent - batch" batchAdjacentBenchmarks
    ,bgroup "ulp - single" singleUlpBenchmarks
    ,bgroup "ulp - batch" batchUlpBenchmarks
    ]
