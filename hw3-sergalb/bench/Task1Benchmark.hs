{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task1Benchmark where

import Control.Parallel.Strategies (rpar, rseq)
import Criterion.Main (Benchmark, bench, bgroup, nf)
import Task1Geometry

squarePolytope :: Int -> [Point]
squarePolytope n =
  let down = Point <$> [0 .. n] <*> [0]
      right = Point <$> [n] <*> [1 .. n]
      up = Point <$> (reverse [0 .. n - 1]) <*> [n]
      left = Point <$> [0] <*> (reverse [1 .. n - 1])
   in down ++ right ++ up ++ left

perimeterBenchmark :: Benchmark
perimeterBenchmark =
  let power :: Int = 7
      !polytope = (squarePolytope $ (10 ^ power) `div` 4)
   in bgroup
        ("perimeter, vertexes count: 10^" ++ show power)
        [ bench "simple" $ nf simplePerimeter polytope
        , bench "rpar on each element" $ nf (perimeterStrategyEachComputation rpar) polytope
        , bench "rseq on each element" $ nf (perimeterStrategyEachComputation rseq) polytope
        , bench "devide by chunks rpar" $ nf (perimeterDivided rpar) polytope
        , bench "devide by chunks rseq" $ nf (perimeterDivided rseq) polytope
        ]

areaBenchmark :: Benchmark
areaBenchmark =
  let power :: Int = 7
      !polytope = (squarePolytope $ (10 ^ power `div` 4))
   in bgroup
        ("area, vertexes count: 10^" ++ show power)
        [ bench "simple" $ nf simplePerimeter polytope
        , bench "rpar on each element" $ nf (perimeterStrategyEachComputation rpar) polytope
        , bench "rseq on each element" $ nf (perimeterStrategyEachComputation rseq) polytope
        , bench "devide by chunks rpar" $ nf (perimeterDivided rpar) polytope
        , bench "devide by chunks rseq" $ nf (perimeterDivided rseq) polytope
        ]


-- | My results
-- benchmarking perimeter, vertexes count: 10^7/simple
-- time                 336.4 ms   (328.9 ms .. 345.1 ms)
--                      0.999 R²   (0.998 R² .. 1.000 R²)
-- mean                 298.0 ms   (275.6 ms .. 314.0 ms)
-- std dev              24.41 ms   (12.14 ms .. 32.55 ms)
-- variance introduced by outliers: 18% (moderately inflated)
-- 
-- benchmarking perimeter, vertexes count: 10^7/rpar on each element
-- time                 2.115 s    (1.951 s .. 2.485 s)
--                      0.996 R²   (0.993 R² .. 1.000 R²)
-- mean                 1.994 s    (1.961 s .. 2.059 s)
-- std dev              64.37 ms   (46.01 μs .. 75.90 ms)
-- variance introduced by outliers: 19% (moderately inflated)
-- 
-- benchmarking perimeter, vertexes count: 10^7/rseq on each element
-- time                 969.4 ms   (967.7 ms .. 970.9 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 969.3 ms   (969.1 ms .. 969.5 ms)
-- std dev              315.6 μs   (223.8 μs .. 380.3 μs)
-- variance introduced by outliers: 19% (moderately inflated)
-- 
-- benchmarking perimeter, vertexes count: 10^7/devide by chunks rpar
-- time                 596.7 ms   (590.0 ms .. 607.5 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 600.1 ms   (596.2 ms .. 606.4 ms)
-- std dev              5.977 ms   (1.338 ms .. 7.631 ms)
-- variance introduced by outliers: 19% (moderately inflated)
-- 
-- benchmarking perimeter, vertexes count: 10^7/devide by chunks rseq
-- time                 617.3 ms   (500.9 ms .. 720.0 ms)
--                      0.996 R²   (0.984 R² .. 1.000 R²)
-- mean                 616.4 ms   (606.0 ms .. 634.9 ms)
-- std dev              17.82 ms   (1.806 ms .. 22.29 ms)
-- variance introduced by outliers: 19% (moderately inflated)
-- 
-- benchmarking area, vertexes count: 10^7/simple
-- time                 268.1 ms   (260.5 ms .. 274.8 ms)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 261.9 ms   (255.9 ms .. 265.1 ms)
-- std dev              6.100 ms   (1.868 ms .. 8.419 ms)
-- variance introduced by outliers: 16% (moderately inflated)
-- 
-- benchmarking area, vertexes count: 10^7/rpar on each element
-- time                 1.975 s    (1.940 s .. 1.999 s)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 1.968 s    (1.959 s .. 1.972 s)
-- std dev              8.027 ms   (3.953 ms .. 9.822 ms)
-- variance introduced by outliers: 19% (moderately inflated)
-- 
-- benchmarking area, vertexes count: 10^7/rseq on each element
-- time                 1.099 s    (992.3 ms .. 1.278 s)
--                      0.996 R²   (0.995 R² .. 1.000 R²)
-- mean                 999.0 ms   (966.4 ms .. 1.050 s)
-- std dev              49.10 ms   (17.29 ms .. 64.68 ms)
-- variance introduced by outliers: 19% (moderately inflated)
-- 
-- benchmarking area, vertexes count: 10^7/devide by chunks rpar
-- time                 596.3 ms   (577.9 ms .. 614.6 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 594.9 ms   (591.8 ms .. 596.5 ms)
-- std dev              3.133 ms   (2.031 ms .. 3.838 ms)
-- variance introduced by outliers: 19% (moderately inflated)
-- 
-- benchmarking area, vertexes count: 10^7/devide by chunks rseq
-- time                 606.1 ms   (603.8 ms .. 608.4 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 604.5 ms   (602.8 ms .. 605.4 ms)
-- std dev              1.633 ms   (169.0 μs .. 2.025 ms)
-- variance introduced by outliers: 19% (moderately inflated)