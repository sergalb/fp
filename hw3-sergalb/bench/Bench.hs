module Bench where

import Criterion.Main (defaultMain)
import Task1Benchmark

main :: IO ()
main = defaultMain [perimeterBenchmark, areaBenchmark]
