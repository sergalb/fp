{-# LANGUAGE ScopedTypeVariables #-}

module Task1GeometryTests
  ( task1Tests
  ) where

import Control.Parallel.Strategies
import Task1Geometry
import Test.Tasty
import Test.Tasty.HUnit

squarePolytope :: Int -> [Point]
squarePolytope n =
  let down = Point <$> [0 .. n] <*> [0]
      right = Point <$> [n] <*> [1 .. n]
      up = Point <$> (reverse [0 .. n - 1]) <*> [n]
      left = Point <$> [0] <*> (reverse [1 .. n - 1])
   in down ++ right ++ up ++ left

task1Tests :: TestTree
task1Tests = testGroup "Task1 tests" [areaTests, perimeterTests]

perimeterTests :: TestTree
perimeterTests =
  let power :: Int = 3
      polytope = (squarePolytope $ (10 ^ power))
   in testGroup
        "perimeter"
        [ testCase "simple function" $
          assertEqual
            "wrong perimeter"
            (4 * 10 ^ power)
            (simplePerimeter polytope)
        , testCase "rpar on each computation" $
          assertEqual
            "wrong perimeter"
            (4 * 10 ^ power)
            ((perimeterStrategyEachComputation rpar) polytope)
        , testCase "rpar on chunks" $
          assertEqual
            "wrong perimeter"
            (4 * 10 ^ power)
            ((perimeterDivided rpar) polytope)
        ]

areaTests :: TestTree
areaTests =
  let power :: Int = 3
      polytope = (squarePolytope $ (10 ^ power))
   in testGroup
        "area"
        [ testCase "simple function" $
          assertEqual "wrong area" (2 * 10 ^ (2 * power)) (simpleArea polytope)
        , testCase "rpar on chunks" $
          assertEqual
            "wrong area"
            (2 * 10 ^ (2 * power))
            ((areaDivided rpar) polytope)
        ]
