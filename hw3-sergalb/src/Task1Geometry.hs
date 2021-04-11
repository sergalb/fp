{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Task1Geometry
  ( Point(..)
  , plus
  , minus
  , scalarProduct
  , crossProduct
  , perimeter
  , area
  , simplePerimeter
  , simpleArea
  , perimeterStrategyEachComputation
  , perimeterDivided
  , areaDivided
  ) where

import Control.DeepSeq (NFData(..), rnf)
import Control.Parallel.Strategies (Strategy, rpar, runEval, withStrategy)
import Data.List.Split (chunksOf)

data Point =
  Point
    { x :: !Int
    , y :: !Int
    }

instance Show Point where
  show (Point x y) = "{" ++ show x ++ "; " ++ show y ++ "}"

instance NFData Point where
  rnf (Point x y) = rnf x `seq` rnf y

plus :: Point -> Point -> Point
plus (Point ax ay) (Point bx by) = Point (ax + bx) (ay + by)

minus :: Point -> Point -> Point
minus (Point ax ay) (Point bx by) = Point (ax - bx) (ay - by)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point ax ay) (Point bx by) = (ax * bx + ay * by)

crossProduct :: Point -> Point -> Int
crossProduct (Point ax ay) (Point bx by) = (ax * by) - (ay * bx)

{-# INLINE crossProduct #-}
perimeter :: [Point] -> Double
perimeter = perimeterDivided rpar

area :: [Point] -> Int
area = areaDivided rpar

simplePerimeter :: [Point] -> Double
simplePerimeter polytope@(p:_) = funcOnLine 0 distance (polytope ++ [p])
simplePerimeter _ = 0.0

perimeterDivided :: Strategy Double -> [Point] -> Double
perimeterDivided = dividedComputation distance

simpleArea :: [Point] -> Int
simpleArea polytope@(p:_) = funcOnLine 0 crossProduct (polytope ++ [p])
simpleArea _ = 0

areaDivided :: Strategy Int -> [Point] -> Int
areaDivided = dividedComputation crossProduct

perimeterStrategyEachComputation :: Strategy Double -> [Point] -> Double
perimeterStrategyEachComputation strat polytope@(p:_) =
  evaluation 0.0 (polytope ++ [p])
  where
    evaluation :: Double -> [Point] -> Double
    evaluation !acc (l:ls:lss) =
      runEval $ do
        let edge = withStrategy strat $ distance l ls
        return $ evaluation (acc + edge) (ls : lss)
    evaluation !acc _ = acc
perimeterStrategyEachComputation _ _ = 0.0

dividedComputation ::
     Num a => (Point -> Point -> a) -> Strategy a -> [Point] -> a
dividedComputation func strat polytope@(p:_) =
  let chunkSize = floor $ (sqrt $ fromIntegral $ length polytope :: Double)
      firstAndLast = func p (last polytope)
      chunks = chunksOf chunkSize polytope
   in (funcOnChunks firstAndLast func strat chunks)
dividedComputation _ _ _ = 0

funcOnChunks ::
     Num a => a -> (Point -> Point -> a) -> Strategy a -> [[Point]] -> a
funcOnChunks !acc func strat (l:ls:lss) =
  runEval $ do
    let edge = withStrategy strat $ funcOnLine 0 func l
    let connection = func (last l) (head ls)
    return $ funcOnChunks (acc + edge + connection) func strat (ls : lss)
funcOnChunks !acc func strat [l] = withStrategy strat $ funcOnLine acc func l
funcOnChunks !acc _ _ _ = acc

funcOnLine :: (Num a) => a -> (Point -> Point -> a) -> [Point] -> a
funcOnLine !acc func (l:ls:lss) =
  let edge = func l ls
   in funcOnLine (acc + edge) func (ls : lss)
funcOnLine !acc _ _ = acc

distance :: Point -> Point -> Double
distance a b =
  sqrt $
  fromIntegral $ (x a - x b) ^ (2 :: Integer) + (y a - y b) ^ (2 :: Integer)
