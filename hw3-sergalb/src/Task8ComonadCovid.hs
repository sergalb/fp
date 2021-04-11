{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task8ComonadCovid where

import Control.Comonad (Comonad, duplicate, extend, extract)
import Control.Monad (liftM2)
import System.Random (StdGen, genRange, mkStdGen, next)

data ListZipper a =
  LZ [a] a [a]

newtype Grid a =
  Grid
    { unGrid :: ListZipper (ListZipper a)
    }

data PopulationConfig =
  PopulationConfig
    { infectionProbability :: Float
    , incubation :: Int
    , illness :: Int
    , immunity :: Int
    }

data IllState
  = Healthy
  | Ill Int
  | Incubation Int
  | Immunity Int

data Man =
  Man
    { ill :: IllState
    , generator :: StdGen
    }

instance Show IllState where
  show Healthy = "o"
  show (Ill _) = "#"
  show (Incubation _) = "i"
  show (Immunity _) = "@"

instance Show Man where
  show man = show $ ill man

listLeft, listRight :: ListZipper a -> ListZipper a
listLeft (LZ (a:as) x bs) = LZ as a (x : bs)
listLeft _ = error "listLeft"

listRight (LZ as x (b:bs)) = LZ (x : as) b bs
listRight _ = error "listRight"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

instance Functor ListZipper where
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

genericMove :: (z a -> z a) -> (z a -> z a) -> z a -> ListZipper (z a)
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)

up, down :: Grid a -> Grid a
up (Grid g) = Grid (listLeft g)

down (Grid g) = Grid (listRight g)

left, rightBoard :: Grid a -> Grid a
left (Grid g) = Grid (fmap listLeft g)

rightBoard (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left rightBoard

vertical = genericMove up down

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (LZ _ x _) = x
  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = genericMove listLeft listRight

instance Functor Grid where
  fmap :: (a -> b) -> Grid a -> Grid b
  fmap f (Grid (LZ ls x rs)) =
    Grid $ LZ (map (fmap f) ls) (fmap f x) (map (fmap f) rs)

instance Comonad Grid where
  extract :: Grid a -> a
  extract = gridRead
  duplicate :: Grid a -> Grid (Grid a)
  duplicate = Grid . fmap horizontal . vertical

initPopulation :: Grid Man
initPopulation =
  let grid =
        Grid $
        genericMove
          (randomListLeft)
          (randomListRight)
          (LZ (startList 2) (Man (Ill 0) (mkStdGen 1)) (startList 3))
   in grid
  where
    startList :: Int -> [Man]
    startList seed = (iterate nextMan (Man Healthy (mkStdGen seed)))
    nextMan :: Man -> Man
    nextMan (Man _ gen) = Man Healthy (snd (next gen))
    randomListLeft, randomListRight :: ListZipper Man -> ListZipper Man
    randomListLeft (LZ (a:as) x bs) =
      let newGen = mkStdGen $ (fst $ next $ generator x) - 1
          nng = snd $ next $ newGen
       in LZ as (a {generator = newGen}) ((Man Healthy nng) : bs)
    randomListLeft _ = error "listLeft"
    randomListRight (LZ as x (b:bs)) =
      let newGen = mkStdGen $ (fst $ next $ generator x) + 1
          nng = snd $ next $ newGen
       in LZ ((Man Healthy nng) : as) (b {generator = newGen}) bs
    randomListRight _ = error "listRight"

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where
    horizontals = [left, rightBoard]
    verticals = [up, down]

rule :: PopulationConfig -> Grid Man -> Man
rule config grid =
  let (Man state gen) = extract grid
   in case state of
        Healthy ->
          let (ill, newGen) = tryInfect gen grid
           in if ill
                then Man (Ill 0) newGen
                else (Man Healthy newGen)
        (Ill period) ->
          if (period == (illness config))
            then Man (Immunity 0) gen
            else Man (Ill $ period + 1) gen
        (Incubation period) ->
          if (period == (incubation config))
            then Man (Ill 0) gen
            else Man (Incubation $ period + 1) gen
        (Immunity period) ->
          if (period == (immunity config))
            then Man (Healthy) gen
            else Man (Immunity $ period + 1) gen
  where
    tryInfect :: StdGen -> Grid Man -> (Bool, StdGen)
    tryInfect startGen g =
      foldl
        (\(isIll, gen) direction ->
           if isIll
             then (isIll, gen)
             else ((randomInfectMan gen) $ extract $ direction g))
        (False, startGen)
        neighbours
    randomInfectMan :: StdGen -> Man -> (Bool, StdGen)
    randomInfectMan gen (Man curState _) =
      case curState of
        (Ill _) ->
          let (rand, newGen) = next gen
           in (checkInfection (infectionProbability config) gen rand, newGen)
        (Incubation _) ->
          let (rand, newGen) = next gen
           in (checkInfection (infectionProbability config) gen rand, newGen)
        _ -> (False, gen)
    checkInfection :: Float -> StdGen -> Int -> Bool
    checkInfection prob gen rand =
      let (leftBoard, right) = genRange gen
          normalized =
            (fromIntegral (rand - leftBoard)) /
            (fromIntegral (right - leftBoard))
       in normalized <= prob

simulateStep :: PopulationConfig -> Grid Man -> Grid Man
simulateStep config = extend (rule config)

printPopulation :: Int -> Grid Man -> IO ()
printPopulation size grid = do
  let rows = toList (unGrid grid) size
  let population = fmap (`toList` size) rows
  mapM_ ((putStrLn . unwords) . map show) population

simulateNStep :: Int -> Int -> PopulationConfig -> IO ()
simulateNStep steps populationSize config = step steps initPopulation
  where
    step :: Int -> Grid Man -> IO ()
    step n grid =
      if (n == 0)
        then putStrLn "simulation done"
        else do
          printPopulation (populationSize) grid
          putStrLn ""
          step (n - 1) (simulateStep config grid)
