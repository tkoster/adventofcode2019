{-# LANGUAGE OverloadedLists #-}

module Day15 (part1, part2, automaticChart, drawChart, selfTest) where

import           Control.Monad.Writer (execWriter, tell)
import           Data.Foldable (for_)
import           Data.Function ((&))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Vector.Unboxed (Vector)
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec

import           Intcode3 as Intcode

data Tile = Floor | Wall | OxygenSystem | Oxygen
  deriving Eq

type Chart = HashMap (Int, Int) Tile

data Direction = North | South | West | East
  deriving (Bounded, Enum)

directionToInput :: Direction -> Int
directionToInput dir =
  case dir of
    North -> 1
    South -> 2
    West  -> 3
    East  -> 4

data Status = Collided | Moved | FoundOxygenSystem

statusFromOutput :: Int -> Status
statusFromOutput output =
  case output of
    0 -> Collided
    1 -> Moved
    2 -> FoundOxygenSystem
    _ -> error "invalid status"

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) dir =
  case dir of
    North -> (x, y - 1)
    South -> (x, y + 1)
    West  -> (x - 1, y)
    East  -> (x + 1, y)

automaticChart :: (Chart, Maybe Int)
automaticChart = fork initialChart initialPosition initialState 0 Nothing
  where
    initialChart = [((0, 0), Floor)]
    initialPosition = (0, 0)
    initialState = Intcode.step day15Input 0 0

    fork :: Chart -> (Int, Int) -> Result -> Int -> Maybe Int -> (Chart, Maybe Int)
    fork chart pos state moves movesToOxygen =
      let -- Fork into four threads, each updating the chart in 4 directions from the current position:
          (chart1, movesToOxygen1) = stepDroid chart pos state moves movesToOxygen North
          (chart2, movesToOxygen2) = stepDroid chart pos state moves movesToOxygen South
          (chart3, movesToOxygen3) = stepDroid chart pos state moves movesToOxygen West
          (chart4, movesToOxygen4) = stepDroid chart pos state moves movesToOxygen East
          -- Merge the charts
          chart' = chart1 <> chart2 <> chart3 <> chart4
          -- Get the minimum moves to the oxygen system:
          Just a `mm` Just b = Just (a `min` b)
          Nothing `mm` b     = b
          a `mm` Nothing     = a
          movesToOxygen' = movesToOxygen1 `mm` movesToOxygen2 `mm` movesToOxygen3 `mm` movesToOxygen4
      in  (chart', movesToOxygen')

    stepDroid !chart pos (Await continue) !moves movesToOxygen dir
      | pos' `HashMap.member` chart =
        -- The location to move to is already mapped.
        -- Return the chart unchanged.
        (chart, movesToOxygen)
      | otherwise =
        -- The location to move to is not mapped.
        -- Command the droid to try to move to the location.
        case continue (directionToInput dir) of
          Yield output state ->
            case statusFromOutput output of
              Collided ->
                -- The droid hit a wall.
                -- Update the chart.
                -- Stop looking down this branch.
                (HashMap.insert pos' Wall chart, Nothing)
              Moved ->
                -- The droid moved to the location.
                -- Update the chart and fork again.
                fork (HashMap.insert pos' Floor chart) pos' state (moves + 1) movesToOxygen
              FoundOxygenSystem ->
                -- The droid moved to the location and found the oxygen system here.
                -- Update the chart and fork again.
                fork (HashMap.insert pos' OxygenSystem chart) pos' state (moves + 1) (Just (moves + 1))
          _ -> error "fork: invalid state, expecting yield"
      where
        pos' = move pos dir
    stepDroid _ _ _ _ _ _ = error "fork: invalid state, expecting await"

timeToFill :: Chart -> Int
timeToFill mappedChart = fill 0 initialChart
  where
    [locationOfOxygenSystem] = HashMap.keys $ HashMap.filter (== OxygenSystem) mappedChart
    initialChart = HashMap.insert locationOfOxygenSystem Oxygen mappedChart

    fill !time chart
      | chart /= chart' = fill (time + 1) chart'
      | otherwise       = time
      where
        chart' = HashMap.mapWithKey fillCell chart
        cell pos = HashMap.lookupDefault Wall pos chart
        fillCell pos Floor | adjacentToOxygen pos = Oxygen
        fillCell _   tile                         = tile
        adjacentToOxygen pos = [North ..] & map (cell . move pos) & any (== Oxygen)

tileChar :: Tile -> Char
tileChar tile =
  case tile of
    Wall -> '#'
    Floor -> '.'
    OxygenSystem -> '*'
    Oxygen -> 'O'


drawChart :: Chart -> String
drawChart chart = execWriter $ do
  let xs = map fst (HashMap.keys chart)
      ys = map snd (HashMap.keys chart)
  for_ @[] [minimum ys .. maximum ys] $ \y -> do
    for_ @[] [minimum xs .. maximum xs] $ \x ->
      tell $
        case HashMap.lookup (x, y) chart of
          Nothing   -> " "
          Just tile -> [tileChar tile]
    tell "\n"

day15Input :: Vector Int
day15Input = Intcode.parse $ unsafePerformIO (readFile "day15input.txt")

part1 :: Int
part1 = let Just moves = snd automaticChart in moves

part2 :: Int
part2 = let chart = fst automaticChart in timeToFill chart

selfTest :: IO ()
selfTest = hspec $ do
  describe "part1" $
    it "is 242" $
      part1 `shouldBe` 242

  describe "part2" $
    it "is 276" $
      part2 `shouldBe` 276
