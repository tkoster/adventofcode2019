{-# LANGUAGE BangPatterns, RecordWildCards, ViewPatterns #-}

module Day3 where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List (foldl', sort, sortOn)
import Data.Maybe
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector
import Data.Word
import Debug.Trace
import Test.Hspec

import Csv

type Path = [Leg]

data Leg = Leg Direction Int
  deriving (Eq, Show)

data Direction = R | U | L | D
  deriving (Eq, Show)

type Point = (Int, Int)

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

origin :: Point
origin = (0, 0)

data Edge = Edge Point Direction Int
  deriving (Eq, Show)

parsePath :: String -> Path
parsePath = map parseLeg . csv

parseLeg :: String -> Leg
parseLeg input | (dir, remain) <- parseDirection input = Leg dir (read remain)

parseDirection :: String -> (Direction, String)
parseDirection input =
  case input of
    'R' : remain -> (R, remain)
    'U' : remain -> (U, remain)
    'L' : remain -> (L, remain)
    'D' : remain -> (D, remain)

readInputFile :: FilePath -> IO [Path]
readInputFile path = do
  input <- readFile path
  return $ map parsePath (lines input)

move :: Direction -> Int -> Point -> Point
move dir dist (x, y) =
  case dir of
    R -> (x + dist, y)
    U -> (x, y + dist)
    L -> (x - dist, y)
    D -> (x, y - dist)

edges :: Path -> [Edge]
edges path = go (0, 0) path
  where
    go _   [] = []
    go pos (Leg dir dist : next) =
      let pos' = move dir dist pos
      in  Edge pos dir dist : go pos' next

edgePoints :: Edge -> [Point]
edgePoints (Edge pos dir dist) = map (flip (move dir) pos) [0 .. dist]

pathSet :: Path -> HashSet Point
pathSet path = HashSet.fromList $ edges path >>= edgePoints

intersection :: Path -> Path -> [Point]
intersection (pathSet -> path1) (pathSet -> path2) =
  HashSet.toList $ HashSet.intersection path1 path2

closestTo :: Point -> [Point] -> Maybe Point
closestTo target points
  | (closest : _) <- sortOn (distance target) points = Just closest
  | otherwise = Nothing

part1 :: Path -> Path -> Maybe Int
part1 path1 path2 =
  (distance origin <$>) . closestTo origin . filter (/= origin) $ intersection path1 path2

pathLengthToTarget :: Path -> Point -> Maybe Int
pathLengthToTarget path target = go 0 (edges path)
  where
    go _ [] = Nothing -- target was not on path
    go !l (edge@(Edge pos _ dist) : remain)
      | target `elem` edgePoints edge = -- target on path: stop at target
          Just $ l + distance pos target
      | otherwise = -- target not on edge
          go (l + dist) remain

part2 :: Path -> Path -> Int
part2 path1 path2 =
  let targets = filter (/= origin) (intersection path1 path2)
      scores = map score targets
  in  minimum (catMaybes scores)
  where
    score target = liftM2 (+) (pathLengthToTarget path1 target) (pathLengthToTarget path2 target)

main :: IO ()
main = do
  selfTest
  [path1, path2] <- readInputFile "day3input.txt"
  -- Part 1
  print $ part1 path1 path2
  -- Part 2
  print $ part2 path1 path2

--------

selfTest :: IO ()
selfTest = hspec $ do

  describe "distance" $
    it "works" $ do
      distance (0, 0) (2, 3) `shouldBe` 5
      distance (2, 3) (0, 0) `shouldBe` 5

  let path1 = parsePath "R8,U5,L5,D3"
      path2 = parsePath "U7,R6,D4,L4"

  describe "parsePath" $
    it "parses valid inputs" $ do
      path1 `shouldBe` [Leg R 8, Leg U 5, Leg L 5, Leg D 3]
      path2 `shouldBe` [Leg U 7, Leg R 6, Leg D 4, Leg L 4]

  describe "edges" $
    it "works" $ do
      edges [] `shouldBe` []
      edges path1 `shouldBe` [Edge (0, 0) R 8, Edge (8, 0) U 5, Edge (8, 5) L 5, Edge (3, 5) D 3]
      edges path2 `shouldBe` [Edge (0, 0) U 7, Edge (0, 7) R 6, Edge (6, 7) D 4, Edge (6, 3) L 4]

  describe "edgePoints" $
    it "works" $ do
      edgePoints (Edge (0, 0) R 2) `shouldBe` [(0, 0), (1, 0), (2, 0)]
      edgePoints (Edge (1, 1) U 2) `shouldBe` [(1, 1), (1, 2), (1, 3)]

  describe "pathSet" $ do
    it "works for R0" $
      pathSet (parsePath "R0") `shouldBe` HashSet.fromList [(0, 0)]
    it "works for R1" $
      pathSet (parsePath "R1") `shouldBe` HashSet.fromList [(0, 0), (1, 0)]
    it "works for L1" $
      pathSet (parsePath "L1") `shouldBe` HashSet.fromList [(0, 0), (-1, 0)]
    it "works for R1,U1,L2" $
      pathSet (parsePath "R1,U1,L2") `shouldBe` HashSet.fromList [(0, 0), (1, 0), (1, 1), (0, 1), (-1, 1)]

  describe "closestTo" $
    it "works" $
      closestTo (0, 0) [(4, 3), (2, 1), (1, 1)] `shouldBe` Just (1, 1)

  describe "pathLengthToTarget" $ do
    it "is Nothing when target not on path" $
      pathLengthToTarget (parsePath "R4") (1, 1) `shouldBe` Nothing
    it "works" $
      pathLengthToTarget (parsePath "R4,U4") (4, 2) `shouldBe` Just 6

  describe "part1" $ do
    it "works for example 1" $ do
      let path1 = parsePath "R75,D30,R83,U83,L12,D49,R71,U7,L72"
          path2 = parsePath "U62,R66,U55,R34,D71,R55,D58,R83"
      part1 path1 path2 `shouldBe` Just 159
    it "works for example 2" $ do
      let path1 = parsePath "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
          path2 = parsePath "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
      part1 path1 path2 `shouldBe` Just 135

  describe "part2" $ do
    it "works for example 1" $ do
      let path1 = parsePath "R75,D30,R83,U83,L12,D49,R71,U7,L72"
          path2 = parsePath "U62,R66,U55,R34,D71,R55,D58,R83"
      part2 path1 path2 `shouldBe` 610
    it "works for example 2" $ do
      let path1 = parsePath "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
          path2 = parsePath "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
      part2 path1 path2 `shouldBe` 410
