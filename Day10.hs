{-# LANGUAGE RecordWildCards, OverloadedStrings, ViewPatterns #-}

module Day10 where

import Data.Foldable
import Data.Function
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Test.Hspec

--data Starmap = Starmap { width :: Int, height :: Int, asteroids :: HashSet (Int, Int) }
type Starmap = HashSet (Int, Int)

readMapFile :: FilePath -> IO Starmap
readMapFile path = parseMapFile <$> Text.readFile path

parseMapFile :: Text -> Starmap
parseMapFile input@(Text.lines -> rows)
  | (firstRow : _) <- rows
  , width <- Text.length firstRow
  , height <- length rows
  , (all (== width) . map Text.length) rows -- all rows identical length; map is square
  = HashSet.fromList
      [pos | y <- [0 .. height - 1]
           , x <- [0 .. width - 1]
           , let pos = (x, y)
           , (width + 1, input) ! pos == '#'
      ]
parseMapFile _ =
    error "Empty or non-square starmap."

(!) :: (Int, Text) -> (Int, Int) -> Char
(stride, values) ! (x, y) =
  let offset = y * stride + x
  in  Text.index values offset

-- Cast a ray from the origin to the target and return the points
-- the ray passes through.
--
-- The origin and the target points are not returned.
--
-- e.g.
--
-- cast (2, 0) (8, 3) == [(4, 1), (6, 2)]
--
-- ..o.........
-- ....*.......
-- ......*.....
-- ........t...
-- ............
cast :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
cast posA posB | posA == posB = []
cast (x0, y0) (xt, yt) = go 1
  where
    (dx, dy) = (xt - x0, yt - y0)
    h = gcd dx dy
    (dx', dy') = (dx `div` h, dy `div` h)
    go n | n < h = (x0 + n * dx', y0 + n * dy') : go (n + 1)
         | otherwise = []

-- Find the asteroids in the starmap that are visible from the argument
-- position.
findVisible :: (Int, Int) -> Starmap -> HashSet (Int, Int)
findVisible posA asteroids =
  (HashSet.filter isVisible . HashSet.delete posA) asteroids
  where
    -- An asteroid B is visible from asteroid A iff the ray cast from
    -- A to B intersects no other asteroids.
    isVisible = all (not . (`elem` asteroids)) . cast posA

findBestLocationForMonitoringStation :: Starmap -> ((Int, Int), HashSet (Int, Int))
findBestLocationForMonitoringStation asteroids =
  let visible = (HashMap.mapWithKey g . HashSet.toMap) asteroids
  in  maximumBy (compare `on` (length . snd)) $ toList visible
  where
    g pos () = (pos, findVisible pos asteroids)

part1 :: IO ()
part1 = do
  asteroids <- readMapFile "day10input.txt"
  let (pos, length -> len) = findBestLocationForMonitoringStation asteroids
  print pos
  print len

selfTest :: IO ()
selfTest = hspec $ do

  describe "parseMapFile" $ do

    it "accepts example 1" $ do
      parseMapFile example1File `shouldBe` HashSet.fromList example1Asteroids

    it "accepts the puzzle input" $ do
      asteroids <- readMapFile "day10input.txt"
      asteroids `shouldNotBe` mempty

  describe "cast" $ do

    it "casts along x axis" $
      cast (0, 0) (3, 0) `shouldBe` [(1, 0), (2, 0)]

    it "casts along -x axis" $
      cast (3, 0) (0, 0) `shouldBe` [(2, 0), (1, 0)]

    it "casts along y axis" $
      cast (0, 0) (0, 3) `shouldBe` [(0, 1), (0, 2)]

    it "casts along diagonal" $
      cast (0, 0) (4, 4) `shouldBe` [(1, 1), (2, 2), (3, 3)]

    it "casts along non-cardinal diagonal" $
      cast (0, 0) (4, 8) `shouldBe` [(1, 2), (2, 4), (3, 6)]

  describe "findVisible" $ do

    it "works for asteroid (1, 0) in example 1" $ do
      let starmap  = parseMapFile example1File
          expected = HashSet.fromList
                        [ (4, 0), (0, 2), (1, 2), (2, 2)
                        , (3, 2), (4, 2), (4, 4)
                        ]
          actual   = findVisible (1, 0) starmap
      actual `shouldBe` expected

    it "returns the expected number of asteroids for all in example 1" $ do
      let starmap = parseMapFile example1File
          counts = map (length . flip findVisible starmap) example1Asteroids
      counts `shouldBe` [7, 7, 6, 7, 7, 7, 5, 7, 8, 7]

  describe "findBestLocationForMonitoringStation" $ do

    it "returns the expected answer for larger example 1" $ do
      let (pos, length -> len) = findBestLocationForMonitoringStation largerExample1
      (pos, len) `shouldBe` ((5, 8), 33)

    it "returns the expected answer for larger example 2" $ do
      let (pos, length -> len) = findBestLocationForMonitoringStation largerExample2
      (pos, len) `shouldBe` ((1, 2), 35)

    it "returns the expected answer for larger example 3" $ do
      let (pos, length -> len) = findBestLocationForMonitoringStation largerExample3
      (pos, len) `shouldBe` ((6, 3), 41)

    it "returns the expected answer for larger example 4" $ do
      let (pos, length -> len) = findBestLocationForMonitoringStation largerExample4
      (pos, len) `shouldBe` ((11, 13), 210)

--  01234
-- 0.#..#
-- 1.....
-- 2#####
-- 3....#
-- 4...##
example1File :: Text
example1File = ".#..#\n.....\n#####\n....#\n...##\n"

example1Asteroids :: [(Int, Int)]
example1Asteroids = [(1, 0), (4, 0), (0, 2), (1, 2), (2, 2), (3, 2), (4, 2), (4, 3), (3, 4), (4, 4)]

largerExample1 :: Starmap
largerExample1 = parseMapFile "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####\n"

largerExample2 :: Starmap
largerExample2 = parseMapFile "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###.\n"

largerExample3 :: Starmap
largerExample3 = parseMapFile ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#..\n"

largerExample4 :: Starmap
largerExample4 = parseMapFile ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##\n"

