{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Day18 where

import           Control.Monad (guard)
import           Data.Bifunctor (second)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Char (isLower, isSpace, isUpper, toUpper)
import           Data.Function (on)
import           Data.Hashable (Hashable)
import           Data.List (foldl', sort, sortBy)
import           Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Heap (Heap, Entry (..))
import qualified Data.Heap as Heap
import           System.IO.Unsafe (unsafePerformIO)

import           Debug.Trace
import           Test.Hspec

type Pos = (Int, Int)

data Image = Image
  { width   :: Int
  , height  :: Int
  , raw     :: ByteString
  , toPos   :: Int -> Pos
  , fromPos :: Pos -> Int
  }

parseImage :: ByteString -> Image
parseImage input = Image { width, height, raw, toPos, fromPos }
  where
    raw = ByteString.filter (not . isSpace) input
    width = fromMaybe 0 $ ByteString.findIndex isSpace input
    height = ByteString.length raw `div` width
    toPos i = (i `mod` width, i `div` width)
    fromPos (x, y) = y * width + x

-- | Return the character at a position in the image.
--
-- If the position is outside the bounds of the image, this functions returns
-- '#' (an impassable wall).
sample :: Image -> (Int, Int) -> Char
sample Image { width, height, raw } (x, y)
  | x >= 0, y < width, y >= 0, y < height = ByteString.index raw (y * width + x)
  | otherwise = '#'

-- | Return the position of all characters in the image where the predicate is true.
findAll :: (Char -> Bool) -> Image -> [Pos]
findAll predicate Image { raw, toPos } =
  map toPos $ ByteString.findIndices predicate raw

isEntrance, isDoor, isKey, isWall, isFloor :: Char -> Bool
isEntrance = (== '@')
isDoor = isUpper
isKey = isLower
isWall = (== '#')
isFloor = (== '.')

-- | Compute the shortest path between two nodes of a graph.
--
-- The graph is given as a function returning nodes adjacent to the argument node.
--
-- This is a fairly literal transliteration of Wikipedia's description of the
-- A* search algorithm.
astar
  :: (Eq a, Hashable a)
  => (a -> [(a, Int)])
  -> (a -> Int)
  -> a
  -> a
  -> Maybe [(a, Int)]
astar adjacentTo heuristic start goal =
  step
    (Heap.singleton (Entry 0 start))
    HashMap.empty
    (HashMap.singleton start 0)
    (HashMap.singleton start (heuristic start))
  where
    step openSet cameFrom gScore fScore = do
      (Entry _ current, openSet') <- Heap.uncons openSet
      if current == goal
        then
          return (makePath cameFrom current)
        else
          let (openSet'', cameFrom', gScore', fScore') = foldl' (updateNeighbour current) (openSet', cameFrom, gScore, fScore) (adjacentTo current)
          in  step openSet'' cameFrom' gScore' fScore'
    updateNeighbour current a@(openSet, cameFrom, gScore, fScore) (neighbour, distance) =
      if score < neighbourG
        then (openSet', cameFrom', gScore', fScore')
        else a
        where
          score      = currentG + distance
          currentG   = HashMap.lookupDefault maxBound current gScore
          neighbourG = HashMap.lookupDefault maxBound neighbour gScore
          neighbourF = score + heuristic neighbour
          openSet'   = Heap.insert (Entry neighbourF neighbour) openSet
          cameFrom'  = HashMap.insert neighbour (distance, current) cameFrom
          gScore'    = HashMap.insert neighbour score gScore
          fScore'    = HashMap.insert neighbour (neighbourG + score) fScore
    makePath cameFrom = go []
      where
        go acc current
          | Just (distance, current') <- HashMap.lookup current cameFrom = go ((current, distance) : acc) current'
          | otherwise = acc

-- | Create a function usable as the neighbours argument to the astar function
-- from an adjacency list.
fromAdjacencyList :: Eq a => [(a, (a, Int))] -> a -> [(a, Int)]
fromAdjacencyList list name = map snd $ filter ((== name) . fst) list

manhattan :: Pos -> Pos -> Int
manhattan (x1, y1) (x2, y2) = abs (y2 - y1) + abs (x2 - x1)

-- | The position of every feature of the image.
findFeatures :: (Char -> Bool) -> Image -> [(Pos, Char)]
findFeatures isFeature Image { raw, toPos } = map (\i -> (toPos i, ByteString.index raw i)) $ ByteString.findIndices isFeature raw

-- | A graph of features in the image.
--
-- A feature is everything except walls, i.e. entrances, floors, keys and
-- doors.
allFeatures :: Image -> HashMap Pos Char
allFeatures image = HashMap.fromList $ findFeatures isFeature image
  where
    isFeature c = isEntrance c || isFloor c || isKey c || isDoor c

-- | Create a function usable as the neighbours argument to the astar function
-- from a map of features.
--
-- Traversible features are entrances, floors and a goal.
neighboursFromFeatures :: (Char -> Bool) -> HashMap Pos Char -> Pos -> [(Pos, Int)]
neighboursFromFeatures isTraversible features = neighbours
  where
    neighbours (x, y) = catMaybes [east, north, west, south]
      where
        east  = lookupFeature (x + 1, y)
        north = lookupFeature (x, y - 1)
        west  = lookupFeature (x - 1, y)
        south = lookupFeature (x, y + 1)
        lookupFeature pos = HashMap.lookup pos features >>= asNeighbour pos
        asNeighbour pos c
          | isTraversible c = Just (pos, 1)
          | otherwise       = Nothing

showImage :: Image -> [(Pos, Char)] -> String
showImage Image { width, height, fromPos, raw } markers = unlines rows
  where
    rows = [cols y | y <- [0 .. height - 1]]
    cols y = [charAt (x, y) | x <- [0 .. width - 1]]
    charAt pos = HashMap.lookupDefault (ByteString.index raw $ fromPos pos) pos markerMap
    markerMap = HashMap.fromList markers

journeys :: Image -> [([Char], Int)]
journeys image = map (second length) solutions
  where
    [startPos] = findAll isEntrance image
    solutions = go startPos (allFeatures image) (findFeatures isKey image) [] []
    go _ _ [] inventory journey =
      -- No more keys to find.
      -- Return the inventory and paths.
      [(reverse inventory, concat (reverse journey))]
    go currentPos currentFeatures keys inventory journey = do
      -- Compute the paths to keys reachable from the current position.
      let pathsToKeys = catMaybes $ map pathToKey keys
      (keyPos, key, path) <- pathsToKeys
      -- Move the key from the goals list to the inventory.
      let keys' = filter ((/= key) . snd) keys
          inventory' = key : inventory
      -- Remove the key from the floor and unlock the door that this key opens.
      -- (Change the key and the door to a floor.)
      let door = toUpper key
          doorPos = findAll (== door) image
          currentFeatures' = setFeatures (keyPos : doorPos) '.' currentFeatures
      -- Add the path to the journey log.
      let journey' = path : journey
      -- Continue.
      go keyPos currentFeatures' keys' inventory' journey'
      where
        pathToKey (goalPos, goal) = do
          path <- astar (neighboursFromFeatures isTraversible currentFeatures) (manhattan goalPos) currentPos goalPos
          return (goalPos, goal, path)
          where
            isTraversible c = (c == goal) || isEntrance c || isFloor c
    setFeatures positions value features = foldl' (\acc pos -> HashMap.insert pos value acc) features positions

part1 :: Image -> Maybe Int
part1 image = snd <$> bestJourney
  where
    sortedJourneys = sortBy (compare `on` snd) $ journeys image
    bestJourney = listToMaybe $ take 1 sortedJourneys



day18Input, example1Input, example2Input, example3Input, example4Input, example5Input :: ByteString
day18Input = unsafePerformIO $ ByteString.readFile "day18input.txt"
example1Input = unsafePerformIO $ ByteString.readFile "day18example1input.txt"
example2Input = unsafePerformIO $ ByteString.readFile "day18example2input.txt"
example3Input = unsafePerformIO $ ByteString.readFile "day18example3input.txt"
example4Input = unsafePerformIO $ ByteString.readFile "day18example4input.txt"
example5Input = unsafePerformIO $ ByteString.readFile "day18example5input.txt"

inputImage, example1Image, example2Image, example3Image, example4Image, example5Image :: Image
inputImage = parseImage day18Input
example1Image = parseImage example1Input
example2Image = parseImage example2Input
example3Image = parseImage example3Input
example4Image = parseImage example4Input
example5Image = parseImage example5Input



selfTest :: IO ()
selfTest = hspec $ do
  describe "readImage" $
    it "returns size 81x81 given the puzzle input" $ do
      let Image {..} = inputImage
      (width, height) `shouldBe` (81, 81)

  describe "findAll" $ do
    it "returns [] for a nonexistent char" $
      findAll (== '?') inputImage `shouldBe` []
    it "returns (40,40) for the location of the tunnel entrance in the puzzle input" $
      findAll (== '@') inputImage `shouldBe` [(40,40)]

  describe "astar" $ do
    it "returns Nothing for an empty graph" $
      astar (const []) (const 1) 'a' 'b' `shouldBe` Nothing
    it "returns Nothing when the goal is unreachable" $ do
      let neighbours = fromAdjacencyList [('a', ('b', 1))]
          heuristic = const 1
      astar neighbours heuristic 'a' 'c' `shouldBe` Nothing
    it "returns the only path between two nodes in a graph of two nodes" $ do
      let neighbours = fromAdjacencyList [('a', ('b', 1))]
          heuristic = const 1
      astar neighbours heuristic 'a' 'b' `shouldBe` Just [('b', 1)]
    it "returns the only path along three nodes in a graph of three nodes" $ do
      let neighbours = fromAdjacencyList [('a', ('b', 1)), ('b', ('c', 2))]
          heuristic = const 1
      astar neighbours heuristic 'a' 'c' `shouldBe` Just [('b', 1),  ('c', 2)]
    it "returns the shortest path amongst two alternatives in a graph of three nodes" $ do
      let neighbours = fromAdjacencyList [('a', ('b', 1)), ('a', ('c', 2)), ('b', ('c', 2))]
          heuristic = const 1
      astar neighbours heuristic 'a' 'c' `shouldBe` Just [('c', 2)]

  describe "journeys" $ do
    it "returns exactly the expected path for the first example map" $
      journeys example1Image `shouldBe` [("ab", 8)]
    it "returns the expected paths for the second example map" $
      sort (journeys example2Image) `shouldBe` sort [("abcdef", 86), ("abcedf", 114)]
    it "does not return \"abdcef\" for the second example map" $
      -- "abdcef" is odd because the path from "b" to "d" goes through "c", so
      -- this journey is in fact identical to "abcdef".
      --
      -- This case is avoided in the journeys function by making all keys other
      -- than the goal key not traversible (as if it were a wall).
      journeys example2Image `shouldSatisfy` (not . elem ("abdcef", 86))
    it "returns the expected path for the third example map" $
      journeys example3Image `shouldSatisfy` elem ("bacdfeg", 132)
    xit "returns the expected path for the fourth example map" $
      -- TODO: example 4 uses a lot of memory and takes extremely long (I don't know if it terminates)
      journeys example4Image `shouldSatisfy` elem ("afbjgnhdloepcikm", 136)
    it "returns the expected path for the fifth example map" $
      journeys example5Image `shouldSatisfy` elem ("acfidgbeh", 81)

  describe "part1" $ do
    it "returns the expected length for the first example map" $
      part1 example1Image `shouldBe` Just 8
    it "returns the expected path for the second example map" $
      part1 example2Image `shouldBe` Just 86
    it "returns the expected path for the third example map" $
      part1 example3Image `shouldBe` Just 132
