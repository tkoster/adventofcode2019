{-# OPTIONS_GHC -Wno-unused-imports #-}

module Day17 where

import           Control.Monad (guard)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Char (isLower, isSpace, isUpper)
import           Data.Hashable (Hashable)
import           Data.List (foldl')
import           Data.Maybe (fromMaybe)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Heap (Heap, Entry (..))
import qualified Data.Heap as Heap
import           System.IO.Unsafe (unsafePerformIO)

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

isEntrance, isDoor, isKey, isWall :: Char -> Bool
isEntrance = (== '@')
isDoor = isUpper
isKey = isLower
isWall = (== '#')

entrances, doors, keys, walls :: Image -> [Pos]
entrances = findAll isEntrance
doors = findAll isDoor
keys = findAll isKey
walls = findAll isWall

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
astar adjacentTo heuristic start goal = step (Heap.singleton (Entry 0 start)) HashMap.empty (HashMap.singleton start 0) (HashMap.singleton start (heuristic start))
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

fromAdjacencyList :: Eq a => [(a, (a, Int))] -> a -> [(a, Int)]
fromAdjacencyList list name = map snd $ filter ((== name) . fst) list

manhattan :: Pos -> Pos -> Int
manhattan (x1, y1) (x2, y2) = abs (y2 - y1) + abs (x2 - x1)



day18Input :: ByteString
day18Input = unsafePerformIO $ ByteString.readFile "day18input.txt"

inputImage :: Image
inputImage = parseImage day18Input



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
