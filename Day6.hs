{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Day6 where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (intersect)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Test.Hspec

data Orbit = Orbit Text Text
  deriving (Eq, Show)

parseInput :: Text -> [Orbit]
parseInput = map parseLine . Text.lines

parseLine :: Text -> Orbit
parseLine input =
  let (parent, child) = Text.breakOn ")" input
  in  Orbit parent (Text.drop 1 child)

readInputFile :: FilePath -> IO [Orbit]
readInputFile path = parseInput <$> Text.readFile path

type Starmap = HashMap Text Text

fromOrbits :: [Orbit] -> Starmap
fromOrbits = HashMap.fromList . map (\(Orbit parent child) -> (child, parent))

lookupParent :: Text -> Starmap -> Maybe Text
lookupParent = HashMap.lookup

listStars :: Starmap -> [Text]
listStars = HashMap.keys

foldParents :: (Text -> a -> a) -> a -> Starmap -> Text -> a
foldParents f z starmap star = go z star
  where
    go !a child
      | Just parent <- lookupParent child starmap = go (f parent a) parent
      | otherwise = a

depth :: Starmap -> Text -> Int
depth = foldParents (const (+1)) 0

ancestors :: Starmap -> Text -> [Text]
ancestors = foldParents (:) []

part1 :: Starmap -> Int
part1 starmap = sum . map (depth starmap) $ listStars starmap

mostRecentCommonAncestor :: Starmap -> Text -> Text -> Maybe Text
mostRecentCommonAncestor starmap star1 star2 = do
  listToMaybe . take 1 . reverse $ intersect star1ancestors star2ancestors
  where
    star1ancestors = ancestors starmap star1
    star2ancestors = ancestors starmap star2

minTransfers :: Starmap -> Text -> Text -> Int
minTransfers starmap star1 star2 =
  let Just mrca = mostRecentCommonAncestor starmap star1 star2
      mrcaDepth = depth' mrca
      star1Depth = depth' star1
      star2Depth = depth' star2
  in  star1Depth + star2Depth - 2 * mrcaDepth - 2
  where
    depth' = depth starmap

part2 :: Starmap -> Int
part2 starmap = minTransfers starmap "YOU" "SAN"

main :: IO ()
main = do
  orbits <- readInputFile "day6input.txt"
  let starmap = fromOrbits orbits
  print $ part1 starmap
  print $ part2 starmap

--------

selfTest :: IO ()
selfTest = hspec $ do

  describe "parseLine" $
    it "works" $
      parseLine "97W)B43" `shouldBe` Orbit "97W" "B43"

  describe "parseInput" $
    it "works" $
      parseInput "97W)B43\nR63)RTM" `shouldBe` [Orbit "97W" "B43", Orbit "R63" "RTM"]

  describe "depth" $ do
    it "returns 0 for nonexistent star" $
      depth mempty "Saturn" `shouldBe` 0
    it "returns 1 for a star orbiting the COM" $
      depth (fromOrbits [Orbit "COM" "Saturn"]) "Saturn" `shouldBe` 1
    it "returns 2 for a satellite orbiting a star that orbits the COM" $ do
      let starmap = fromOrbits [Orbit "COM" "Saturn", Orbit "Saturn" "Enceladus"]
      depth starmap "Enceladus" `shouldBe` 2

  describe "ancestors" $ do
    it "returns [] for nonexistent star" $
      ancestors mempty "Saturn" `shouldBe` []
    it "returns [\"COM\"] for a star orbiting the COM" $
      ancestors (fromOrbits [Orbit "COM" "Saturn"]) "Saturn" `shouldBe` ["COM"]
    it "returns [\"COM\", \"Saturn\"] for a satellite orbiting a star that orbits the COM" $ do
      let starmap = fromOrbits [Orbit "COM" "Saturn", Orbit "Saturn" "Enceladus"]
      ancestors starmap "Enceladus" `shouldBe` ["COM", "Saturn"]

  describe "mostRecentCommonAncestor" $ do
    it "returns Nothing for nonexistent star" $
      mostRecentCommonAncestor mempty "ABC" "DEF" `shouldBe` Nothing
    it "returns Just \"COM\" for two stars orbiting the COM" $ do
      let starmap = fromOrbits [Orbit "COM" "Saturn", Orbit "COM" "Neptune"]
      mostRecentCommonAncestor starmap "Saturn" "Neptune" `shouldBe` Just "COM"
    it "returns Just \"D\" for the example starmap" $ do
      mostRecentCommonAncestor example2Starmap "YOU" "SAN" `shouldBe` Just "D"
      mostRecentCommonAncestor example2Starmap "SAN" "YOU" `shouldBe` Just "D"

  describe "part1" $
    it "returns 42 for the example starmap" $
      part1 example1Starmap `shouldBe` 42

  describe "minTransfers" $
    it "returns 4 for the example starmap" $
      minTransfers example2Starmap "YOU" "SAN" `shouldBe` 4

example1Starmap :: Starmap
example1Starmap = fromOrbits (parseInput "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L")

example2Starmap :: Starmap
example2Starmap = fromOrbits (parseInput "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN")
