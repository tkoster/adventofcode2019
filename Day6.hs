{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Day6 where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
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

depth :: Starmap -> Text -> Int
depth starmap star = go 0 star
  where
    go !n child
      | Just parent <- lookupParent child starmap = go (n + 1) parent
      | otherwise = n

part1 :: Starmap -> Int
part1 starmap = sum . map (depth starmap) $ listStars starmap

main :: IO ()
main = do
  orbits <- readInputFile "day6input.txt"
  let starmap = fromOrbits orbits
  print $ part1 starmap

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

  describe "part1" $ do
    it "returns 3 for a satellits orbiting a star that orbits the COM" $ do
      let starmap = fromOrbits [Orbit "COM" "Saturn", Orbit "Saturn" "Enceladus"]
      part1 starmap `shouldBe` 3

