{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Data.Function
import Data.List (foldl1', minimumBy)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Vector.Unboxed (Vector, fromList, toList)
import qualified Data.Vector.Unboxed as Vector
import Data.Word
import Test.Hspec

type Layer = Vector Word8

decode :: Int -> Int -> Text -> [Layer]
decode width height input = go input
  where
    go s
      | Text.null s || s == "\n" =
          -- EOF
          []
      | Text.length s < layerLength =
          -- Not enough input remaining for a full layer.
          error $ "not enough data " ++ show s
      | otherwise =
          let (layer, remain) = Text.splitAt layerLength s
          in  (fromList . unpack) layer : go remain
    layerLength = width * height

decodeFile :: Int -> Int -> FilePath -> IO [Layer]
decodeFile width height path = decode width height <$> Text.readFile path

-- unpack "01234" returns ["0", "1", "2", "3", "4"]
unpack :: Text -> [Word8]
unpack s
  | Text.null s = []
  | otherwise =
      let (c, s') = Text.splitAt 1 s
      in  parse c : unpack s'
  where
    parse c =
      case Text.decimal c of
        Right (value, remain)
          | Text.null remain -> value
          | otherwise -> error "invalid input (A)"
        Left _err -> error "invalid input (B)"

count :: Word8 -> Layer -> Int
count digit layer = Vector.foldl' f 0 layer
  where
    f acc value
      | value == digit = acc + 1
      | otherwise      = acc

part1 :: IO Int
part1 = do
  image <- decodeFile 25 6 "day8input.txt"
  let layerWithLeastZeroes = minimumBy (compare `on` count 0) image
      ones = count 1 layerWithLeastZeroes
      twos = count 2 layerWithLeastZeroes
  return (ones * twos)

mergeLayers :: [Layer] -> Layer
mergeLayers = foldl1' mergeLayer

mergeLayer :: Layer -> Layer -> Layer
mergeLayer = Vector.zipWith mergeValue

mergeValue :: Word8 -> Word8 -> Word8
mergeValue 2 b = b
mergeValue a _ = a

showLayer :: Layer -> Int -> Int -> [Text]
showLayer layer width height
  | Vector.null layer = []
  | otherwise =
      let (row, remain) = Vector.splitAt width layer
          shownRow = (Text.pack . map showValue . toList) row
      in  shownRow : showLayer remain width height
      where
        showValue 1 = '█'
        showValue _ = ' '

part2 :: IO ()
part2 = do
  image <- mergeLayers <$> decodeFile 25 6 "day8input.txt"
  mapM_ Text.putStrLn (showLayer image 25 6)

selfTest :: IO ()
selfTest = hspec $ do
  describe "decode" $ do
    it "accepts 3x2 123456789012" $ do
      let layer1 = fromList [1, 2, 3, 4, 5, 6]
          layer2 = fromList [7, 8, 9, 0, 1, 2]
      decode 3 2 "123456789012" `shouldBe` [layer1, layer2]
    it "ignores trailing newline" $ do
      decode 1 1 "9\n" `shouldBe` [fromList [9]]

  describe "count" $
    it "works" $ do
      let layer = fromList [1, 2, 3, 4, 2, 3, 4, 3, 4, 4]
      count 1 layer `shouldBe` 1
      count 2 layer `shouldBe` 2
      count 3 layer `shouldBe` 3
      count 4 layer `shouldBe` 4

  describe "mergeValue" $ do
    it "returns black when top is black" $ do
      mergeValue 0 1 `shouldBe` 0
      mergeValue 0 2 `shouldBe` 0
    it "returns white when top is white" $ do
      mergeValue 1 0 `shouldBe` 1
      mergeValue 1 2 `shouldBe` 1
    it "returns black when top is transparent and bottom is black" $
      mergeValue 2 0 `shouldBe` 0
    it "returns white when top is transparent and bottom is white" $
      mergeValue 2 1 `shouldBe` 1
    it "returns transparent when top and bottom are transparent" $
      mergeValue 2 2 `shouldBe` 2

  describe "mergeLayer" $
    it "works" $ do
      let layer1 = fromList [0, 2, 2, 2]
          layer2 = fromList [1, 1, 1, 1]
      mergeLayer layer1 layer2 `shouldBe` fromList [0, 1, 1, 1]

  describe "mergeLayers" $
    it "works (example)" $ do
      let layer1 = fromList [0, 2, 2, 2]
          layer2 = fromList [1, 1, 2, 2]
          layer3 = fromList [2, 2, 1, 2]
          layer4 = fromList [0, 0, 0, 0]
      mergeLayers [layer1, layer2, layer3, layer4] `shouldBe` fromList [0, 1, 1, 0]

