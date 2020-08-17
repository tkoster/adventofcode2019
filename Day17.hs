module Day17 (part1, selfTest) where

import           Data.Char (chr)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Maybe (fromMaybe)
import           Data.String (IsString, fromString)
import           Data.Vector.Unboxed (Vector)
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec (describe, hspec, it, shouldBe)

import           Intcode3 (Result (..))
import qualified Intcode3 as Intcode

program :: Vector Int
program = Intcode.parse $ unsafePerformIO $ readFile "day17input.txt"

programOutput :: ByteString
programOutput = ByteString.pack $ go (Intcode.runIncremental program [])
  where
    go result =
      case result of
        Yield c next -> chr c : go next
        Await _ -> ""
        Done _ -> ""

data Image = Image
  { raw    :: ByteString
  , width  :: Int
  , height :: Int
  , sample :: ((Int, Int) -> Char)
  }

instance IsString Image where
  fromString = view . fromString

view :: ByteString -> Image
view raw = Image {..}
  where
    width = fromMaybe 0 $ ByteString.elemIndex '\n' raw
    height = ByteString.count '\n' raw
    sample (col, row) = ByteString.index raw (row * (width + 1) + col)

intersections :: Image -> [(Int, Int)]
intersections Image {..} =
  [point | col <- [1 .. width-2], row <- [1 .. height-2], let point = (col, row), isIntersection point]
  where
    cross (col, row) = [(col, row), (col - 1, row), (col + 1, row), (col, row - 1), (col, row + 1)]
    isIntersection = all (== '#') . map sample . cross

alignmentParameter :: (Int, Int) -> Int
alignmentParameter (col, row) = col * row

calibrate :: Image -> Int
calibrate = sum . map alignmentParameter . intersections

part1 :: Int
part1 = calibrate (view programOutput)



selfTest :: IO ()
selfTest = hspec $ do
  describe "intersections" $ do
    it "returns an empty list for images less than 3 units a side" $ do
      intersections "" `shouldBe` []
      intersections "#" `shouldBe` []
      intersections "##\n##" `shouldBe` []
    it "does not return T intersections" $
      intersections "###\n # \n###" `shouldBe` []
    it "returns a + intersection" $
      intersections " # \n###\n # \n" `shouldBe` [(1,1)]
    it "returns the intersections in the sample image" $
      intersections sampleImage `shouldBe`
        [(2,2), (2,4), (6,4), (10,4)]

  describe "calibrate" $
    it "returns 76 given the sample image" $
      calibrate sampleImage `shouldBe` 76

  describe "part1" $
    it "returns 3608" $
      part1 `shouldBe` 3608

sampleImage :: Image
sampleImage = "..#..........\n..#..........\n#######...###\n#.#...#...#.#\n#############\n..#...#...#..\n..#####...^..\n"
