{-# LANGUAGE ViewPatterns #-}

module Day13 where

import           Data.Foldable (toList)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec

import           Csv
import qualified Intcode3 as Intcode

data Tile = Empty | Wall | Block | Paddle | Ball
  deriving (Eq, Show, Bounded, Enum)

type Display = HashMap (Int, Int) Tile

day13Input :: Vector Int
day13Input = Vector.fromList . map read . csv $ unsafePerformIO $ readFile "day13Input.txt"

draw :: [Int] -> Display -> Display
draw instructions display =
  case instructions of
    [] -> display
    (x : y : (toEnum -> tile) : remain) -> draw remain (HashMap.insert (x, y) tile display)
    _ -> error $ "not a valid draw instruction: " ++ show instructions

part1 :: Int
part1 =
  length $ filter (== Block) $ toList display
  where
    (_, outputs) = Intcode.run day13Input []
    display = draw outputs mempty

selfTest :: IO ()
selfTest = hspec $ do
  describe "draw" $
    it "draws a block" $
      draw [1, 3, 2] HashMap.empty `shouldBe` HashMap.singleton (1, 3) Block
