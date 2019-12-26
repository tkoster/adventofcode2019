module Day7 where

import Data.List
import Data.Vector.Unboxed (Vector)
import Test.Hspec

import Intcode2

phases :: [(Int, Int, Int, Int, Int)]
phases = [(a, b, c, d, e) | [a, b, c, d, e] <- permutations [0, 1, 2, 3, 4]]

runSample :: Vector Int -> (Int, Int, Int, Int, Int) -> Int
runSample program (a, b, c, d, e) =
  step e . step d . step c . step b . step a $ 0
  where
    step phase input =
      let (_, [output]) = Intcode2.exec program [phase, input]
      in  output

part1 :: IO ()
part1 = do
  program <- Intcode2.parse <$> readFile "day7input.txt"
  let thrusts = map (runSample program) phases
  print $ maximum thrusts

--------

selfTest :: IO ()
selfTest = hspec $ do
  describe "phases" $
    it "returns 120 permutations" $
      length phases `shouldBe` 120
