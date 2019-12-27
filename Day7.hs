module Day7 where

import Data.List
import Data.Vector.Unboxed (Vector)
import Test.Hspec

import Intcode3 as Intcode

runSample :: Vector Int -> (Int, Int, Int, Int, Int) -> Int
runSample program (a, b, c, d, e) =
  step e . step d . step c . step b . step a $ 0
  where
    step phase input =
      let (_, [output]) = Intcode.run program [phase, input]
      in  output

runSampleWithFeedback :: Vector Int -> (Int, Int, Int, Int, Int) -> Int
runSampleWithFeedback program (a, b, c, d, e) =
 -- Initialise amplifier programs
  let k1 = runIncremental program [a, 0]
      k2 = runIncremental program [b]
      k3 = runIncremental program [c]
      k4 = runIncremental program [d]
      k5 = runIncremental program [e]
  in  step1 k1 k2 k3 k4 k5
  where
    -- Thread outputs through the amplifiers
    step1 k1 k2 k3 k4 k5 = let (k1', k2') = pipe k1 k2 in step2 k1' k2' k3  k4  k5
    step2 k1 k2 k3 k4 k5 = let (k2', k3') = pipe k2 k3 in step3 k1  k2' k3' k4  k5
    step3 k1 k2 k3 k4 k5 = let (k3', k4') = pipe k3 k4 in step4 k1  k2  k3' k4' k5
    step4 k1 k2 k3 k4 k5 = let (k4', k5') = pipe k4 k5 in step5 k1  k2  k3  k4' k5'
    step5 (Done _) _ _ _ (Yield value _) = value
    step5 k1 k2 k3 k4 k5 = let (k5', k1') = pipe k5 k1 in step1 k1' k2  k3  k4  k5'
    -- Pipe the output of one computer in Yield state to the input of another in Await state.
    pipe (Yield value k1) (Await k2) = (k1, k2 value)
    pipe _ _ = error "unexpected state"

part1 :: IO ()
part1 = do
  program <- Intcode.parse <$> readFile "day7input.txt"
  let phases = [(a, b, c, d, e) | [a, b, c, d, e] <- permutations [0, 1, 2, 3, 4]]
  let thrusts = map (runSample program) phases
  print $ maximum thrusts

part2 :: IO ()
part2 = do
  program <- Intcode.parse <$> readFile  "day7input.txt"
  let phases = [(a, b, c, d, e) | [a, b, c, d, e] <- permutations [5, 6, 7, 8, 9]]
  let thrusts = map (runSampleWithFeedback program) phases
  print $ maximum thrusts

--------

selfTest :: IO ()
selfTest = hspec $ do
  Intcode.spec

  describe "runSample" $ do
    it "works for example 1" $ do
      let program = Intcode.parse "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
      runSample program (4, 3, 2, 1, 0) `shouldBe` 43210
    it "works for example 2" $ do
      let program = Intcode.parse "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
      runSample program (0, 1, 2, 3, 4) `shouldBe` 54321
    it "works for example 3" $ do
      let program = Intcode.parse "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
      runSample program (1, 0, 4, 3, 2) `shouldBe` 65210

  describe "runSampleWithFeedback" $ do
    it "works for example 1" $ do
      let program = Intcode.parse "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
      runSampleWithFeedback program (9, 8, 7, 6, 5) `shouldBe` 139629729
    it "works for example 2" $ do
      let program = Intcode.parse "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
      runSampleWithFeedback program (9, 7, 8, 5, 6) `shouldBe` 18216

