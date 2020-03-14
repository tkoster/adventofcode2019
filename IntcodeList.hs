module IntcodeList where

import Test.Hspec
import Csv

parse :: String -> [Int]
parse = map read . csv

data StepResult = Continue | Halted

run :: [Int] -> [Int]
run = loop 0
  where
    loop pc memory =
      case step pc memory of
        (pc', memory', Continue) -> loop pc' memory'
        (_,   memory', Halted)   -> memory'

step :: Int -> [Int] -> (Int, [Int], StepResult)
step pc memory =
  case opcode of
    1 -> add
    2 -> mul
    99 -> halt
    _  -> error $ "Invalid opcode at position " <> show pc <> ": " <> show opcode
  where
    opcode = memory !! pc
    binop f =
      let aptr = memory !! (pc + 1)
          a    = memory !! aptr
          bptr = memory !! (pc + 2)
          b    = memory !! bptr
          cptr = memory !! (pc + 3)
          memory' = take cptr memory ++ [f a b] ++ drop (cptr + 1) memory
      in  (pc + 4, memory', Continue)
    add = binop (+)
    mul = binop (*)
    halt = (pc, memory, Halted)

selfTest :: IO ()
selfTest = hspec $ do
  describe "run" $ do
    it "given [1,9,10,3,2,3,11,0,99,30,40,50] returns [3500,9,10,70,2,3,11,0,99,30,40,50]" $
      run [1,9,10,3,2,3,11,0,99,30,40,50] `shouldBe` [3500,9,10,70,2,3,11,0,99,30,40,50]

    it "given [1,0,0,0,99] returns [2,0,0,0,99]" $
      run [1,0,0,0,99] `shouldBe` [2,0,0,0,99]

    it "given [2,3,0,3,99] returns [2,3,0,6,99]" $
      run [2,3,0,3,99] `shouldBe` [2,3,0,6,99]

    it "given [2,4,4,5,99,0] returns [2,4,4,5,99,9801]" $
      run [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]

    it "given [1,1,1,4,99,5,6,0,99] returns [30,1,1,4,2,5,6,0,99]" $
      run [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]

