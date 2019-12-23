module Day5 (main, selfTest) where

import Data.Vector.Unboxed ((!), toList)
import Test.Hspec

import Intcode2

main :: IO ()
main = do
  input <- readFile "day5input.txt"
  let mem = Intcode2.parse input
      (_, output) = Intcode2.exec mem [1]
  print output

selfTest :: IO ()
selfTest = hspec $ do

  describe "Intcode" $ do

    describe "Examples" $ do

      it "1002,4,3,4,33 computes 1002,4,3,4,99" $ do
        let mem = Intcode2.parse "1002,4,3,4,33"
            (mem', []) = Intcode2.exec mem []
        toList mem' `shouldBe` [1002, 4, 3, 4, 99]

      it "1101,100,-1,4,0 computes 1101,100,-1,4,99" $ do
        let mem = Intcode2.parse "1101,100,-1,4,0"
            (mem', []) = Intcode2.exec mem []
        toList mem' `shouldBe` [1101, 100, -1, 4, 99]

    describe "Parsing" $

      describe "parseOp" $

        it "parses 1002" $
          Intcode2.parseOp 0 1002 `shouldBe` Mul Indirect Immediate

    describe "Arithmetic" $ do

      describe "add" $

        it "1,5,6,7,99,30,40,0 computes 30 + 40" $ do
          let mem = Intcode2.parse "1,5,6,7,99,30,40,0"
              (mem', []) = Intcode2.exec mem []
          mem' ! 7 `shouldBe` 70

      describe "mul" $
        it "2,5,6,7,99,30,40,0 computes 30 * 40" $ do
          let mem = Intcode2.parse "2,5,6,7,99,30,40,0"
              (mem', []) = Intcode2.exec mem []
          mem' ! 7 `shouldBe` 1200

    describe "I/O" $ do

      it "3,0,4,0,99 echoes input" $ do
        let mem = Intcode2.parse "3,0,4,0,99"
            (_, output) = Intcode2.exec mem [12345]
        output `shouldBe` [12345]

      it "3,0,4,0,3,0,4,0,99 echoes input twice" $ do
        let mem = Intcode2.parse "3,0,4,0,3,0,4,0,99"
            (_, output) = Intcode2.exec mem [12345, 54321]
        output `shouldBe` [12345, 54321]
