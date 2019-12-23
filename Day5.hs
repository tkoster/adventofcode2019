module Day5 (main, selfTest) where

import Data.Foldable (for_)
import Data.Vector.Unboxed ((!), toList)
import Test.Hspec

import Intcode2

main :: IO ()
main = do
  -- Read program from input file:
  input <- readFile "day5input.txt"
  let mem = Intcode2.parse input

  -- Part 1:
  let (_, output1) = Intcode2.exec mem [1]
  print output1

  -- Part 2:
  let (_, output2) = Intcode2.exec mem [5]
  print output2

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

      describe "part 2 'a larger example'" $ do

        let mem = Intcode2.parse "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

        it "given 7 outputs 999" $
          snd (Intcode2.exec mem [7]) `shouldBe` [999]

        it "given 8 outputs 1000" $
          snd (Intcode2.exec mem [8]) `shouldBe` [1000]

        it "given 9 outputs 1001" $
          snd (Intcode2.exec mem [9]) `shouldBe` [1001]

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

    describe "Compare" $ do

      describe "3,9,8,9,10,9,4,9,99,-1,8 (equal to 8, indirect)" $ do

        let mem = Intcode2.parse "3,9,8,9,10,9,4,9,99,-1,8"

        for_ [6,7] $ \ n ->
          it ("given " <> show n <> " outputs 0") $ do
            let (_, outputs) = Intcode2.exec mem [n]
            outputs `shouldBe` [0]

        it "given 8 outputs 1" $ do
          let (_, outputs) = Intcode2.exec mem [8]
          outputs `shouldBe` [1]

      describe "3,9,7,9,10,9,4,9,99,-1,8 (less than 8, indirect)" $ do

        let mem = Intcode2.parse "3,9,7,9,10,9,4,9,99,-1,8"

        for_ [6,7] $ \ n ->
          it ("given " <> show n <> " outputs 1") $ do
            let (_, outputs) = Intcode2.exec mem [n]
            outputs `shouldBe` [1]

        it "given 8 outputs 0" $ do
          let (_, outputs) = Intcode2.exec mem [8]
          outputs `shouldBe` [0]

      describe "3,3,1108,-1,8,3,4,3,99 (equal to 8, immediate)" $ do

        let mem = Intcode2.parse "3,3,1108,-1,8,3,4,3,99"

        for_ [6,7] $ \ n ->
          it ("given " <> show n <> " outputs 0") $ do
            let (_, outputs) = Intcode2.exec mem [n]
            outputs `shouldBe` [0]

        it "given 8 outputs 1" $ do
          let (_, outputs) = Intcode2.exec mem [8]
          outputs `shouldBe` [1]

      describe "3,3,1107,-1,8,3,4,3,99 (less than 8, immediate)" $ do

        let mem = Intcode2.parse "3,3,1107,-1,8,3,4,3,99"

        for_ [6,7] $ \ n ->
          it ("given " <> show n <> " outputs 1") $ do
            let (_, outputs) = Intcode2.exec mem [n]
            outputs `shouldBe` [1]

        it "given 8 outputs 0" $ do
          let (_, outputs) = Intcode2.exec mem [8]
          outputs `shouldBe` [0]

    describe "Jump" $ do

      describe "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 (is nonzero, indirect)" $ do

        let mem = Intcode2.parse "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"

        it "given 0 outputs 0" $
          snd (Intcode2.exec mem [0]) `shouldBe` [0]

        it "given 1 outputs 1" $
          snd (Intcode2.exec mem [1]) `shouldBe` [1]

      describe "3,3,1105,-1,9,1101,0,0,12,4,12,99,1 (is nonzero, immediate)" $ do

        let mem = Intcode2.parse "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"

        it "given 0 outputs 0" $
          snd (Intcode2.exec mem [0]) `shouldBe` [0]

        it "given 1 outputs 1" $
          snd (Intcode2.exec mem [1]) `shouldBe` [1]

    describe "I/O" $ do

      it "3,0,4,0,99 echoes input" $ do
        let mem = Intcode2.parse "3,0,4,0,99"
            (_, output) = Intcode2.exec mem [12345]
        output `shouldBe` [12345]

      it "3,0,4,0,3,0,4,0,99 echoes input twice" $ do
        let mem = Intcode2.parse "3,0,4,0,3,0,4,0,99"
            (_, output) = Intcode2.exec mem [12345, 54321]
        output `shouldBe` [12345, 54321]
