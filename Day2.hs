module Main where

import Control.Exception (assert)
import Control.Monad (when)
import Data.List (nub)
import Data.Vector.Unboxed ((!), fromList, update)
import System.Exit (exitFailure)

import Intcode

main :: IO ()
main = do
  selfTest
  part1
  part2

part1 = do
  input <- readFile "day2input.txt"
  let program = Intcode.parse input
      result  = run program 12 2
  print (result ! 0)

part2 = do
  input <- readFile "day2input.txt"
  let program = Intcode.parse input
      results = [ (noun, verb, result)
                | noun <- [0..99]
                , verb <- [0..99]
                , let result = run program noun verb
                , result ! 0 == 19690720
                ]
  let ((noun, verb, _) : _) = results
  print (100 * noun + verb)

run program noun verb =
  let program' = update program $ fromList [(1, noun), (2, verb)]
      result = Intcode.exec program'
  in  result

selfTest = do
  testExec "1,9,10,3,2,3,11,0,99,30,40,50" "3500,9,10,70,2,3,11,0,99,30,40,50"
  testExec "1,0,0,0,99" "2,0,0,0,99"
  testExec "2,3,0,3,99" "2,3,0,6,99"
  testExec "2,4,4,5,99,0" "2,4,4,5,99,9801"
  testExec "1,1,1,4,99,5,6,0,99" "30,1,1,4,2,5,6,0,99"

testExec input expected = do
  let actual = (exec . parse) input
  when (actual /= parse expected) $ do
    print expected
    print actual
    exitFailure
