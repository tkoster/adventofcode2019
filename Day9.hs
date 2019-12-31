module Day9 where

import Test.Hspec

import qualified Intcode3 as Intcode

part1 :: IO ()
part1 = do
  mem <- Intcode.parse <$> readFile "day9input.txt"
  let [keycode] = Intcode.eval mem [1]
  print keycode

part2 :: IO ()
part2 = do
  mem <- Intcode.parse <$> readFile "day9input.txt"
  print $ Intcode.eval mem [2]

selfTest :: IO ()
selfTest = hspec $
  Intcode.spec
