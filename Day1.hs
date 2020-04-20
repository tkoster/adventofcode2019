module Day1 where

main :: IO ()
main = do
  input <- readFile "day1input.txt"
  print $ sum . map (fuel    . read) . lines $ input
  print $ sum . map (fixfuel . read) . lines $ input

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

fixfuel :: Int -> Int
fixfuel mass
  | fuelmass <- fuel mass, fuelmass > 0 = fuelmass + fixfuel fuelmass
  | otherwise = 0
