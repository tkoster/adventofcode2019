-- Day 16 can't run in GHCi because optimizations are required, especially
-- vector fusion.
--
-- Compile this program with optimizations and run it to print the day 16
-- solutions.

module Main (main) where

import Day18

main :: IO ()
main = do
  selfTest

