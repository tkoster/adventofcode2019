module Csv (csv) where

import Data.List (break)

csv :: String -> [String]
csv [] = []
csv input =
  case break (== ',') input of
    ([], remain) ->     csv (drop 1 remain)
    (a,  remain) -> a : csv (drop 1 remain)

