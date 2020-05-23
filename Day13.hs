{-# LANGUAGE ViewPatterns #-}

module Day13 where

import           Data.Foldable (for_, toList)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Vector.Unboxed (Vector, (//))
import qualified Data.Vector.Unboxed as Vector
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec

import           Csv
import qualified Intcode3 as Intcode

data Tile = Empty | Wall | Block | Paddle | Ball
  deriving (Eq, Show, Bounded, Enum)

type Display = (HashMap (Int, Int) Tile, Int)

day13Input :: Vector Int
day13Input = Vector.fromList . map read . csv $ unsafePerformIO $ readFile "day13Input.txt"

draw :: [Int] -> Display -> Display
draw instructions s@(display, score) =
  case instructions of
    [] -> s
    (-1 : 0 : score' : remain) -> draw remain (display, score')
    (x : y : (toEnum -> tile) : remain) -> draw remain (HashMap.insert (x, y) tile display, score)
    _ -> error $ "not a valid draw instruction: " ++ show instructions

render :: Display -> IO ()
render (display, score) = do
  let w = maximum $ map fst $ HashMap.keys display
  let h = maximum $ map snd $ HashMap.keys display
  for_ [0..h] $ \y -> do
    for_ [0..w] $ \x -> do
      case HashMap.lookupDefault Empty (x, y) display of
        Empty  -> putChar ' '
        Wall   -> putChar '█'
        Block  -> putChar '◻'
        Paddle -> putChar '⎺'
        Ball   -> putChar '●'
    putChar '\n'
  putStr "Score: "
  print score

play :: (Display -> IO Int) -> IO Display
play getInput = go initialDisplay
  where
    initialDisplay = (HashMap.empty, 0)
    initialMemory  = day13Input // [(0, 2)]
    go display = go' display (Intcode.step initialMemory 0 0) []
    go' display result outputs =
      case result of
        Intcode.Await continue -> do
          input <- getInput display
          let result' = continue input
          go' display result' outputs
        Intcode.Yield value continue ->
          case value : outputs of
            [tile, y, x] -> do
              let display' = draw [x, y, tile] display
              go' display' continue []
            outputs' -> go' display continue outputs'
        Intcode.Done _ ->
          return display

interactiveMove :: Display -> IO Int
interactiveMove display = do
  render display
  putStr "(0 = neutral, -1 = left, 1 = right) > "
  line <- getLine
  return $
    if line == "-1" || line == "h" then -1
    else if line == "1" || line == "l" then 1
    else 0

autoMove :: Display -> IO Int
autoMove (display, _) = do
  let ballX = findX Ball
      paddleX = findX Paddle
  return $
    if ballX < paddleX then -1
    else if ballX > paddleX then 1
    else 0
  where
    findX tile =
      head [x | ((x, _), tile') <- HashMap.toList display, tile == tile']

part1 :: Int
part1 =
  length $ filter (== Block) $ toList display
  where
    (_, outputs) = Intcode.run day13Input []
    (display, _score) = draw outputs (mempty, 0)

part2 :: IO ()
part2 = do
  (_, score) <- play autoMove
  print score

selfTest :: IO ()
selfTest = hspec $ do
  describe "draw" $ do
    it "draws a block" $
      draw [1, 3, 2] (HashMap.empty, 0) `shouldBe` (HashMap.singleton (1, 3) Block, 0)
    it "sets the score" $
      draw [-1, 0, 234] (HashMap.empty, 0) `shouldBe` (HashMap.empty, 234)
