{-# LANGUAGE BangPatterns, LambdaCase, RecordWildCards, ViewPatterns #-}

module Day11 where

import           Control.Monad (forM_)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List (nub)
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec

import           Intcode3 (Memory, Result (..))
import qualified Intcode3 as Intcode

program :: Memory
program = unsafePerformIO $ Intcode.parse <$> readFile "day11input.txt"

type Position = (Int, Int)

data Direction = North | West | South | East
  deriving (Bounded, Enum, Eq, Show)

data Turn = L | R
  deriving (Bounded, Enum, Eq, Show)

turn :: Turn -> Direction -> Direction
turn L North = West
turn R North = East
turn L West  = South
turn R West  = North
turn L South = East
turn R South = West
turn L East  = North
turn R East  = South

data Colour = B | W
  deriving (Bounded, Enum, Eq, Show)

move :: Direction -> Position -> Position
move North (x, y) = (x, y - 1)
move West  (x, y) = (x - 1, y)
move South (x, y) = (x, y + 1)
move East  (x, y) = (x + 1, y)

data RobotInput = Sensor Colour
  deriving (Eq, Show)

data RobotOutput = Paint Colour Turn
  deriving (Eq, Show)

data RobotState = RobotState { continue :: (Int -> Result) }

startRobot :: Memory -> RobotState
startRobot memory =
  case Intcode.runIncremental memory [] of
    Await continue -> RobotState continue
    Yield _ _ -> error "Unexpected yield from robot program (A)."
    Done  _   -> error "Unexpected exit from robot program (B)."

stepRobot :: RobotState -> RobotInput -> (RobotOutput, Maybe RobotState)
stepRobot RobotState {..} (toIntcode -> input) =
  case continue input of
    Yield bw (Yield lr k) | output <- Paint (toEnum bw) (toEnum lr) ->
      case k of
        Await continue' -> (output, Just (RobotState continue'))
        Done _          -> (output, Nothing)
        Yield _ _       -> error "Unexpected yield from robot program (C)."
    Yield _ (Await _)   -> error "Unexpected await from robot program (D)."
    Yield _ (Done  _)   -> error "Unexpected exit from robot program (E)."
    Await _             -> error "Unexpected await from robot program (F)."
    Done  _             -> error "Unexpected exit from robot program (G)."

toIntcode :: RobotInput -> Int
toIntcode (Sensor value) = fromEnum value

paint :: Position -> Colour -> HashMap Position [Colour] -> HashMap Position [Colour]
paint pos colour =
  HashMap.alter merge pos
  where
    merge = maybe (Just [colour]) (\history -> Just (colour : history))

runRobot :: Colour -> [(Position, Colour)]
runRobot startingColour =
  go (startRobot program) (0, 0) North (HashMap.singleton (0, 0) [startingColour])
  where
    go state pos dir hull =
      let input = Sensor (head $ HashMap.lookupDefault [B] pos hull)
          (Paint bw lr, maybeState) = stepRobot state input
          dir'   = turn lr dir
          pos'   = move dir' pos
          hull'  = paint pos bw hull
          remain = case maybeState of
                     Just state' -> go state' pos' dir' hull'
                     Nothing     -> []
      in  (pos, bw) : remain

part1 :: Int
part1 = length . nub . map fst $ runRobot B

part2 :: IO ()
part2 = do
  let hull = HashMap.fromList (runRobot W)
      positions = HashMap.keys hull
      left   = minimum (map fst positions)
      right  = maximum (map fst positions)
      top    = minimum (map snd positions)
      bottom = maximum (map snd positions)
  forM_ [top .. bottom] $ \y -> do
    forM_ [left .. right] $ \x -> do
      let colour = HashMap.lookupDefault B (x, y) hull
      putStr [renderColour colour]
    putStrLn ""

renderColour :: Colour -> Char
renderColour B = ' '
renderColour W = '█'

selfTest :: IO ()
selfTest = hspec $ do
  Intcode.spec

  describe "turn" $ do
    describe "(turn L . turn R) == id" $
      forM_ [North .. East] $ \dir ->
        it (show dir) $
          (turn L . turn R) dir `shouldBe` dir
    describe "(turn L . turn L . turn L . turn L) == id" $
      forM_ [North .. East] $ \dir ->
        it (show dir) $
          (turn L . turn L . turn L . turn L) dir `shouldBe` dir

  describe "enums" $ do
    it "fromEnum L == 0" $
      fromEnum L `shouldBe` 0
    it "fromEnum R == 1" $
      fromEnum R `shouldBe` 1
    it "fromEnum B == 0" $
      fromEnum B `shouldBe` 0
    it "fromEnum W == 1" $
      fromEnum W `shouldBe` 1

  describe "stepRobot" $ do
    it "returns the expected output for the first step of the example program" $ do
      let state = startRobot program
          (output, _) = stepRobot state (Sensor B)
      output `shouldBe` Paint W L

  describe "part1" $
    it "returns the correct answer 1732" $
      part1 `shouldBe` 1732
