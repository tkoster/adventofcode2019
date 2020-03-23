{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-} -- fight me

module Day12 where

import           Control.Applicative (optional)
import           Control.Monad (void)
import           Data.Attoparsec.Text
                  ( Parser, parseOnly, skipWhile, isHorizontalSpace, many1, sepBy, string, decimal, signed, endOfLine)
import           Data.Foldable (foldl', for_)
import           Data.Monoid (Sum (..), getSum)
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Test.Hspec

data V3 = V3 Int Int Int
  deriving (Eq, Show)

zero :: V3
zero = V3 0 0 0

add :: V3 -> V3 -> V3
add (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 x3 y3 z3
  where
    x3 = x1 + x2
    y3 = y1 + y2
    z3 = z1 + z2

data Step = Step { index :: Int, moons :: [Moon] }
  deriving (Eq, Show)

-------- Parsing --------

parse :: Parser a -> Text -> a
parse parser input =
  case parseOnly parser input of
    Left err -> error err
    Right x  -> x

hs :: Parser ()
hs = skipWhile isHorizontalSpace

vectorList :: Parser [V3]
vectorList = vector `sepBy` (many1 endOfLine)

vector :: Parser V3
vector = do
  void $ string "<x="
  hs
  x <- signed decimal
  hs
  void $ string ","
  hs
  void $ string "y="
  hs
  y <- signed decimal
  hs
  void $ string ","
  hs
  void $ string "z="
  hs
  z <- signed decimal
  hs
  void $ string ">"
  hs
  return (V3 x y z)

moon :: Parser Moon
moon = do
  void $ string "pos="
  pos <- vector
  hs
  void $ string ","
  hs
  void $ string "vel="
  hs
  vel <- vector
  hs
  return (Moon pos vel)

stepTrace :: Parser Step
stepTrace = do
  void $ string "After "
  hs
  n <- decimal
  void $ string " "
  hs
  void $ string "step"
  void $ optional $ string "s"
  void $ string ":"
  endOfLine
  moons <- moon `sepBy` (many1 endOfLine)
  return (Step n moons)

stepTraceList :: Parser [Step]
stepTraceList = stepTrace `sepBy` (many1 endOfLine)

-------- Simulation --------

-- 'velocity a b' is the velocity of moon 'a' due to the gravitational force between 'a' and 'b'.
velocity :: V3 -> V3 -> V3
velocity (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 x3 y3 z3
  where
    x3 = d x1 x2
    y3 = d y1 y2
    z3 = d z1 z2
    d a b =
      case compare a b of
        LT -> 1
        EQ -> 0
        GT -> -1

totalVelocity :: V3 -> [V3] -> V3
totalVelocity a =
  foldl' add zero . map (velocity a)

data Moon = Moon { pos :: V3, vel :: V3 }
  deriving (Eq, Show)

fromInitialPosition :: V3 -> Moon
fromInitialPosition = flip Moon zero

move :: V3 -> Moon -> Moon
move vel (Moon pos vel') = Moon (add pos vel'') vel''
  where vel'' = add vel vel'

step :: [Moon] -> [Moon]
step moons = [move vel moon | moon <- moons, let vel = totalVelocity (pos moon) (map pos moons)]

eval :: Int -> [Moon] -> [Moon]
eval n moons = iterate step moons !! n

potentialEnergy :: Moon -> Int
potentialEnergy (Moon (V3 x y z) _) = abs x + abs y + abs z

kineticEnergy :: Moon -> Int
kineticEnergy (Moon _ (V3 dx dy dz)) = abs dx + abs dy + abs dz

energy :: Moon -> Int
energy moon = potentialEnergy moon * kineticEnergy moon

totalEnergy :: [Moon] -> Int
totalEnergy = getSum . foldMap (Sum . energy)

-------- Main --------

part1 :: IO ()
part1 = do
  day12input <- Text.readFile "day12input.txt"
  let initialPositions = parse vectorList day12input
      states = eval 1000 (map fromInitialPosition initialPositions)
  print $ totalEnergy states

-------- Tests --------

selfTest :: IO ()
selfTest = hspec $ do
  describe "parsing" $ do
    describe "parse vector" $ do
      it "accepts a vector" $
        parse vector "<x=1,y=2,z=3>" `shouldBe` V3 1 2 3
      it "accepts spaces after =" $
        parse vector "<x=  1, y= 2, z=         3>" `shouldBe` V3 1 2 3
      it "accepts spaces after ," $
        parse vector "<x=1, y=2,       z=3>" `shouldBe` V3 1 2 3
    describe "parse vectorList" $ do
      it "accepts day12input.txt" $ do
        day12input <- Text.readFile "day12input.txt"
        parse vectorList day12input `shouldBe` [V3 5 4 4, V3 (-11) (-11) (-3), V3 0 7 0, V3 (-13) 2 10]
      it "accepts day12example1input.txt" $ do
        day12example1input <- Text.readFile "day12example1input.txt"
        parse vectorList day12example1input `shouldSatisfy` (== 4) . length
    describe "parse moon" $
      it "accepts a moon" $ do
        let input = "pos=<x=1,y=2,z=3>,vel=<x=4,y=5,z=6>"
        parse moon input `shouldBe` Moon (V3 1 2 3) (V3 4 5 6)
    describe "parse stepTrace" $
      it "accepts a step" $ do
        let input = "After 2 steps:\npos=<x=1,y=2,z=3>,vel=<x=4,y=5,z=6>\npos=<x=7,y=8,z=9>,vel=<x=1,y=2,z=3>"
        parse stepTrace input `shouldBe` Step 2 [Moon (V3 1 2 3) (V3 4 5 6), Moon (V3 7 8 9) (V3 1 2 3)]
    describe "parse stepTraceList" $
      it "accepts day12example1output.txt" $ do
        input <- Text.readFile "day12example1output.txt"
        let output = parse stepTraceList input
        length output `shouldBe` 11 -- After 0-10 steps
        output !! 0 `shouldBe` Step 0
                                [ Moon (V3 (-1) 0 2) (V3 0 0 0)
                                , Moon (V3 2 (-10) (-7)) (V3 0 0 0)
                                , Moon (V3 4 (-8) 8) (V3 0 0 0)
                                , Moon (V3 3 5 (-1)) (V3 0 0 0)
                                ]
        output !! 10 `shouldBe` Step 10
                                [ Moon (V3 2 1 (-3)) (V3 (-3) (-2) 1)
                                , Moon (V3 1 (-8) 0) (V3 (-1) 1 3)
                                , Moon (V3 3 (-6) 1) (V3 3 2 (-3))
                                , Moon (V3 2 0 4) (V3 1 (-1) (-1))
                                ]

  describe "simulation" $ do
    example1input  <- runIO $ Text.readFile "day12example1input.txt"
    example1output <- runIO $ Text.readFile "day12example1output.txt"

    describe "velocity" $ do
      it "ganymede-callisto example" $ do
        velocity (V3 3 0 0) (V3 5 0 0) `shouldBe` V3 1 0 0
        velocity (V3 5 0 0) (V3 3 0 0) `shouldBe` V3 (-1) 0 0

    describe "step" $ do
      for_ (parse stepTraceList example1output) $ \ (Step n expectedStates) ->
        it ("calculates the expected state after " <> show n <> " steps") $ do
          let initialPositions = parse vectorList example1input
              states = eval n (map fromInitialPosition initialPositions)
          states `shouldBe` expectedStates

    describe "totalEnergy" $ do
      it "is 179 after 10 steps of example 1" $ do
        let initialPositions = parse vectorList example1input
            states = eval 10 (map fromInitialPosition initialPositions)
        totalEnergy states `shouldBe` 179

