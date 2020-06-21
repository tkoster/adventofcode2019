module Day16 (part1, part2, selfTest) where

import           Data.Char (digitToInt, isDigit)
import           Control.Monad.ST (ST)
import           Data.List (isSuffixOf)
import           Data.Foldable (for_)
import           Data.Function ((&))
import           Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec (describe, hspec, it, shouldBe, shouldSatisfy, specify)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (getNonEmpty, getNonNegative)

parse :: String -> Vector Int
parse input = input & filter isDigit & map digitToInt & Vector.fromList

day16Signal :: Vector Int
day16Signal = parse day16Input

day16Input :: String
day16Input = unsafePerformIO $ readFile "day16input.txt"

phase :: Vector Int -> Vector Int
phase input = Vector.generate (Vector.length input) element
  where
    element outputIndex = Vector.imap (term outputIndex) input & Vector.sum & mod10
    term outputIndex inputIndex n = n * pattern inputIndex outputIndex

mod10 :: Int -> Int
mod10 n = abs n `mod` 10

addMod10 :: Int -> Int -> Int
addMod10 x y = mod10 (x + y)

pattern :: Int -> Int -> Int
pattern inputIndex outputIndex =
  case ((1 + inputIndex) `div` (1 + outputIndex)) `mod` 4 of
    0 -> 0
    1 -> 1
    2 -> 0
    _ -> -1

-- | Apply a function n times. Similar to iterate, but does not accumulate a
-- list of intermediate values, returning on the final value.
dup :: Int -> (a -> a) -> a -> a
dup 0 _ x = x
dup n f x = dup (n - 1) f (f x)

toInt :: Vector Int -> Int
toInt = Vector.foldl' (\a x -> 10 * a + x) 0

part1 :: Int
part1 = dup 100 phase day16Signal & Vector.take 8 & toInt

-- | A faster version of phase that is only correct for the second half of the
-- signal.
--
-- This version makes use of the following observations of the second half of
-- the signal:
-- * the last element in the signal is never changed
-- * the phase of element N is the signal at N plus the phase of element N+1
fastphase :: Vector Int -> Vector Int
fastphase signal
  | n < 3 = signal
  | otherwise = Vector.modify (doPhase $ n - 2) signal
  where
    doPhase :: Int -> MVector.MVector s Int -> ST s ()
    doPhase (-1) _ = return ()
    doPhase index vec = do
      p <- MVector.read vec (index + 1)
      MVector.modify vec (addMod10 p) index
      doPhase (index - 1) vec
    n = Vector.length signal

-- | Read the offset from the signal.
offset :: Vector Int -> Int
offset signal = toInt (Vector.take 7 signal)

-- | Get the 8-digit message from the signal.
message :: Vector Int -> Vector Int
message signal = Vector.take 8 phased
  where
    n = Vector.length signal
    extendedSignal = Vector.generate (n * 10000) (\i -> signal ! (i `mod` n))
    phased = dup 100 fastphase (Vector.drop (offset signal) extendedSignal)

part2 :: Int
part2 = toInt (message day16Signal)

selfTest :: IO ()
selfTest = hspec $ do
  describe "pattern" $ do
    it "returns the base pattern for output index 0" $
      map (`pattern` 0) [0..3] `shouldBe` [1, 0, -1, 0]
    it "returns the expected pattern for output index 1" $
      map (`pattern` 1) [0..14] `shouldBe` [0, 1, 1, 0, 0, -1, -1, 0, 0, 1, 1, 0, 0, -1, -1]

  describe "phase" $ do
    it "returns the 48226158 for input 12345678" $
      phase (parse "12345678") `shouldBe` parse "48226158"
    it "returns [0,1] for input [0,1]" $
      phaseList [0,1] `shouldBe` [0,1]

  describe "phaseN" $ do
    it "after 1 phase, returns 48226158 for input 12345678" $
      dup 1 phase (parse "12345678") `shouldBe` parse "48226158"
    it "after 2 phases, returns 34040438 for input 12345678" $
      dup 2 phase (parse "12345678") `shouldBe` parse "34040438"
    it "after 3 phases, returns 03415518 for input 12345678" $
      dup 3 phase (parse "12345678") `shouldBe` parse "03415518"
    it "after 3 phases, returns 01029498 for input 12345678" $
      dup 4 phase (parse "12345678") `shouldBe` parse "01029498"
    it "after 100 phases, returns 24176176 for input 80871224585914546619083218645595" $ do
      let input = parse "80871224585914546619083218645595"
      Vector.take 8 (dup 100 phase input) `shouldBe` parse "24176176"
    it "after 100 phases, returns 73745418 for input 19617804207202209144916044189917" $ do
      let input = parse "19617804207202209144916044189917"
      Vector.take 8 (dup 100 phase input) `shouldBe` parse "73745418"
    it "after 100 phases, returns 52432133 for input 69317163492948606335995924319873" $ do
      let input = parse "69317163492948606335995924319873"
      Vector.take 8 (dup 100 phase input) `shouldBe` parse "52432133"

  describe "fastphase" $ do
    prop "returns the same as phase for the second half of the signal" $ \ input -> do
      let signal = Vector.fromList $ map (mod10 . getNonNegative) input
          n = 1 + Vector.length signal `div` 2
      Vector.drop n (fastphase signal) `shouldBe` Vector.drop n (phase signal)

  describe "message" $ do
    it "given 03036732577212944063491565474664 returns 84462026" $
      message (parse "03036732577212944063491565474664") `shouldBe` parse "84462026"
    it "given 02935109699940807407585447034323 returns 78725270" $
      message (parse "02935109699940807407585447034323") `shouldBe` parse "78725270"
    it "given 03081770884921959731165446850517 returns 53553731" $
      message (parse "03081770884921959731165446850517") `shouldBe` parse "53553731"

  describe "hints" $ do
    prop "for output index n, the first n-1 coefficients are zero" $ \n ->
      map (`pattern` n) [0..n-1] `shouldSatisfy` all (== 0)
    prop "for output index n, the coefficients from n to 2n are 1" $ \n ->
      map (`pattern` n) [n..2*n] `shouldSatisfy` all (== 1)
    specify "the offset is in the second half of the signal" $
      offset day16Signal `shouldSatisfy` (> (10000 * Vector.length day16Signal) `div` 2)
    describe "there appears to be a recursive definition of phase" $ do
      prop "the last number in the signal is not changed by phase" $ \ input -> do
        let signal = Vector.fromList $ map (mod10 . getNonNegative) (getNonEmpty input)
        Vector.last (phase signal) `shouldBe` Vector.last signal
      specify "phase [1,1,1,1,1,1,1,1,1] ends with [5,4,3,2,1]" $
        phaseList [1,1,1,1,1,1,1,1,1] `shouldSatisfy` ([5,4,3,2,1] `isSuffixOf`)
      prop "phase(signal)[n] = signal[n] + phase(signal)[n+1]" $ \ input -> do
        let signal = Vector.fromList $ map (mod10 . getNonNegative) (getNonEmpty input)
            n = 1 + (m `div` 2)
            m = Vector.length signal - 1
            phased = phase signal
        for_ [n..m-1] $ \index ->
          phased ! index `shouldBe` mod10 ((signal ! index) + (phased ! (index+1)))

phaseList :: [Int] -> [Int]
phaseList = Vector.toList . phase . Vector.fromList

