module Day4 where

import Data.List
import Test.Hspec

numbers :: [Int]
numbers = [372037 .. 905157]

digits :: Int -> [Int]
digits value = reverse (digits' value)
  where
    digits' n
      | n < 10    = [n]
      | otherwise = n `mod` 10 : digits' (n `div` 10)

pairs :: [a] -> [(a, a)]
pairs l = zip l (drop 1 l)

hasConsecutiveDigits :: Int -> Bool
hasConsecutiveDigits = any (uncurry (==)) . pairs . digits

hasAscendingDigits :: Int -> Bool
hasAscendingDigits = all (uncurry (<=)) . pairs . digits

isCandidate :: Int -> Bool
isCandidate value = hasConsecutiveDigits value && hasAscendingDigits value

part1 :: Int
part1 = (length . filter isCandidate) numbers

hasPair :: Int -> Bool
hasPair = any (== 2) . map length . group . digits

isCandidate2 :: Int -> Bool
isCandidate2 value = hasPair value && hasAscendingDigits value

part2 :: Int
part2 = (length . filter isCandidate2) numbers

main :: IO ()
main = do
  print part1
  print part2

--------

selfTest :: IO ()
selfTest = hspec $ do

  describe "digits" $ do
    it "works for 0" $
      digits 0 `shouldBe` [0]
    it "works for 1" $
      digits 1 `shouldBe` [1]
    it "works for 2" $
      digits 2 `shouldBe` [2]
    it "works for 10" $
      digits 10 `shouldBe` [1, 0]
    it "works for 11" $
      digits 11 `shouldBe` [1, 1]
    it "works for 12" $
      digits 12 `shouldBe` [1, 2]
    it "works for 100" $
      digits 100 `shouldBe` [1, 0, 0]
    it "works for 101" $
      digits 101 `shouldBe` [1, 0, 1]
    it "works for 123" $
      digits 123 `shouldBe` [1, 2, 3]

  describe "pairs" $ do
    it "works for ''" $
      pairs "" `shouldBe` []
    it "works for 'a'" $
      pairs "a" `shouldBe` []
    it "works for 'ab'" $
      pairs "ab" `shouldBe` [('a', 'b')]
    it "works for 'abc'" $
      pairs "abc" `shouldBe` [('a', 'b'), ('b', 'c')]

  describe "hasConsecutiveDigits" $ do
    it "is False for 1" $
      hasConsecutiveDigits 1 `shouldBe` False
    it "is False for 123456" $
      hasConsecutiveDigits 123456 `shouldBe` False
    it "is True for 122345" $
      hasConsecutiveDigits 122345 `shouldBe` True
    it "is False for 123123" $
      hasConsecutiveDigits 123123 `shouldBe` False

  describe "hasAscendingDigits" $ do
    it "is True for 1" $
      hasAscendingDigits 1 `shouldBe` True
    it "is True for 123456" $
      hasAscendingDigits 123456 `shouldBe` True
    it "is True for 122345" $
      hasAscendingDigits 122345 `shouldBe` True
    it "is False for 123123" $
      hasAscendingDigits 123123 `shouldBe` False

  describe "isCandidate" $ do
    it "is True for 111111" $
      isCandidate 111111 `shouldBe` True
    it "is False for 223450" $
      isCandidate 223450 `shouldBe` False
    it "is False for 123789" $
      isCandidate 123789 `shouldBe` False

  describe "hasPair" $ do
    it "is False for 111111" $
      hasPair 111111 `shouldBe` False
    it "is True for 112233" $
      hasPair 112233 `shouldBe` True
    it "is False for 123444" $
      hasPair 123444 `shouldBe` False
    it "is True for 111122" $
      hasPair 111122 `shouldBe` True
