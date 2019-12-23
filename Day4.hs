module Day4 where

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
pairs [] = []
pairs [_] = []
pairs (x : y : ys) = (x, y) : pairs (y : ys)

hasConsecutiveDigits :: Int -> Bool
hasConsecutiveDigits = any (uncurry (==)) . pairs . digits

hasAscendingDigits :: Int -> Bool
hasAscendingDigits = all (uncurry (<=)) . pairs . digits

isCandidate :: Int -> Bool
isCandidate value = hasConsecutiveDigits value && hasAscendingDigits value

part1 :: Int
part1 = (length . filter isCandidate) numbers


main :: IO ()
main = do
  print part1

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
