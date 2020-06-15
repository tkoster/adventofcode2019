{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Day14 (part1, part2, selfTest) where

import           Control.Exception (evaluate)
import           Control.Monad (void)
import           Data.Attoparsec.Text
import           Data.Char (isAlpha)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (listToMaybe)
import           Data.List (nub)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec

---- Parsing recipes

data Recipe = Recipe { consumes :: HashMap Text Int, produces :: (Text, Int) }
  deriving (Eq, Show)

fromList :: [Recipe] -> HashMap Text Recipe
fromList = HashMap.fromList . map recipeKvp
  where recipeKvp r = (fst $ produces r, r)

recipeConsumes :: Recipe -> Text -> Bool
recipeConsumes (Recipe cs _) item = item `HashMap.member` cs

parseRecipe :: Text -> Recipe
parseRecipe = either error id . parseOnly recipe
  where
    recipe :: Parser Recipe
    recipe = do
      skipSpace
      inputParts <- part `sepBy1` comma
      arrow
      outputPart <- part
      end
      return $ Recipe (HashMap.fromList inputParts) outputPart

    part :: Parser (Text, Int)
    part = do
      skipSpace
      amount <- decimal
      skipSpace
      item <- takeWhile1 isAlpha
      return (item, amount)

    comma :: Parser ()
    comma = skipSpace *> void ","

    arrow :: Parser ()
    arrow = skipSpace *> void "=>"

    end :: Parser ()
    end = skipSpace *> endOfInput


---- Solving

part1 :: Int
part1 = solve (fromList day14Recipes) 1 "FUEL"

part2 :: Int
part2 = rsolve (fromList day14Recipes) "FUEL" oneTrillion

oneTrillion :: Int
oneTrillion = 1000000000000

-- Calculate how much ore is required to produce a given amount of an item.
--
--   Initialise a collection of ingredients needed with one entry, e.g. 1 FUEL.
--   Loop:
--     Choose an ingredient from the bag that does not appear on the LHS of any recipe.
--     Look up its recipe and add (the required multiple) of its ingredients to the collection.
--     Remove the recipe from the recipe book.
--     Stop once the collection shows how much ORE is needed.
solve :: HashMap Text Recipe -> Int -> Text -> Int
solve recipes0 qty0 item0 = go recipes0 [(item0, qty0)]
  where
    go :: HashMap Text Recipe -> HashMap Text Int -> Int
    go recipes requests
      | [("ORE", qty)] <- HashMap.toList requests = qty
      | Just (item, qty) <- chooseNextIngredient,
        Just (Recipe consumes (_, producesQty)) <- HashMap.lookup item recipes =
          let recipeQty = qty `ceildiv` producesQty
              request   = HashMap.map (* recipeQty) consumes
              requests' = HashMap.delete item . HashMap.unionWith (+) request $ requests
              recipes'  = HashMap.delete item recipes
          in  go recipes' requests'
      | otherwise = error $ "No makeable ingredients in " ++ show requests
      where
        chooseNextIngredient = listToMaybe $ HashMap.toList $ HashMap.filterWithKey isReadyItem requests
        isReadyItem item _ = not . any (`recipeConsumes` item) $ recipes

-- Calculate how much of an item can be produced with a given amount of ore.
--
-- Find the least n such that producing n FUEL requires more than 10e12 ore.
-- (n - 1) is how much FUEL we can make with 10e12 ore.
rsolve :: HashMap Text Recipe -> Text -> Int -> Int
rsolve recipes item availableOre = go 1 availableOre
  where
    go lo hi
      | lo < hi, mid <- (lo + hi) `div` 2 =
          case solve recipes mid item of
            ore
              | ore < availableOre -> go (mid + 1) hi
              | otherwise -> go lo mid
      | otherwise =
          lo - 1

ceildiv :: Integral a => a -> a -> a
a `ceildiv` b = (a + b - 1) `div` b

---- Recipes

example1Recipes :: HashMap Text Recipe
example1Recipes = fromList $ map parseRecipe
  [
    "10 ORE => 10 A",
    "1 ORE => 1 B",
    "7 A, 1 B => 1 C",
    "7 A, 1 C => 1 D",
    "7 A, 1 D => 1 E",
    "7 A, 1 E => 1 FUEL"
  ]

example2Recipes :: HashMap Text Recipe
example2Recipes = fromList $ map parseRecipe
  [
    "9 ORE => 2 A",
    "8 ORE => 3 B",
    "7 ORE => 5 C",
    "3 A, 4 B => 1 AB",
    "5 B, 7 C => 1 BC",
    "4 C, 1 A => 1 CA",
    "2 AB, 3 BC, 4 CA => 1 FUEL"
  ]

example3Recipes :: HashMap Text Recipe
example3Recipes = fromList $ map parseRecipe
  [
    "157 ORE => 5 NZVS",
    "165 ORE => 6 DCFZ",
    "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL",
    "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ",
    "179 ORE => 7 PSHF",
    "177 ORE => 5 HKGWZ",
    "7 DCFZ, 7 PSHF => 2 XJWVT",
    "165 ORE => 2 GPVTF",
    "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
  ]

example4Recipes :: HashMap Text Recipe
example4Recipes = fromList $ map parseRecipe
  [
    "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG",
    "17 NVRVD, 3 JNWZP => 8 VPVL",
    "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL",
    "22 VJHF, 37 MNCFX => 5 FWMGM",
    "139 ORE => 4 NVRVD",
    "144 ORE => 7 JNWZP",
    "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC",
    "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV",
    "145 ORE => 6 MNCFX",
    "1 NVRVD => 8 CXFTF",
    "1 VJHF, 6 MNCFX => 4 RFSQX",
    "176 ORE => 6 VJHF"
  ]

example5Recipes :: HashMap Text Recipe
example5Recipes = fromList $ map parseRecipe
  [
    "171 ORE => 8 CNZTR",
    "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL",
    "114 ORE => 4 BHXH",
    "14 VRPVC => 6 BMBT",
    "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL",
    "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT",
    "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW",
    "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW",
    "5 BMBT => 4 WPTQ",
    "189 ORE => 9 KTJDG",
    "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP",
    "12 VRPVC, 27 CNZTR => 2 XDBXC",
    "15 KTJDG, 12 BHXH => 5 XCVML",
    "3 BHXH, 2 VRPVC => 7 MZWV",
    "121 ORE => 7 VRPVC",
    "7 XCVML => 6 RJRHP",
    "5 BHXH, 4 VRPVC => 5 LTCX"
  ]

day14Recipes :: [Recipe]
day14Recipes = unsafePerformIO $ do
  input <- Text.readFile "day14input.txt"
  return $ map parseRecipe (Text.lines input)

---- Tests

selfTest :: IO ()
selfTest = hspec $ do
  describe "parseRecipe" $ do
    it "accepts '1 A => 2 B'" $
      parseRecipe "1 A => 2 B" `shouldBe` Recipe [("A", 1)] ("B", 2)
    it "accepts '1 A, 2 B => 3 C'" $
      parseRecipe "1 A, 2 B => 3 C" `shouldBe` Recipe [("A", 1), ("B", 2)] ("C", 3)

  describe "day14Recipes" $ do
    it "has 62 recipes" $
      length day14Recipes `shouldBe` 62
    it "does not contain multiple recipes that produce the same item" $ do
      let produced = map (fst . produces) day14Recipes
      (length . nub) produced `shouldBe` length produced

  describe "solve" $ do
    it "throws when no recipe exists" $
      evaluate (solve [] 1 "FOO") `shouldThrow` anyErrorCall
    it "returns 31 ore for example 1" $
      solve example1Recipes 1 "FUEL" `shouldBe` 31
    it "returns 165 ore for example 2" $
      solve example2Recipes 1 "FUEL" `shouldBe` 165
    it "returns 13312 ore for example 3" $
      solve example3Recipes 1 "FUEL" `shouldBe` 13312
    it "returns 180697 ore for example 4" $
      solve example4Recipes 1 "FUEL" `shouldBe` 180697
    it "returns 2210736 ore for example 5" $
      solve example5Recipes 1 "FUEL" `shouldBe` 2210736
    it "returns less than 217885 ore for the puzzle input" $
      solve (fromList day14Recipes) 1 "FUEL" `shouldSatisfy` (< 217885)

  describe "rsolve" $ do
    it "returns 82892753 fuel for example 3" $
      rsolve example3Recipes "FUEL" oneTrillion `shouldBe` 82892753
    it "returns 5586022 fuel for example 4" $
      rsolve example4Recipes "FUEL" oneTrillion `shouldBe` 5586022
    it "returns 460664 fuel for example 5" $
      rsolve example5Recipes "FUEL" oneTrillion `shouldBe` 460664
