module Day17 (part1, part2, selfTest) where

import           Control.Monad (guard)
import           Data.Char (chr, ord)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.List (inits, intersperse, intercalate, isPrefixOf, sort, stripPrefix)
import           Data.Maybe (fromMaybe)
import           Data.String (IsString, fromString)
import           Data.Vector.Unboxed (Vector, (//))
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec (describe, hspec, it, shouldBe, shouldSatisfy)

import           Intcode3 (Result (..))
import qualified Intcode3 as Intcode

program :: Vector Int
program = Intcode.parse $ unsafePerformIO $ readFile "day17input.txt"

programOutput :: ByteString
programOutput = ByteString.pack $ go (Intcode.runIncremental program [])
  where
    go result =
      case result of
        Yield c next -> chr c : go next
        Await _ -> ""
        Done _ -> ""

data Image = Image
  { raw    :: ByteString
  , width  :: Int
  , height :: Int
  , pitch  :: Int
  , sample :: ((Int, Int) -> Char)
  }

instance IsString Image where
  fromString = view . fromString

-- | Make an Image data structure from the raw robot output, for easier sampling.
view :: ByteString -> Image
view raw = Image {..}
  where
    rows = filter (not . ByteString.null) . ByteString.lines $ raw
    width = fromMaybe 0 $ ByteString.elemIndex '\n' raw
    height = length rows
    pitch = width + 1
    sample (col, row)
      | col >= 0, col < width, row >= 0, row < height = ByteString.index raw (row * pitch + col)
      | otherwise = ' '

-- | Return the location of every scaffolding intersection in the image.
intersections :: Image -> [(Int, Int)]
intersections Image {..} =
  [point | col <- [1 .. width-2], row <- [1 .. height-2], let point = (col, row), isIntersection point]
  where
    cross (col, row) = [(col, row), (col - 1, row), (col + 1, row), (col, row - 1), (col, row + 1)]
    isIntersection = all (== '#') . map sample . cross

alignmentParameter :: (Int, Int) -> Int
alignmentParameter (col, row) = col * row

calibrate :: Image -> Int
calibrate = sum . map alignmentParameter . intersections

part1 :: Int
part1 = calibrate (view programOutput)

-- | Find a character in the image. The coordinates of its location are
-- returned, if it is found.
locate :: Char -> Image -> Maybe (Int, Int)
locate search Image {..} = toPoint <$> ByteString.elemIndex search raw
  where
    toPoint index = (index `mod` pitch, index `div` pitch)

data Direction = E | N | W | S

data Command = R | L | F Int deriving (Eq, Show)

-- | Calculate the robot's track along the scaffolding.
track :: Image -> [Command]
track image@Image {..} = go 0 initialPosition initialDirection
  where
    (initialPosition, initialDirection) = whereami
    whereami
      | Just p <- locate '>' image = (p, E)
      | Just p <- locate '^' image = (p, N)
      | Just p <- locate '<' image = (p, W)
      | Just p <- locate 'v' image = (p, S)
      | otherwise = error "we lost our vacuum robot 😭"
    go !steps p d | p' <- step d p =
      case sample p' of -- what is ahead?
        '#' -> go (steps + 1) p' d -- continue forwards on the scaffolding
        _ | Just (cmd, d') <- chooseDirection p d ->
              showMove steps (cmd : go 0 p d') -- turn and continue
        _ | otherwise ->
              showMove steps [] -- dead end
      where
        showMove 0 = id
        showMove distance = (F distance :)
    chooseDirection p d =
      -- sample to the left and right to determine which way to turn
      case d of
        E | goNorth -> Just (L, N)
        E | goSouth -> Just (R, S)
        N | goWest  -> Just (L, W)
        N | goEast  -> Just (R, E)
        W | goSouth -> Just (L, S)
        W | goNorth -> Just (R, N)
        S | goEast  -> Just (L, E)
        S | goWest  -> Just (R, W)
        _ -> Nothing -- dead end (no backtracking)
      where
        goEast  = sample (step E p) == '#'
        goNorth = sample (step N p) == '#'
        goWest  = sample (step W p) == '#'
        goSouth = sample (step S p) == '#'

-- | Take a step in a direction.
step :: Direction -> (Int, Int) -> (Int, Int)
step E (col, row) = (col + 1, row)
step N (col, row) = (col, row - 1)
step W (col, row) = (col - 1, row)
step S (col, row) = (col, row + 1)

-- | Return all the ways to cover the string with a sequence of substrings
-- taken from the dictionary of substrings.
--
-- Returns a list of indices into the dictionary.
cover :: Eq a => [[a]] -> [a] -> [[Int]]
cover _ [] = [[]]
cover substrings xs = do
  -- Select a substring that is a prefix of the input.
  (index, substring) <- filter (isMatch xs) (zip [0..] substrings)
  -- Select the rest.
  more <- cover substrings (drop (length substring) xs)
  -- Combine.
  return (index : more)
  where
    isMatch ys (_, s) = s `isPrefixOf` ys

-- | List all the configurations of 3 covering substrings (that are valid
-- according to a validation procedure).
--
-- The validation procedure is checked as early as possible to short circuit
-- dead branches, mitigating the severity of the O(n^3) search used here.
coveringSubstrings :: Eq a => ([a] -> Bool) -> [a] -> [([Int], [a], [a], [a])]
coveringSubstrings _ [] = []
coveringSubstrings isValid xs = do
  -- One substring must be a non-empty prefix.
  a <- inits xs
  guard $ (not . null) a
  guard $ isValid a
  let ys = drop (length a) xs

  -- Another substring must be a non-empty prefix of what comes after the first
  -- prefix.
  b <- inits ys
  guard $ (not . null) b
  guard $ isValid b
  let zs = drop (length b) ys

  -- The third substring is a non-empty prefix of whatever is left.
  let zs' = strip [a, b] zs
  c <- inits zs'
  guard $ (not . null) c
  guard $ isValid c

  -- Cover the input string with the chosen test substrings.
  let dictionary = [a, b, c]
  indices <- cover dictionary xs
  return (indices, a, b, c)

-- | Repeatedly strip prefixes from a string.
strip :: Eq a => [[a]] -> [a] -> [a]
strip prefixes = go prefixes
  where
    go [] l = l
    go (p:pp) l
      | Just l' <- stripPrefix p l = go prefixes l'
      | otherwise = go pp l

-- | A routine is valid if it is non-empty and its ASCII representation is not
-- longer than the maximum allowable routine length.
--
-- Use this to call 'coveringSubstrings'.
routineIsValid :: Int -> [Command] -> Bool
routineIsValid _         [] = False
routineIsValid maxLength l  = length (showRoutine l) <= maxLength

showCommand :: Command -> String
showCommand L = "L"
showCommand R = "R"
showCommand (F n) = show n

showRoutine :: [Command] -> String
showRoutine = intercalate "," . map showCommand

showIndices :: [Int] -> String
showIndices = intersperse ',' . map chr . map (+ ord 'A')

encode :: String -> [Int]
encode = map ord

part2 :: Int
part2 = last outputs
  where
    image = view programOutput
    commands = track image
    routines = coveringSubstrings (routineIsValid 20) commands
    ((indices, a, b, c) : _) = routines
    inputs = encode $ unlines
      [
        showIndices indices,
        showRoutine a,
        showRoutine b,
        showRoutine c,
        "n"
      ]
    program' = program // [(0, 2)]
    (_, outputs) = Intcode.run program' inputs



selfTest :: IO ()
selfTest = hspec $ do
  describe "intersections" $ do
    it "returns an empty list for images less than 3 units a side" $ do
      intersections "" `shouldBe` []
      intersections "#" `shouldBe` []
      intersections "##\n##" `shouldBe` []
    it "does not return T intersections" $
      intersections "###\n # \n###" `shouldBe` []
    it "returns a + intersection" $
      intersections " # \n###\n # \n" `shouldBe` [(1,1)]
    it "returns the intersections in the sample image" $
      intersections sampleImage `shouldBe`
        [(2,2), (2,4), (6,4), (10,4)]

  describe "calibrate" $
    it "returns 76 given the sample image" $
      calibrate sampleImage `shouldBe` 76

  describe "part1" $
    it "returns 3608" $
      part1 `shouldBe` 3608

  describe "locate" $
    it "returns (10, 6) given the sample image" $
      locate '^' sampleImage `shouldBe` Just (10, 6)

  describe "track" $ do
    it "returns [] for a 1x1 scaffolding" $ do
      track ">\n" `shouldBe` []
      track "^\n" `shouldBe` []
      track "<\n" `shouldBe` []
      track "v\n" `shouldBe` []
    it "returns [1] for a 1x2 scaffolding ahead" $
      track "#\n^\n" `shouldBe` [F 1]
    it "returns [3] for a 4x1 scaffolding ahead" $
      track ">###\n" `shouldBe` [F 3]
    it "returns [R,1] for a 2x1 scaffolding to the right" $
      track "^#\n" `shouldBe` [R,F 1]
    it "returns [L,1] for a 2x1 scaffolding to the left" $
      track "<\n#\n" `shouldBe` [L,F 1]
    it "returns [1,R,1] for a 2x2 scaffolding in a L shape" $
      track ">#\n #\n" `shouldBe` [F 1,R,F 1]
    it "returns [2,L,2,L,2] for a 3x3 scaffolding in a U shape" $
      track "v #\n# #\n###\n" `shouldBe` [F 2,L,F 2,L,F 2]
    it "returns the expected track for the sample image" $
      displayTrack (track sampleImage2) `shouldBe` "R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2"
    it "returns a track for the puzzle input" $
      track (view programOutput) `shouldSatisfy` all (const True)

  describe "cover" $ do
    it "returns [] when the dictionary is empty" $
      cover [] "a" `shouldBe` []
    it "returns [] when the input contains a symbol not covered" $
      cover ["a", "b"] "abc" `shouldBe` []
    it "returns a single program when the covering is unique" $
      cover ["a", "b"] "aabbab" `shouldBe` [[0,0,1,1,0,1]]
    it "returns all programs when two coverings are possible" $
      cover ["a", "aa"] "aa" `shouldBe` [[0,0], [1]]
    it "returns the example covering" $ do
      let input = "R8R8R4R4R8L6L2R4R4R8R8R8L6L2"
          dictionary = ["R8R8","R4R4R8","L6L2"]
      cover dictionary input `shouldSatisfy` (elem [0,1,2,1,0,2])

  describe "coveringSubstrings" $ do
    it "returns [] when the input is empty" $
      coveringSubstrings (const True) "" `shouldBe` []
    it "returns a single program when the covering is unique" $
      coveringSubstrings ((== 1) . length) "abc" `shouldBe` [([0,1,2], "a", "b", "c")]
    it "returns all programs when three coverings are possible" $
      sort (coveringSubstrings ((<= 2). length) "abcd") `shouldBe` sort
        [ ([0,1,2], "ab", "c", "d")
        , ([0,1,2], "a", "bc", "d")
        , ([0,1,2], "a", "b", "cd")
        ]
    it "returns the expected routines from the example route" $ do
      let input = "R8R8R4R4R8L6L2R4R4R8R8R8L6L2"
          actual = coveringSubstrings ((<= 6) . length) input
          isExpected (indices, a, b, c) =
            indices == [0,1,2,1,0,2]
            && a == "R8R8"
            && b == "R4R4R8"
            && c == "L6L2"
      actual `shouldSatisfy` any isExpected

displayTrack :: [Command] -> String
displayTrack = intercalate "," . map displayCommand
  where
    displayCommand L = "L"
    displayCommand R = "R"
    displayCommand (F n) = show n

sampleImage :: Image
sampleImage = "..#..........\n..#..........\n#######...###\n#.#...#...#.#\n#############\n..#...#...#..\n..#####...^..\n"

sampleImage2 :: Image
sampleImage2 = "#######...#####\n#.....#...#...#\n#.....#...#...#\n......#...#...#\n......#...###.#\n......#.....#.#\n^########...#.#\n......#.#...#.#\n......#########\n........#...#..\n....#########..\n....#...#......\n....#...#......\n....#...#......\n....#####......\n"
