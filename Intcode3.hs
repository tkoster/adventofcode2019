module Intcode3
  ( Memory
  , parse
  , run
  , exec
  , eval
  , Result (..)
  , runIncremental
  , step
  , spec
  )
where

import Data.Foldable (for_)
import Data.Vector.Unboxed (Vector, (!), (//), fromList, toList)
import qualified Data.Vector.Unboxed as Vector
import Test.Hspec

import Csv

type Memory = Vector Int

-- Source code parsing

parse :: String -> Vector Int
parse = fromList . map read . csv

-- Instruction parsing

data Mode = Indirect | Immediate | Relative
  deriving (Eq, Show)

data OpCode
  = Add
  | Mul
  | Input
  | Output
  | JumpIfTrue
  | JumpIfFalse
  | Less
  | Equal
  | RelativeBase
  | Stop
  deriving (Eq, Show)

parseOp :: Int -> Int -> OpCode
parseOp position code =
  case code `mod` 100 of
    1 -> Add
    2 -> Mul
    3 -> Input
    4 -> Output
    5 -> JumpIfTrue
    6 -> JumpIfFalse
    7 -> Less
    8 -> Equal
    9 -> RelativeBase
    99 -> Stop
    opcode -> error $ "Invalid opcode " <> show opcode <> " at position " <> show position <> ": " <> show code

parseMode :: Int -> Int -> Int -> Mode
parseMode position argumentIndex code =
  case digit (2 + argumentIndex) code of
    0 -> Indirect
    1 -> Immediate
    2 -> Relative
    mode -> error $ "Invalid mode " <> show mode <> " at position " <> show position <> ": " <> show code

digit :: Int -> Int -> Int
digit i value = value `div` (10 ^ i) `mod` 10

-- Evaluation

run :: Vector Int -> [Int] -> (Vector Int, [Int])
run program inputs = go (step program 0 0) inputs []
  where
    go result ins outs =
      case result of
        Done memory -> (memory, reverse outs)
        Yield output continue -> go continue ins (output : outs)
        Await continue
          | (input : ins') <- ins -> go (continue input) ins' outs
          | otherwise -> error "no more inputs"

exec :: Vector Int -> [Int] -> Vector Int
exec = (fst .) . run

eval :: Vector Int -> [Int] -> [Int]
eval = (snd .) . run

-- Incremental evaluation

data Result
  = Done (Vector Int)
  | Yield Int Result
  | Await (Int -> Result)

runIncremental :: Vector Int -> [Int] -> Result
runIncremental program inputs = go (step program 0 0) inputs
  where
    go result ins =
      case result of
        Await continue
          | (input : ins') <- ins -> go (continue input) ins'
        _ -> result

step :: Vector Int -> Int -> Int -> Result
step memory pc rb = dispatch
  where
    op = parseOp pc (memory ! pc)
    mode i = parseMode pc i (memory ! pc)

    dispatch =
      case op of
        Add               -> add
        Mul               -> mul
        Input             -> input
        Output            -> output
        JumpIfTrue        -> jumpIfTrue
        JumpIfFalse       -> jumpIfFalse
        Less              -> less
        Equal             -> equal
        RelativeBase      -> relativeBase
        Stop              -> stop

    load i =
      case mode i of
        Indirect  | addr  <- memory ! (pc + i + 1) -> load' addr
        Immediate | value <- memory ! (pc + i + 1) -> value
        Relative  | addr  <- memory ! (pc + i + 1) -> load' (rb + addr)

    store i =
      case mode i of
        Indirect  | addr  <- memory ! (pc + i + 1) -> store' addr
        Immediate -> error "invalid store immediate"
        Relative  | addr  <- memory ! (pc + i + 1) -> store' (rb + addr)

    load' addr
      | addr < Vector.length memory = memory ! addr
      | otherwise = 0

    store' addr value
      | addr < Vector.length memory = memory // [(addr, value)]
      | otherwise = (memory <> Vector.replicate (1 + addr - Vector.length memory) 0) // [(addr, value)]

    binop f =
      let a       = load  0
          b       = load  1
          memory' = store 2 (f a b)
      in step memory' (pc + 4) rb

    add = binop (+)

    mul = binop (*)

    input =
      let continuation value =
            let memory' = store 0 value
            in  step memory' (pc + 2) rb
      in  Await continuation

    output =
      let value = load 0
          continuation = step memory (pc + 2) rb
      in  Yield value continuation

    cmpop f = binop (\ a b -> if f a b then 1 else 0)

    less = cmpop (<)

    equal = cmpop (==)

    jumpop f =
      if f (load 0)
        then
          step memory (load 1) rb
        else
          step memory (pc + 3) rb

    jumpIfTrue = jumpop (/= 0)

    jumpIfFalse = jumpop (== 0)

    relativeBase = step memory (pc + 2) (rb + load 0)

    stop = Done memory

spec :: Spec
spec = describe "Intcode" $ do

  describe "Examples" $ do

    it "1002,4,3,4,33 computes 1002,4,3,4,99" $ do
      let mem = parse "1002,4,3,4,33"
      toList (exec mem []) `shouldBe` [1002, 4, 3, 4, 99]

    it "1101,100,-1,4,0 computes 1101,100,-1,4,99" $ do
      let mem = parse "1101,100,-1,4,0"
      toList (exec mem []) `shouldBe` [1101, 100, -1, 4, 99]

    describe "part 2 'a larger example'" $ do

      let mem = parse "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

      it "given 7 outputs 999" $
        eval mem [7] `shouldBe` [999]

      it "given 8 outputs 1000" $
        eval mem [8] `shouldBe` [1000]

      it "given 9 outputs 1001" $
        eval mem [9] `shouldBe` [1001]

  describe "Parsing" $ do

    describe "parseOp" $

      it "parses 1002" $
        parseOp 0 1002 `shouldBe` Mul

    describe "parseMode" $ do

      it "returns Indirect for the first argument of 21002" $
        parseMode 0 0 21002 `shouldBe` Indirect

      it "returns Immediate for the second argument of 21002" $
        parseMode 0 1 21002 `shouldBe` Immediate

      it "returns Relative for the third argument of 21002" $
        parseMode 0 2 21002 `shouldBe` Relative

  describe "Arithmetic" $ do

    describe "add" $

      it "1,5,6,7,99,30,40,0 computes 30 + 40" $ do
        let mem = parse "1,5,6,7,99,30,40,0"
        (exec mem []) ! 7 `shouldBe` 70

    describe "mul" $
      it "2,5,6,7,99,30,40,0 computes 30 * 40" $ do
        let mem = parse "2,5,6,7,99,30,40,0"
        (exec mem []) ! 7 `shouldBe` 1200

  describe "Compare" $ do

    describe "3,9,8,9,10,9,4,9,99,-1,8 (equal to 8, indirect)" $ do

      let mem = parse "3,9,8,9,10,9,4,9,99,-1,8"

      for_ [6,7] $ \ n ->
        it ("given " <> show n <> " outputs 0") $
          eval mem [n] `shouldBe` [0]

      it "given 8 outputs 1" $
        eval mem [8] `shouldBe` [1]

    describe "3,9,7,9,10,9,4,9,99,-1,8 (less than 8, indirect)" $ do

      let mem = parse "3,9,7,9,10,9,4,9,99,-1,8"

      for_ [6,7] $ \ n ->
        it ("given " <> show n <> " outputs 1") $
          eval mem [n] `shouldBe` [1]

      it "given 8 outputs 0" $
        eval mem [8] `shouldBe` [0]

    describe "3,3,1108,-1,8,3,4,3,99 (equal to 8, immediate)" $ do

      let mem = parse "3,3,1108,-1,8,3,4,3,99"

      for_ [6,7] $ \ n ->
        it ("given " <> show n <> " outputs 0") $
          eval mem [n] `shouldBe` [0]

      it "given 8 outputs 1" $
        eval mem [8] `shouldBe` [1]

    describe "3,3,1107,-1,8,3,4,3,99 (less than 8, immediate)" $ do

      let mem = parse "3,3,1107,-1,8,3,4,3,99"

      for_ [6,7] $ \ n ->
        it ("given " <> show n <> " outputs 1") $
          eval mem [n] `shouldBe` [1]

      it "given 8 outputs 0" $
        eval mem [8] `shouldBe` [0]

  describe "Jump" $ do

    describe "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 (is nonzero, indirect)" $ do

      let mem = parse "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"

      it "given 0 outputs 0" $
        eval mem [0] `shouldBe` [0]

      it "given 1 outputs 1" $
        eval mem [1] `shouldBe` [1]

    describe "3,3,1105,-1,9,1101,0,0,12,4,12,99,1 (is nonzero, immediate)" $ do

      let mem = parse "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"

      it "given 0 outputs 0" $
        eval mem [0] `shouldBe` [0]

      it "given 1 outputs 1" $
        eval mem [1] `shouldBe` [1]

  describe "I/O" $ do

    it "3,0,4,0,99 echoes input" $ do
      let mem = parse "3,0,4,0,99"
      eval mem [12345] `shouldBe` [12345]

    it "3,0,4,0,3,0,4,0,99 echoes input twice" $ do
      let mem = parse "3,0,4,0,3,0,4,0,99"
      eval mem [12345, 54321] `shouldBe` [12345, 54321]

  describe "Relative base" $ do

    it "output relative mode args works" $ do
      let mem = parse "4,11,109,1,204,11,109,1,204,11,99,1,2,3"
      eval mem [] `shouldBe` [1, 2, 3]

  describe "Large memory" $

    it "quine program works" $ do
      let mem = parse "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
      eval mem [] `shouldBe` [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

  describe "Large integers" $ do

    it "adds two large numbers" $ do
      let mem = parse "1102,34915192,34915192,7,4,7,99,0"
      eval mem [] `shouldBe` [1219070632396864]

    it "outputs a large number" $ do
      let mem = parse "104,1125899906842624,99"
      eval mem [] `shouldBe` [1125899906842624]

  it "passes the day-9 BOOST program test mode" $ do
    mem <- parse <$> readFile "day9input.txt"
    eval mem [1] `shouldBe` [2204990589]
