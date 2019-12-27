module Intcode3 (parse, run, exec, eval, Result (..), runIncremental, spec) where

import Data.Foldable (for_)
import Data.Vector.Unboxed (Vector, (!), (//), fromList, toList)
import Test.Hspec

import Csv

-- Source code parsing

parse :: String -> Vector Int
parse = fromList . map read . csv

-- Instruction parsing

data Mode = Indirect | Immediate
  deriving (Eq, Show)

data OpCode
  = Add Mode Mode
  | Mul Mode Mode
  | Input
  | Output Mode
  | JumpIfTrue Mode Mode
  | JumpIfFalse Mode Mode
  | Less Mode Mode
  | Equal Mode Mode
  | Stop
  deriving (Eq, Show)

parseOp :: Int -> Int -> OpCode
parseOp position code =
  case code `mod` 100 of
    1 -> Add (parseMode 0) (parseMode 1)
    2 -> Mul (parseMode 0) (parseMode 1)
    3 -> Input
    4 -> Output (parseMode 0)
    5 -> JumpIfTrue (parseMode 0) (parseMode 1)
    6 -> JumpIfFalse (parseMode 0) (parseMode 1)
    7 -> Less (parseMode 0) (parseMode 1)
    8 -> Equal (parseMode 0) (parseMode 1)
    99 -> Stop
    opcode -> error $ "Invalid opcode " <> show opcode <> " at position " <> show position <> ": " <> show code
  where
    parseMode i =
      case digit (2 + i) code of
        0 -> Indirect
        1 -> Immediate
        mode -> error $ "Invalid mode " <> show mode <> " at position " <> show position <> ": " <> show code

digit :: Int -> Int -> Int
digit i value = value `div` (10 ^ i) `mod` 10

-- Evaluation

run :: Vector Int -> [Int] -> (Vector Int, [Int])
run program inputs = go (step program 0) inputs []
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

data Result
  = Done (Vector Int)
  | Yield Int Result
  | Await (Int -> Result)

runIncremental :: Vector Int -> [Int] -> Result
runIncremental program inputs = go (step program 0) inputs
  where
    go result ins =
      case result of
        Await continue
          | (input : ins') <- ins -> go (continue input) ins'
        _ -> result

step :: Vector Int -> Int -> Result
step memory pc = dispatch
  where
    op = parseOp pc (memory ! pc)

    dispatch =
      case op of
        Add m0 m1         -> add m0 m1
        Mul m0 m1         -> mul m0 m1
        Input             -> input
        Output m          -> output m
        JumpIfTrue m0 m1  -> jumpIfTrue m0 m1
        JumpIfFalse m0 m1 -> jumpIfFalse m0 m1
        Less m0 m1        -> less m0 m1
        Equal m0 m1       -> equal m0 m1
        Stop              -> stop

    arg i Indirect  | addr <- memory ! (pc + i + 1), value <- memory ! addr = value
    arg i Immediate | value <- memory ! (pc + i + 1) = value

    binop f mode0 mode1 =
      let a   = arg 0 mode0
          b   = arg 1 mode1
          dst = memory ! (pc + 3)
          memory' = memory // [(dst, (f a b))]
      in step memory' (pc + 4)

    add = binop (+)

    mul = binop (*)

    input =
      let dst = memory ! (pc + 1)
          continuation value =
            let memory' = memory // [(dst, value)]
            in  step memory' (pc + 2)
      in  Await continuation

    output mode =
      let value = arg 0 mode
          continuation = step memory (pc + 2)
      in  Yield value continuation

    cmpop f = binop (\ a b -> if f a b then 1 else 0)

    less = cmpop (<)

    equal = cmpop (==)

    jumpop f mode0 mode1 =
      if f (arg 0 mode0)
        then
          step memory (arg 1 mode1)
        else
          step memory (pc + 3)

    jumpIfTrue = jumpop (/= 0)

    jumpIfFalse = jumpop (== 0)

    stop = Done memory

spec :: Spec
spec = describe "Intcode" $ do

  describe "Examples" $ do

    it "1002,4,3,4,33 computes 1002,4,3,4,99" $ do
      let mem = parse "1002,4,3,4,33"
          (mem', []) = run mem []
      toList mem' `shouldBe` [1002, 4, 3, 4, 99]

    it "1101,100,-1,4,0 computes 1101,100,-1,4,99" $ do
      let mem = parse "1101,100,-1,4,0"
          (mem', []) = run mem []
      toList mem' `shouldBe` [1101, 100, -1, 4, 99]

    describe "part 2 'a larger example'" $ do

      let mem = parse "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

      it "given 7 outputs 999" $
        snd (run mem [7]) `shouldBe` [999]

      it "given 8 outputs 1000" $
        snd (run mem [8]) `shouldBe` [1000]

      it "given 9 outputs 1001" $
        snd (run mem [9]) `shouldBe` [1001]

  describe "Parsing" $

    describe "parseOp" $

      it "parses 1002" $
        parseOp 0 1002 `shouldBe` Mul Indirect Immediate

  describe "Arithmetic" $ do

    describe "add" $

      it "1,5,6,7,99,30,40,0 computes 30 + 40" $ do
        let mem = parse "1,5,6,7,99,30,40,0"
            (mem', []) = run mem []
        mem' ! 7 `shouldBe` 70

    describe "mul" $
      it "2,5,6,7,99,30,40,0 computes 30 * 40" $ do
        let mem = parse "2,5,6,7,99,30,40,0"
            (mem', []) = run mem []
        mem' ! 7 `shouldBe` 1200

  describe "Compare" $ do

    describe "3,9,8,9,10,9,4,9,99,-1,8 (equal to 8, indirect)" $ do

      let mem = parse "3,9,8,9,10,9,4,9,99,-1,8"

      for_ [6,7] $ \ n ->
        it ("given " <> show n <> " outputs 0") $ do
          let (_, outputs) = run mem [n]
          outputs `shouldBe` [0]

      it "given 8 outputs 1" $ do
        let (_, outputs) = run mem [8]
        outputs `shouldBe` [1]

    describe "3,9,7,9,10,9,4,9,99,-1,8 (less than 8, indirect)" $ do

      let mem = parse "3,9,7,9,10,9,4,9,99,-1,8"

      for_ [6,7] $ \ n ->
        it ("given " <> show n <> " outputs 1") $ do
          let (_, outputs) = run mem [n]
          outputs `shouldBe` [1]

      it "given 8 outputs 0" $ do
        let (_, outputs) = run mem [8]
        outputs `shouldBe` [0]

    describe "3,3,1108,-1,8,3,4,3,99 (equal to 8, immediate)" $ do

      let mem = parse "3,3,1108,-1,8,3,4,3,99"

      for_ [6,7] $ \ n ->
        it ("given " <> show n <> " outputs 0") $ do
          let (_, outputs) = run mem [n]
          outputs `shouldBe` [0]

      it "given 8 outputs 1" $ do
        let (_, outputs) = run mem [8]
        outputs `shouldBe` [1]

    describe "3,3,1107,-1,8,3,4,3,99 (less than 8, immediate)" $ do

      let mem = parse "3,3,1107,-1,8,3,4,3,99"

      for_ [6,7] $ \ n ->
        it ("given " <> show n <> " outputs 1") $ do
          let (_, outputs) = run mem [n]
          outputs `shouldBe` [1]

      it "given 8 outputs 0" $ do
        let (_, outputs) = run mem [8]
        outputs `shouldBe` [0]

  describe "Jump" $ do

    describe "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 (is nonzero, indirect)" $ do

      let mem = parse "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"

      it "given 0 outputs 0" $
        snd (run mem [0]) `shouldBe` [0]

      it "given 1 outputs 1" $
        snd (run mem [1]) `shouldBe` [1]

    describe "3,3,1105,-1,9,1101,0,0,12,4,12,99,1 (is nonzero, immediate)" $ do

      let mem = parse "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"

      it "given 0 outputs 0" $
        snd (run mem [0]) `shouldBe` [0]

      it "given 1 outputs 1" $
        snd (run mem [1]) `shouldBe` [1]

  describe "I/O" $ do

    it "3,0,4,0,99 echoes input" $ do
      let mem = parse "3,0,4,0,99"
          (_, output) = run mem [12345]
      output `shouldBe` [12345]

    it "3,0,4,0,3,0,4,0,99 echoes input twice" $ do
      let mem = parse "3,0,4,0,3,0,4,0,99"
          (_, output) = run mem [12345, 54321]
      output `shouldBe` [12345, 54321]