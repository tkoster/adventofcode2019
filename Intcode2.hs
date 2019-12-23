{-# LANGUAGE RankNTypes #-}

module Intcode2 where

import Prelude hiding (read)
import qualified Prelude
import Control.Monad.ST
import Data.Vector.Unboxed (Vector, fromList, freeze, thaw)
import Data.Vector.Unboxed.Mutable (STVector, read, write)

import Csv

parse :: String -> Vector Int
parse = fromList . map Prelude.read . csv

exec :: Vector Int -> [Int] -> (Vector Int, [Int])
exec program inputs = runST $ do
  v <- thaw program
  outputs <- execST v inputs
  result <- freeze v
  return (result, outputs)

newtype Addr = A Int

newtype Imm = I Int

data Mode = Indirect | Immediate
  deriving (Eq, Show)

data OpCode
  = Add Mode Mode
  | Mul Mode Mode
  | Input
  | Output Mode
  | Stop
  deriving (Eq, Show)

parseOp :: Int -> Int -> OpCode
parseOp position code =
  case code `mod` 100 of
    1 -> Add (parseMode 0) (parseMode 1)
    2 -> Mul (parseMode 0) (parseMode 1)
    3 -> Input
    4 -> Output (parseMode 0)
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

execST :: STVector s Int -> [Int] -> ST s [Int]
execST program inputs = go 0 inputs []
  where
    go pc ins outs = do
      op <- read program pc
      case parseOp pc op of
        Add m0 m1 -> add m0 m1
        Mul m0 m1 -> mul m0 m1
        Input     -> input
        Output m  -> output m
        Stop      -> stop
      where
        arg i mode = do
          value <- read program (pc + i + 1)
          case mode of
            Indirect -> read program value
            Immediate -> return value

        binop f mode0 mode1 = do
          a   <- arg 0 mode0
          b   <- arg 1 mode1
          dst <- arg 2 Immediate
          write program dst (f a b)
          go (pc + 4) ins outs

        add = binop (+)

        mul = binop (*)

        input
          | (value : ins') <- ins = do
              dst <- read program (pc + 1)
              write program dst value
              go (pc + 2) ins' outs
          | otherwise = error "EOF input"

        output mode = do
          value <- arg 0 mode
          go (pc + 2) ins (value : outs)

        stop = return (reverse outs)

