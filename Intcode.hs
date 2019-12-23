{-# LANGUAGE RankNTypes #-}

module Intcode where

import Prelude hiding (read, write)
import qualified Prelude
import Control.Monad.ST
import Data.Vector.Unboxed (Vector, fromList, freeze, thaw)
import Data.Vector.Unboxed.Mutable (STVector, read, write)

import Csv

parse :: String -> Vector Int
parse = fromList . map Prelude.read . csv

exec :: Vector Int -> Vector Int
exec program = runST $ do
  v <- thaw program
  execST v
  freeze v

execST program = go 0
  where
    go pc = do
      op <- read program pc
      case op of
        1  -> add
        2  -> mul
        99 -> stop
        _  -> error $ "Invalid opcode at position " <> show pc <> ": " <> show op
      where
        binop f = do
          pa  <- read program (pc + 1)
          a   <- read program pa
          pb  <- read program (pc + 2)
          b   <- read program pb
          dst <- read program (pc + 3)
          write program dst (f a b)
          go (pc + 4)
        add = binop (+)
        mul = binop (*)
        stop = return ()
