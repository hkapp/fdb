{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import TPCH.Functional.Q1 (q1)
import FDB.Dot (toDotGraph)
import qualified Utils.Dot as Dot

import Foreign (Storable)
import FDB.RustFFI(Q, CUInt32, readT, execQ)
import Data.Foldable(traverse_)

main :: IO ()
main = execQTest

-- execQ test

type QVal = CUInt32;

execQTest = ((readT "foo") :: IO (Q QVal)) >>= execAndPrint

-- execAndPrint :: Q QVal -> IO ()
-- execAndPrint query =
  -- do
    -- resList <- execQ query
    -- traverse_  print resList

-- This signature does not work: why?
execAndPrint :: (Show a, Storable a) => Q a -> IO ()
execAndPrint query =
  do
    resList <- execQ query
    traverse_  print resList

-- Query Test

queryTest = writeFile fileName text

fileName = "out.dot"

text = Dot.prettyPrint dotGraph

dotGraph = toDotGraph query

query = q1 13
-- query = mapQ id $ readT (Table "Employees")
