{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import TPCH.Functional.Q1 (q1)
import FDB.Dot (toDotGraph)
import qualified Utils.Dot as Dot

import Foreign (Storable)
import FDB.RustFFI(Q, CUInt32, readT, filterQ, execQ, initDB)
import Data.Foldable(traverse_)

main :: IO ()
main = execQTest

-- execQ test


execQTest = qry >>= execAndPrint

type QVal = CUInt32;

qry :: IO (Q QVal)
qry =
  do
    ctx <- initDB
    q1  <- readT ctx "foo"
    q2  <- filterQ topLevelFilter2 "Main.topLevelFilter2" q1
    return q2

topLevelFilter :: QVal -> Bool
topLevelFilter x = x <= 3

topLevelFilter2 x = x <= 3

topLevelFilter3 = (>) 3

execAndPrint :: (Show a, Storable a) => Q a -> IO ()
execAndPrint query =
  do
    resList <- execQ query
    traverse_  print resList

-- Old query Test

queryTest = writeFile fileName text

fileName = "out.dot"

text = Dot.prettyPrint dotGraph

dotGraph = toDotGraph query

query = q1 13
-- query = mapQ id $ readT (Table "Employees")
