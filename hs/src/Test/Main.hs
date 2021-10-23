{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import TPCH.Functional.Q1 (q1)
import FDB.Dot (toDotGraph)
import qualified Utils.Dot as Dot

import Foreign (Storable(..))
import Foreign.Ptr (castPtr)
import FDB.RustFFI(Q, CUInt32, readT, filterQ, execQ, initDB)
import Data.Foldable(traverse_)

main :: IO ()
main = execQTest

-- execQ test


execQTest = qry >>= execAndPrint

type QVal = CUInt32;

qry = qrya

qrya :: IO (Q QVal)
qrya =
  do
    ctx <- initDB
    q1  <- readT ctx "foo"
    q2  <- filterQ topLevelFilter2 "Main.topLevelFilter2" q1
    return q2

topLevelFilter :: QVal -> Bool
topLevelFilter x = x <= 3

topLevelFilter2 x = x <= 3

topLevelFilter3 = (>) 3

data QValB = QValB CUInt32 CUInt32

instance Show QValB where
  show (QValB x y) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")"

instance Storable QValB where
  sizeOf _ = sizeOf (undefined :: CUInt32) * 2

  alignment = sizeOf

  peek ptr =
    do
      let arrPtr = castPtr ptr
      x <- peekElemOff arrPtr 0
      y <- peekElemOff arrPtr 1
      return $ QValB x y

  poke ptr (QValB x y) =
    do
      let arrPtr = castPtr ptr
      pokeElemOff arrPtr 0 x
      pokeElemOff arrPtr 1 y

qryb :: IO (Q QValB)
qryb =
  do
    ctx <- initDB
    q1  <- readT ctx "pairs"
    q2  <- filterQ topLevelFilterB "Main.topLevelFilterB" q1
    return q2

-- FIXME this doesn't get parsed properly with GHC 8.0.2
topLevelFilterB :: QValB -> Bool
topLevelFilterB (QValB x y) = x <= y

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
