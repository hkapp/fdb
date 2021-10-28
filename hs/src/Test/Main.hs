{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import TPCH.Functional.Q1 (q1)
import FDB.Dot (toDotGraph)
import qualified Utils.Dot as Dot

import Foreign (Storable(..))
import Foreign.Ptr (castPtr)
import FDB.RustFFI(Q, CUInt32, readT, filterQ, execQ, initDB, mapQ)
import Data.Foldable(traverse_)

main :: IO ()
main = allTests

allTests =
  do
    testFilterFoo;
    testFilterPairs;
    testMapFoo;

testQry :: (Show a, Storable a) => IO (Q a) -> [a] -> IO ()
testQry qry _ =
  qry >>= execAndPrint

execAndPrint :: (Show a, Storable a) => Q a -> IO ()
execAndPrint query =
  do
    resList <- execQ query
    traverse_  print resList

-- Test filterQ on table 'foo'

testFilterFoo =
  let
    expectedResult = [1, 2, 3, 4, 5]
    test f n = testQry (filterFoo f n) expectedResult
  in
    do
      test fooFilter1 "Main.fooFilter1";
      test fooFilter2 "Main.fooFilter2";
      -- test fooFilter3 "Main.fooFilter3";

type QVal = CUInt32;

qry = qryc

qrya :: IO (Q QVal)
qrya =
  filterFoo fooFilter2 "Main.fooFilter2"

filterFoo :: (QVal -> Bool) -> String -> IO (Q QVal)
filterFoo predicate predicateName =
  do
    ctx <- initDB
    q1  <- readT ctx "foo"
    q2  <- filterQ predicate predicateName q1
    return q2

fooFilter1 :: QVal -> Bool
fooFilter1 x = x <= 3

fooFilter2 x = x <= 3

fooFilter3 = (>) 3

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

-- Test filterQ on table 'pairs'

testFilterPairs =
  let
    expectedResult =
      [
        QValB 1 1,
        QValB 1 2,
        QValB 1 3,
        QValB 1 4,
        QValB 1 5,
        QValB 2 2,
        QValB 2 3,
        QValB 2 4,
        QValB 2 5,
        QValB 3 3,
        QValB 3 4,
        QValB 3 5,
        QValB 4 4,
        QValB 4 5,
        QValB 5 5
      ]
    test f n = testQry (filterPairs f n) expectedResult
  in
    do
      test pairsFilter1 "Main.pairsFilter1";

qryb :: IO (Q QValB)
qryb =
  filterPairs pairsFilter1 "Main.pairsFilter1"

filterPairs :: (QValB -> Bool) -> String -> IO (Q QValB)
filterPairs predicate predicateName =
  do
    ctx <- initDB
    q1  <- readT ctx "pairs"
    q2  <- filterQ predicate predicateName q1
    return q2

-- FIXME this doesn't get parsed properly with GHC 8.0.2
pairsFilter1 :: QValB -> Bool
pairsFilter1 (QValB x y) = x <= y

-- Test mapQ on table 'foo'

testMapFoo =
  let
    expectedResult =
      [
        2,
        3,
        4,
        5,
        6
      ]
    test f n = testQry (mapFoo f n) expectedResult
  in
    do
      test fooMapFun1 "Main.fooMapFun1";


qryc :: IO (Q QVal)
qryc =
  mapFoo fooMapFun1 "Main.fooMapFun1"

mapFoo :: (QVal -> a) -> String -> IO (Q a)
mapFoo mapfun mapfunName =
  do
    ctx <- initDB
    q1  <- readT ctx "foo"
    q2  <- mapQ mapfun mapfunName q1
    return q2

fooMapFun1 :: QVal -> QVal
fooMapFun1 x = x + 1




-- Old query Test

queryTest = writeFile fileName text

fileName = "out.dot"

text = Dot.prettyPrint dotGraph

dotGraph = toDotGraph query

query = q1 13
-- query = mapQ id $ readT (Table "Employees")
