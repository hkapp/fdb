{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-} -- C pre-processor to check GHC version
module Main where

import TPCH.Functional.Q1 (q1)
import FDB.Dot (toDotGraph)
import qualified Utils.Dot as Dot

import Foreign (Storable(..))
import Foreign.Ptr (castPtr)
import FDB.RustFFI(Q, DbInst, CUInt32, readT, filterQ, execQ, initDB, mapQ)
import Data.Foldable(traverse_)

main :: IO ()
main = allTests

allTests =
  do
    dbCtx <- initDB
    testFilterFoo dbCtx;
-- pairsFilter1 is not parsed properly with GHC 8.0.2
-- https://stackoverflow.com/questions/28292476/ghc-version-check-in-code
#if __GLASGOW_HASKELL__ > 800
    testFilterPairs dbCtx;
#else
    putStrLn "Skipping testFilterPairs: incompatible GHC version";
#endif
    testMapFoo dbCtx;

testQry :: (Show a, Storable a, Eq a) => IO (Q a) -> [a] -> IO ()
testQry qry expected =
  do
    qres <- qry >>= execQ
    traverse_  print qres
    if qres == expected
      then putStrLn "PASS"
      else putStrLn "FAIL"

execAndPrint :: (Show a, Storable a) => Q a -> IO ()
execAndPrint query =
  do
    resList <- execQ query
    traverse_  print resList

-- Test filterQ on table 'foo'

testFilterFoo dbCtx =
  let
    expectedResult = [1, 2, 3]
    test f n = testQry (filterFoo dbCtx f n) expectedResult
  in
    do
      test fooFilter1 "Main.fooFilter1";
      test fooFilter2 "Main.fooFilter2";
      -- test fooFilter3 "Main.fooFilter3";

type QVal = CUInt32;

filterFoo :: DbInst -> (QVal -> Bool) -> String -> IO (Q QVal)
filterFoo dbCtx predicate predicateName =
  readT dbCtx "foo" >>=
    filterQ predicate predicateName

fooFilter1 :: QVal -> Bool
fooFilter1 x = x <= 3

fooFilter2 x = x <= 3

fooFilter3 = (>) 3

data QValB = QValB CUInt32 CUInt32

instance Show QValB where
  show (QValB x y) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")"

instance Eq QValB where
  (==) (QValB x1 y1) (QValB x2 y2) =
    (x1 == x2) && (y1 == y2)

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

testFilterPairs dbCtx =
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
    test f n = testQry (filterPairs dbCtx f n) expectedResult
  in
    do
      test pairsFilter1 "Main.pairsFilter1";

filterPairs :: DbInst -> (QValB -> Bool) -> String -> IO (Q QValB)
filterPairs dbCtx predicate predicateName =
  readT dbCtx "pairs" >>=
    filterQ predicate predicateName

-- FIXME this doesn't get parsed properly with GHC 8.0.2
pairsFilter1 :: QValB -> Bool
pairsFilter1 (QValB x y) = x <= y

-- Test mapQ on table 'foo'

testMapFoo dbCtx =
  let
    expectedResult =
      [
        2,
        3,
        4,
        5,
        6
      ]
    test f n = testQry (mapFoo dbCtx f n) expectedResult
  in
    do
      test fooMapFun1 "Main.fooMapFun1";


mapFoo :: DbInst -> (QVal -> a) -> String -> IO (Q a)
mapFoo dbCtx mapfun mapfunName =
  readT dbCtx "foo" >>=
    mapQ mapfun mapfunName

fooMapFun1 :: QVal -> QVal
fooMapFun1 x = x + 1




-- Old query Test

queryTest = writeFile fileName text

fileName = "out.dot"

text = Dot.prettyPrint dotGraph

dotGraph = toDotGraph query

query = q1 13
-- query = mapQ id $ readT (Table "Employees")
