{-# INCLUDE "cffi.h" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import FDB.FDB
import FDB.Dot (toDotGraph)
import FDB.GADTForall (Table(..))

import TPCH.Functional.Q1 (q1)

import qualified Utils.Dot as Dot

main :: IO ()
main = ffiTest

-- FFI test

foreign import ccall "exp" c_exp :: Double -> Double
foreign import ccall "foo" c_foo :: Double -> Double

ffiTest = print $ c_foo 2.0

-- Query Test

queryTest = writeFile fileName text

fileName = "out.dot"

text = Dot.prettyPrint dotGraph

dotGraph = toDotGraph query

query = q1 13
-- query = mapQ id $ readT (Table "Employees")
