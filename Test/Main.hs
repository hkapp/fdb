module Main where

import FDB.FDB
import FDB.Dot (toDotGraph)
import FDB.GADTForall (Table(..))

import TPCH.Functional.Q1 (q1)

import qualified Utils.Dot as Dot

main :: IO ()
main = writeFile fileName text

fileName = "out.dot"

text = Dot.prettyPrint dotGraph

dotGraph = toDotGraph query

query = q1 13
-- query = mapQ id $ readT (Table "Employees")
