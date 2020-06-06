module Main where

import FDB.FDB
import FDB.GADTForall (toDotGraph, Table(..))

import qualified Utils.Dot as Dot

main :: IO ()
main = writeFile fileName text

fileName = "out.dot"

text = Dot.prettyPrint dotGraph

dotGraph = toDotGraph query

query = mapQ id $ readT (Table "Employees")
