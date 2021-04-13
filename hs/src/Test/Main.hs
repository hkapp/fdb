{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import FDB.FDB
import FDB.Dot (toDotGraph)
import FDB.GADTForall (Table(..))

import TPCH.Functional.Q1 (q1)

import qualified Utils.Dot as Dot

import Foreign.Storable
import Foreign (Storable, Ptr)
import Foreign.C (CSize, CUInt)

main :: IO ()
main = execQTest

-- execQ test

foreign import ccall "execQ"  rust_execQ  :: IO QResultPtr
foreign import ccall "closeQ" rust_closeQ :: QResultPtr -> IO ()

type CUInt32 = CUInt

type QResultPtr = Ptr QResult

data QResult = QResult {
                qresLength :: CSize,
                qresArray  :: Ptr CUInt32
              }

instance Storable QResult where
  sizeOf x = (sizeOf $ qresLength x) + (sizeOf $ qresArray x)
  alignment = sizeOf
  peek ptr = do len <- peekByteOff ptr 0
                arr <- peekByteOff ptr (sizeOf len)
                return (QResult len arr)
  poke ptr = return undefined

execQTest =
  do
    resPtr <- rust_execQ
    resVal <- peek resPtr
    printQRes resVal
    rust_closeQ resPtr

printQRes (QResult len arr) =
  let
    indices = enumFromTo 0 ((fromIntegral len) - 1)
    (<&>) l f = map f l
    values = indices <&> (peekElemOff arr)
  in
    foldMap (\v -> v >>= print) values

-- FFI test

foreign import ccall "bar" rust_bar :: Double -> Double

ffiTest = print $ rust_bar 2.0

-- Query Test

queryTest = writeFile fileName text

fileName = "out.dot"

text = Dot.prettyPrint dotGraph

dotGraph = toDotGraph query

query = q1 13
-- query = mapQ id $ readT (Table "Employees")
