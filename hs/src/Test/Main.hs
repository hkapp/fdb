{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import FDB.FDB
import FDB.Dot (toDotGraph)
import FDB.GADTForall (Table(..))

import TPCH.Functional.Q1 (q1)

import qualified Utils.Dot as Dot

import Foreign.Storable
import Foreign (Storable, Ptr, ForeignPtr, FunPtr,
                newForeignPtr, withForeignPtr)
import Foreign.C (CSize(..), CUInt)
import Foreign.C.String (CString, newCStringLen)

main :: IO ()
main = execQTest

-- execQ test

foreign import ccall "execQ"  rust_execQ  :: CString -> CSize -> IO QResultPtr
foreign import ccall "&closeQ" rust_closeQ :: FunPtr (QResultPtr -> IO ())

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
    resPtr <- execQ "foo"
    resVal <- foreignPeek resPtr
    printQRes resVal
    {-rust_closeQ resPtr-}

execQ :: String -> IO (ForeignPtr QResult)
execQ tabName =
  do
    (strBuf, strLen) <- newCStringLen tabName
    rawPtr           <- rust_execQ strBuf (fromIntegral strLen)
    newForeignPtr rust_closeQ rawPtr

printQRes (QResult len arr) =
  let
    indices = enumFromTo 0 ((fromIntegral len) - 1)
    (<&>) l f = map f l
    values = indices <&> (peekElemOff arr)
  in
    foldMap (\v -> v >>= print) values

foreignPeek :: (Storable a) => ForeignPtr a -> IO a
foreignPeek fgnPtr = withForeignPtr fgnPtr peek

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
