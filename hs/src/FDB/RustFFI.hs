{-# LANGUAGE ForeignFunctionInterface #-}
module FDB.RustFFI where

import GHC.Base (assert)
import Foreign.Storable
import Foreign (Storable, Ptr, ForeignPtr, FunPtr,
                newForeignPtr, withForeignPtr)
import Foreign.C (CSize(..), CUInt)
import Foreign.C.String (CString, withCStringLen)
import Foreign.Marshal.Array (allocaArray, peekArray)

{- Import Rust functions -}

{- 1. Ctx API -}
foreign import ccall "initDB"
  rs_initDB :: IO (Ptr DbCtx)
foreign import ccall "&release_ctx"
  rs_releaseCtx :: FunPtr (Ptr DbCtx -> IO ())

{- 2. QPlan API -}
foreign import ccall "readT"
  rs_readT :: CString -> CSize -> IO (Ptr (QPlan a))
foreign import ccall "&release_qplan"
  rs_releaseQPlan :: FunPtr (Ptr (QPlan a) -> IO ())

{- 3. execQ API -}
foreign import ccall "execQ"
  rs_execQ :: Ptr (QPlan a) -> Ptr a -> CSize -> IO CSize

{- The pointer returned by Rust is effectively (void *) -}
type Void = ();
newtype QPlan a = QPlan Void;

type Q a = ForeignPtr (QPlan a)

makeQ :: Ptr (QPlan a) -> IO (Q a)
makeQ = newForeignPtr rs_releaseQPlan

type CUInt32 = CUInt

execQ :: (Storable a) => Q a -> IO [a]
execQ =
  let
    maxRowCount = 5
    writeToBuf qplanFgn buffer =
      do
        resRowCount <- withForeignPtr qplanFgn (wrapExecQ buffer)
        let intRowCount   = fromIntegral resRowCount
        let validRowCount = assert (intRowCount >= 0 && intRowCount < maxRowCount) intRowCount
        peekArray validRowCount buffer
    wrapExecQ buffer qplan = rs_execQ qplan buffer (fromIntegral maxRowCount)
  in
    allocaArray maxRowCount . writeToBuf

readT :: String -> IO (Q a)
readT tabName =
  let
    wrapReadT (strBuf, strLen) =
      do
        rawPtr <- rs_readT strBuf (fromIntegral strLen)
        makeQ rawPtr
  in
    withCStringLen tabName wrapReadT


newtype DbCtx = DbCtx Void

initDB :: IO (ForeignPtr DbCtx)
initDB = rs_initDB >>= (newForeignPtr rs_releaseCtx)
