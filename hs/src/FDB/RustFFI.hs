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
  rs_readT :: Ptr DbCtx -> CString -> CSize -> IO (Ptr (QPlan a))
foreign import ccall "filterQ"
  rs_filterQ :: Ptr DbCtx -> Ptr (QPlan a) -> CString -> CSize -> IO (Ptr (QPlan a))
foreign import ccall "&release_qplan"
  rs_releaseQPlan :: FunPtr (Ptr (QPlan a) -> IO ())

{- 3. execQ API -}
foreign import ccall "execQ"
  rs_execQ :: Ptr DbCtx -> Ptr (QPlan a) -> Ptr a -> CSize -> IO CSize

{- The pointer returned by Rust is effectively (void *) -}
type Void = ();
newtype QPlan a = QPlan Void;

data Q a = Q (ForeignPtr DbCtx) (ForeignPtr (QPlan a))

makeQ :: ForeignPtr DbCtx -> Ptr (QPlan a) -> IO (Q a)
makeQ ctxFgn planRaw =
  do
    planFgn <- newForeignPtr rs_releaseQPlan planRaw
    return (Q ctxFgn planFgn)

makeQFrom :: Q a -> Ptr (QPlan a) -> IO (Q a)
makeQFrom (Q ctxFgn _) = makeQ ctxFgn

withQ :: Q a -> (Ptr DbCtx -> Ptr (QPlan a) -> IO b) -> IO b
withQ (Q ctxFgn planFgn) f =
    withForeignPtr ctxFgn (\ctxRaw ->
      withForeignPtr planFgn (\planRaw -> (
        f ctxRaw planRaw)))

transformQ :: Q a -> (Ptr DbCtx -> Ptr (QPlan a) -> IO (Ptr (QPlan a))) -> IO (Q a)
transformQ qctx f =
  let
    wrapF ctxRaw planRaw =
      (f ctxRaw planRaw) >>= (makeQFrom qctx)
  in
    withQ qctx wrapF

type CUInt32 = CUInt

execQ :: (Storable a) => Q a -> IO [a]
execQ qctx =
  let
    maxRowCount = 5

    writeToBuf buffer =
      do
        resRowCount <- withQ qctx (wrapExecQ buffer)
        peekArray (validate resRowCount) buffer

    wrapExecQ buffer dbctx qplan =
      rs_execQ dbctx qplan buffer (fromIntegral maxRowCount)

    validate resRowCount =
        let
          intRowCount   = fromIntegral resRowCount
          isValid       = (intRowCount >= 0 && intRowCount < maxRowCount)
        in
          assert isValid intRowCount
  in
    allocaArray maxRowCount writeToBuf

readT :: ForeignPtr DbCtx -> String -> IO (Q a)
readT ctxFgn tabName =
  let
    wrapReadT ctxRaw (strBuf, strLen) =
      do
        planRaw <- rs_readT ctxRaw strBuf (fromIntegral strLen)
        makeQ ctxFgn planRaw
  in
    withForeignPtr ctxFgn (\ctxRaw ->
      withCStringLen tabName (wrapReadT ctxRaw))

filterQ :: Q a -> (a -> Bool) -> String -> IO (Q a)
filterQ prevQ _ funName =
  let
    wrapFilterQ ctxRaw prevPlanRaw (strBuf, strLen) =
      rs_filterQ ctxRaw prevPlanRaw strBuf (fromIntegral strLen)
  in
    transformQ prevQ (\ctxRaw prevPlanRaw ->
      withCStringLen funName (wrapFilterQ ctxRaw prevPlanRaw))


newtype DbCtx = DbCtx Void

initDB :: IO (ForeignPtr DbCtx)
initDB = rs_initDB >>= (newForeignPtr rs_releaseCtx)
