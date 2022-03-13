{-# LANGUAGE ForeignFunctionInterface #-}
module FDB.RustFFI where

import GHC.Base (assert)
import Foreign.Storable
import Foreign (Storable, Ptr, ForeignPtr, FunPtr,
                newForeignPtr, withForeignPtr)
import Foreign.C (CSize(..), CUInt)
import Foreign.C.String (CString, withCStringLen)
import Foreign.Ptr (nullPtr)
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
foreign import ccall "mapQ"
  rs_mapQ :: Ptr DbCtx -> Ptr (QPlan a) -> CString -> CSize -> IO (Ptr (QPlan b))
foreign import ccall "foldQ"
  rs_foldQ :: Ptr DbCtx -> Ptr (QPlan a) -> CString -> CSize -> CString -> CSize -> IO (Ptr (SQPlan b))
                                            {-   agg fun  -}    {-    zero    -}
foreign import ccall "&release_qplan"
  rs_releaseQPlan :: FunPtr (Ptr (QPlan a) -> IO ())

{- 3. execQ API -}
foreign import ccall "execQ"
  rs_execQ :: Ptr DbCtx -> Ptr (QPlan a) -> Ptr a -> CSize -> IO CSize

{- The pointer returned by Rust is effectively (void *) -}
type Void = ();
newtype QPlan a  = QPlan Void;
newtype SQPlan a = SQPlan Void;
-- type SQPlan a = QPlan a; {- no real difference, these are all (void *) pointers -}

data DbPtr p = DbPtr (ForeignPtr DbCtx) (ForeignPtr p)
type Q a  = DbPtr (QPlan a)
type SQ a = DbPtr (SQPlan a)
-- data Q a = Q (ForeignPtr DbCtx) (ForeignPtr (QPlan a))
type DbInst = ForeignPtr DbCtx

assertNotNull :: Ptr a -> Ptr a
assertNotNull ptr = if (ptr == nullPtr)
                      then error "Rust returned a NULL pointer"
                      else ptr

type ReleaseFun p = FunPtr (Ptr p -> IO ())

makeDbPtr :: ReleaseFun p -> ForeignPtr DbCtx -> Ptr p -> IO (DbPtr p)
makeDbPtr releaseFun ctxFgn ptrRaw =
  do
    let ptrValid = assertNotNull ptrRaw
    ptrFgn <- newForeignPtr releaseFun ptrValid
    return (DbPtr ctxFgn ptrFgn)

makeQ :: ForeignPtr DbCtx -> Ptr (QPlan a) -> IO (Q a)
makeQ = makeDbPtr rs_releaseQPlan

makeQFrom :: DbPtr p -> Ptr (QPlan b) -> IO (Q b)
makeQFrom (DbPtr ctxFgn _) = makeQ ctxFgn

withDbPtr :: DbPtr p -> (Ptr DbCtx -> Ptr p -> IO b) -> IO b
withDbPtr (DbPtr ctxFgn ptrFgn) f =
    withForeignPtr ctxFgn (\ctxRaw ->
      withForeignPtr ptrFgn (\ptrRaw -> (
        f ctxRaw ptrRaw)))

withQ :: Q a -> (Ptr DbCtx -> Ptr (QPlan a) -> IO b) -> IO b
withQ = withDbPtr

transformQ :: Q a -> (Ptr DbCtx -> Ptr (QPlan a) -> IO (Ptr (QPlan b))) -> IO (Q b)
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
    maxRowCount = 50 -- FIXME we could do with a smaller buffer here

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

filterQ :: (a -> Bool) -> String -> Q a -> IO (Q a)
filterQ _ funName prevQ =
  let
    wrapFilterQ ctxRaw prevPlanRaw (strBuf, strLen) =
      rs_filterQ ctxRaw prevPlanRaw strBuf (fromIntegral strLen)
  in
    transformQ prevQ (\ctxRaw prevPlanRaw ->
      withCStringLen funName (wrapFilterQ ctxRaw prevPlanRaw))

mapQ :: (a -> b) -> String -> Q a -> IO (Q b)
mapQ _ funName prevQ =
  let
    wrapMapQ ctxRaw prevPlanRaw (strBuf, strLen) =
      rs_mapQ ctxRaw prevPlanRaw strBuf (fromIntegral strLen)
  in
    transformQ prevQ (\ctxRaw prevPlanRaw ->
      withCStringLen funName (wrapMapQ ctxRaw prevPlanRaw))


newtype DbCtx = DbCtx Void

initDB :: IO (ForeignPtr DbCtx)
initDB =
  do
    ctxRaw      <- rs_initDB
    let ctxValid = assertNotNull ctxRaw
    ctxFgn      <- newForeignPtr rs_releaseCtx ctxValid
    return ctxFgn
