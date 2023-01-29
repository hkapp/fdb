{-# LANGUAGE ForeignFunctionInterface #-}
module FDB.RustFFI (
  Q,
  SQ,
  DbInst,
  Backend,
  CUInt32,
  readT,
  filterQ,
  execQ,
  initDB,
  mapQ,
  foldQ,
  execSQ,
  allBackends
)
  where

import GHC.Base (assert)
import Foreign.Storable
import Foreign (Storable, Ptr, ForeignPtr, FunPtr,
                newForeignPtr, withForeignPtr)
import Foreign.C (CSize(..), CUInt, CUChar(..))
import Foreign.C.String (CString, withCStringLen)
import Foreign.Ptr (nullPtr)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Alloc (alloca)

{- Import Rust functions -}

{- 1. Ctx API -}
foreign import ccall "initDB"
  rs_initDB :: CUChar -> IO (Ptr DbCtx)
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
foreign import ccall "&release_sqplan"
  rs_releaseSQPlan :: FunPtr (Ptr (SQPlan a) -> IO ())

{- 3. execQ API -}
foreign import ccall "execQ"
  rs_execQ :: Ptr DbCtx -> Ptr (QPlan a) -> Ptr a -> CSize -> IO CSize
foreign import ccall "execSQ"
  rs_execSQ :: Ptr DbCtx -> Ptr (SQPlan a) -> Ptr a -> IO CUChar

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

data Backend = RawSQL | NaiveRow | LazyMat | RowColumnar {- To be kept in sync with fql::backend::Backend -}

allBackends = [RawSQL, NaiveRow, LazyMat, RowColumnar]

assertNotNull :: Ptr a -> Ptr a
assertNotNull ptr = if (ptr == nullPtr)
                      then error "Rust returned a NULL pointer"
                      else ptr

type ReleaseFun p = FunPtr (Ptr p -> IO ())

class RustOwn a where
  rustFree :: ReleaseFun a

instance RustOwn (QPlan a) where
  rustFree = rs_releaseQPlan

instance RustOwn (SQPlan a) where
  rustFree = rs_releaseSQPlan

makeDbPtr :: (RustOwn p) => ForeignPtr DbCtx -> Ptr p -> IO (DbPtr p)
makeDbPtr ctxFgn ptrRaw =
  do
    let ptrValid = assertNotNull ptrRaw
    ptrFgn <- newForeignPtr rustFree ptrValid
    return (DbPtr ctxFgn ptrFgn)

makeQ :: ForeignPtr DbCtx -> Ptr (QPlan a) -> IO (Q a)
makeQ = makeDbPtr

makeDbPtrFrom :: (RustOwn b) => DbPtr a -> Ptr b -> IO (DbPtr b)
makeDbPtrFrom (DbPtr ctxFgn _) = makeDbPtr ctxFgn

withDbPtr :: DbPtr p -> (Ptr DbCtx -> Ptr p -> IO b) -> IO b
withDbPtr (DbPtr ctxFgn ptrFgn) f =
    withForeignPtr ctxFgn (\ctxRaw ->
      withForeignPtr ptrFgn (\ptrRaw -> (
        f ctxRaw ptrRaw)))

withQ :: Q a -> (Ptr DbCtx -> Ptr (QPlan a) -> IO b) -> IO b
withQ = withDbPtr

withSQ :: SQ a -> (Ptr DbCtx -> Ptr (SQPlan a) -> IO b) -> IO b
withSQ = withDbPtr

transformDbPtr :: (RustOwn b) => DbPtr a -> (Ptr DbCtx -> Ptr a -> IO (Ptr b)) -> IO (DbPtr b)
transformDbPtr dbptr f =
  let
    wrapF ctxRaw ptrRaw =
      (f ctxRaw ptrRaw) >>= (makeDbPtrFrom dbptr)
  in
    withDbPtr dbptr wrapF

transformQ :: Q a -> (Ptr DbCtx -> Ptr (QPlan a) -> IO (Ptr (QPlan b))) -> IO (Q b)
transformQ = transformDbPtr

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

foldQ :: (a -> b -> a) -> String -> a -> String -> Q b -> IO (SQ a)
foldQ _ foldName _ zeroName prevQ =
  let
    wrapFoldQ ctxRaw prevPlanRaw (foldStrBuf, foldStrLen) (zeroStrBuf, zeroStrLen) =
      rs_foldQ ctxRaw prevPlanRaw foldStrBuf (fromIntegral foldStrLen) zeroStrBuf (fromIntegral zeroStrLen)
  in
    transformDbPtr prevQ (\ctxRaw prevPlanRaw ->
      withCStringLen foldName (\foldCStr ->
        withCStringLen zeroName (wrapFoldQ ctxRaw prevPlanRaw foldCStr)))

newtype DbCtx = DbCtx Void

initDB :: Backend -> IO (ForeignPtr DbCtx)
initDB backend =
  do
    ctxRaw      <- rs_initDB backend_ffi
    let ctxValid = assertNotNull ctxRaw
    ctxFgn      <- newForeignPtr rs_releaseCtx ctxValid
    return ctxFgn
  where
    backend_ffi = case backend of {- To be kept in sync with Rust -}
                    RawSQL      -> 0
                    NaiveRow    -> 1
                    LazyMat     -> 2
                    RowColumnar -> 3

execSQ :: (Storable a) => SQ a -> IO a
execSQ sqctx =
  let
    isCTrue n = n /= 0

    writeToBuf buffer =
      do
        resultExists <- withSQ sqctx (wrapExecSQ buffer)
        assert (isCTrue resultExists) (peek buffer)

    wrapExecSQ buffer dbctx qplan =
      rs_execSQ dbctx qplan buffer
  in
    alloca writeToBuf
