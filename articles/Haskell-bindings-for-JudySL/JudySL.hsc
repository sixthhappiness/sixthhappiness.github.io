{- -*- coding:utf-8; compile-command:"hsc2hs JudySL.hsc" -*- -}

{-
The following code is original work by me.  I now place it in the
public domain.  Do not plagiarize.

Andrew Choi.  Calgary, Canada, May 27, 2010.
-}

{-# LANGUAGE ForeignFunctionInterface #-}

#include <Judy.h>

#{def
void ppjslFinalizer(PPvoid_t ppjsl)
{
  (void) JudySLFreeArray(ppjsl, PJE0);
}
}

module JudySL where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import Data.ByteString
import Data.ByteString.UTF8

import Control.Monad (liftM, liftM2, when)
import System.IO.Unsafe (unsafeInterleaveIO)

data JSL = JSL
type PJSL = Ptr JSL
type PPJSL = ForeignPtr PJSL

data JError = JError
type PJError = Ptr JError

pje0 = nullPtr :: PJError

foreign import ccall "&ppjslFinalizer" ppjslFinalizer ::
    FunPtr (Ptr PJSL -> IO ())

new = do
    p <- mallocForeignPtr
    addForeignPtrFinalizer ppjslFinalizer p
    withForeignPtr p (`poke` nullPtr)
    return p

foreign import ccall unsafe "JudySLIns" judySLIns ::
    Ptr PJSL -> CString -> PJError -> IO (Ptr CULong)

put j k v =
  withForeignPtr j $ \j' ->
      useAsCString k $ \k' -> do
        pv <- judySLIns j' k' pje0
        poke pv v

puts j k = put j (fromString k)

-- Update value for key k with function f.  If key doesn't exist first
-- initialize it to zero, then update with function f.
update0 j k f =
  withForeignPtr j $ \j' ->
    useAsCString k $ \k' -> do
      pv <- judySLIns j' k' pje0
      v <- peek pv
      poke pv (f v)

update0s j k = update0 j (fromString k)

foreign import ccall unsafe "JudySLGet" judySLGet ::
    PJSL -> CString -> PJError -> IO (Ptr CULong)

-- Update value for key k with function f.  If key doesn't exist, do
-- nothing.
update j k f =
  withForeignPtr j $ \j' -> do
    pjsl <- peek j'
    useAsCString k $ \k' -> do
      pv <- judySLGet pjsl k' pje0
      when (pv /= nullPtr) $ do
        v <- peek pv
        poke pv (f v)

updates j k = update j (fromString k)

get j k =
  withForeignPtr j $ \j' -> do 
    pjsl <- peek j'
    useAsCString k $ \k' -> do
      pv <- judySLGet pjsl k' pje0
      if pv /= nullPtr
        then liftM Just (peek pv)
        else return Nothing

gets j k = get j (fromString k)

foreign import ccall unsafe "JudySLDel" judySLDel ::
    Ptr PJSL -> CString -> PJError -> IO CInt

del j k =
  withForeignPtr j $ \j' ->
      useAsCString k $ \k' -> do
        r <- judySLDel j' k' pje0
        return (r == 1)

dels j k = del j (fromString k)

foreign import ccall unsafe "JudySLFirst" judySLFirst ::
    PJSL -> CString -> PJError -> IO (Ptr CULong)

foreign import ccall unsafe "JudySLLast" judySLLast ::
    PJSL -> CString -> PJError -> IO (Ptr CULong)

foreign import ccall unsafe "JudySLNext" judySLNext ::
    PJSL -> CString -> PJError -> IO (Ptr CULong)

foreign import ccall unsafe "JudySLPrev" judySLPrev ::
    PJSL -> CString -> PJError -> IO (Ptr CULong)

maxByteStringLen = 256

judySLSearchFun fun j k =
  withForeignPtr j $ \j' -> do
  pjsl <- peek j'
  allocaArray0 maxByteStringLen $ \pc ->
    useAsCString k $ \k' -> do
      l <- lengthArray0 0 k'
      copyArray pc k' (l + 1)
      pv <- fun pjsl pc pje0
      if pv /= nullPtr
        then do
          kr <- packCString pc
          vr <- peek pv
          return $ Just (toString kr, vr)
        else return Nothing

first = judySLSearchFun judySLFirst
firsts j k = first j (fromString k)

last = judySLSearchFun judySLLast
lasts j k = JudySL.last j (fromString k)

next = judySLSearchFun judySLNext
nexts j k = next j (fromString k)

prev = judySLSearchFun judySLPrev
prevs j k = prev j (fromString k)

foreach j action =
    let loop Nothing = return ()
        loop (Just (k, v)) = do
          action k v
          nexts j k >>= loop
    in
      firsts j "" >>= loop

toList j = do
  let loop Nothing = return []
      loop (Just p@(k, _)) = unsafeInterleaveIO (do
        rest <- (nexts j k >>= loop)
        return (p : rest))
  firsts j "" >>= loop

fromList l = do
  j <- new
  mapM_ (uncurry (puts j)) l
  return j
