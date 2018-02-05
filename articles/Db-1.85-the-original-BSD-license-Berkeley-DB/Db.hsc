{- -*- coding:utf-8; compile-command:"hsc2hs Db.hsc" -*- -}

{-
The following code is original work by me.  I now place it in the
public domain.  Do not plagiarize.

Andrew Choi.  Calgary, Canada, May 27, 2010.
-}

{-# LANGUAGE ForeignFunctionInterface #-}

module Db where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Error
import Foreign.Marshal.Alloc
import Data.Word
import Data.Bits
import Data.ByteString as BS (useAsCStringLen, packCStringLen, concat)
import Data.ByteString.UTF8 (fromString, toString)
import Data.ByteString.Lazy (fromChunks, toChunks)
import Data.Int
import Data.Binary (Binary, encode, decode)
import Control.Monad (liftM)

#include <db.h>
#include <fcntl.h>
#include <sys/stat.h>

#{
let alignment t =
  "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
}

newtype DbType = DbType { unDbType :: CInt }

#{
enum DbType, DbType,
  dbTypeBTree = DB_BTREE,
  dbTypeHash = DB_HASH,
  dbTypeRecNo = DB_RECNO
}

type HashFun = Ptr () -> CSize -> IO Word32

data HashInfo = HashInfo
    {
      bsize :: Word32,
      ffactor :: Word32,
      nelem :: Word32,
      cachesize :: Word32,
      hash :: FunPtr HashFun,
      lorder :: CInt
    }

instance Storable HashInfo where
    alignment _ = #{alignment HASHINFO}

    sizeOf _ = #{size HASHINFO}

    peek ptr = do
      bsize <- #{peek HASHINFO, bsize} ptr
      ffactor <- #{peek HASHINFO, ffactor} ptr
      nelem <- #{peek HASHINFO, nelem} ptr
      cachesize <- #{peek HASHINFO, cachesize} ptr
      hash <- #{peek HASHINFO, hash} ptr
      lorder <- #{peek HASHINFO, lorder} ptr
      return (HashInfo bsize ffactor nelem cachesize hash lorder)

    poke ptr (HashInfo bsize ffactor nelem cachesize hash lorder) = do
      #{poke HASHINFO, bsize} ptr bsize
      #{poke HASHINFO, ffactor} ptr ffactor
      #{poke HASHINFO, nelem} ptr nelem
      #{poke HASHINFO, cachesize} ptr cachesize
      #{poke HASHINFO, hash} ptr hash
      #{poke HASHINFO, lorder} ptr lorder

data Dbt = Dbt
    {
      data_ :: Ptr (),
      size :: CSize
    }

instance Storable Dbt where
    alignment _ = #{alignment DBT}

    sizeOf _ = #{size DBT}

    peek ptr = do
      data_ <- #{peek DBT, data} ptr
      size <- #{peek DBT, size} ptr
      return (Dbt data_ size)

    poke ptr (Dbt data_ size) = do
      #{poke DBT, data} ptr data_
      #{poke DBT, size} ptr size

type CloseFun = Ptr Db -> IO CInt
type DelFun = Ptr Db -> Ptr Dbt -> CUInt -> IO CInt
type GetFun = Ptr Db -> Ptr Dbt -> Ptr Dbt -> CUInt -> IO CInt
type PutFun = Ptr Db -> Ptr Dbt -> Ptr Dbt -> CUInt -> IO CInt
type SeqFun = Ptr Db -> Ptr Dbt -> Ptr Dbt -> CUInt -> IO CInt
type SyncFun = Ptr Db -> CUInt -> IO CInt
type FdFun = Ptr Db -> IO CInt

data Db = Db
    {
      type_ :: CInt,
      close :: FunPtr CloseFun,
      del :: FunPtr DelFun,
      get :: FunPtr GetFun,
      put :: FunPtr PutFun,
      seq :: FunPtr SeqFun,
      sync :: FunPtr SyncFun,
      internal :: Ptr (),
      fd :: FunPtr FdFun
    }

instance Storable Db where
    alignment _ = #{alignment DB}

    sizeOf _ = #{size DB}

    peek ptr = do
      type_ <- #{peek DB, type} ptr
      close <- #{peek DB, close} ptr
      del <- #{peek DB, del} ptr
      get <- #{peek DB, get} ptr
      put <- #{peek DB, put} ptr
      seq <- #{peek DB, seq} ptr
      sync <- #{peek DB, sync} ptr
      internal <- #{peek DB, internal} ptr
      fd <- #{peek DB, fd} ptr
      return (Db type_ close del get put seq sync internal fd)

    poke ptr (Db type_ close del get put seq sync internal fd) = do
      #{poke DB, type} ptr type_
      #{poke DB, close} ptr close
      #{poke DB, del} ptr del
      #{poke DB, get} ptr get
      #{poke DB, put} ptr put
      #{poke DB, seq} ptr seq
      #{poke DB, sync} ptr sync
      #{poke DB, internal} ptr internal
      #{poke DB, fd} ptr fd

newtype Flag = Flag { unFlag :: CInt }

{- Manpage of dbopen also includes flags O_EXLOCK and O_SHLOCK but
   these aren't available on Linux. -}
#{
enum Flag, Flag,
  flagCreat = O_CREAT,
  flagExcl = O_EXCL,
  flagNonblock = O_NONBLOCK,
  flagRdOnly = O_RDONLY,
  flagRdWr = O_RDWR,
  flagTrunc = O_TRUNC
}

newtype Mode = Mode { unMode :: CInt }

#{
enum Mode, Mode,
  modeIRWXU = S_IRWXU,
  modeIRUSR = S_IRUSR,
  modeIWUSR = S_IWUSR,
  modeIXUSR = S_IXUSR,

  modeIRWXG = S_IRWXG,
  modeIRGRP = S_IRGRP,
  modeIWGRP = S_IWGRP,
  modeIXGRP = S_IXGRP,

  modeIRWXO = S_IRWXO,
  modeIROTH = S_IROTH,
  modeIWOTH = S_IWOTH,
  modeIXOTH = S_IXOTH
}

newtype RoutineFlag = RoutineFlag { unRoutineFlag :: CUInt }

#{
enum RoutineFlag, RoutineFlag,
  routineFlagCursor = R_CURSOR,
  routineFlagFirst = R_FIRST,
  routineFlagIAfter = R_IAFTER,
  routineFlagIBefore = R_IBEFORE,
  routineFlagLast = R_LAST,
  routineFlagNext = R_NEXT,
  routineFlagNoOverwrite = R_NOOVERWRITE,
  routineFlagPrev = R_PREV,
  routineFlagSetCursor = R_SETCURSOR,
  routineFlagRecNoSync = R_RECNOSYNC
}

foreign import ccall "db.h dbopen" dbopen ::
    Ptr CChar -> CInt -> CInt -> CInt -> Ptr () -> IO (Ptr Db)

foreign import ccall "dynamic" mkCloseFun :: FunPtr CloseFun -> CloseFun
foreign import ccall "dynamic" mkDelFun :: FunPtr DelFun -> DelFun
foreign import ccall "dynamic" mkGetFun :: FunPtr GetFun -> GetFun
foreign import ccall "dynamic" mkPutFun :: FunPtr PutFun -> PutFun
foreign import ccall "dynamic" mkSeqFun :: FunPtr SeqFun -> SeqFun
foreign import ccall "dynamic" mkSyncFun :: FunPtr SyncFun -> SyncFun
foreign import ccall "dynamic" mkFdFun :: FunPtr FdFun -> FdFun

dbOpen path flags modes dbType infoPtr = do
    let flags' = foldr ((.|.) . unFlag) 0 flags
    let modes' = foldr ((.|.) . unMode) 0 modes
    withCString path $ \s->
      throwErrnoIfNull "dbHashOpen" $
        dbopen s flags' modes' dbType infoPtr

dbHashOpen p f m = dbOpen p f m (unDbType dbTypeHash) nullPtr

dbHashOpeni :: String -> [Flag] -> [Mode] -> HashInfo -> IO (Ptr Db)
dbHashOpeni p f m info = do
  alloca $ \infoPtr -> do
    poke infoPtr info
    dbOpen p f m (unDbType dbTypeHash) (castPtr infoPtr)

dbClose db = do
  db' <- peek db
  throwErrnoIfMinus1 "dbClose" $ mkCloseFun (close db') db
  return ()

dbPut db k v = do
  db' <- peek db
  useAsCStringLen k $ \(kdata, klen) ->
    alloca $ \kdbt -> do
      poke kdbt (Dbt (castPtr kdata) (fromIntegral klen))
      useAsCStringLen v $ \(vdata, vlen) ->
        alloca $ \vdbt -> do
          poke vdbt (Dbt (castPtr vdata) (fromIntegral vlen))
          throwErrnoIfMinus1 "dbPut" $ mkPutFun (put db') db kdbt vdbt 0
          return ()

dbPutss db k v = dbPut db (fromString k) (fromString v)

dbPuts :: Binary a => Ptr Db -> String -> a -> IO ()
dbPuts db k v = dbPut db (fromString k) (BS.concat (toChunks (encode v)))

dbGet db k = do
  db' <- peek db
  useAsCStringLen k $ \(kdata, klen) ->
    alloca $ \kdbt -> do
      poke kdbt (Dbt (castPtr kdata) (fromIntegral klen))
      alloca $ \vdbt -> do
        r <- mkGetFun (get db') db kdbt vdbt 0
        case r of
          0 -> do
            v <- peek vdbt
            v' <- packCStringLen (castPtr (data_ v), fromIntegral (size v))
            return $ Just v'
          1 ->
              return Nothing
          _ ->
              throwErrno "dbGet"

dbGetss db k = liftM toString `liftM` dbGet db (fromString k)

dbGets :: Binary a => Ptr Db -> String -> IO (Maybe a)
dbGets db k =
  liftM (decode . fromChunks . (:[])) `liftM` dbGet db (fromString k)

dbSeq db routineFlag = do
  db' <- peek db
  alloca $ \kdbt ->
    alloca $ \vdbt -> do
      let rf = unRoutineFlag routineFlag
      r <- mkSeqFun (Db.seq db') db kdbt vdbt rf
      case r of
        0 -> do
          k <- peek kdbt
          k' <- packCStringLen (castPtr (data_ k), fromIntegral (size k))
          v <- peek vdbt
          v' <- packCStringLen (castPtr (data_ v), fromIntegral (size v))
          return $ Just (k', v')
        1 ->
             return Nothing  -- Don't use exception for loop termination.
        _ ->
            throwErrno "dbSeq"  -- Use exception for error.

dbDel db k = do
  db' <- peek db
  useAsCStringLen k $ \(kdata, klen) ->
    alloca $ \kdbt -> do
      poke kdbt (Dbt (castPtr kdata) (fromIntegral klen))
      throwErrnoIf (/= 0) "dbDel" $ mkDelFun (del db') db kdbt 0

dbDels db k = dbDel db (fromString k)

dbSync db = do
  db' <- peek db
  throwErrnoIfMinus1 "dbSync" $ mkSyncFun (sync db') db 0
