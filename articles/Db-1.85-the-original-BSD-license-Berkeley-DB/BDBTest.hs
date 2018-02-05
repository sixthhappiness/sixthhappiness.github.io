import Database.Berkeley.Db

import System.IO
import Data.ByteString.UTF8 (fromString)
import Data.Maybe (isJust)
import Control.Monad (foldM, liftM2, liftM)

buildWordDb = do
  dbenv <- dbEnv_create []
  dbEnv_open [DB_CREATE, DB_INIT_MPOOL, DB_PRIVATE] 0 dbenv "."
  db <- db_create [] dbenv
  db_open [DB_CREATE, DB_TRUNCATE] DB_HASH 0 db Nothing "bdbwords.db" Nothing

  withFile "/usr/share/dict/words" ReadMode $ \h -> do
    c <- hGetContents h
    mapM_ (\w -> db_put [] db Nothing (fromString w) (fromString "1")) (lines c)

  db_close [] db
  dbEnv_close [] dbenv

verifyWordDb = do
  dbenv <- dbEnv_create []
  dbEnv_open [DB_CREATE, DB_INIT_MPOOL, DB_PRIVATE] 0 dbenv "."
  db <- db_create [] dbenv
  db_open [DB_RDONLY] DB_HASH 0 db Nothing "bdbwords.db" Nothing

  r <- withFile "/usr/share/dict/words" ReadMode $ \h -> do
    c <- hGetContents h
    foldM (\r w -> liftM2 (&&) (liftM isJust (db_get [] db Nothing (fromString w))) (return r)) True (lines c)

  db_close [] db
  dbEnv_close [] dbenv

  return r

main = do
  buildWordDb
  verifyWordDb >>= print
