% Db 1.85 — the Original, BSD-licensed Berkeley DB
% Andrew Choi
% 26 May 2010

Db 1.85
=======

I've been writing a few programs to extract and experiement with
[_n-grams_][SLMSurvey].  As the programs continue to collect data from
the Internet, the data structures required by the algorithms have
grown so large that they won't fit in main memory.  So I looked for
programming libraries of disk-based hash tables and B-trees to
continue with my work.

[SLMSurvey]: http://www.cs.cmu.edu/~roni/papers/survey-slm-IEEE-PROC-0004.pdf

My requirements for this library are simple.  Each table keeps track
of key-value pairs: each pair is typically a UTF-8 encoded string (or
an _n_-tuple of them) and a numerical value.  There can be tens of
millions of pairs in each table.  Access to them has a high degree of
locality, i.e., certain keys are looked up much more frequently than
others.  A number of tables may need to be opened at the same time.
Of course the library must be efficient both in time and space.

After a bit of searching and evaluation, I was quite surprised by how
difficult it was to find a library that fits these requirements well!
A [search][DebianDBPackages] for Debian packages with the word
"database" in their description returns ones falling into two main
categories: "real" relational database systems (such as [MySQL][] and
[SQLite][]), and disk-based map data structures (such as [Berkeley DB
4.x][] and [Tokyo Cabinet][]).  I tested all four of these systems
and, as one might have predicted, their performances for my
requirements ranked roughly in the order: Berkeley DB (best), Tokyo
Cabinet, SQLite3, and MySQL (worst).

[DebianDBPackages]: http://packages.debian.org/search?keywords=database&searchon=all&suite=stable&section=all
[Berkeley DB 4.x]: http://www.oracle.com/technology/products/berkeley-db/db/index.html
[MySQL]: http://www.mysql.com/
[SQLite]: http://www.sqlite.org/
[Tokyo Cabinet]: http://1978th.net/tokyocabinet/

There are at least two problems for me to use Berkeley DB in my
programs.  It's an overkill for my simple task.  It supports
transactions, locking, "environments", and [many other
features][BDB-API], which I don't need.  Perhaps worse, it's released
basically [under the GPL][BDB-License].  Not LGPL!  Certainly not
under the BSD license that the name of the software may imply!

[BDB-API]: http://www.oracle.com/technology/documentation/berkeley-db/db/programmer_reference/index.html
[BDB-License]: http://www.oracle.com/technology/software/products/berkeley-db/htdocs/oslicense.html

OK, I promise not go on a diatribe about software licenses.  Some
history of the licensing of Berkeley DB (formerly just _db_ in the BSD
Unix distribution, obviously released under a BSD license) can be
found [on Wikipedia][BDB-Wikipedia].  On most Unix systems today, the
manpages for `dbopen(3)`, `hash(3)`, and `btree(3)` and the _include_
file `db.h` still describe and specify the API for db 1.85, a simpler
disk-based hash table and B-tree library.  Most of them (on Mac OS X
e.g.), however, don't have `libdb.a` (or `libdb.so`, or `libdb.dylib`)
installed in the library directories.  That library has been
superceded by the "db 1.85 compatibility mode" in Berkeley DB 4.x.
Unfortunately, most systems don't have Berkeley DB 4.x installed by
default.  Therefore if I distribute a program I write that uses
Berkeley DB 4.x, I will need to distribute the latter with it and have
to release my program under the GPL too.

[BDB-Wikipedia]: http://en.wikipedia.org/wiki/Berkeley_db

So, what'll work best for me is to install the original version 1.85
of `libdb` myself on systems I used for development: Macs and Linux
PCs.  Fortunately it's not that difficult.  On a Linux system,
download [`Berkeley_DB_1.85.tar.gz`][DB185-Download] from Oracle.
Apply my patch file [`db.1.85.diff`][db.1.85.diff].  Go into the
`PORT/linux` directory and type `make`.  Ignore the error message
about `tsort`.  Build a shared library by typing: `gcc -shared -o
libdb.so *.o`.  Move the header file and shared library into the
appropriate directories.

[DB185-Download]: http://download.oracle.com/berkeley-db/db.1.85.tar.gz
[db.1.85.diff]: db.1.85.diff

In Mac OS X, follow the same steps above (also _make_ in the
`PORT/linux` directory!).  To build a 32-bit dynamic library, type:
`CC="gcc -arch i386" make`.  Then: `gcc -arch i386 -dynamiclib *.o -o
libdb.dylib`.

So that's all you need to do to get the actual library for the
`dbopen`, `hash`, and `btree` API.

Haskell FFI for Db 1.85
=======================

Of course that is too easy for a full article in my blog :-).  So
here's the twist!

I didn't mention that I was writing all my _n_-gram programs in
Haskell.  The database and disk-based hash table and B-tree libraries
were all tested through their Haskell wrappers:
[Database.HaskellDB.HDBC.MySQL][H-MySQL],
[Database.HaskellDB.HDBC.SQLite3][H-SQLite3],
[Database.Berkeley.Db][H-BDB], and [Database.TokyoCabinet][H-TC].  For
other Haskell packages, look under the _Database_ category in the
[HackageDB package list][HackageDB]. If you're writing relational
database applications in Haskell, [HaskellDB][] _beautifully_ supports
construction of queries and is definitely worth a look.

[H-BDB]: http://hackage.haskell.org/package/BerkeleyDB
[H-MySQL]: http://hackage.haskell.org/package/haskelldb-hdbc-mysql
[H-SQLite3]: http://hackage.haskell.org/package/haskelldb-hdbc-sqlite3
[H-TC]: http://hackage.haskell.org/package/tokyocabinet-haskell
[HackageDB]: http://hackage.haskell.org/packages/archive/pkg-list.html
[HaskellDB]: http://haskelldb.sourceforge.net/

However, my current problem is to access `dbopen(3)`, `hash(3)`, and
`btree(3)` from Haskell.  Haskell has very nice [_foreign function
interface_ (FFI)][FFI] support, which, among other things, allows you
to call C functions from Haskell and vice versa.  In fact if you
haven't use FFI and its related features in Haskell, you couldn't
realize how low-level and imperative a programming language Haskell
_can_ be!  Unfortunately, there aren't too many good descriptions on
its use on the Web.  My suggestion is to read the [The Haskell 98
Foreign Function Interface 1.0: Addendum to the Haskell 98
Report][FFI], then the [Wikibooks Haskell/FFI page][Wikibooks-FFI],
then some Haskell FFI code such as the [Haskell package _unix_][unix].

[FFI]: http://www.cse.unsw.edu.au/~chak/haskell/ffi/
[Wikibooks-FFI]: http://en.wikibooks.org/wiki/Haskell/FFI
[unix]: http://hackage.haskell.org/package/unix

So here's the Haskell FFI for db I wrote: [`Db.hsc`][Db.hsc].  I
believe it's also a good code example to read if one is learning to
use the Haskell FFI.  It shows at least one interesting technique that
I couldn't find elsewhere: calling a C function through a pointer
stored in a C structure.  Also the db 1.85 API is not too simple or
complex so this code serves as a reasonably practical example.
Everything is done in Haskell; no glue code written in C is needed.
To use this interface module, convert the file `Db.hsc` to Haskell by
typing:

    hsc2hs Db.hsc

[Db.hsc]: Db.hsc

Then, start the Haskell interpreter like so (after having installed db
1.85):

    ghci -ldb -L/Users/choi/Desktop/db.1.85/PORT/linux
    
The `-L` option shouldn't be necessary if you've installed `libdb.so`
or `libdb.dylib` in a standard location such as `/usr/local/lib`.
Also, you may first need to install a few Hackages (see `import`
statements in `Db.hs`).  This is easy to do if you use Cabal.

In the interpreter, load the `Db` module by saying,

    :load Db.hs
    
Then you can start to experiment with the `Db` module.  What follows
is an example of a complete program ([Literate Haskell source
available][contents.lhs]) that demonstrates some of its functions.
First we `import` a few modules.

[contents.lhs]: contents.lhs

> module Main where
>
> import Db
> import System.IO
> import Data.Maybe (isJust)
> import Control.Monad (foldM, liftM2, liftM)
> import Data.Int
> import Data.ByteString.UTF8 (fromString, toString)

`Test1` shows a few basic calls to create, add pairs to, search, and
sequence through a hash table.  `DbHashOpen` returns a `Ptr Db`, a
handle used by all other functions.  `DbPutss` and `dbGetss` store and
retrieve key-value pairs to and from the hash table, respectively,
when both key and value are of type `String`.  `DbDels` deletes a pair
with a given `String` value key, and `dbClose` closes the hash table.
`DbSeq` is used to sequence through all pairs in the table, as shown
in the function `printDb`.

> test1 = do
>   db <- dbHashOpen "test1.db" [flagRdWr, flagCreat, flagTrunc] [modeIRWXU]
> 
>   dbPutss db "a" "A"
>   dbPutss db "b" "B"
>   dbPutss db "c" "C"
> 
>   printDb db
> 
>   dbDels db "b"
> 
>   putStrLn ""
>   printDb db
> 
>   putStrLn ""
>   putStrLn dbGetss db "c"
> 
>   dbClose db
>   
>     where
>       printDb db =
>           let loop Nothing = return ()
>               loop (Just (k, v)) = do
>                 putStrLn $ toString k ++ " -- " ++ toString v
>                 dbSeq db routineFlagNext >>= loop
>           in
>             dbSeq db routineFlagFirst >>= loop

For more flexibility in opening a hash table, use `dbHashOpeni`, which
accepts additional `HashInfo` data, to allow page and cache sizes to
be specified, e.g.  For even more flexibility (to add B-tree support,
etc.), use `dbOpen`.

For more flexibility in storing and fetching key-value pairs, use the
functions `dbGet` and `dbPut` for keys and values of type
`ByteString`.  The functions `dbGets` and `dbPuts` can be used for
keys of type `String` and values of type `a`, as long as there is an
instance of `Binary a` (see `Data.Binary`).  Very cool!

Therefore with `dbPuts` and `dbGets` one can also store and retrieve
integers values like this:

    dbPuts db "abc" (1 :: Int32)
    dbGets db "abc" :: IO (Maybe Int32)

Since the Haskell `String` type supports Unicode and Haskell source
file are assumed to be in UTF-8 encoding and Haskell IO honors
_locale_ environment variable settings, one can also write:

    dbPutss db "索引可用中文" "值也可是中文"
    
Nice, isn't it?

The following is a more advance example where values are of type `()`,
since the hash table is being used as a _set_ instead of a _map_ data
structure.  `BuildWordDb` reads and enters into the hash table all the
words in the standard Unix word list file `/usr/share/dict/words`.

> buildWordDb = do
>   db <- dbHashOpen "words.db" [flagRdWr, flagCreat, flagTrunc] [modeIRUSR, modeIWUSR]
>   withFile "/usr/share/dict/words" ReadMode $ \h -> do
>     c <- hGetContents h
>     mapM_ (\w -> dbPuts db w ()) (lines c)
>   dbClose db

`VerifyWordDb` verifies that each word in this word list file is
indeed in the hash table.

> verifyWordDb = do
>   db <- dbHashOpen "words.db" [flagRdOnly] [modeIRUSR]
>   r <- withFile "/usr/share/dict/words" ReadMode $ \h -> do
>     c <- hGetContents h
>     let dbGetsn = dbGets :: (Ptr Db) -> String -> IO (Maybe ())
>     foldM (\r w -> liftM2 (&&) (liftM isJust (dbGetsn db w)) (return r)) True (lines c)
>   dbClose db
>   return r

Now to exercise everything.

> main = do
>   -- test1
>   buildWordDb
>   verifyWordDb >>= print

This should print out: `True`.

So how does it perform?  To find out, I wrote [an equivalent
program][BDBTest.hs] using the [Database.Berkeley.Db][H-BDB] package.
Note that this program makes calls to the Berkeley DB 4.x API directly
and not through its 1.85 compatibility mode.  Note also how the API is
now more cumbersome to use.  The output of the `time` command is shown
below.  As expected the two implementations perform almost equally.

    $ /usr/bin/time -f "%E real,%U user,%S sys" ./DbTest
    True
    0:02.68 real,1.75 user,0.92 sys
    $ /usr/bin/time -f "%E real,%U user,%S sys" ./BDBTest
    True
    0:02.76 real,1.68 user,1.02 sys
    $

[BDBTest.hs]: BDBTest.hs

A bit more performance can be squeezed out from db by tuning the
bucket size, fill factor, and cache size by supplying a `HashInfo` to
`dbHashOpeni`.  The following set of parameter values more than half
the time required for `DbTest`, while maintaining approximately the
same size for the disk file written:

    let h = HashInfo 256 16 1 2097152 (castFunPtr nullFunPtr) 0
    db <- dbHashOpeni "words.db" _flags_ _modes_ h
    ...

Sample run with this change in both `buildWordDb` and `verifyWordDb`:

    $ /usr/bin/time -f "%E real,%U user,%S sys" ./DbTest
    True
    0:01.20 real,1.11 user,0.09 sys
    $

All tests were run on a 2.5 GHz Dual-Core Intel PC running Linux.

For me this has been a bit of extra work which has resulted from a
certain company's decision to change a software license for a classic
piece of code.  It did give me the motivation to learn a little more
Haskell, which is always a fun thing to do.  When life gives you
lemons, eh?
