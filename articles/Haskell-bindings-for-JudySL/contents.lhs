% Haskell Bindings for JudySL
% Andrew Choi
% 13 June 2010

Judy Arrays
===========

[Judy arrays][] are really just compressed tries that [integrates a
collection of techniques][JudyShopManual] to achieve a high degree of
compression.  Compressed tries are far from new; I used one [when it
was new thirty years ago][C-trie] in a spelling checker.  Compression
techniques have certainly evolved _a lot_ in Judy arrays.  It
shouldn't be surprising however that, as our computers can now hold
most or all the data structures of programs in main memory, techniques
like compressed tries are being rediscovered all over again.

[Judy arrays]: http://judy.sourceforge.net/
[JudyShopManual]: http://judy.sourceforge.net/doc/shop_interm.pdf
[C-trie]: http://doi.acm.org/10.1145/360248.360258

In a [previous article][DbArticle], I described the use of the hash
table functions in the Db library as an associative array in a
program.  It worked OK, but I continued to look for a data structure
that might be more time and space efficient.  The Judy library seemed
to be a good candidate because it is often mentioned in answer to
complaints about poor performance of the `Data.HashTable` module on
Haskell mailing lists.

[DbArticle]: ../Db-1.85-the-original-BSD-license-Berkeley-DB/index.html

So I looked at two Haskell packages: [HSJudy][] and [judy][].  The
first one has a very general design and parts of its interface beyond
the basic functionalities are difficult to understand due to a lack of
documentation.  Looking at its source code, the rationale for some of
it design choices seem unclear and unnatural (e.g., why not
`Data.Binary` and `ByteString` instead of `Stringable` and `String`?
And why does one need to `freeze` before lazy IO?  Why a mini GC?).  I
tinkered with its code a little but decided it wasn't worth the
effort.  The second one covers only [`JudyL`][JudyL], which maps
integer keys to integer values.

I need an interface for [`JudySL`][JudySL], a map from keys that are
variable-length byte sequences to integer values.  My solution was (of
course) to write one.  The result is this source file:
[`JudySL.hsc`][JudySL.hsc].

[HSJudy]: http://hackage.haskell.org/package/HsJudy
[judy]: http://hackage.haskell.org/package/judy
[JudyL]: http://judy.sourceforge.net/doc/JudyL_funcs_3x.htm
[JudySL]: http://judy.sourceforge.net/doc/JudySL_funcs_3x.htm
[JudySL.hsc]: JudySL.hsc

This FFI is quite simple to use.  The function `new` returns a new,
empty JudySL array, which is equivalent to initializing a pointer with
a `NULL` value in C code (the representation for an empty Judy array).
The "foreign pointer" returned by `new` has the added advantage of
calling `JudySLFreeArray` automatically (to free the Judy array) when
Haskell determines that it is no longer in use.  The functions `put`,
`get`, and `del` are equivalent to their C counterparts.  So are the
functions `first`, `last`, `next` and `prev`.  For convenience,
versions of these functions with the suffix `s` are supplied (`puts`,
`gets`, etc.); they operate on `String` instead of `ByteString` type
keys.

The convenience function `update` applies a function to change the
value corresponding to a key if the latter exists in the JudySL array.
Otherwise it does nothing.  `Update0` also changes the value for a
key, except when it doesn't exist, its value is first initialized to
zero before the update function is applied.  Its only use is probably
to implement some form of "counters", such as frequency counters for
_n_-grams.  Versions of these two functions with the `s` suffix are
also provided.

There are also convenience functions for converting entire Judy arrays
to and from lists of key-value pairs.  The `toList` function returns
its results _lazily_, so a Judy array can be written to disk (say)
very efficiently even when only a small part of main memory remains
free for heap allocation.

So how well do Judy arrays work?  Here's a version of the program I
used to test my Db 1.85 FFI and `Database.Berkeley.Db` in [my last
article][DbArticle], now modified to use this JudySL FFI.

> module Main where
> 
> import System.IO
> import Data.Maybe (isJust)
> import Control.Monad (foldM, mapM_, liftM2, liftM)
> 
> import qualified JudySL as J

It reads all the words in the standard Unix word list file
`/usr/share/dict/words`, builds a Judy array with them, and writes it
to the file `words.db`.

> buildWordDb = do
>   j <- J.new
> 
>   withFile "/usr/share/dict/words" ReadMode $ \ h -> do
>     c <- hGetContents h
>     mapM_ (\ w -> J.puts j w 1) (lines c)
> 
>   withFile "words.db" WriteMode $ \g ->
>     J.toList j >>= mapM_ (\ (k, _) -> hPutStrLn g k)

Then it reads the word list file again and verifies that each word in
it is indeed in the Judy array.

> verifyWordDb =
>   withFile "words.db" ReadMode $ \ h -> do
>     c <- hGetContents h
>     j <- J.fromList [(w, 1) | w <- lines c]
>     withFile "/usr/share/dict/words" ReadMode $ \ h -> do
>       c <- hGetContents h
>       foldM (\r w -> liftM2 (&&) (liftM isJust (J.gets j w)) (return r))
>             True
>             (lines c)

Here's the main function which should print `True` when it is run.

> main = do
>   buildWordDb
>   verifyWordDb >>= print

The running time for the tuned, Db 1.85 version of this program was:

    $ /usr/bin/time -f "%E real, %U user, %S sys" ./DbTest
    True
    0:01.20 real, 1.11 user, 0.09 sys
    $
    
The running time for the above program that uses Judy array is:

    $ /usr/bin/time -f "%E real, %U user, %S sys." ./JudyTest
    True
    0:00.60 real, 0.57 user, 0.02 sys.
    $ 

So a Judy array uses half the time required by the Db hash table
version.  And it doesn't required any tuning!  The `words.db` file
written by `JudyTest` is simply a text file and contains the same text
as the word list file (in my case 931708 bytes).  The `words.db` file
used by `DbTest` is a hash table file that is 2.5 Mb in size.  This
makes Judy arrays' performance quite impressive really.

Just for fun, here's a [Python version of the same
program][BDBTest.py] that uses the package [`bsddb3`][bsddb3]--the
Python bindings for Berkeley DB 4.x.  Here's how it performs:

    $ /usr/bin/time -f "%E real, %U user, %S sys." python3 BDBTest.py
    0:02.63 real, 1.40 user, 1.16 sys.
    $ 
    
[BDBTest.py]: BDBTest.py
[bsddb3]: http://pypi.python.org/pypi/bsddb3/5.0.0

It's only about 4x slower than the Haskell/Judy array solution, 2x
slower than the tuned Haskell/Db solution, and slightly worse than the
untuned Haskell/Db and Haskell/Berkeley DB solutions.  Not so bad
actually for an interpreted and dynamically typed language.  But note
that the bottleneck of the present task lies in the library routines
being called.  The results therefore reflect the relative performance
of Berkeley DB 4.x, Db 1.85, and Judy.  The size of the hash table
written by this Python version is also 2.5 Mb in size.
