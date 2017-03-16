% Haskell, Literate Programming, Pandoc, Carbon XEmacs, ...
% Andrew Choi
% 28 October 2009

Haskell
=======

I have been studying and using the programming language [Haskell][] in
the past two months.  I [wrote about][OCamlArticle] how great OCaml
was when I used it to implement and experiment with harmonic analysis
algorithms for [my jazz harmonic analysis paper][HarmAnaPaper].  I
found OCaml to be highly expressive and the compiled code to be very
efficient.  Having rewritten these algorithms in Haskell, I must say I
now love Haskell equally, and perhaps even a little more for its
elegance.

[Haskell]: http://www.haskell.org/
[OCamlArticle]: ../ocaml-python-scheme-tuareg-mode-camldebug-and-carbon-xemacs/index.html
[HarmAnaPaper]: ../jazz-harmonic-analysis-paper/index.html

Ultimately the main criteria for choice of a programming language are
expressiveness and efficiency of code written in it.  Expressiveness
makes it easy (or at least not unpleasant) to code in that language.
Efficiency because time and effort are not wasted on writing code that
are inherently slow, like so many programming languages that have
become popular in recent years.  When I [was choosing][SpeedArticle] a
programming language for my harmonic analysis work, I noticed that
both OCaml and Haskell ranked very high in [a certain set of
benchmarks][Benchmarks].  Both are modern functional programming
languages and seem very expressive.  At the time I thought OCaml was a
little "less weird" than Haskell, partly because of its imperative
language features, which would be nice to have to fall back on "just
in case".  But over the past few weeks I've rewritten my [T2
algorithm][T2] in Haskell and reimplementing things like dynamic
programming/array construction and I/O in it was both fun and led to
more readable code.  I don't miss statement sequencing, *for* loops,
mutable arrays, etc. at all!  I highly recommend anyone who's starting
a new programming project to take a close look at Haskell.

[SpeedArticle]: ../python-scheme-and-ocaml-speed-comparison/index.html
[Benchmarks]: http://shootout.alioth.debian.org/gp4/index.php
[T2]: http://members.shaw.ca/akochoi-T2/jazz-harmonic-analysis/index.html

Here are a few places to look to get started with Haskell.  Install
[GHC][], the Glasgow Haskell Compiler.  For Mac OS X grab the
installer package.  Since the Debian Haskell packages are lagging
behind and for no good reason divide the hierarchical libraries into
many small pieces, for Debian-based Linux systems also grab the binary
package and don't install through the Linux package managers.  Then
you can use Cabal, Haskell's own build and package system, to install
additional Haskell tools such as [Pandoc][].

[GHC]: http://www.haskell.org/ghc/
[Pandoc]: http://johnmacfarlane.net/pandoc/

For an introduction to Haskell, it's tempting to recommend the [Real
World Haskell book][RWH], because one can read it online for free, and
a number of [Haskell tutorials][Tutorials].  However, [a Gentle
Introduction to Haskell 98][Gentle] is much better written, and
shorter.  Together with the [Haskell 98 Report][Report], it will
provide basically everything you need to know.

[RWH]: http://book.realworldhaskell.org/
[Tutorials]: http://www.haskell.org/haskellwiki/Tutorials
[Gentle]: http://www.haskell.org/tutorial/
[Report]: http://www.haskell.org/haskellwiki/Language_and_library_specification

At some point you'll wonder how much you need to understand about
*monads* to proceed.  So many people have tried to write about them
that there's a [monad tutorials timeline][MonadTimeline]!  Why not get
it over with at the very beginning by reading [Wadler's
paper][Wadler], which is well-written and more comprehensible than any
of the tutorials.  If a more practical approach is preferred,
certainly first learn to do I/O, then read some sample code for and
learn to use a library that uses monads, such as [Parsec][].  Another
hurdle you'll probably encounter is to understand *laziness* in
Haskell.  The difference in execution model and a lack of intuition on
*what* gets optimized *when* can cause difficulties in writing
efficient programs.  I've found the [Haskell Wiki's Performance
section][WikiPerformance] to be useful.  So are Jones and Marlow's
paper [Secrets of the Glasgow Haskell Compiler inliner][Inline] and
Jones et al.'s paper [The Glasgow Haskell compiler: a technical
overview][TechOverview].  You can also find [many other
reports][Compilation] on compilation of Haskell programs.

[MonadTimeline]: http://www.haskell.org/haskellwiki/Monad_tutorials_timeline
[Wadler]: http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
[Parsec]: http://legacy.cs.uu.nl/daan/parsec.html
[WikiPerformance]: http://www.haskell.org/haskellwiki/Performance
[Inline]: http://research.microsoft.com/en-us/um/people/simonpj/Papers/inlining/
[TechOverview]: http://research.microsoft.com/~simonpj/Papers/grasp-jfit.ps.Z
[Compilation]: http://www.haskell.org/haskellwiki/Research_papers/Compilation


Literate Programming, Pandoc, and Carbon XEmacs
===============================================

Once GHC is installed and set up, one will need a program editor for
writing Haskell code.  On my Mac I of course use [Carbon XEmacs][],
and in Linux I use (GTK) GNU Emacs.  You'll also want to set up
[Haskell mode][].  Unfortunately the Haskell mode code is slightly
incompatible with XEmacs 21.5.29 so a few trivial changes must be made
to make it run on XEmacs.

[Carbon XEmacs]: http://members.shaw.ca/akochoi-xemacs/
[Haskell mode]: http://www.haskell.org/haskellwiki/Haskell_mode_for_Emacs

If you also want to write [literate Haskell programs][LitHaskell],
you'll need to decide on which typesetting/text markup language to
use.  Many people use LaTeX through tools like [lhs2TeX][] and
[SimpleLit][].  If you're a bit adventurous, or need to include other
markup or programming languages, go with [Noweb][].

[LitHaskell]: http://www.haskell.org/haskellwiki/Literate_programming
[lhs2TeX]: http://people.cs.uu.nl/andres/lhs2tex/
[SimpleLit]: http://www.cit.gu.edu.au/~arock/SimpleLit/SimpleLit.pdf
[Noweb]: http://www.cs.tufts.edu/~nr/noweb/

I started out by experimenting with lhs2TeX and spending some time
configuring [MMM mode][] for Emacs so it will support both LaTeX and
Haskell code in the same buffer.  Even had to fix Carbon XEmacs so
AUCTeX mode will work correctly in it.  After doing all that, I was
quite disappointed to find that MMM mode doesn't really work all that
well.  The biggest problem is that regions in submodes aren't indented
correctly.  Indentation support is one of the main reasons why one
would want different modes for different regions in a buffer to begin
with!  The need for and messiness of a minor mode such as MMM in Emacs
points to the importance of integrating multi-language, indentation,
and syntax coloring support into the core design of a program editor.
But that'll be another article for another time.  For the problem at
hand, my current solution, although far from perfect, is to use MMM
mode with [Markdown mode][] (more on Markdown below) as the main mode
and literate Haskell mode as the submode to edit the "commentary"
parts of programs, so syntax coloring work correctly for the entire
buffer.  Then I switch manually to literate Haskell mode when I need
to edit code.

[MMM Mode]: http://mmm-mode.sourceforge.net/
[Markdown mode]: http://jblevins.org/projects/markdown-mode/

There are a few disadvantages in using LaTeX for literate programming.
I've generally found that it requires too much concentration to be
typesetting LaTeX while writing code in Haskell (I am getting old you
know :-)).  Also, although the typeset end result may look great, the
source code isn't too pretty to edit and look at!  Another
consideration is if one wishes to post a literate program to the Web,
or convert it to some other formats, LaTeX usually doesn't do very
well.  [Many tools][Converters] exist for converting LaTeX to HTML.
But since LaTeX is quite a complicated language, these converters have
many restrictions and none really generates aesthetically pleasing
results.

[Converters]: http://tug.org/utilities/texconv/textopc.html

In fact the same kind of problems exist in marking up articles such as
the one you're currently reading for publishing on the Web.  One
certainly doesn't need the full power of HTML, XML, or LaTeX for that!
Up until now, I have written all the articles on this website directly
in XHTML.  I've always found HTML and XML to be a clumsy
representation to write and edit manually.  But I'm very attached to
the idea of having XHTML-formatted articles so my entire website can
be [generated by a handful of XSLT templates using
xsltproc][XSLTTemplates], or in fact completely on the client side if
I only post the `.xml` files.

[XSLTTemplates]: ../xslt-website-templates/index.html

So, why not solve both problems with one solution: the [Markdown][]
markup language?  Markdown is a tiny language for formatting web
articles.  Its design goal is that formatted sources should be as
(humanly) readable as possible, much like how one would format text
Email messages.  As an example, you can view the [Markdown-formatted
source][Source] that was used to produce this article.  You can see
that it's indeed very "plain text-like".  A number of Markdown
processors are available.  I recommend [Pandoc][], which has a very
useful feature (see below) and interestingly is written in Haskell.
Having adopted Pandoc and the Markdown format, I need to modify my
work flow and tools to run Pandoc on the source of my Web articles.
I've found it easiest to put the processing commands in a simple
[Makefile][].  A Emacs Lisp function bound to a function key then
automatically saves the file, invokes `make`, and previews the output
in a web browser.

[Markdown]: http://daringfireball.net/projects/markdown/
[Source]: contents.lhs
[Makefile]: Make_file.txt

So let's get back to the choice of a markup language for literate
programming in Haskell.  It turns out that [Pandoc supports literate
Haskell][PandocHaskell] really well.  The Markdown format has just
enough features for writing commentaries in programs.  When it is
insufficient, HTML and LaTeX code can be used directly.  Pandoc will
even colorize your code in HTML output if [built with syntax
highlighting support][PandocHighlight] (which is easy to do using
Cabal).

[PandocHaskell]: http://johnmacfarlane.net/pandoc/README.html#literate-haskell-support
[PandocHighlight]: http://johnmacfarlane.net/pandoc/INSTALL.html#optional-syntax-highlighting-support

Here's a little demonstration of a Haskell function `isqrt` that
computes the square root of an integer using Newton's method.

> isqrt n =
>   isqrt' n
>     where
>       isqrt' s =
>         let u = (s + n `div` s) `div` 2
>         in
>           if u >= s then s else isqrt' u

So the [source file of this article][Source] is a literate Haskell
program that can be loaded directly into the GHC interpreter (or
compiler) to define the function `isqrt`.  At the same time it is the
Markdown formatted source that generates the HTML file you're now
reading.  All that in a markup language that's as readable as a text
Email message!

I'd consider that pretty cool, wouldn't you?

OK, if you've read this far, I think you deserve some reward (you also
have way too much time!).  So, here is some useful and exclusive
information.  The GHC 6.10.4 compiler `ghc` does not work under Snow
Leopard out of the box.  The fix for this is not difficult to [find on
the Web][GHCFix].  Simply edit the file `/usr/bin/ghc` and add the
options

    -optc-m32 -opta-m32 -optl-m32

to the `exec` command.  The difficult part is the installation of
Pandoc.  To do that, download `Cabal-1.6.0.2`, `HTTP-4000.0.8`,
`zlib-0.5.2.0`, and `cabal-install-0.6.2` from the [Hackage site][]
and install them by hand in that order.  For each package, `ungzip`,
`untar`, then at the top directory, do:

    runhaskell Setup configure --ld-options="-arch i386" --gcc-option=-m32 --user
    runhaskell Setup build
    runhaskell Setup install

At this point use `cabal` to install `utf8-string` and `zip-archive`:

    cabal install utf8-string
    cabal install zip-archive

Then install Pandoc:

    cabal install pandoc --flags=highlighting

This will build Pandoc with highlighting support, which will colorize
your Haskell code as demonstrated by the `isqrt` function above.

Have fun!!

[GHCFix]: http://www.mail-archive.com/haskell-cafe@haskell.org/msg64017.html
[Hackage site]: http://hackage.haskell.org/packages/archive/pkg-list.html
