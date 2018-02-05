T2

T2 is a program that performs roman numeral analysis on jazz chord
charts in a notation similar to

  B. Nettles and R. Graf.  The Chord Scale Theory and Jazz Harmony.
  Advance Music.  1997.

and

  A. Jaffe.  Jazz Theory.  Wm. C. Brown Company Publishers.  1983.

T2 is part of my on-going "computational jazz" project and will be
updated from time to time.  Another useful program in the "suite" is
MJB2Lite, a MIDI application for generating jazz accompaniments.
Information about and downloads for current and future versions of T2,
MJB2Lite, and other programs can always be found at my website:

  http://www.sixthhappiness.ca/index.html

That website also contains my blog, discussion forums, my bio, and a
lot of other information.


FILE FORMAT

T2 reads chord charts in the "TOE" format: text files with extensions
".toe".  Here is an example of the contents of such a file:

| FMaj7 | Em7b5 A7 | Dm7 G7 | Cm7 F7 |
| BbMaj7 | Bbm7 | Am7 | Abm7 Db7 |
| Gm7 | C7 | FMaj7 D7 | Gm7 C7 |
| FMaj7 |

You can type your own chord charts into a text editor and save the
file in "plain text" (ASCII) format with the ".toe" extension.  The
Mac versions of T2 are applications that can read and edit ".toe"
documents, so a text editor is not needed.  The Windows and "Other
OSes" versions are command-line program so the input chord charts must
be created with a text editor.

If you want to use chord charts written by the Band-in-a-Box program
(you can find an abundance of these files on the Internet), download
my ChartTranslate program (from my website, see above).  It will let
you convert Band-in-a-Box files into TOE format.

Currently T2 can only analyze chord charts in 4/4 time.


USAGE

On Mac OS X (tested only under 10.4.10), the downloaded and unzipped
folder T2-OSX.PPC or T2-OSX.Intel should contain the application
T2.ppc or T2.intel.  Be sure to download the correct version according
to whether you have a PowerPC or Intel Mac.

Drag and drop one or more input files in the TOE format (e.g., the
included sample input files "Blues40.toe" and "Solar.toe") onto this
application icon.  A "document" window should open for each file.
Each document window will contain an upper panel, showing the file's
contents, and a lower panel, showing the analysis which T2 generates
for that file.  The input may be edited and saved (just like in a
"regular" text editor).  Hit the "Analyze" button to perform a new
analysis on an modified document.  Text in the lower panel can be
selected and copied-and-pasted, or dragged-and-dropped to the Finder
(to create "clippings") or to other applications.

On Windows (tested only on Windows XP), the downloaded and unzipped
folder "T2-Windows" should contain the program "t2.exe".  Invoke this
program from the command line and give it the name of the input files
as parameter:

  t2 <inputfile> ...

The analysis will be performed and displayed.

On other systems (such as Linux and BSD), you need to have OCaml
3.10.0 installed (http://caml.inria.fr/).  The downloaded, gunzipped,
and untarred directory "T2-Other" should contain the file t2.byte.  In
a shell, type:

  ocamlrun t2.byte <inputfile> ...

Alternatively if your OCaml bytecode interpreter has been installed as
/usr/local/bin/ocamlrun, you can make t2.byte executable using
"chmod".  Then type:

  ./t2.byte <inputfile> ...

If you have questions, please ask them in my discussion forums or
write to me (please read the guidelines on the "Contact" page at my
website).

Enjoy the T2 program!

Andrew Choi.
akochoi (at) shaw (dot) ca
