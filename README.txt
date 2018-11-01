Installing lmlc/hbc
-------------------
Send a mail to hbc@cs.chalmers.se and say that you have a copy.  This
way you will also be on the mailing list for lmlc/hbc.

If you have a previous version of the compiler you should remove
all installed files from that version before installing the new
one (or move it to a safe place).

Get and unpack the distribution (if you are reading this, you have
probably done that).  Then

        cd <whereever-you-unpacked-it>
        make BIN=<platform> install

where <platform> corresponds to the tar file you unpacked.  This will
install the executables in /usr/local/bin and the various other files
in /usr/local/lib/lmlc.  If you have the documentation you can also do
        make install-man
which install the manual pages in /usr/man/manl.

If you decide not to install the compiler in /usr/local/lib/lmlc you
must set the environment variable LMLDIR to whatever directory you
used instead to be able to compile.

*** WARNING
*** The new compiler is incompatible with older versions.  If you
*** have old files .o files around you have to remove them!
If you have recursive modules it may take a few iterations before
the interface files stablize.

Recompiling the compiler
------------------------
This is not necessary to just compile and run programs, but you may
want to do it to prepare for future upgrades in the form of patches.

The first time you do this you have to do the following steps:
0	Install the binaries as above, get and unpack the sources.
1	cd src
2	./configure
                Answer the questions.
                You must answer what kind of system you have
                and what you want to build.
3	make universe
                This will take quite a while.  Any warnings from the C
                compiler may be ignored.
4	If you want to make really sure that the new compiler is
        capable of recompiling itself, then run the script fixtst.
        This will recompile twice with successive new versions
        and compare them.  THIS TAKES A LOT OF TIME!

Running Haskell
---------------
Here's some advice for those of you that don't have the patience to
read the manual.  To run the interactive system just type hbi.
E.g.

dogbert% hbi
Welcome to interactive Haskell B. 1.3 version 0.9999.0 Pentium 1996 Apr 17!
Loading prelude... 1559 values, 194 types found.
Type "help;" to get help.
> 1+2;
3

> let inc x = x+1;;
inc :: (Prelude.Num a) => a -> a
> inc 6;
7

> asin (2 :+ 0);
1.5707963267948966 :+ (-1.3169578969248166)

> let fac 0 = 1
#     fac n = n * fac(n-1)
# ;
fac :: (Prelude.Num a) => a -> a
> fac 100;
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000

> whatis (+);
method (+) :: (Prelude.Num b) => b -> b -> b
> import Sort;
Loading "/usr/local/lib/lmlc/hbc_library1.3/Sort.o"
> whatis Sort;
module Sort
  sort :: (Prelude.Ord b) => [b] -> [b]
  sortBy :: (b -> b -> Bool) -> [b] -> [b]
> nub (sort "the quick brown fox jumps over the lazy dog");
" abcdefghijklmnopqrstuvwxyz"

> quit;
Bye
dogbert%

The compiler is called hbc.  It has the about the same flags as
ordinary UNIX compiler.  If the file Main.hs contains

main = putStr "ahoy, przygodo\n"

then it can be compiled and run by

dogbert% hbc Main.hs
dogbert% a.out | pol2eng
hello, world
dogbert%

To avoid having to keep track of dependencies and also
writing Makefiles you should use hbcmake which figures
out what has to be done automatically.

dogbert% hbcmake Main
hbc -c Main.hs
hbc -o Main Main.o
dogbert% Main
ahoy, przygodo
dogbert%


Bugs and problems
-----------------
Send reports to hbc@cs.chalmers.se.


        Share and enjoy!
        -- Lennart Augustsson
