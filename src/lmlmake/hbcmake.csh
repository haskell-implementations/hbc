#! /bin/csh -fe
# hbcmake - make for Haskell programs
# Author: Thomas Hallgren, hallgren@cs.chalmers.se

if ($?LMLDIR) then
	set lib = $LMLDIR/bin
else
	set lib = /usr/local/lib/lmlc/bin
endif

#set exec = 'csh -ef'
set exec = 'sh -e'
set hversion
set usehlibs

set modules = ( )
set flags = ( )
set rtflags = ( )       # run-time flags for lmlmk
set xdgflags = ( )

setenv LMLMK_LANG hbc

if ( ! $?LMLMK ) setenv LMLMK $lib/lmlmk
if ( ! -x $LMLMK ) setenv LMLMK /usr/local/lib/lmlc/bin/lmlmk

if ( ! $?OLDER ) setenv OLDER $lib/older
if ( ! -x $OLDER ) setenv OLDER /usr/local/lib/lmlc/bin/older

#if ( ! $?HBC ) setenv HBC $lib/hbcmakehbc
#if ( ! -x $HBC ) setenv HBC /usr/local/lib/lmlc/bin/hbcmakehbc
if ( ! $?HBC ) setenv HBC hbc

setenv LMLINCLUDE .	# list of source directories (misleading var name!!)

if ( ! $?LMLDIR ) setenv LMLDIR /usr/local/lib/lmlc	# Or vlmlc ...
if ( ! $?HBCINCPATH ) then
  setenv HBCINCPATH ''
else
  unset usehlibs
endif


# Take compiler/linker flags from the environment
if ( ! $?HBCFLAGS ) then
  setenv HBCFLAGS ''
endif
if ( ! $?LDFLAGS ) then
  setenv LDFLAGS ''
endif

while ( $#argv > 0 )
  switch ( $1 )
    case -1.2:
      set hversion = -1.2
      breaksw
    case -1.3:
      set hversion = -1.3
      breaksw
    case -xdg:
      # Show dependency graph with xhbdg instead of compiling
      set xdg
      breaksw
    case -t:
    case -g
      set quiet
    case -P:
    case -debug:
    case -n:
      set exec = cat
    case -s:
      set flags = ( $flags $1 )
      breaksw
    case -[AHhBSX]*:
    case -gc*:
    #Any more useful run-time flags?
      set rtflags = ( $rtflags $1 )
      breaksw
    case -f:
      shift
      set makefile = $1
      breaksw
    case -d:
      #-d switches on distributed make. Needs distcom to work.
      setenv LMLMKDISTR ''
      breaksw
    case -C:
      shift
      setenv HBCFLAGS "$HBCFLAGS $1"
      breaksw
    case -nd:
    case -strip:
      breaksw
    case -nostrip:
      set nostrip
      breaksw
    case -[xy]space:
      set xdgflags = ( $xdgflags $1 $2 )
      shift
      breaksw
    case -o:
      shift
      setenv LDFLAGS "$LDFLAGS -o $1"
      breaksw
    case *.so:
    case *.a:
    case -l*:
      setenv LDFLAGS "$LDFLAGS $1"
      breaksw
    case -i:
      unset usehlibs
      breaksw
    case -i*:
      setenv HBCINCPATH "`echo $1 | sed -e s/-i//`:$HBCINCPATH"
      breaksw
    case -I:
      shift
      setenv LMLINCLUDE "${LMLINCLUDE}:$1"
      setenv HBCFLAGS "$HBCFLAGS -i$1"	# should not be used when linking !!!
      breaksw
    case -T:
      setenv HBCFLAGS "$HBCFLAGS $1"
      set nostrip
      breaksw
    case -*:
      setenv HBCFLAGS "$HBCFLAGS $1"
      breaksw
    default:
      set modules = ( $modules $1 )
      breaksw
  endsw
  shift
end

if ( $?usehlibs ) then
  if ($hversion == -1.2) then
    setenv HBCINCPATH "$HBCINCPATH":.:$LMLDIR/hbc_library
  else
    setenv HBCINCPATH "$HBCINCPATH":.:$LMLDIR/hbc_library1.3:$LMLDIR/hlib1.3/
  endif
endif

if(! $?nostrip) setenv LDFLAGS "$LDFLAGS -s"

if ( $#modules < 1 ) then
  echo 'Usage: hbcmake [-n] [-t] [-g] [-s] [-d] [-f makefile] modules'
  exit 1
endif

if ( ! $?makefile ) then
  if ( -r Makefile ) then
    set makefile = Makefile
  else
    set makefile = makefile
  endif
endif

# Also try to extract compiler/linker flags from a Makefile
if ( -r $makefile ) then
  setenv HBCFLAGS "$HBCFLAGS `grep '^[ ]*HBCFLAGS[ 	]*=' $makefile | sed 's/.*=//'`"
  setenv LDFLAGS "$LDFLAGS `grep '^[ ]*LDFLAGS[ 	]*=' $makefile | sed 's/.*=//'`"
endif

setenv HBCFLAGS "$hversion $HBCFLAGS"

#if ( ! $?quiet ) then
#  echo setenv HBCFLAGS $HBCFLAGS
#  echo setenv LDFLAGS $LDFLAGS
#  echo setenv OLDER $OLDER
#  echo setenv HBCINCPATH $HBCINCPATH
#endif

#Here we go...
if ( $?xdg ) then
  echo xhbdg $rtflags $modules $xdgflags
  xhbdg $rtflags $modules $xdgflags
else
  $LMLMK $rtflags - $flags $modules | $exec
endif
