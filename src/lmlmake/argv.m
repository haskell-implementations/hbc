module -- argv - parse command line
export modules,mklines,isopt,ifopt,ifnotopt,ifdebug,beQuiet;

rec isflag [] = false
 || isflag (c._) = c='+' | c='-'

and flags = (map tl o filter isflag) argv

and modules = filter (not o isflag) argv

and mklines = concmap (@"\n")

and isopt opt = mem opt flags

and ifopt opts s = if Or (map isopt ("d".opts)) then mklines s else ""

and ifnotopt opts s = if isopt "d" | not (Or (map isopt opts)) then mklines s else ""

and ifdebug = ifopt []

and beQuiet = isopt "s" | isopt "n"

end
