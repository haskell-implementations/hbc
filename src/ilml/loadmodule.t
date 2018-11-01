#ifdef I_AOUT
import loadmodule:List Char->OK (List Char) (List((List Char) # Univ)) {# ARITY loadmodule = 1 #};
import loadmodules:List (List Char)->OK (List Char) (List((List Char) # Univ)) {# ARITY loadmodules = 1 #};
#else
#  define ERR (fail "no a.out routines")
#  define loadmodule ERR
#  define loadmodules ERR
#endif
