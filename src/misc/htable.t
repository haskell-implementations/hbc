import lookuphtbl: ((LArray (List ((List Char) # *a)))->((List Char)->(*a->*a))) {# ARITY _ = 3 #}{# STRICTNESS _ = "0&1,F" ST #};
import makehtbl: ((List ((List Char) # *a))->(LArray (List ((List Char) # *a)))) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
