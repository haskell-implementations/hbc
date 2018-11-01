import findloc: ((List ((List Char) # (String # Int)))->((List Char)->((List (List Char))->(List Char)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "2,F" ST #};
import merror: (Texpr->((List Char)->Texpr)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import errors: ((List ((List Char) # (String # Int)))->(Texpr->(List (List Char)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
