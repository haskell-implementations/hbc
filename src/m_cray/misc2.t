import mkconst: (Int->(List Char)) {# ARITY mkconst = 1 #}{# STRICTNESS mkconst = "0,F" ST #};
import mkglob: ((List Char)->(List Char)) {# ARITY mkglob = 1 #}{# STRICTNESS mkglob = "F,F" ST #};
import mklbl: ((List Char)->(Char->(List Char))) {# ARITY mklbl = 2 #}{# STRICTNESS mklbl = "0,F" ST #};
