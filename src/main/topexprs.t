import curmod: String {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import pragmas: (List (Id # (List Pragma))) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import preenv: Renv {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import startu: Int {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import allinsts: (List Id) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import unum1: Int {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import expr2: Texpr {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import expr1: Texpr {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import expr0: Texpr {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import errmap: (List ((List Char) # ((List Char) # Int))) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
