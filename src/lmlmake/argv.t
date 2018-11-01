import beQuiet: Bool {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import ifdebug: ((List (List Char))->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import ifnotopt: ((List (List Char))->((List (List Char))->(List Char))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};
import ifopt: ((List String)->((List (List Char))->(List Char))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};
import isopt: ((List Char)->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import mklines: ((List (List Char))->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import modules: (List (List Char)) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
