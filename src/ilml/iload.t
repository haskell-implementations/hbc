import stripextension: ((List Char)->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import baseName: ((List Char)->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import dloadmodules: ((List String)->(OK String (List (Symbol String Univ)))) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import dloadmodule: (String->(OK String (List (Symbol String Univ)))) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import ccload: (State->((List String)->(Import->(OK String (Renv # State))))) {# ARITY _ = 3 #}{# STRICTNESS _ = "T,F" ST #};
import dconvies: (Renv->((List (Symbol String Univ))->(List (Symbol Id Cexpr)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import cload: (State->((List String)->((List Impid)->(OK String (Renv # State))))) {# ARITY _ = 3 #}{# STRICTNESS _ = "T,F" ST #};
