import parallelcmds: ((List (List String))->(List String)) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import link: (String->((List String)->(List Char))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};
import compile: ((List Char)->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import ifolder: ((List Char)->((List (List Char))->((List Char)->(List Char)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "T,F" ST #};
