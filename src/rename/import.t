import importe: ((String->Bool)->(String->(Int->(Renv->((List Import)->((List Import)->(#3 Int Renv String))))))) {# ARITY _ = 6 #}{# STRICTNESS _ = "T,F" ST #};
import badmodname: (Id->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import importenv: (Id->(Renv->(Int->((List Import)->(#3 Int Renv String))))) {# ARITY _ = 4 #}{# STRICTNESS _ = "0,F" ST #};
