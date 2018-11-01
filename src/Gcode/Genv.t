import listenv: (Genv->(Int->(List Id))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import addenv: (Genv->((List Id)->(Int->(Genv # Int)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "0,0" ST #};
import lookenv: (Genv->(Id->Int)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import nilenv: Genv {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
