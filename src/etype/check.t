import prposs: ((List (#3 (List Texpr) Subst Ttype))->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import Wp: (Prefix->(Texpr->(Ttype->(Int->((List (#4 (List Texpr) (List (Id # Ttype)) Subst Ttype)) # Int))))) {# ARITY _ = 4 #}{# STRICTNESS _ = "1,F" ST #};
import W: (Prefix->(Texpr->(Ttype->(Int->((List (#3 (List Texpr) Subst Ttype)) # Int))))) {# ARITY _ = 4 #}{# STRICTNESS _ = "1,F" ST #};
import Wb: (Prefix->(Binding->(Int->(#4 (List Texpr) (List (Id # Ttype)) Subst Int)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "1,F" ST #};
