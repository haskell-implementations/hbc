import badu: ((List Char)->(Ttype->(Ttype->Subst))) {# ARITY _ = 3 #}{# STRICTNESS _ = "T,T" ST #};
import Unify: (Ttype->(Ttype->Subst)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
