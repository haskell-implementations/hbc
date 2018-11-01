import addmets: (Renv->Renv) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import buildinsts: (*a->(Renv->((#3 (List (Id # (List (Id # IDecl)))) (List (Id # (List (Id # IDecl)))) *a) # (List (List Char))))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,T" ST #};
