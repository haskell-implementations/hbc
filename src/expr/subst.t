import substl: ((List (Id # Texpr))->(Texpr->Texpr)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import subst: (Texpr->(Id->(Texpr->Texpr))) {# ARITY _ = 3 #}{# STRICTNESS _ = "2,F" ST #};
