import id_callmethod: (Int->Id) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,T" ST #};
import xflatsuper: ((Int->*a)->((*a->Int)->(Id->((List *a)->(List *a))))) {# ARITY _ = 4 #}{# STRICTNESS _ = "T,F" ST #};
import classtrans: (Expr->Expr) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
