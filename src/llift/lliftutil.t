import deltype: ((*a # Expr)->((Id # Expr)->(Id # Expr))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import strictprimitive: (Id->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import solve: ((List (#3 Id (List Id) (List Id)))->(List (Id # (List Id)))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import funsof: ((List (Id # Expr))->(List Id)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import varsof: ((List (Id # Expr))->(List Id)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import idexp: (Id->Expr) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,T" ST #};
import numid: (Int->(Id->Id)) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,T" ST #};
