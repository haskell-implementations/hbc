import concstr: (Expr->Expr) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import xgetid: (Expr->(List Id)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import refc: ((List Id)->(List (Id # Int))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import isc: (Expr->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import value: (Expr->Int) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import cval: (Constr->Int) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
