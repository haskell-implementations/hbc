import apply: ((List Expr)->(Expr->Expr)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import simpl: (Int->((List (Id # Expr))->(Expr->(Int # Expr)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "2,F" ST #};
import Esimpl: (Int->(Expr->(Int # Expr))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
