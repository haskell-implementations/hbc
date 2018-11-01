import Ealphasubst: ((List (Id # Id))->(Expr->Expr)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import Esubst: (Expr->(Id->(Expr->Expr))) {# ARITY _ = 3 #}{# STRICTNESS _ = "2,F" ST #};
