import transap: ((List Id)->(Expr->((List Expr)->Expr))) {# ARITY _ = 3 #}{# STRICTNESS _ = "0&2,F" ST #};
import apconv: (Expr->Expr) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
