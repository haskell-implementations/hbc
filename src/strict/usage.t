import compr: ((List Bool)->((List *a)->(List *a))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import usageanal: (Expr->(Expr->((List (Id # (#3 Int BT BT)))->Expr))) {# ARITY _ = 3 #}{# STRICTNESS _ = "1,F" ST #};
