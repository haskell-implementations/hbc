import sccds: ((List (Id # Expr))->(List (Bool # (List (Id # Expr))))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import unrec: (Expr->Expr) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
